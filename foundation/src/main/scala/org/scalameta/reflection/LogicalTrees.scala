package scala
package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.reflect.internal.{Flags, HasFlags}
import scala.reflect.internal.Flags._
import scala.collection.mutable
import _root_.org.scalameta.invariants._
import _root_.org.scalameta.unreachable

trait LogicalTrees {
  self: _root_.org.scalameta.reflection.GlobalToolkit =>

  import global.{require => _, _}
  import definitions._
  import treeInfo._
  import build._

  // NOTE: The idea behind LogicalTrees is to provide a layer that undoes
  // anti-syntactic ensugarings and encodings of scalac (i.e. ones that make scala.reflect trees
  // lose resemblance to the original syntax of the program that they are modeling).
  //
  // The motivation for existence of this layer is the desire to make
  // the scala.reflect > scala.meta converter as modular as possible.
  // It turns out that it's really easy to turn the converter into spaghetti,
  // so we need to be vigilant towards this danger. The approach that I like the most for now
  // is to split the converter into: 1) LogicalTrees, 2) ToMtree.
  // The former is this later, the latter is a trivial pattern match of the form:
  // `case l.Something(a, b, c) => m.Something(convert(a), convert(b), convert(c))`.
  //
  // Now there can be multiple ways of exposing the extractors for ToMtree,
  // but here I'm opting for one that has the lowest overhead:
  // no intermediate data structures are created unless absolutely necessary,
  // so the only thing that we have to do is to write Something.unapply methods and that's it
  // (later on, we might optimize them to use name-based pattern matching ala Dmitry's backend interface).
  //
  // Guidelines for creating extractors:
  //
  // 1) Most m.Something nodes will need one or, much more rarely, more l.Something extractors that
  //    should mirror the fields that m.Something accepts. For instance, the l.ClassDef extractor
  //    returns not the 4 fields that are present in g.ClassDef (mods, name, tparams, impl),
  //    but rather the 5 fields that are present in m.Defn.Class (mods, name, tparams, ctor, template).
  //
  // 2) Intermediate data structures should only be created when the correspoding scala.reflect concept
  //    can't be modelled as a tree (and therefore can't be processed in a modular fashion, because
  //    it can't have attachments, which at the very least means that it can't have parent links).
  //    E.g. we don't have `case class PrimaryCtorDef(...)`, because all the necessary information
  //    can be figured out from the corresponding g.DefDef (possibly requiring navigation through parent links).
  //    But we do have `case class Modifiers(...)`, because g.Modifiers isn't a tree, so we can't really
  //    adorn it with additional metadata that will help the converter.
  //
  // 3) Since we generally don't create intermediate data structures, the unapply methods in extractors
  //    should have comments that explain the fields that they are extracting.
  //
  // Source code that one might find helpful in implementing extractors:
  //
  // 1) Ensugar.scala (the resugaring module of the previous implementation of scalahost).
  //    Contains several dozen clearly modularized recipes for resugarings.
  //    Want to know what synthetic members are generated for lazy abstract vals?
  //    What about default parameters on class constructors? It's all there.
  //    https://github.com/scalameta/scalahost/blob/92b65b841685871b4401f00456a25de2b7a177b6/foundation/src/main/scala/org/scalameta/reflection/Ensugar.scala
  //
  // 2) ToMtree.scala (the converter module of the previous implementation of scalahost).
  //    Transforms scala.reflect's encodings of Scala syntax into scala.meta.
  //    Contains knowledge how to collapse multipart val/var definitions and other related stuff.
  //    https://github.com/scalameta/scalahost/blob/92b65b841685871b4401f00456a25de2b7a177b6/interface/src/main/scala/scala/meta/internal/hosts/scalac/converters/ToMtree.scala
  //
  // 3) ReificationSupport.scala (the quasiquote support module of scala/scala)
  //    Contains various SyntacticXXX extractors that do things similar to our l.XXX extractors.
  //    https://github.com/scala/scala/blob/1fbce4612c21a4d0c553ea489b4765494828c09f/src/reflect/scala/reflect/internal/ReificationSupport.scala
  trait LogicalTrees { l =>
    object EmptyPackageDef {
      // stats
      def unapply(tree: g.PackageDef): Option[List[g.Tree]] = {
        if (tree.pid.name == nme.EMPTY_PACKAGE_NAME) {
          require(tree.parent.isEmpty)
          Some(tree.stats)
        } else {
          None
        }
      }
    }

    object ToplevelPackageDef {
      // pid, stats
      def unapply(tree: g.PackageDef): Option[(l.Name, List[g.Tree])] = {
        if (tree.pid.name != nme.EMPTY_PACKAGE_NAME && tree.parent.isEmpty) {
          Some((Name(tree.pid), tree.stats))
        } else {
          None
        }
      }
    }

    object NestedPackageDef {
      // pid, stats
      def unapply(tree: g.PackageDef): Option[(l.Name, List[g.Tree])] = {
        if (tree.pid.name != nme.EMPTY_PACKAGE_NAME && tree.parent.nonEmpty) {
          Some((Name(tree.pid), tree.stats))
        } else {
          None
        }
      }
    }

    object PackageModuleDef {
      // mods, name, primaryCtor, template
      def unapply(tree: g.PackageDef): Option[(l.Modifiers, l.Name, g.DefDef, g.Template)] = tree match {
        case PackageDef(pid, List(pkgobj: ModuleDef))
        if pid.name != nme.EMPTY_PACKAGE_NAME && pkgobj.name == nme.PACKAGE =>
          ???
        case _ =>
          None
      }
    }

    object ClassDef {
      // mods, name, tparams, primaryCtor, template
      def unapply(tree: g.ClassDef): Option[(l.Modifiers, l.Name, List[g.TypeDef], g.DefDef, g.Template)] = {
        val g.ClassDef(gmods, gname, gtparams, gtempl) = tree
        if (gmods.hasFlag(TRAIT)) return None
        else {
          val primaryCtor = tree.impl.body.collectFirst { case ctor @ l.PrimaryCtorDef(_, _, _) => ctor }.get
          Some((Modifiers(tree), Name(tree), tree.tparams, primaryCtor, tree.impl))
        }
      }
    }

    private def removeSyntheticDefinitions(stats: List[g.Tree]): List[g.Tree] = {
      // TODO: relevant synthetics are described in:
      // * subclasses of MultiEnsugarer: DefaultGetter, VanillaAccessor, AbstractAccessor, LazyAccessor
      // * ToMtree.mstats: PatDef, InlinableHoistedTemporaryVal
      val result = mutable.ListBuffer[g.Tree]()
      var i = 0
      while (i < stats.length) {
        val stat = stats(i)
        i += 1
        stat match {
          case PrimaryCtorDef(_, _, _) => // skip this
          case _ => result += stat
        }
      }
      result.toList
    }

    object Template {
      // early, parents, self, stats
      def unapply(tree: g.Template): Some[(List[g.Tree], List[g.Tree], g.ValDef, List[g.Tree])] = {
        def indexOfFirstCtor(trees: List[Tree]) = trees.indexWhere { case LowlevelCtor(_, _) => true; case _ => false }
        object LowlevelCtor {
          def unapply(tree: Tree): Option[(List[List[ValDef]], List[Tree])] = tree match {
            case DefDef(mods, nme.MIXIN_CONSTRUCTOR, _, _, _, SyntacticBlock(lvdefs :+ _)) =>
              Some((Nil, lvdefs))
            case DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, _, SyntacticBlock(lvdefs :+ _ :+ _)) =>
              Some((vparamss, lvdefs))
            case _ => None
          }
        }
        val g.Template(parents, self, stats) = tree
        val (rawEdefs, rest) = stats.span(isEarlyDef)
        val (gvdefs, etdefs) = rawEdefs.partition(isEarlyValDef)
        val (fieldDefs, LowlevelCtor(_, lvdefs) :: body) = rest.splitAt(indexOfFirstCtor(rest))
        val evdefs = gvdefs.zip(lvdefs).map {
          case (gvdef @ g.ValDef(_, _, tpt, _), ValDef(_, _, _, rhs)) =>
            copyValDef(gvdef)(tpt = tpt, rhs = rhs)
        }
        val edefs = evdefs ::: etdefs
        Some((edefs, parents, self, removeSyntheticDefinitions(stats)))
      }
    }

    object Parent {
      // tpt, ctor, argss
      def unapply(tree: g.Tree): Option[(g.Tree, l.Name, List[List[g.Tree]])] = {
        ???
      }
    }

    object SelfDef {
      // mods, name, tpt
      def unapply(tree: g.ValDef): Option[(l.Modifiers, l.Name, g.Tree)] = {
        ???
      }
    }

    object PrimaryCtorDef {
      // mods, name, paramss
      def unapply(tree: g.DefDef): Option[(l.Modifiers, l.Name, List[List[g.ValDef]])] = {
        ???
      }
    }

    object SecondaryCtorDef {
      // mods, name, paramss, rhs
      def unapply(tree: g.DefDef): Option[(l.Modifiers, l.Name, List[g.TypeDef], List[List[g.ValDef]], g.Tree, g.Tree)] = {
        ???
      }
    }

    object AbstractTypeDef {
      // mods, name, tparams, typeBounds
      def unapply(tree: g.TypeDef): Option[(l.Modifiers, l.Name, List[g.TypeDef], g.TypeBoundsTree)] = {
        ???
      }
    }

    object TypeDef {
      // mods, name, tparams, rhs
      def unapply(tree: g.TypeDef): Option[(l.Modifiers, l.Name, List[g.TypeDef], g.Tree)] = {
        ???
      }
    }

    object TypeParamDef {
      // mods, name, tparams, typeBounds, viewBounds, contextBounds
      def unapply(tree: g.TypeDef): Option[(l.Modifiers, l.Name, List[g.TypeDef], g.TypeBoundsTree, g.Tree, g.Tree)] = {
        ???
      }
    }

    object AbstractValDef {
      // mods, pats, tpt
      def unapply(tree: g.ValDef): Option[(l.Modifiers, List[l.Name], g.Tree)] = {
        ???
      }
    }

    object AbstractVarDef {
      // mods, pats, tpt
      def unapply(tree: g.ValDef): Option[(l.Modifiers, List[l.Name], g.Tree)] = {
        ???
      }
    }

    object ValDef {
      // mods, pats, tpt, rhs
      def unapply(tree: g.ValDef): Option[(l.Modifiers, List[g.Tree], g.Tree, g.Tree)] = {
        ???
      }
    }

    object VarDef {
      // mods, pats, tpt, rhs
      def unapply(tree: g.ValDef): Option[(l.Modifiers, List[g.Tree], g.Tree, g.Tree)] = {
        ???
      }
    }

    object ParamDef {
      // mods, pats, tpt, default
      def unapply(tree: g.ValDef): Option[(l.Modifiers, l.Name, g.Tree, g.Tree)] = {
        ???
      }
    }

    object AbstractDefDef {
      // mods, name, tparams, paramss, tpt
      def unapply(tree: g.DefDef): Option[(l.Modifiers, l.Name, List[g.TypeDef], List[List[g.ValDef]], g.Tree, g.Tree)] = {
        ???
      }
    }

    object DefDef {
      // mods, name, tparams, paramss, tpt, rhs
      def unapply(tree: g.DefDef): Option[(l.Modifiers, l.Name, List[g.TypeDef], List[List[g.ValDef]], g.Tree, g.Tree)] = {
        ???
      }
    }

    object MacroDef {
      // mods, name, tparams, paramss, tpt, rhs
      def unapply(tree: g.DefDef): Option[(l.Modifiers, l.Name, List[g.TypeDef], List[List[g.ValDef]], g.Tree, g.Tree)] = {
        ???
      }
    }

    case class Modifiers(flags: Long, privateWithin: l.Name, annots: List[l.Annotation]) extends Tree {
      def hasFlag(bit: Long): Boolean = ???
    }
    object Modifiers {
      def apply(tree: g.MemberDef): l.Modifiers = {
        ???
      }
    }

    case class Annotation(val tree: g.Tree) extends Tree
    object Annotation {
      def apply(info: g.AnnotationInfo): l.Annotation = {
        // TODO: set Annotation.tree's parent to g.EmptyTree
        ???
      }
    }

    trait Namespace
    object AnonymousNamespace extends Namespace
    object TermNamespace extends Namespace
    object TypeNamespace extends Namespace
    object CtorNamespace extends Namespace
    object IndeterminateNamespace extends Namespace

    case class Name(namespace: l.Namespace, pre: g.Type, sym: g.Symbol, value: String) extends Tree
    object Name {
      def apply(tree: g.NameTree): l.Name = {
        val result = tree match {
          case g.DefDef(_, g.nme.CONSTRUCTOR, _, _, _, _) =>
            val mparentname = Name(tree.parent.require[g.MemberDef])
            Name(CtorNamespace, g.NoType, g.NoSymbol, mparentname.value)
          case _ =>
            val sname = tree.displayName
            if (sname == "_") Name(AnonymousNamespace, g.NoType, g.NoSymbol, sname)
            else if (tree.name.isTermName) Name(TermNamespace, g.NoType, g.NoSymbol, sname)
            else if (tree.name.isTypeName) Name(TypeNamespace, g.NoType, g.NoSymbol, sname)
            else unreachable(debug(tree, g.showRaw(tree)))
        }
        result.setParent(tree)
      }
    }
  }

  // NOTE: This is a mandatory piece of infrastructure that enriches scala.reflect trees
  // with information about parents and positions within their siblings.
  // As noted in ToMtree, it would be nice to figure out a mechanism to make this performance-neutral,
  // because currently we need two traversals (one to add stuff and one to remove stuff)
  // in order to transparently provide this functionality.
  implicit class RichFoundationNavigableTree[T <: Tree](tree: T) {
    def parent: Tree = tree.metadata.get("parent").map(_.require[Tree]).getOrElse(EmptyTree)
    def parents: List[Tree] = {
      def loop(tree: Tree, acc: List[Tree]): List[Tree] = {
        val parent = tree.metadata.get("parent").map(_.require[Tree])
        val recursion = parent.map(parent => loop(parent, acc :+ parent))
        recursion.getOrElse(acc)
      }
      loop(tree, Nil)
    }
    def setParent(parent: Tree): T = tree.appendMetadata("parent" -> parent).asInstanceOf[T]
    def index: Int = tree.metadata.get("index").map(_.require[Int]).getOrElse(-1)
    def setIndex(index: Int): T = tree.appendMetadata("index" -> index).asInstanceOf[T]
    def installNavigationLinks(): Unit = {
      object installNavigationLinks extends Traverser {
        private var parents = List[Tree]()
        override def traverse(tree: Tree): Unit = {
          if (tree.hasMetadata("parent")) return
          tree.setParent(parents.headOption.getOrElse(EmptyTree))
          parents = tree :: parents
          super.traverse(tree)
          parents = parents.tail
        }
        override def traverseStats(stats: List[Tree], exprOwner: Symbol): Unit = {
          var i = 0
          while (i < stats.length) {
            stats(i).setIndex(i)
            i += 1
          }
          super.traverseStats(stats, exprOwner)
        }
      }
      // NOTE: If you get a MatchError in xtraverse here,
      // it means that one of the custom logical trees declared below in this file
      // was created without its parent set (i.e. its tree.hasMetadata("parent") is false).
      // This should be fixed at the point where the logical tree is created, not here.
      installNavigationLinks.traverse(tree)
    }
    def removeNavigationLinks(): Unit = {
      object removeNavigationLinks extends Traverser {
        override def traverse(tree: Tree): Unit = {
          if (!tree.hasMetadata("parent") && !tree.hasMetadata("index")) return
          tree.removeMetadata("parent")
          tree.removeMetadata("index")
        }
      }
      // NOTE: If you get a MatchError in xtraverse here,
      // it means that one of the custom logical trees declared below in this file
      // has escaped the confines of the scala.reflect > scala.meta converter
      // and tries to become part of the compilation pipeline.
      // This should be fixed at the point where the logical tree ends up in the output, not here
      removeNavigationLinks.traverse(tree)
    }
  }
}

// TODO: commented out remnants of the first take at the new converter
// TODO: something here is in the middle of migration to the new style, so I didn't just remove it in favor of git history

// object ParameterizedDef {
//   def unapply(tree: Tree) = tree match {
//     case tree @ ClassDef(_, _, tparams, Template(_, _, _, stats)) =>
//       def tparam(targ: Tree): Option[TypeDef] = tparams.filter(tparam => {
//         if (tparam.symbol != NoSymbol) tparam.symbol == targ.symbol
//         else targ match { case Ident(name) => name == tparam.name; case _ => false }
//       }).headOption

//       object ViewBound {
//         def unapply(tree: ValDef): Option[(TypeDef, Tree)] = tree match {
//           case ValDef(_, _, tpt @ TypeTree(), _) =>
//             val tycon = if (tpt.tpe != null) tpt.tpe.typeSymbolDirect else NoSymbol
//             val targs = if (tpt.tpe != null) tpt.tpe.typeArgs else Nil
//             val (fromTpe, toTpe) = targs match { case List(from, to) => (from, to); case _ => (NoType, NoType) }
//             val fromSym = fromTpe.typeSymbol
//             val tyconMatches = tycon == definitions.FunctionClass(1)
//             if (tyconMatches) tparam(Ident(fromSym)).map(tparam => (tparam, TypeTree(toTpe)))
//             else None
//           case ValDef(_, _, AppliedTypeTree(tycon, from :: to :: Nil), _) =>
//             val tyconMatches = tycon match {
//               // NOTE: this doesn't handle every possible case (e.g. an Ident binding to renamed import),
//               // but it should be ood enough for 95% of the situations
//               case Select(pre, TermName("Function1")) => pre.symbol == definitions.ScalaPackage
//               case tycon: RefTree => tycon.symbol == definitions.FunctionClass(1)
//               case _ => false
//             }
//             if (tyconMatches) tparam(from).map(tparam => (tparam, to))
//             else None
//           case _ =>
//             None
//         }
//       }

//       object ContextBound {
//         def unapply(tree: ValDef): Option[(TypeDef, Tree)] = tree match {
//           case ValDef(_, _, tpt @ TypeTree(), _) =>
//             val tycon = if (tpt.tpe != null) tpt.tpe.typeSymbolDirect else NoSymbol
//             val targs = if (tpt.tpe != null) tpt.tpe.typeArgs else Nil
//             val targ = targs.map(_.typeSymbol) match { case List(sym) => sym; case _ => NoSymbol }
//             tparam(Ident(targ)).map(tparam => (tparam, Ident(tycon)))
//           case ValDef(_, _, AppliedTypeTree(tycon, targ :: Nil), _) =>
//             tparam(targ).map(tparam => (tparam, tycon))
//           case _ =>
//             None
//         }
//       }

//       val ctor = stats.collectFirst{ case ctor: DefDef if ctor.name == nme.CONSTRUCTOR => ctor }
//       val vparamss = ctor.map(_.vparamss).getOrElse(Nil)
//       val (explicitss, implicitss) = vparamss.partition(_.exists(_.mods.hasFlag(IMPLICIT)))
//       val (bounds, implicits) = implicitss.flatten.partition(_.name.startsWith(nme.EVIDENCE_PARAM_PREFIX))
//       val vparamss = gexplicitss ++ (if (gimplicits.isEmpty) Nil else List(gimplicits))
//     case _ =>
//       None
//   }

//   implicit classDefIsParameterizedDef(cdef: ClassDef) = new ParameterizedDef {

//     // val (gexplicitss, gimplicitss) = gvparamss.partition(_.exists(_.mods.hasFlag(IMPLICIT)))
//     // val (gbounds, gimplicits) = gimplicitss.flatten.partition(_.name.startsWith(g.nme.EVIDENCE_PARAM_PREFIX))
//     // val gtparams1 = gtparams.map(gtparam => {
//     //   val gviewBounds = gbounds.flatMap(ViewBound.unapply).filter(_._1.name == gtparam.name).map(_._2)
//     //   val gcontextBounds = gbounds.flatMap(ContextBound.unapply).filter(_._1.name == gtparam.name).map(_._2)
//     //   gtparam.appendMetadata("viewBounds" -> gviewBounds, "contextBounds" -> gcontextBounds)
//     // })
//     // val gvparamss1 = gexplicitss ++ (if (gimplicits.isEmpty) Nil else List(gimplicits))
//   }
// }

// class ParameterOps(tparams: List[TypeDef], vparamss: List[List[ValDef]]) {
// }

// private def mname(ltree: g.NameTree): m.Name = {
//   val sname = ltree.displayName
//   val mname = {
//     if (sname == "_") m.Name.Anonymous()
//     else if (ltree.name.isTermName) m.Term.Name(sname)
//     else if (ltree.name.isTypeName) m.Type.Name(sname)
//     else unreachable(debug(ltree, g.showRaw(ltree)))
//   }
//   // TODO: attach denotations here
//   mname
// }

// private def mctorname(gctor: g.DefDef): m.Ctor.Name = {
//   val gparent = gctor.parent.require[g.MemberDef]
//   val mname = m.Ctor.Name(this.mname(gparent).value)
//   // TODO: attach denotations here
//   // if gctor.nonEmpty, then it's a class ctor - should be quite simple
//   // if gctor.isEmpty, then gparent is a trait or a module
//   // check out how this is handled in the earlier version of scalahost:
//   // https://github.com/scalameta/scalahost/blob/master/interface/src/main/scala/scala/meta/internal/hosts/scalac/converters/ToMtree.scala#L50
//   mname
// }

// private def mmods(gmdef: g.MemberDef): Seq[m.Mod] = {
//   val ops = new ClassParameterOps(gmdef)
//   val isClassParameter =

//   case class ClassParameter(gparam: g.ValDef, gfield: Option[g.ValDef], gctor: g.DefDef, gclass: g.ClassDef)
//   val gclassparam: Option[ClassParameter] = {
//     val syntactically = {
//       gparents match {
//         case
//           (gparam @ g.ValDef(_, _, _, _)) ::
//           (gctor @ g.DefDef(_, g.nme.CONSTRUCTOR, _, _, _, _)) ::
//           (gclass @ g.ClassDef(_, _, _, g.Template(_, _, gsiblings))) :: _ =>
//             import g.TermNameOps
//             val gfield = gsiblings.collectFirst { case gfield: g.ValDef if gfield.name == gparam.name.localName => gfield }
//             val gprim = gsiblings.collectFirst { case gprim: g.DefDef if gprim.name == g.nme.CONSTRUCTOR => gprim }
//             val isClassParameter = gprim.map(gprim => gctor == gprim).getOrElse(false)
//             if (isClassParameter) Some(ClassParameter(gparam, gfield, gctor, gclass))
//             else None
//         case _ =>
//           None
//       }
//     }
//     val semantically = {
//       if (gmdef.symbol.owner.isPrimaryConstructor) {
//         val gparam = gmdef.require[g.ValDef]
//         val gctorSym = gmdef.symbol.owner
//         val gctor = g.DefDef(gctorSym, g.EmptyTree)
//         val gclassSym = gctorSym.owner
//         val gclass = g.ClassDef(gclassSym, g.Template(Nil, g.noSelfType, Nil))
//         val ggetterSym = gclassSym.info.member(gmdef.name)
//         val gfieldSym = ggetterSym.map(_.owner.info.member(ggetterSym.localName))
//         val gfield = if (gfieldSym != g.NoSymbol) Some(g.ValDef(gfieldSym)) else None
//         Some(ClassParameter(gparam, gfield, gctor, gclass))
//       } else None
//     }
//     syntactically.orElse(semantically)
//   }

//   val annotationMods: Seq[m.Mod] = {
//     mannots(gmdef)
//   }

//   val accessQualifierMods: Seq[m.Mod] = {
//     if (gmods.hasFlag(SYNTHETIC) && gmods.hasFlag(ARTIFACT)) {
//       // NOTE: some sick artifact vals produced by mkPatDef can be private to method (whatever that means)
//       Nil
//     } else if (gmods.hasFlag(LOCAL)) {
//       // TODO: attach denotations here
//       if (gmods.hasFlag(PROTECTED)) List(m.Mod.Protected(m.Term.This(m.Name.Anonymous())))
//       else if (gmods.hasFlag(PRIVATE)) List(m.Mod.Private(m.Term.This(m.Name.Anonymous())))
//       else unreachable(debug(gmods))
//     } else if (gmods.hasAccessBoundary && gmods.privateWithin != g.tpnme.EMPTY) {
//       // TODO: attach denotations here
//       // TODO: `private[pkg] class C` doesn't have PRIVATE in its flags
//       // so we need to account for that!
//       val mqualifier = m.Name.Indeterminate(gmods.privateWithin.displayName)
//       if (gmods.hasFlag(PROTECTED)) List(m.Mod.Protected(mqualifier))
//       else List(m.Mod.Private(mqualifier))
//     } else {
//       // TODO: attach denotations here
//       if (gmods.hasFlag(PROTECTED)) List(m.Mod.Protected(m.Name.Anonymous()))
//       else if (gmods.hasFlag(PRIVATE)) List(m.Mod.Private(m.Name.Anonymous()))
//       else Nil
//     }
//   }

//   val otherMods: Seq[m.Mod] = {
//     val mmods = scala.collection.mutable.ListBuffer[m.Mod]()
//     val gmods = gmdef.mods
//     if (gmods.hasFlag(IMPLICIT)) mmods += m.Mod.Implicit()
//     if (gmods.hasFlag(FINAL)) mmods += m.Mod.Final()
//     if (gmods.hasFlag(SEALED)) mmods += m.Mod.Sealed()
//     if (gmods.hasFlag(OVERRIDE)) mmods += m.Mod.Override()
//     if (gmods.hasFlag(CASE)) mmods += m.Mod.Case()
//     if (gmods.hasFlag(ABSTRACT) && gmdef.isInstanceOf[g.ClassDef] && !gmods.hasFlag(TRAIT)) mmods += m.Mod.Abstract()
//     if (gmods.hasFlag(ABSOVERRIDE)) { mmods += m.Mod.Abstract(); mmods += m.Mod.Override() }
//     if (gmods.hasFlag(COVARIANT) && gmdef.isInstanceOf[g.TypeDef]) mmods += m.Mod.Covariant()
//     if (gmods.hasFlag(CONTRAVARIANT) && gmdef.isInstanceOf[g.TypeDef]) mmods += m.Mod.Contravariant()
//     if (gmods.hasFlag(LAZY)) mmods += m.Mod.Lazy()
//     mmods.toList
//   }

//   val valVarParamMods: Seq[m.Mod] = {
//     val mmods = scala.collection.mutable.ListBuffer[m.Mod]()
//     val gfield = gclassparam.flatMap(_.gfield)
//     val isImmutableField = gfield.map(!_.mods.hasFlag(MUTABLE)).getOrElse(false)
//     val isMutableField = gfield.map(_.mods.hasFlag(MUTABLE)).getOrElse(false)
//     val inCaseClass = gclassparam.map(_.gclass).map(_.mods.hasFlag(CASE)).getOrElse(false)
//     if (isMutableField) mmods += m.Mod.VarParam()
//     if (isImmutableField && !inCaseClass) mmods += m.Mod.ValParam()
//     mmods.toList
//   }

//   // NOTE: we can't discern `class C(x: Int)` and `class C(private[this] val x: Int)`
//   // so let's err on the side of the more popular option
//   val result = annotationMods ++ accessQualifierMods ++ otherMods ++ valVarParamMods
//   if (gclassparam.nonEmpty) result.filter({ case m.Mod.Private(m.Term.This(_)) => false; case _ => true })
//   else result
// }

// private def mannots(gmdef: g.MemberDef): Seq[m.Mod.Annot] = {
//   val gannotsSyn = gmdef.mods.annotations
//   val gannotsSem = gmdef.symbol.annotations
//   val gannots = {
//     if (gannotsSyn.nonEmpty) gannotsSyn.zip(1.to(gannotsSyn.length).map(_ => g.ErroneousAnnotation))
//     else 1.to(gannotsSem.length).map(_ => g.EmptyTree).zip(gannotsSem)
//   }
//   require(gannotsSyn.isEmpty || gannotsSem.isEmpty, debug(gmdef))
//   gannots.map({ case (gannotSyn, gannotSem) =>
//     ???
//   })
// }
