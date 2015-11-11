package scala.meta.internal.hosts.scalac
package reflect

import scala.tools.nsc.Global
import scala.reflect.internal.{Flags, HasFlags}
import scala.reflect.internal.Flags._
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable

trait LogicalTrees {
  self: ReflectToolkit =>

  import global.{require => _, abort => _, _}
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
  // The former is this file, the latter is a trivial pattern match of the form:
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
  //    can be figured out from the corresponding g.DefDef (possibly requiring additional attachments).
  //    But we do have `case class Modifiers(...)`, because g.Modifiers isn't a tree, so we can't really
  //    adorn it with additional metadata that will help the converter.
  //
  // 3) Since we generally don't create intermediate data structures, the unapply methods in extractors
  //    should have comments that explain the fields that they are extracting.
  //
  // 4) Extractors and supporting intermediate data structures should be created in the same order
  //    that the corresponding AST nodes in scala/meta/Trees.scala are in.
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
  trait LogicalTrees { l: self.l.type =>
    // ============ NAMES ============

    case class Denotation(pre: g.Type, sym: l.Symbol) {
      def isEmpty = pre == g.NoType || sym == l.Zero
      def nonEmpty = !isEmpty
    }
    implicit class RichDenotationTree(tree: g.NameTree) {
      def prefix = tree match {
        case tree @ g.RefTree(qual, _) if qual.tpe != null && qual.tpe != g.NoType => qual.tpe
        case tree @ g.RefTree(_, _) => tree.symbol.prefix
        case tree: g.DefTree => tree.symbol.prefix
        case _ => unreachable(debug(tree, showRaw(tree)))
      }
      def denot = l.Denotation(tree.prefix, tree.symbol.toLogical)
    }

    trait Name extends Tree { def denot: l.Denotation }

    case class AnonymousName(denot: l.Denotation) extends Name with TermParamName with TypeParamName with QualifierName
    object AnonymousName {
      def apply(pre: g.Type, sym: l.Symbol): l.AnonymousName = apply(l.Denotation(pre, sym))
    }

    case class IndeterminateName(denot: l.Denotation, value: String) extends Name with QualifierName
    object IndeterminateName {
      def apply(pre: g.Type, sym: l.Symbol, value: String): l.IndeterminateName = {
        l.IndeterminateName(l.Denotation(pre, sym), value)
      }
    }

    trait QualifierName extends Name

    implicit class RichNameTree(tree: Tree) {
      def displayName: String = {
        val custom = tree.metadata.get("LdisplayName").map(_.asInstanceOf[String])
        custom.getOrElse(tree match {
          case tree: ModuleDef if tree.name == nme.PACKAGE => abort(tree)
          case tree: NameTree => tree.name.displayName
          case This(name) => name.displayName // NOTE: This(tpnme.EMPTY) is also accounted for
          case Super(_, name) => name.displayName
          case _ => unreachable(debug(tree, showRaw(tree)))
        })
      }
    }

    // ============ TERMS ============

    object TermThis {
      def unapply(tree: g.This): Option[l.QualifierName] = {
        val ldenot = l.Denotation(tree.symbol.prefix, tree.symbol.toLogical)
        if (tree.qual == tpnme.EMPTY) Some(l.AnonymousName(ldenot))
        else Some(l.IndeterminateName(ldenot, tree.displayName))
      }
    }

    case class TermName(denot: l.Denotation, value: String) extends Name with TermParamName
    object TermName {
      def apply(tree: g.NameTree): l.TermName = {
        require(tree.name.isTermName && tree.name != nme.WILDCARD && tree.name != nme.CONSTRUCTOR && debug(tree, showRaw(tree)))
        new l.TermName(tree.denot, tree.displayName).setType(tree.tpe)
      }
    }

    object TermIdent {
      def unapply(tree: g.Ident): Option[l.TermName] = tree match {
        case tree @ g.Ident(g.TermName(value)) =>
          if (tree.hasMetadata("isLpat") && tree.name == nme.WILDCARD) return None
          Some(l.TermName(tree.denot, value).setType(tree.tpe))
        case _ =>
          None
      }
    }

    object TermSelect {
      def unapply(tree: g.Select): Option[(g.Tree, l.TermName)] = {
        val g.Select(qual, name) = tree
        if (name.isTypeName) return None
        Some((qual, l.TermName(tree)))
      }
    }

    object TermApply {
      def unapply(tree: g.Apply): Option[(g.Tree, List[g.Tree])] = {
        if (tree.hasMetadata("isLparent")) return None
        if (tree.hasMetadata("isLpat")) return None
        Some((tree.fun, tree.args))
      }
    }

    object TermApplyType {
      def unapply(tree: g.TypeApply): Option[(g.Tree, List[g.Tree])] = {
        Some((tree.fun, tree.args))
      }
    }

    object TermBlock {
      def unapply(tree: g.Block): Option[List[g.Tree]] = {
        val lstats = blockStats(tree, tree.stats :+ tree.expr)
        Some(lstats)
      }
    }

    object TermIf {
      def unapply(tree: g.If): Option[(g.Tree, g.Tree, g.Tree)] = {
        Some((tree.cond, tree.thenp, tree.elsep))
      }
    }

    object TermMatch {
      def unapply(tree: g.Match): Option[(g.Tree, List[g.Tree])] = {
        val g.Match(scrut, cases) = tree
        Some((scrut, cases))
      }
    }

    object TermFunction {
      def unapply(tree: g.Function): Option[(List[g.Tree], g.Tree)] = {
        val g.Function(params, body) = tree
        val lparams = params.map(_.appendMetadata("isLparam" -> true))
        Some((lparams, body))
      }
    }

    trait TermParamName extends Name

    object TermParamDef {
      // mods, pats, tpt, default
      def unapply(tree: g.ValDef): Option[(List[l.Modifier], l.TermName, g.Tree, g.Tree)] = {
        if (!tree.hasMetadata("isLparam")) return None
        val g.ValDef(_, _, tpt, default) = tree
        Some((l.Modifiers(tree), l.TermName(tree), tpt, default))
      }
    }

    // ============ TYPES ============

    object TypeTree {
      def unapply(tpt: g.TypeTree): Option[g.Type] = {
        if (tpt.nonEmpty) Some((tpt.tpe)) else None
      }
    }

    case class TypeName(denot: l.Denotation, value: String) extends Name with TypeParamName
    object TypeName {
      def apply(tree: g.NameTree): l.TypeName = {
        require(tree.name.isTypeName && tree.name != tpnme.WILDCARD && debug(tree, showRaw(tree)))
        new l.TypeName(tree.denot, tree.displayName)
      }
    }

    object TypeIdent {
      def unapply(tree: g.Tree): Option[l.TypeName] = tree match {
        case tree @ g.Ident(g.TypeName(value)) =>
          Some(l.TypeName(tree.denot, value))
        case _ =>
          None
      }
    }

    object TypeSelect {
      def unapply(tree: g.Select): Option[(g.Tree, l.TypeName)] = {
        val g.Select(qual, name) = tree
        if (name.isTermName) return None
        Some((qual, l.TypeName(tree)))
      }
    }

    object TypeApply {
      def unapply(tree: g.AppliedTypeTree): Option[(g.Tree, List[g.Tree])] = {
        val g.AppliedTypeTree(tpt, args) = tree
        Some((tpt, args))
      }
    }

    object TypeBounds {
      def unapply(tree: g.TypeBoundsTree): Option[(Option[g.Tree], Option[g.Tree])] = {
        val g.TypeBoundsTree(glo, ghi) = tree
        val llo = if (glo.nonEmpty) Some(glo) else None
        val lhi = if (ghi.nonEmpty) Some(ghi) else None
        Some((llo, lhi))
      }
    }

    trait TypeParamName extends Name

    object TypeParamDef {
      // mods, name, tparams, tbounds, vbounds, cbounds
      def unapply(tree: g.TypeDef): Option[(List[l.Modifier], l.TypeName, List[g.TypeDef], g.TypeBoundsTree, List[g.Tree], List[g.Tree])] = {
        if (!tree.hasMetadata("isLtparam")) return None
        val g.TypeDef(_, name, tparams, rhs) = tree
        val ltparams = tparams.map(_.appendMetadata("isLtparam" -> true))
        val ltbounds = {
          rhs match {
            case rhs: g.TypeBoundsTree =>
              rhs
            case rhs: g.TypeTree =>
              val g.TypeBounds(lo, hi) = rhs.tpe
              g.TypeBoundsTree(g.TypeTree(lo), g.TypeTree(hi))
          }
        }
        val lvbounds = tree.metadata.getOrElse("lvbounds", Nil)
        val lcbounds = tree.metadata.getOrElse("lcbounds", Nil)
        Some((l.Modifiers(tree), l.TypeName(tree), ltparams, ltbounds, lvbounds, lcbounds))
      }
    }

    // ============ DECLS ============

    object AbstractValDef {
      // mods, pats, tpt
      def unapply(tree: g.ValDef): Option[(List[l.Modifier], List[l.TermName], g.Tree)] = {
        ???
      }
    }

    object AbstractVarDef {
      // mods, pats, tpt
      def unapply(tree: g.ValDef): Option[(List[l.Modifier], List[l.TermName], g.Tree)] = {
        ???
      }
    }

    object AbstractDefDef {
      // mods, name, tparams, paramss, tpt
      def unapply(tree: g.DefDef): Option[(List[l.Modifier], l.TermName, List[g.TypeDef], List[List[g.ValDef]], g.Tree, g.Tree)] = {
        ???
      }
    }

    object AbstractTypeDef {
      // mods, name, tparams, tbounds
      def unapply(tree: g.TypeDef): Option[(List[l.Modifier], l.TypeName, List[g.TypeDef], g.TypeBoundsTree)] = {
        ???
      }
    }

    // ============ PATTERNS ============

    object PatVarTerm {
      // name
      def unapply(tree: g.Bind): Option[l.TermName] = {
        if (!tree.hasMetadata("isLpatvar")) return None
        if (tree.name.isTypeName) return None
        Some(l.TermName(tree).setType(tree.tpe))
      }
    }

    object PatWildcard {
      def unapply(tree: g.Ident): Boolean = {
        if (!tree.hasMetadata("isLpat")) return false
        tree.name == nme.WILDCARD
      }
    }

    object PatBind {
      def unapply(tree: g.Bind): Option[(g.Tree, g.Tree)] = {
        if (tree.hasMetadata("isLpatvar")) return None
        if (tree.name.isTypeName) return None
        val g.Bind(name, body) = tree
        val llhs = {
          val core = g.Bind(name, Ident(nme.WILDCARD))
          core.setSymbol(tree.symbol).appendMetadata("isLpatvar" -> true)
          core
        }
        val lrhs = body.appendMetadata("isLpat" -> true)
        Some((llhs, lrhs))
      }
    }

    object PatExtract {
      def unapply(tree: g.Tree): Option[(g.Tree, List[g.Tree], List[g.Tree])] = {
        if (!tree.hasMetadata("isLpat")) return None
        val (fun, targs, args) = tree match {
          case g.Apply(g.TypeApply(fun, targs), args) => (fun, targs, args)
          case g.Apply(fun, args) => (fun, Nil, args)
          case g.UnApply(g.Apply(g.TypeApply(fun, targs), List(unapplySelector)), args) => (fun, targs, args)
          case g.UnApply(g.Apply(fun, List(unapplySelector)), args) => (fun, Nil, args)
          case _ => return None
        }
        val largs = args.map(_.appendMetadata("isLpat" -> true))
        Some((fun, targs, largs))
      }
    }

    object PatTyped {
      def unapply(tree: g.Typed): Option[(g.Tree, g.Tree)] = {
        if (!tree.hasMetadata("isLpat")) return None
        val g.Typed(lhs, rhs) = tree
        val llhs = lhs.appendMetadata("isLpat" -> true)
        Some((llhs, rhs))
      }
    }

    // ============ LITERALS ============

    object Literal {
      // value
      def unapply(tree: g.Literal): Option[Any] = tree match {
        case g.Literal(g.Constant(_: g.Type)) => None
        case g.Literal(g.Constant(_: g.Symbol)) => None
        case g.Literal(g.Constant(value)) => Some(value)
        case _ => None
      }
    }

    // ============ DEFNS ============

    object ValDef {
      // mods, pats, tpt, rhs
      def unapply(tree: g.ValDef): Option[(List[l.Modifier], List[g.Tree], g.Tree, g.Tree)] = {
        val g.ValDef(_, name, tpt, rhs) = tree
        if (tree.hasMetadata("isLself") || tree.hasMetadata("isLparam")) return None
        if (tree.mods.hasFlag(MUTABLE)) return None
        val lpats = {
          // TODO: support multi-pat valdefs
          val core = g.Bind(name, Ident(nme.WILDCARD))
          core.setSymbol(tree.symbol).appendMetadata("isLpatvar" -> true)
          List(core)
        }
        Some((l.Modifiers(tree), lpats, tpt, rhs))
      }
    }

    object VarDef {
      // mods, pats, tpt, rhs
      def unapply(tree: g.ValDef): Option[(List[l.Modifier], List[g.Tree], g.Tree, g.Tree)] = {
        ???
      }
    }

    object DefDef {
      // mods, name, tparams, paramss, tpt, rhs
      def unapply(tree: g.DefDef): Option[(List[l.Modifier], l.TermName, List[g.TypeDef], List[List[g.ValDef]], g.Tree, g.Tree)] = {
        val g.DefDef(_, _, tparams, paramss, tpt, rhs) = tree
        if (tree.name == g.nme.CONSTRUCTOR || rhs.isEmpty || tree.mods.hasFlag(MACRO)) return None
        val ltparams = applyBounds(tparams, paramss).map(_.appendMetadata("isLtparam" -> true))
        val lparamss = removeBounds(paramss).map(_.map(_.appendMetadata("isLparam" -> true)))
        Some((l.Modifiers(tree), l.TermName(tree), ltparams, lparamss, tpt, rhs))
      }
    }

    object MacroDef {
      // mods, name, tparams, paramss, tpt, rhs
      def unapply(tree: g.DefDef): Option[(List[l.Modifier], l.TermName, List[g.TypeDef], List[List[g.ValDef]], g.Tree, g.Tree)] = {
        ???
      }
    }

    object TypeDef {
      // mods, name, tparams, rhs
      def unapply(tree: g.TypeDef): Option[(List[l.Modifier], l.TypeName, List[g.TypeDef], g.Tree)] = {
        ???
      }
    }

    object ClassDef {
      // mods, name, tparams, primaryCtor, template
      def unapply(tree: g.ClassDef): Option[(List[l.Modifier], l.TypeName, List[g.TypeDef], g.DefDef, g.Template)] = {
        val g.ClassDef(_, _, classTparams, templ @ g.Template(_, _, body)) = tree
        if (tree.mods.hasFlag(TRAIT)) return None
        else {
          // TODO: context bounds
          val lprimaryctor = body.collectFirst { case ctor @ g.DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => ctor }.get
          lprimaryctor.appendMetadata("isLprimaryCtor" -> true, "LimplDef" -> tree)
          val ltparams = applyBounds(classTparams, lprimaryctor.vparamss)
          val ltempl = templ.appendMetadata("LimplDef" -> tree)
          Some((l.Modifiers(tree), l.TypeName(tree), ltparams, lprimaryctor, ltempl))
        }
      }
    }

    object TraitDef {
      // mods, name, tparams, template
      def unapply(tree: g.ClassDef): Option[(List[l.Modifier], l.TypeName, List[g.TypeDef], g.Template)] = {
        val g.ClassDef(_, _, tparams, templ) = tree
        val ltempl = templ.appendMetadata("LimplDef" -> tree)
        if (!tree.mods.hasFlag(TRAIT)) return None
        else Some((l.Modifiers(tree), l.TypeName(tree), tparams, ltempl))
      }
    }

    object ObjectDef {
      // mods, name, tparams, template
      def unapply(tree: g.ModuleDef): Option[(List[l.Modifier], l.TermName, g.Template)] = {
        val g.ModuleDef(_, name, templ) = tree
        val ltempl = templ.appendMetadata("LimplDef" -> tree)
        if (name == nme.PACKAGE) return None
        Some((l.Modifiers(tree), l.TermName(tree), ltempl))
      }
    }

    // ============ PKGS ============

    object EmptyPackageDef {
      // stats
      def unapply(tree: g.PackageDef): Option[List[g.Tree]] = {
        if (tree.pid.name == nme.EMPTY_PACKAGE_NAME) {
          require(!tree.hasMetadata("isLnestedPkg"))
          val lstats = toplevelStats(tree, tree.stats)
          Some(lstats)
        } else {
          None
        }
      }
    }

    object ToplevelPackageDef {
      // pid, stats
      def unapply(tree: g.PackageDef): Option[(l.TermName, List[g.Tree])] = {
        if (tree.pid.name != nme.EMPTY_PACKAGE_NAME && !tree.hasMetadata("isLnestedPkg")) {
          val lstats = toplevelStats(tree, tree.stats)
          Some((l.TermName(tree.pid), lstats))
        } else {
          None
        }
      }
    }

    object NestedPackageDef {
      // pid, stats
      def unapply(tree: g.PackageDef): Option[(l.TermName, List[g.Tree])] = {
        if (tree.pid.name != nme.EMPTY_PACKAGE_NAME && tree.hasMetadata("isLnestedPkg")) {
          val lstats = toplevelStats(tree, tree.stats)
          Some((l.TermName(tree.pid), lstats))
        } else {
          None
        }
      }
    }

    object PackageModuleDef {
      // mods, name, primaryCtor, template
      def unapply(tree: g.PackageDef): Option[(List[l.Modifier], l.TermName, g.DefDef, g.Template)] = tree match {
        case g.PackageDef(pid, List(pkgobj: g.ModuleDef))
        if pid.name != nme.EMPTY_PACKAGE_NAME && pkgobj.name == nme.PACKAGE =>
          ???
        case _ =>
          None
      }
    }

    // ============ CTORS ============

    private object CtorDef {
      // isPrimaryCtor, mods, name, paramss, supercall, rhs
      def unapply(tree: g.DefDef): Option[(Boolean, List[l.Modifier], l.CtorName, List[List[g.ValDef]], g.Tree, List[g.Tree])] = {
        (tree, tree.metadata.get("LimplDef")) match {
          case (g.DefDef(_, nme.CONSTRUCTOR, _, _, _, body), Some(implDef: g.ImplDef)) =>
            val lname = l.CtorName(tree, implDef)
            val lparamss = removeBounds(tree.vparamss)
            val g.Block(binit, g.Literal(g.Constant(()))) = body
            val lsupercall +: lstats = binit.dropWhile(_.isInstanceOf[ValDef])
            Some((tree.hasMetadata("isLprimaryCtor"), l.Modifiers(tree), lname, lparamss, lsupercall, lstats))
          case _ =>
            None
        }
      }
    }

    object PrimaryCtorDef {
      // mods, name, paramss
      def unapply(tree: g.DefDef): Option[(List[l.Modifier], l.CtorName, List[List[g.ValDef]])] = tree match {
        case l.CtorDef(true, mods, name, paramss, _, _) => Some((mods, name, paramss))
        case _ => None
      }
    }

    object SecondaryCtorDef {
      // mods, name, paramss, stats
      def unapply(tree: g.DefDef): Option[(List[l.Modifier], l.CtorName, List[List[g.ValDef]], List[g.Tree])] = tree match {
        case l.CtorDef(false, mods, name, paramss, _, stats) => Some((mods, name, paramss, stats))
        case _ => None
      }
    }

    case class CtorName(denot: l.Denotation, value: String) extends Name
    object CtorName {
      def apply(ctorDef: g.DefDef, classDef: g.ImplDef): l.CtorName = {
        require(ctorDef.name == nme.CONSTRUCTOR)
        l.CtorName(ctorDef.denot, classDef.displayName).setType(ctorDef.symbol.info)
      }
    }

    case class CtorIdent(name: l.CtorName) extends Tree
    object CtorIdent {
      def apply(ctorSym: l.Symbol, classRef: g.RefTree): l.CtorIdent = {
        // NOTE: We can't use the commented snippet of code,
        // because then we'll end up in a really strange place when type aliases are involved.
        // Consider the `AnyRef` ctorident in `class C extends scala.AnyRef`.
        // We would like to emit `Ctor.Ref.Name("AnyRef")`, but what denotation should we assign to it?
        // The symbol is obvious: `java.lang#Object.<init>()V`, but the prefix is harder than it looks.
        // The knee-jerk reaction would be not to dealias and take the prefix of `scala.AnyRef`, which is `scala`.
        // But then we'd end up with `scala::java.lang#Object.<init>()V` which doesn't make any sense.
        // Hence here we actually have to disregard the prefix (i.e. inheritance and type aliases) and
        // simply go for the owner of the symbol.
        // val lpre = ctorRef.qualifier.tpe.prefix.orElse(g.NoPrefix)
        val gctor = ctorSym.gsymbol
        val ldenot = l.Denotation(gctor.owner.prefix, ctorSym)
        val lname = l.CtorName(ldenot, classRef.displayName).setType(gctor.info)
        l.CtorIdent(lname)
      }
    }

    // ============ TEMPLATES ============

    object Template {
      // early, parents, self, stats
      def unapply(tree: g.Template): Some[(List[g.Tree], List[g.Tree], g.ValDef, List[g.Tree])] = {
        def indexOfFirstCtor(trees: List[g.Tree]) = trees.indexWhere { case LowlevelCtor(_, _, _, _) => true; case _ => false }
        object LowlevelCtor {
          def unapply(tree: g.Tree): Option[(List[List[g.ValDef]], List[g.Tree], g.Symbol, List[List[g.Tree]])] = tree match {
            case g.DefDef(_, nme.MIXIN_CONSTRUCTOR, _, _, _, SyntacticBlock(lvdefs :+ _)) =>
              Some((Nil, lvdefs, g.NoSymbol, Nil))
            case g.DefDef(_, nme.CONSTRUCTOR, Nil, vparamss, _, SyntacticBlock(lvdefs :+ Applied(superCtor, _, superArgss) :+ _)) =>
              Some((vparamss, lvdefs, superCtor.symbol, superArgss))
            case _ =>
              None
          }
        }
        val limpl = tree.metadata("LimplDef").asInstanceOf[g.ImplDef]
        val g.Template(parents, self, stats) = tree
        val lself = {
          val result = {
            if (self != noSelfType) self
            else g.ValDef(g.Modifiers(), g.nme.WILDCARD, g.TypeTree(), g.EmptyTree)
          }
          result.appendMetadata("isLself" -> true, "LimplDef" -> limpl)
        }
        val (rawEdefs, rest) = stats.span(isEarlyDef)
        val (gvdefs, etdefs) = rawEdefs.partition(isEarlyValDef)
        val (fieldDefs, ctor @ LowlevelCtor(_, lvdefs, superCtor, superArgss) :: body) = rest.splitAt(indexOfFirstCtor(rest))
        val evdefs = gvdefs.zip(lvdefs).map {
          case (gvdef @ g.ValDef(_, _, tpt, _), g.ValDef(_, _, _, rhs)) =>
            copyValDef(gvdef)(tpt = tpt, rhs = rhs)
        }
        val edefs = evdefs ::: etdefs
        val lparents = parents.zipWithIndex.map{ case (parent, i) =>
          val applied = dissectApplied(parent)
          val syntacticArgss = applied.argss
          val semanticArgss = if (i == 0) superArgss else Nil
          var argss = if (syntacticArgss.nonEmpty) semanticArgss else syntacticArgss
          if (argss.isEmpty) argss = List(List())
          val lparent = argss.foldLeft(parent)((curr, args) => g.Apply(curr, args))
          lparent.appendMetadata("isLparent" -> true, "superCtor" -> superCtor.toLogical)
        }
        val lstats = templateStats(limpl, tree, stats)
        Some((edefs, lparents, lself, lstats))
      }
    }

    object Parent {
      // tpt, ctor, argss
      def unapply(tree: g.Tree): Option[(g.Tree, l.CtorIdent, List[List[g.Tree]])] = {
        if (tree.hasMetadata("isLparent")) {
          val applied = dissectApplied(tree)
          (applied.callee, applied.core, applied.argss) match {
            case (tpt, classRef: g.RefTree, argss) =>
              val ctorSym = tree.metadata.get("superCtor").map(_.require[l.Symbol]).getOrElse(l.Zero)
              val ctor = l.CtorIdent(ctorSym, classRef)
              Some((tpt, ctor, argss))
            case _ =>
              None
          }
        } else {
          None
        }
      }
    }

    object SelfDef {
      // name, tpt
      def unapply(tree: g.ValDef): Option[(l.TermParamName, g.Tree)] = {
        if (!tree.hasMetadata("isLself")) return None
        val isAnonymous = tree.name == g.nme.WILDCARD || tree.name.startsWith(g.nme.FRESH_TERM_NAME_PREFIX)
        val ldenot = {
          val lmdef = tree.metadata("LimplDef").asInstanceOf[g.MemberDef]
          val lsym = if (lmdef.symbol != g.NoSymbol) l.Self(lmdef.symbol) else l.Zero
          l.Denotation(lmdef.symbol.thisPrefix, lsym)
        }
        val lvalue = if (isAnonymous) "this" else tree.displayName
        val lname = if (isAnonymous) l.AnonymousName(ldenot) else l.TermName(ldenot, lvalue)
        Some((lname, tree.tpt))
      }
    }

    // ============ MODIFIERS ============

    trait Modifier extends Tree
    case class Private(within: g.Tree) extends Modifier // TODO: `within: l.QualifierName`
    case class Protected(within: g.Tree) extends Modifier // TODO: `within: l.QualifierName`
    case class Implicit() extends Modifier
    case class Final() extends Modifier
    case class Sealed() extends Modifier
    case class Override() extends Modifier
    case class Case() extends Modifier
    case class Abstract() extends Modifier
    case class Covariant() extends Modifier
    case class Contravariant() extends Modifier
    case class Lazy() extends Modifier
    case class ValParam() extends Modifier
    case class VarParam() extends Modifier

    object Modifiers {
      def apply(tree: g.MemberDef): List[l.Modifier] = {
        val mods @ g.Modifiers(flags, privateWithin, _) = tree.mods

        case class ClassParameterInfo(param: g.ValDef, field: Option[g.ValDef], ctor: g.DefDef, cdef: g.ClassDef)
        val cpinfo: Option[ClassParameterInfo] = {
          val syntactically = {
            // TODO: We don't have tests for this, so for now I'll just comment this logic out.
            // tree.hierarchy match {
            //   case
            //     (param @ g.ValDef(_, _, _, _)) ::
            //     (ctor @ g.DefDef(_, nme.CONSTRUCTOR, _, _, _, _)) ::
            //     (cdef @ g.ClassDef(_, _, _, g.Template(_, _, siblings))) :: _ =>
            //       import g.TermNameOps
            //       val field = siblings.collectFirst { case field: g.ValDef if field.name == param.name.localName => field }
            //       val primCtor = siblings.collectFirst { case primCtor: g.DefDef if primCtor.name == nme.CONSTRUCTOR => primCtor }
            //       val isClassParameter = primCtor.map(primCtor => ctor == primCtor).getOrElse(false)
            //       if (isClassParameter) Some(ClassParameterInfo(param, field, ctor, cdef))
            //       else None
            //   case _ =>
            //     None
            // }
            None
          }
          val semantically = {
            if (tree.symbol.owner.isPrimaryConstructor) {
              val param = tree.require[g.ValDef]
              val ctorSym = param.symbol.owner
              val ctor = g.DefDef(ctorSym, g.EmptyTree)
              val classSym = ctorSym.owner
              val cdef = g.ClassDef(classSym, g.Template(Nil, g.noSelfType, Nil))
              val getterSym = classSym.info.member(param.name)
              val fieldSym = getterSym.map(_.owner.info.member(getterSym.localName))
              val field = if (fieldSym != g.NoSymbol) Some(g.ValDef(fieldSym)) else None
              Some(ClassParameterInfo(param, field, ctor, cdef))
            } else None
          }
          syntactically.orElse(semantically)
        }

        val lannotationMods: List[l.Modifier] = {
          aggregateAnnotations(tree)
        }

        val laccessQualifierMods: List[l.Modifier] = {
          val privateWithinCarrierSym = tree.symbol.getterIn(tree.symbol.owner).orElse(tree.symbol)
          val privateWithinSym = privateWithinCarrierSym.privateWithin.orElse(tree.symbol.owner)
          if (mods.hasFlag(SYNTHETIC) && mods.hasFlag(ARTIFACT)) {
            // NOTE: some sick artifact vals produced by mkPatDef can be private to method (whatever that means)
            Nil
          } else if (mods.hasFlag(LOCAL)) {
            val lprivateWithin = g.This(privateWithin.toTypeName).setSymbol(privateWithinSym)
            if (mods.hasFlag(PROTECTED)) List(l.Protected(lprivateWithin))
            else if (mods.hasFlag(PRIVATE)) List(l.Private(lprivateWithin))
            else unreachable(debug(mods))
          } else if (mods.hasAccessBoundary && privateWithin != g.tpnme.EMPTY) {
            // TODO: `private[pkg] class C` doesn't have PRIVATE in its flags
            // so we need to account for that!
            val lprivateWithin = l.IndeterminateName(privateWithinSym.prefix, privateWithinSym.toLogical, privateWithin.toString)
            if (mods.hasFlag(PROTECTED)) List(l.Protected(lprivateWithin))
            else List(l.Private(lprivateWithin))
          } else {
            val privateWithinSym = tree.symbol.owner
            val lprivateWithin = l.AnonymousName(privateWithinSym.prefix, privateWithinSym.toLogical)
            if (mods.hasFlag(PROTECTED)) List(l.Protected(lprivateWithin))
            else if (mods.hasFlag(PRIVATE)) List(l.Private(lprivateWithin))
            else Nil
          }
        }

        val lotherMods: List[l.Modifier] = {
          val lmods = scala.collection.mutable.ListBuffer[l.Modifier]()
          if (mods.hasFlag(IMPLICIT)) lmods += l.Implicit()
          if (mods.hasFlag(FINAL)) lmods += l.Final()
          if (mods.hasFlag(SEALED)) lmods += l.Sealed()
          if (mods.hasFlag(OVERRIDE)) lmods += l.Override()
          if (mods.hasFlag(CASE)) lmods += l.Case()
          if (mods.hasFlag(ABSTRACT) && tree.isInstanceOf[g.ClassDef] && !mods.hasFlag(TRAIT)) lmods += l.Abstract()
          if (mods.hasFlag(ABSOVERRIDE)) { lmods += l.Abstract(); lmods += l.Override() }
          if (mods.hasFlag(COVARIANT) && tree.isInstanceOf[g.TypeDef]) lmods += l.Covariant()
          if (mods.hasFlag(CONTRAVARIANT) && tree.isInstanceOf[g.TypeDef]) lmods += l.Contravariant()
          if (mods.hasFlag(LAZY)) lmods += l.Lazy()
          lmods.toList
        }

        val lvalVarParamMods: List[l.Modifier] = {
          val lmods = scala.collection.mutable.ListBuffer[l.Modifier]()
          val field = cpinfo.flatMap(_.field)
          val isImmutableField = field.map(!_.mods.hasFlag(MUTABLE)).getOrElse(false)
          val isMutableField = field.map(_.mods.hasFlag(MUTABLE)).getOrElse(false)
          val inCaseClass = cpinfo.map(_.cdef).map(_.mods.hasFlag(CASE)).getOrElse(false)
          if (isMutableField) lmods += l.VarParam()
          if (isImmutableField && !inCaseClass) lmods += l.ValParam()
          lmods.toList
        }

        val result = lannotationMods ++ laccessQualifierMods ++ lotherMods ++ lvalVarParamMods

        // NOTE: we can't discern `class C(x: Int)` and `class C(private[this] val x: Int)`
        // so let's err on the side of the more popular option
        if (cpinfo.nonEmpty) result.filter({ case l.Private(g.This(_)) => false; case _ => true })
        else result
      }
    }

    private def aggregateAnnotations(mdef: g.MemberDef): List[l.Annotation] = {
      val syntacticAnnots = mdef.mods.annotations
      val semanticAnnots = mdef.symbol.annotations
      require(syntacticAnnots.isEmpty || semanticAnnots.isEmpty)
      if (syntacticAnnots.nonEmpty) syntacticAnnots.map(Annotation.apply)
      else semanticAnnots.map(Annotation.apply)
    }

    case class Annotation(tree: g.Tree) extends Modifier
    object Annotation {
      def apply(info: g.AnnotationInfo): l.Annotation = {
        // TODO: set Annotation.tree's parent to g.EmptyTree
        ???
      }
    }

    // ============ ODDS & ENDS ============

    object CaseDef {
      def unapply(tree: g.CaseDef): Option[(g.Tree, Option[g.Tree], g.Tree)] = {
        val g.CaseDef(pat, guard, body) = tree
        val lpat = pat.appendMetadata("isLpat" -> true)
        val lguard = if (guard.nonEmpty) Some(guard) else None
        Some((lpat, lguard, body))
      }
    }

    // ============ HELPERS ============

    private def applyBounds(tparams: List[g.TypeDef], paramss: List[List[g.ValDef]]): List[g.TypeDef] = {
      def tparam(targ: Tree): Option[g.TypeDef] = tparams.filter(tparam => {
        if (tparam.symbol != g.NoSymbol) tparam.symbol == targ.symbol
        else targ match { case g.Ident(name) => name == tparam.name; case _ => false }
      }).headOption

      object ViewBound {
        def unapply(tree: g.ValDef): Option[(g.TypeDef, g.Tree)] = tree match {
          case g.ValDef(_, _, tpt @ g.TypeTree(), _) =>
            val tycon = if (tpt.tpe != null) tpt.tpe.typeSymbolDirect else g.NoSymbol
            val targs = if (tpt.tpe != null) tpt.tpe.typeArgs else Nil
            val (fromTpe, toTpe) = targs match { case List(from, to) => (from, to); case _ => (g.NoType, g.NoType) }
            val fromSym = fromTpe.typeSymbol
            val tyconMatches = tycon == g.definitions.FunctionClass(1)
            if (tyconMatches) tparam(Ident(fromSym)).map(tparam => (tparam, g.TypeTree(toTpe)))
            else None
          case g.ValDef(_, _, g.AppliedTypeTree(tycon, from :: to :: Nil), _) =>
            val tyconMatches = tycon match {
              // NOTE: this doesn't handle every possible case (e.g. an Ident binding to renamed import),
              // but it should be ood enough for 95% of the situations
              case g.Select(pre, g.TermName("Function1")) => pre.symbol == g.definitions.ScalaPackage
              case tycon: g.RefTree => tycon.symbol == g.definitions.FunctionClass(1)
              case _ => false
            }
            if (tyconMatches) tparam(from).map(tparam => (tparam, to))
            else None
          case _ =>
            None
        }
      }

      object ContextBound {
        def unapply(tree: g.ValDef): Option[(g.TypeDef, g.Tree)] = tree match {
          case g.ValDef(_, _, tpt @ g.TypeTree(), _) =>
            val tycon = if (tpt.tpe != null) tpt.tpe.typeSymbolDirect else g.NoSymbol
            val targs = if (tpt.tpe != null) tpt.tpe.typeArgs else Nil
            val targ = targs.map(_.typeSymbol) match { case List(sym) => sym; case _ => g.NoSymbol }
            tparam(Ident(targ)).map(tparam => (tparam, Ident(tycon)))
          case g.ValDef(_, _, g.AppliedTypeTree(tycon, targ :: Nil), _) =>
            tparam(targ).map(tparam => (tparam, tycon))
          case _ =>
            None
        }
      }

      val (explicitss, implicitss) = paramss.partition(_.exists(_.mods.hasFlag(IMPLICIT)))
      val (bounds, implicits) = implicitss.flatten.partition(_.name.startsWith(nme.EVIDENCE_PARAM_PREFIX))
      tparams.map(tparam => {
        val vbounds = bounds.flatMap(ViewBound.unapply).filter(_._1.name == tparam.name).map(_._2)
        val cbounds = bounds.flatMap(ContextBound.unapply).filter(_._1.name == tparam.name).map(_._2)
        tparam.appendMetadata("lvbounds" -> vbounds, "lcbounds" -> cbounds)
      })
    }

    private def removeBounds(paramss: List[List[g.ValDef]]): List[List[g.ValDef]] = {
      if (paramss.isEmpty) return Nil
      val init :+ last = paramss
      val hasImplicits = last.exists(_.mods.hasFlag(IMPLICIT))
      val explicitss = if (hasImplicits) init else init :+ last
      val implicits = if (hasImplicits) last else Nil
      val limplicits = implicits.filter(_.name.startsWith(nme.EVIDENCE_PARAM_PREFIX))
      if (limplicits.nonEmpty) explicitss :+ limplicits else explicitss
    }

    private def toplevelStats(pkg: g.PackageDef, stats: List[g.Tree]): List[g.Tree] = {
      stats.map {
        case stat: g.PackageDef => stat.appendMetadata("isLnestedPkg" -> true)
        case stat => stat
      }
    }

    private def templateStats(impl: g.ImplDef, templ: g.Template, stats: List[g.Tree]): List[g.Tree] = {
      // TODO: relevant synthetics are described in:
      // * subclasses of MultiEnsugarer: DefaultGetter, VanillaAccessor, AbstractAccessor, LazyAccessor
      // * ToMtree.mstats: PatDef, InlinableHoistedTemporaryVal
      val lresult = mutable.ListBuffer[g.Tree]()
      var i = 0
      while (i < stats.length) {
        val stat = stats(i)
        i += 1
        stat match {
          case l.PrimaryCtorDef(_, _, _) => // skip this
          case g.DefDef(_, nme.CONSTRUCTOR, _, _, _, _) if impl.isInstanceOf[g.ModuleDef] => // and this
          case g.DefDef(_, nme.MIXIN_CONSTRUCTOR, _, _, _, _) => // and this
          case _ => lresult += stat
        }
      }
      lresult.toList
    }

    private def blockStats(blk: g.Block, stats: List[g.Tree]): List[g.Tree] = {
      stats
    }
  }
}
