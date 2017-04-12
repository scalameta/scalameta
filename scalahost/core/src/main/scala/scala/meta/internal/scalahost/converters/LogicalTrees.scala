package scala.meta
package internal
package scalahost
package converters

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.tools.nsc.Global
import scala.reflect.internal.{Flags => Gflags}
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.adt._
import org.scalameta.debug

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
// 3) Extractors and supporting intermediate data structures should be written in the same order
//    that the corresponding AST nodes in scala/meta/Trees.scala are in.
//
// Source code that one might find helpful in implementing extractors:
//
// 1) Ensugar.scala (the resugaring module of the previous implementation of scalahost).
//    Contains several dozen clearly modularized recipes for resugarings.
//    Want to know what synthetic members are generated for lazy abstract vals?
//    What about default parameters on class constructors? It's all there.
//    https://github.com/scalameta/scalameta/blob/92b65b841685871b4401f00456a25de2b7a177b6/foundation/src/main/scala/org/scalameta/reflection/Ensugar.scala
//
// 2) ToMtree.scala (the converter module of the previous implementation of scalahost).
//    Transforms scala.reflect's encodings of Scala syntax into scala.meta.
//    Contains knowledge how to collapse multipart val/var definitions and other related stuff.
//    https://github.com/scalameta/scalameta/blob/92b65b841685871b4401f00456a25de2b7a177b6/interface/src/main/scala/scala/meta/internal/hosts/scalac/converters/ToMtree.scala
//
// 3) ReificationSupport.scala (the quasiquote support module of scala/scala)
//    Contains various SyntacticXXX extractors that do things similar to our l.XXX extractors.
//    https://github.com/scala/scala/blob/1fbce4612c21a4d0c553ea489b4765494828c09f/src/reflect/scala/reflect/internal/ReificationSupport.scala

class LogicalTrees[G <: Global](val global: G, root: G#Tree) extends ReflectToolkit { l =>
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
  import g.{require => _, abort => _, _}
  import definitions._
  import treeInfo._
  import build._

  // NOTE: For some trees, namely: Apply, Typed and Ident -
  // it is necessary to know whether they are used in term context or in pattern context.
  // This set provides an exhaustive list of such subtrees in the converted tree.
  private val patterns: Set[g.Tree] = root.asInstanceOf[g.Tree].childrenPatterns

  // ============ NAMES ============

  trait Name extends Tree

  trait QualifierName extends Name

  case class AnonymousName() extends Name with TermParamName with TypeParamName with QualifierName
  object AnonymousName {
    def apply(pre: g.Type): l.AnonymousName = apply()
  }

  case class IndeterminateName(value: String) extends Name with QualifierName
  object IndeterminateName {
    def apply(pre: g.Type, value: String): l.IndeterminateName = {
      l.IndeterminateName(value)
    }
  }

  implicit class RichNameTree(tree: Tree) {
    def displayName: String = tree match {
      case tree: ModuleDef if tree.name == nme.PACKAGE =>
        unreachable(debug(tree, showRaw(tree))) // dveim was abort(tree)
      case tree: NameTree => tree.name.displayName
      case This(name) => name.displayName // NOTE: This(tpnme.EMPTY) is also accounted for
      case Super(_, name) => name.displayName
      case tree: l.IndeterminateName => tree.value
      case tree: l.TermName => tree.value
      case tree: l.TypeName => tree.value
      case tree: l.CtorName => tree.value
      case _ => unreachable(debug(tree, showRaw(tree)))
    }
  }

  // Ideally, this logic should move closer where it's used, in the same fashion
  // as other desugaring are handled by the converter. However, we make an exception
  // for cases when the destination meta tree type is unknown. For example, in
  //   before: foo((x$1, x$2) => x$1 + x$2)
  //   after:  foo(_ + _)
  // the argument to `foo` is undesugared from a g.Function to an m.Term.ApplyInfix.
  // the argument could have been any subtype of m.Term, which are a few dozen cases,
  // so we handle the case once here for all subtypes of m.Term.
  object UndoDesugaring {
    def unapply(tree: g.Tree): Option[g.Tree] = {
      tree match {
        // before: function((x$1, x$2) => x$1 + x$2)
        // after:  function(_ + _)
        case g.Function(vparams, body)
            if vparams.forall(x =>
              x.mods.hasFlag(Gflags.SYNTHETIC) &&
                x.name.startsWith(nme.FRESH_TERM_NAME_PREFIX)) =>
          Some(body)
        case t: g.ValDef =>
          Some(l.undoValDefDesugarings(List(t)).head)
        case _ =>
          None
      }
    }
  }

  // ============ TERMS ============

  object TermThis {
    def unapply(tree: g.This): Option[l.QualifierName] = {
      if (tree.qual == tpnme.EMPTY) Some(l.AnonymousName())
      else Some(l.IndeterminateName(tree.displayName))
    }
  }

  object TermSuper {
    def unapply(tree: g.Super): Option[(l.QualifierName, l.QualifierName)] = {
      val g.Super(l.TermThis(lthis), qual) = tree
      val lsuper = {
        if (qual == tpnme.EMPTY) l.AnonymousName()
        else l.IndeterminateName(qual.displayName)
      }
      Some((lthis, lsuper))
    }
  }

  case class TermName(value: String) extends Name with TermParamName
  object TermName {
    def apply(tree: g.NameTree): l.TermName = {
      require(
        tree.name.isTermName &&
          tree.name != nme.WILDCARD &&
          tree.name != nme.CONSTRUCTOR &&
          debug(tree, showRaw(tree)))
      new l.TermName(tree.displayName).setType(tree.tpe)
    }
  }

  object TermIdent {
    def unapply(tree: g.Ident): Option[l.TermName] = {
      if (tree.name.startsWith(nme.FRESH_TERM_NAME_PREFIX)) return None
      val g.Ident(name) = tree
      if (name.isTypeName || tree.name == nme.WILDCARD) return None
      Some(l.TermName(tree.displayName).setType(tree.tpe))
    }
  }

  object TermSelect {
    def unapply(tree: g.Select): Option[(g.Tree, l.TermName)] = {
      val g.Select(qual, name) = tree
      if (TermNew.unapply(tree).isDefined) return None
      if (name.isTypeName) return None
      Some((qual, l.TermName(tree)))
    }
  }

  object TermOrPatInterpolate {
    def unapply(tree: g.Tree): Option[(l.TermName, List[g.Tree], List[g.Tree])] = {
      // scalac desugars bb"a $c" into StringContext("a ", "").bb(c). To detect
      // if the desugaring happened, we make sure that the tree start position
      // starts with the prefix ("bb" from example) followed by double quote character.
      def positionMatchesInterpolation(prefix: Array[Char]): Boolean = {
        val chars =
          tree.pos.source.content.slice(tree.pos.start, tree.pos.start + prefix.length + 1)
        chars.startsWith(prefix) && chars.lastOption.contains('"')
      }
      tree match {
        case g.Apply(g.Select(g.Apply(g.Ident(g.TermName("StringContext")), parts), prefix), args)
            if positionMatchesInterpolation(prefix.toChars) =>
          Some((l.TermName(prefix.displayName), parts, args))
        case _ => None
      }
    }
  }

  object TermInterpolate {
    def unapply(tree: g.Apply): Option[(l.TermName, List[g.Tree], List[g.Tree])] = {
      if (patterns(tree)) return None
      TermOrPatInterpolate.unapply(tree)
    }
  }

  object TermApply {
    def unapply(tree: g.Apply): Option[(g.Tree, List[g.Tree])] = {
      if (TermInterpolate.unapply(tree).isDefined) return None
      if (patterns(tree)) return None
      if (TermNew.unapply(tree).isDefined) return None
      if (TermTuple.unapply(tree).isDefined) return None
      if (TermApplyInfix.unapply(tree).isDefined) return None
      Some((tree.fun, tree.args))
    }
  }

  object TermApplyInfix {
    def unapply(tree: g.Tree): Option[(
        g.Tree,
        l.TermName,
        List[g.Tree],
        List[g.Tree]
    )] = {
      if (patterns(tree)) return None
      tree match {
        case g.treeInfo.Applied(g.Select(lhs, op: g.TermName), targs, List(rhs))
            if op.looksLikeInfix && !op.isRightAssoc =>
          Some((lhs, l.TermName(op.displayName), targs, rhs))
        case g.Block(List(g.ValDef(mods, synthetic1, g.TypeTree(), lhs)),
                     g.treeInfo.Applied(g.Select(rhs, op), targs, List(List(synthetic2))))
            if mods == g.Modifiers(Gflags.SYNTHETIC | Gflags.ARTIFACT) &&
              synthetic1.toString == synthetic2.toString &&
              synthetic1.decodedName.toString.contains("$") =>
          val args = rhs match {
            case q"$tuple(..$args)" if tuple.toString.startsWith("scala.Tuple") => args
            case arg => List(arg)
          }
          Some((lhs, l.TermName(op.displayName), targs, args))
        case _ =>
          None
      }
    }
  }

  object TermApplyType {
    def unapply(tree: g.TypeApply): Option[(g.Tree, List[g.Tree])] = {
      Some((tree.fun, tree.args))
    }
  }

  object TermAssign {
    def unapply(tree: g.Assign): Option[(g.Tree, g.Tree)] = {
      Some((tree.lhs, tree.rhs))
    }
  }

  object TermReturn {
    def unapply(tree: g.Return): Option[g.Tree] = {
      Some(tree.expr)
    }
  }

  object TermThrow {
    def unapply(tree: g.Throw): Option[g.Tree] = {
      Some(tree.expr)
    }
  }

  object TermAscribe {
    def unapply(tree: g.Typed): Option[(g.Tree, g.Tree)] = {
      tree.tpt match {
        case EtaExpansion() => None
        case _ =>
          if (patterns(tree)) return None
          if (TermArg.Repeated.unapply(tree).isDefined) return None
          Some((tree.expr, tree.tpt))
      }
    }
  }

  object TermAnnotate {
    def unapply(gtree: g.Annotated): Option[(g.Tree, List[l.Annotation])] = {
      val (lexpr, lannots) = flattenAnnotated(gtree)
      if (!lexpr.isTerm) return None
      Some((lexpr, lannots.map(Annotation.apply)))
    }
  }

  object TermTuple {
    def unapply(tree: g.Apply): Option[List[g.Tree]] = {
      val g.Apply(fun, args) = tree
      if (patterns(tree)) return None
      if (!fun.toString.startsWith("scala.Tuple") || args.length <= 1) return None
      Some(args)
    }
  }

  object TermBlock {
    def unapply(tree: g.Block): Option[List[g.Tree]] = {
      if (TermNew.unapply(tree).isDefined) return None
      if (TermApplyInfix.unapply(tree).isDefined) return None
      val lstats = blockStats(tree.stats :+ tree.expr)
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
      if (tree.selector.isEmpty) return None
      Some((tree.selector, tree.cases))
    }
  }

  object TermTryWithCases {
    def unapply(tree: g.Try): Option[(g.Tree, List[g.Tree], Option[g.Tree])] = {
      val lfinallyp = if (tree.finalizer != g.EmptyTree) Some(tree.finalizer) else None
      Some((tree.block, tree.catches, lfinallyp))
    }
  }

  object TermFunction {
    def unapply(tree: g.Function): Option[(List[l.TermParamDef], g.Tree)] = {
      Some((tree.vparams.map(l.TermParamDef.apply), tree.body))
    }
  }

  object TermPartialFunction {
    def unapply(tree: g.Match): Option[List[g.Tree]] = {
      if (!tree.selector.isEmpty) return None
      Some(tree.cases)
    }
  }

  object TermWhile {
    def unapply(tree: g.LabelDef): Option[(g.Tree, g.Tree)] = {
      tree match {
        case g.LabelDef(
            name1,
            Nil,
            g.If(cond, g.Block(List(body), g.Apply(Ident(name2), Nil)), UnitConstant()))
            if name1 == name2 && name1.startsWith(nme.WHILE_PREFIX) =>
          Some((cond, body))
        case _ =>
          None
      }
    }
  }

  object TermDo {
    def unapply(tree: g.LabelDef): Option[(g.Tree, g.Tree)] = {
      tree match {
        case g.LabelDef(
            name1,
            Nil,
            g.Block(List(body), g.If(cond, g.Apply(Ident(name2), Nil), UnitConstant())))
            if name1 == name2 && name1.startsWith(nme.DO_WHILE_PREFIX) =>
          Some((body, cond))
        case _ =>
          None
      }
    }
  }

  object TermPlaceholder {
    def unapply(tree: g.Ident): Boolean = tree.name.startsWith(nme.FRESH_TERM_NAME_PREFIX)
  }

  object TermNew {
    def unapply(tree: g.Tree): Option[l.Template] = tree match {
      case g.Apply(g.Select(g.New(tpt), nme.CONSTRUCTOR), args) =>
        val lparent = l.Parent(g.Apply(tpt, args))
        val lself = l.Self(l.AnonymousName(), g.TypeTree())
        Some(l.Template(Nil, List(lparent), lself, None))
      case g.Select(g.New(tpt), nme.CONSTRUCTOR) =>
        val lparent = l.Parent(tpt)
        val lself = l.Self(l.AnonymousName(), g.TypeTree())
        Some(l.Template(Nil, List(lparent), lself, None))
      case g.Block(
          List(
            tree @ g
              .ClassDef(g.Modifiers(Gflags.FINAL, g.tpnme.EMPTY, Nil), g.TypeName(anon1), Nil, templ)),
          g.Apply(g.Select(g.New(g.Ident(g.TypeName(anon2))), nme.CONSTRUCTOR), args))
          if anon1 == tpnme.ANON_CLASS_NAME.toString && anon2 == tpnme.ANON_CLASS_NAME.toString =>
        Some(l.Template(tree))
      case _ =>
        None
    }
  }

  object EtaExpansion {
    def unapply(tree: g.Tree): Boolean = tree match {
      case g.Function(Nil, g.EmptyTree) => true
      case _ => false
    }
  }

  object TermEta {
    def unapply(tree: g.Typed): Option[g.Tree] = {
      tree match {
        case g.Typed(lexpr, EtaExpansion()) =>
          Some(lexpr)
        case _ =>
          None
      }
    }
  }

  object TermArg {
    object Named {
      def unapply(tree: g.Tree): Option[(g.Tree, g.Tree)] = tree match {
        case g.AssignOrNamedArg(lhs, rhs) => Some((lhs, rhs))
        case _ => None
      }
    }

    object Repeated {
      def unapply(tree: g.Tree): Option[g.Ident] = tree match {
        case g.Typed(ident: g.Ident, g.Ident(g.typeNames.WILDCARD_STAR)) => Some(ident)
        case _ => None
      }
    }
  }

  trait TermParamName extends Name

  case class TermParamDef(mods: List[l.Modifier],
                          name: l.TermParamName,
                          tpt: Option[g.Tree],
                          default: Option[g.Tree])
      extends Tree

  object TermParamDef {
    def apply(tree: g.ValDef): l.TermParamDef = {
      val g.ValDef(_, _, tpt, default) = tree
      val ltpt = if (tpt.nonEmpty) Some(tpt) else None
      val ldefault = if (default.nonEmpty) Some(default) else None
      val lname =
        if (tree.name.startsWith(nme.FRESH_TERM_NAME_PREFIX) ||
            tree.name == nme.WILDCARD) l.AnonymousName()
        else l.TermName(tree)
      TermParamDef(l.Modifiers(tree), lname, ltpt, ldefault)
    }
  }

  // ============ TYPES ============

  object TypeTree {
    def unapply(tpt: g.TypeTree): Option[g.Type] = {
      if (tpt.nonEmpty) Some((tpt.tpe)) else None
    }
  }

  case class TypeName(value: String) extends Name with TypeParamName
  object TypeName {
    def apply(tree: g.NameTree): l.TypeName = {
      require(tree.name.isTypeName && tree.name != tpnme.WILDCARD && debug(tree, showRaw(tree)))
      new l.TypeName(tree.displayName)
    }
  }

  object TypeIdent {
    def unapply(tree: g.Ident): Option[l.TypeName] = {
      val g.Ident(name) = tree
      if (name.isTermName) return None
      Some(l.TypeName(tree.displayName))
    }
  }

  object TypeSelect {
    def unapply(tree: g.Select): Option[(g.Tree, l.TypeName)] = {
      val g.Select(qual, name) = tree
      if (name.isTermName) return None
      Some((qual, l.TypeName(tree)))
    }
  }

  object TypeProject {
    def unapply(tree: g.SelectFromTypeTree): Option[(g.Tree, l.TypeName)] = {
      val g.SelectFromTypeTree(qual, name) = tree
      Some((qual, l.TypeName(tree)))
    }
  }

  object TypeSingleton {
    def unapply(tree: g.SingletonTypeTree): Option[g.Tree] = {
      Some(tree.ref)
    }
  }

  object TypeArgByName {
    def unapply(tree: g.AppliedTypeTree): Option[g.Tree] = {
      tree match {
        case TypeApply(TypeSelect(_, TypeName("<byname>")), Seq(tpe)) => Some(tpe)
        case _ => None
      }
    }
  }

  object TypeArgRepeated {
    def unapply(tree: g.AppliedTypeTree): Option[g.Tree] = {
      tree match {
        case TypeApply(TypeSelect(_, TypeName("<repeated>")), Seq(tpe)) => Some(tpe)
        case _ => None
      }
    }
  }

  object TypeOrPatTypeApply {
    def unapply(tree: g.AppliedTypeTree): Option[(g.Tree, List[g.Tree])] = {
      if (TypeApplyInfix.unapply(tree).isDefined) return None
      if (TypeFunction.unapply(tree).isDefined) return None
      if (TypeTuple.unapply(tree).isDefined) return None
      Some((tree.tpt, tree.args))
    }
  }

  object TypeApply {
    def unapply(tree: g.AppliedTypeTree): Option[(g.Tree, List[g.Tree])] = {
      if (patterns(tree)) return None
      TypeOrPatTypeApply.unapply(tree)
    }
  }

  object TypeApplyInfix {
    def unapply(tree: g.AppliedTypeTree): Option[(g.Tree, l.TypeName, g.Tree)] = {
      tree match {
        case g.AppliedTypeTree(op @ Ident(name: g.TypeName), List(lhs, rhs))
            if name.looksLikeInfix =>
          Some((lhs, l.TypeName(op), rhs))
        case _ =>
          None
      }
    }
  }

  object TypeFunction {
    def unapply(tree: g.AppliedTypeTree): Option[(List[g.Tree], g.Tree)] = {
      val g.AppliedTypeTree(tpt, args) = tree
      if (!tpt.toString.startsWith("_root_.scala.Function")) return None
      Some((args.init, args.last))
    }
  }

  object TypeTuple {
    def unapply(tree: g.AppliedTypeTree): Option[List[g.Tree]] = {
      if (patterns(tree)) return None
      val g.AppliedTypeTree(tpt, args) = tree
      if (!tpt.toString.startsWith("scala.Tuple") || args.length <= 1) return None
      Some(args)
    }
  }

  object TypeOrPatTypeWith {
    def unapply(tree: g.Tree): Option[(g.Tree, g.Tree)] = tree match {
      case tree @ g.CompoundTypeTree(g.Template(parents0, _, Nil)) =>
        val parents = parents0.filter(_.toString != "scala.AnyRef")
        parents match {
          case Nil => None
          case List(tpe) => None
          case List(left, right) => Some((left, right))
          case lefts :+ right =>
            Some((g.CompoundTypeTree(g.Template(lefts, noSelfType, Nil)), right))
        }
      case _ =>
        None
    }
  }

  object TypeWith {
    def unapply(tree: g.Tree): Option[(g.Tree, g.Tree)] = {
      if (patterns.contains(tree)) return None
      TypeOrPatTypeWith.unapply(tree)
    }
  }

  object TypeRefine {
    def unapply(tree: g.CompoundTypeTree): Option[(Option[g.Tree], List[g.Tree])] = {
      val g.Template(parents0, _, stats) = tree.templ
      val parents = parents0.filter(_.toString != "scala.AnyRef")
      parents match {
        case Nil =>
          Some((None, stats))
        case List(tpe) =>
          Some((Some(tpe), stats))
        case tpes =>
          if (stats.isEmpty) None
          else Some((Some(g.CompoundTypeTree(g.Template(tpes, noSelfType, Nil))), stats))
      }
    }
  }

  object TypeExistential {
    def unapply(tree: g.ExistentialTypeTree): Option[(g.Tree, List[g.Tree])] = {
      Some((tree.tpt, tree.whereClauses))
    }
  }

  object TypeOrPatTypeAnnotate {
    def unapply(gtree: g.Annotated): Option[(g.Tree, List[l.Annotation])] = {
      val (ltpe, lannots) = flattenAnnotated(gtree)
      if (!ltpe.isType) return None
      Some((ltpe, lannots.map(Annotation.apply)))
    }
  }

  object TypeAnnotate {
    def unapply(gtree: g.Annotated): Option[(g.Tree, List[l.Annotation])] = {
      if (patterns.contains(gtree)) return None
      TypeOrPatTypeAnnotate.unapply(gtree)
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

  case class TypeParamDef(mods: List[l.Modifier],
                          name: l.TypeParamName,
                          tparams: List[l.TypeParamDef],
                          tbounds: g.Tree,
                          vbounds: List[g.Tree],
                          cbounds: List[g.Tree])
      extends Tree

  object TypeParamDef {
    def apply(tree: g.TypeDef, vbounds: List[g.Tree], cbounds: List[g.Tree]): l.TypeParamDef = {
      val g.TypeDef(_, name, tparams, rhs) = tree
      val lname =
        if (tree.name.isAnonymous) l.AnonymousName()
        else l.TypeName(tree)
      val ltparams = mkTparams(tparams, Nil)
      val ltbounds = {
        rhs match {
          case rhs: g.TypeBoundsTree =>
            rhs
          case rhs: g.TypeTree =>
            val g.TypeBounds(lo, hi) = rhs.tpe
            g.TypeBoundsTree(g.TypeTree(lo), g.TypeTree(hi))
        }
      }
      TypeParamDef(l.Modifiers(tree), lname, ltparams, ltbounds, vbounds, cbounds)
    }
  }

  // ============ PATTERNS ============

  case class PatVarTerm(name: l.TermName) extends Tree
  object PatVarTerm {
    def apply(tree: g.DefTree): l.PatVarTerm = {
      l.PatVarTerm(l.TermName(tree).setType(tree.tpe))
    }
  }

  case class PatVarType(name: l.TypeName) extends Tree

  // NOTE. in "val _ = 2", the wildcard is actually an identifier `_`.
  // However, l.TermName requires the name to be non-wildcard so we special case
  // this (very rare) pattern in it's own case object.
  case object ValPatWildcard extends Tree

  object PatWildcard {
    def unapply(tree: g.Tree): Boolean = {
      tree match {
        case g.Ident(name) if patterns(tree) =>
          name == nme.WILDCARD
        case ValPatWildcard =>
          true
        case _ =>
          false
      }
    }
  }

  object PatTypeApply {
    def unapply(tree: g.AppliedTypeTree): Option[(g.Tree, List[g.Tree])] = {
      if (!patterns(tree)) return None
      TypeOrPatTypeApply.unapply(tree).map {
        case (ltpt, gargs) =>
          val largs = gargs.map {
            case g.Bind(typeName @ g.TypeName(name), g.EmptyTree)
                if typeName != typeNames.WILDCARD =>
              l.PatVarType(l.TypeName(name))
            case x => x
          }
          (tree.tpt, largs)
      }
    }
  }

  object PatTypeWith {
    def unapply(tree: g.Tree): Option[(g.Tree, g.Tree)] = {
      if (!patterns.contains(tree)) return None
      TypeOrPatTypeWith.unapply(tree)
    }
  }

  object PatBind {
    def unapply(tree: g.Bind): Option[(g.Tree, g.Tree)] = {
      if (tree.name.isTypeName) return None
      tree match {
        case g.Bind(name, g.Typed(Ident(nme.WILDCARD), tpe)) =>
          None
        case g.Bind(name, body) =>
          Some((l.PatVarTerm(tree), body))
      }

    }
  }

  object PatAlternative {
    def unapply(tree: g.Alternative): Option[(g.Tree, g.Tree)] = {
      val g.Alternative(head :: tail) = tree
      val llhs = head
      val lrhs = tail match {
        case Nil => return None
        case head :: Nil => head
        case trees @ (head :: tail) => g.Alternative(trees)
      }
      Some((llhs, lrhs))
    }
  }

  object PatTuple {
    def unapply(tree: g.Tree): Option[List[g.Tree]] = {
      if (!patterns(tree)) return None
      tree match {
        case g.Apply(fun, args) =>
          if (!fun.toString.startsWith("scala.Tuple") || args.length <= 1) return None
          Some(args)
        case _ =>
          None
      }
    }
  }

  object PatExtract {
    def unapply(tree: g.Tree): Option[(
        g.Tree,
        List[g.Tree],
        List[g.Tree]
    )] = {
      if (!patterns(tree)) return None
      if (PatTuple.unapply(tree).isDefined) return None
      if (TermOrPatInterpolate.unapply(tree).isDefined) return None
      val (fun, targs, args) = tree match {
        case g.Apply(g.TypeApply(fun, targs), args) => (fun, targs, args)
        case g.Apply(fun, args) => (fun, Nil, args)
        case g.UnApply(g.Apply(g.TypeApply(fun, targs), List(unapplySelector)), args) =>
          (fun, targs, args)
        case g.UnApply(g.Apply(fun, List(unapplySelector)), args) => (fun, Nil, args)
        case _ => return None
      }
      Some((fun, targs, args))
    }
  }

  object PatInterpolate {
    def unapply(tree: g.Apply): Option[(l.TermName, List[g.Tree], List[g.Tree])] = {
      if (!patterns(tree)) return None
      TermOrPatInterpolate.unapply(tree)
    }
  }

  object PatTyped {
    def unapply(tree: g.Tree): Option[(g.Tree, g.Tree)] = {
      if (!patterns(tree)) return None
      tree match {
        case g.Typed(lhs, rhs) =>
          Some((lhs, rhs))
        case tree @ g.Bind(lhsname, g.Typed(Ident(nme.WILDCARD), rhs)) =>
          Some((l.PatVarTerm(tree), rhs))
        case _ =>
          None
      }
    }
  }

  object PatArgSeqWildcard {
    def unapply(tree: g.Star): Boolean = {
      tree match {
        case g.Star(g.Ident(g.termNames.WILDCARD)) => true
        case _ => false
      }
    }
  }

  object PatTypeWildcard {
    def unapply(tree: g.Bind): Boolean = {
      if (!patterns(tree)) return false
      tree match {
        case g.Bind(typeNames.WILDCARD, g.EmptyTree) => true
        case _ => false
      }
    }
  }

  object PatTypeAnnotate {
    def unapply(tree: g.Annotated): Option[(g.Tree, List[l.Annotation])] = {
      if (!patterns.contains(tree)) return None
      TypeOrPatTypeAnnotate.unapply(tree)
    }
  }

  // ============ LITERALS ============

  object Literal {
    def unapply(tree: g.Literal): Option[Any] = tree match {
      case g.Literal(g.Constant(_: g.Type)) => None
      case g.Literal(g.Constant(_: g.Symbol)) => None
      case g.Literal(g.Constant(value)) => Some(value)
      case _ => None
    }
  }

  // ============ DECLS ============

  // We introduce 4 logical trees {Defn,Decl}{Val,Var} to map from g.ValDef.
  // g.ValDef has a single TermName on the left-hand side while the meta
  // trees support a list of patterns on the left-hand side. Example,
  // Before:
  //   @foo val Foo(a, b), Foo(c, d) = 2
  // Desugared:
  //  {
  //    <synthetic> <artifact> private[this] val x$1 = 2: @scala.unchecked match {
  //      case Foo((a @ _), (b @ _)) => scala.Tuple2(a, b)
  //    };
  //    @new foo() val a = x$1._1;
  //    @new foo() val b = x$1._2;
  //    <synthetic> <artifact> private[this] val x$2 = 2: @scala.unchecked match {
  //      case Foo((c @ _), (d @ _)) => scala.Tuple2(c, d)
  //    };
  //    @new foo() val c = x$2._1;
  //    @new foo() val d = x$2._2;
  //    ()
  //  }
  sealed abstract class ValOrVarDefs(val pats: List[g.Tree]) extends Tree

  object ValOrVarDefs {
    // the pattern list is non-empty
    def unapply(tree: ValOrVarDefs): Option[g.Tree] = Some(tree.pats.head)

    def apply(tree: g.ValDef, pat: List[g.Tree], tpt: Option[Tree], rhs: g.Tree): l.ValOrVarDefs = {
      val ltpt = tpt.filterNot(_.isEmpty)
      val lmods = l.Modifiers(tree)
      tree match {
        case g.ValDef(mods, _, _, g.EmptyTree) if mods.hasAllFlags(Gflags.DEFERRED | Gflags.MUTABLE) =>
          l.DeclVar(lmods, pat, ltpt.get)
        case g.ValDef(mods, _, _, _) if mods.hasFlag(Gflags.MUTABLE) =>
          val lrhs = if (rhs.isEmpty && mods.hasFlag(Gflags.DEFAULTINIT)) None else Some(rhs)
          l.DefnVar(lmods, pat, ltpt, lrhs)
        case g.ValDef(mods, _, _, g.EmptyTree) =>
          l.DeclVal(lmods, pat, ltpt.get)
        case g.ValDef(_, _, _, _) =>
          l.DefnVal(lmods, pat, ltpt, rhs)
      }
    }
  }

  case class DeclVal(mods: List[l.Modifier], override val pats: List[g.Tree], tpt: g.Tree)
      extends ValOrVarDefs(pats)
  case class DeclVar(mods: List[l.Modifier], override val pats: List[g.Tree], tpt: g.Tree)
      extends ValOrVarDefs(pats)

  object AbstractDefDef {
    def unapply(tree: g.DefDef): Option[(
        List[l.Modifier],
        l.TermName,
        List[l.TypeParamDef],
        List[List[l.TermParamDef]],
        g.Tree
    )] = {
      tree match {
        case g.DefDef(mods, _, tparams, paramss, tpt, rhs) if mods.hasFlag(Gflags.DEFERRED) =>
          require(tpt.nonEmpty && rhs.isEmpty)
          val ltparams = mkTparams(tparams, paramss)
          val lparamss = mkVparamss(paramss)
          Some((l.Modifiers(tree), l.TermName(tree), ltparams, lparamss, tpt))
        case _ =>
          None
      }
    }
  }

  object AbstractTypeDef {
    def unapply(tree: g.TypeDef): Option[(
        List[l.Modifier],
        l.TypeName,
        List[l.TypeParamDef],
        g.TypeBoundsTree
    )] = {
      tree match {
        case g.TypeDef(mods, name, tparams, rhs: g.TypeBoundsTree) if mods.hasFlag(Gflags.DEFERRED) =>
          val ltparams = mkTparams(tparams, Nil)
          Some((l.Modifiers(tree), l.TypeName(tree), ltparams, rhs))
        case _ =>
          None
      }
    }
  }

  // ============ DEFNS ============

  case class DefnVal(mods: List[l.Modifier],
                     override val pats: List[g.Tree],
                     tpt: Option[g.Tree],
                     rhs: g.Tree)
      extends ValOrVarDefs(pats)

  case class DefnVar(mods: List[l.Modifier],
                     override val pats: List[g.Tree],
                     tpt: Option[g.Tree],
                     rhs: Option[g.Tree])
      extends ValOrVarDefs(pats)

  object DefOrMacroDef {
    def unapply(tree: g.DefDef): Option[(
        List[l.Modifier],
        l.TermName,
        List[l.TypeParamDef],
        List[List[l.TermParamDef]],
        Option[g.Tree],
        g.Tree
    )] = {
      tree match {
        case g.DefDef(mods, _, tparams, paramss, tpt, rhs)
            if !mods.hasFlag(Gflags.DEFERRED) &&
              !nme.isConstructorName(tree.name) =>
          val ltparams = mkTparams(tparams, paramss)
          val lparamss = mkVparamss(paramss)
          val ltpt = if (tpt.nonEmpty) Some(tpt) else None
          Some((l.Modifiers(tree), l.TermName(tree), ltparams, lparamss, ltpt, rhs))
        case _ =>
          None
      }
    }
  }

  object DefDef {
    def unapply(tree: g.DefDef): Option[(
        List[l.Modifier],
        l.TermName,
        List[l.TypeParamDef],
        List[List[l.TermParamDef]],
        Option[g.Tree],
        g.Tree
    )] = {
      if (tree.mods.hasFlag(Gflags.MACRO)) return None
      DefOrMacroDef.unapply(tree)
    }
  }

  object MacroDef {
    def unapply(tree: g.DefDef): Option[(
        List[l.Modifier],
        l.TermName,
        List[l.TypeParamDef],
        List[List[l.TermParamDef]],
        Option[g.Tree],
        g.Tree
    )] = {
      if (!tree.mods.hasFlag(Gflags.MACRO)) return None
      l.DefOrMacroDef.unapply(tree)
    }
  }

  object TypeDef {
    def unapply(tree: g.TypeDef): Option[(
        List[l.Modifier],
        l.TypeName,
        List[l.TypeParamDef],
        g.Tree
    )] = {
      tree match {
        case g.TypeDef(mods, name, tparams, rhs) if !mods.hasFlag(Gflags.DEFERRED) =>
          val ltparams = mkTparams(tparams, Nil)
          Some((l.Modifiers(tree), l.TypeName(tree), ltparams, rhs))
        case _ =>
          None
      }
    }
  }

  object ClassDef {
    def unapply(tree: g.ClassDef): Option[(
        List[l.Modifier],
        l.TypeName,
        List[l.TypeParamDef],
        g.Tree,
        l.Template
    )] = {
      tree match {
        case g.ClassDef(mods, _, tparams, templ @ g.Template(_, _, body))
            if !mods.hasFlag(Gflags.TRAIT) =>
          var lprimaryctor = l.PrimaryCtorDef(tree)
          val ltparams = mkTparams(tparams, tree.primaryCtor.vparamss)
          Some((l.Modifiers(tree), l.TypeName(tree), ltparams, lprimaryctor, l.Template(tree)))
        case _ =>
          None
      }
    }
  }

  object TraitDef {
    def unapply(tree: g.ClassDef): Option[(
        List[l.Modifier],
        l.TypeName,
        List[l.TypeParamDef],
        g.Tree,
        l.Template
    )] = {
      tree match {
        case g.ClassDef(mods, _, tparams, templ @ g.Template(_, _, body)) if mods.hasFlag(Gflags.TRAIT) =>
          val ltparams = mkTparams(tparams, Nil)
          Some((l.Modifiers(tree), l.TypeName(tree), ltparams, g.EmptyTree, l.Template(tree)))
        case _ =>
          None
      }
    }
  }

  object ObjectDef {
    def unapply(tree: g.ModuleDef): Option[(
        List[l.Modifier],
        l.TermName,
        l.Template
    )] = {
      tree match {
        case g.ModuleDef(_, name, templ) if name != nme.PACKAGE =>
          Some((l.Modifiers(tree), l.TermName(tree), l.Template(tree)))
        case _ =>
          None
      }
    }
  }

  // ============ PKGS ============

  object PackageObject {
    def unapply(tree: g.Tree): Option[List[g.Tree]] = tree match {
      case g.ModuleDef(_,
                       nme.PACKAGE,
                       g.Template(
                         _,
                         _,
                         g.DefDef(_,
                                  nme.CONSTRUCTOR,
                                  Nil,
                                  List(Nil),
                                  _,
                                  g.Block(
                                    List(g.pendingSuperCall),
                                    UnitConstant()
                                  )) :: stats
                       )) =>
        Some(templateStats(stats))
      case _ => None
    }
  }

  object PackageDef {
    def unapply(tree: g.PackageDef): Option[(g.Tree, List[g.Tree])] = {
      require(tree.pid.name != nme.EMPTY_PACKAGE_NAME)
      tree.stats match {
        case PackageObject(_) :: _ =>
          None
        case _ =>
          val lpid = tree.pid
          val lstats = tree.stats
          Some((lpid, lstats))
      }
    }
  }

  object PackageObjectDef {
    def unapply(tree: g.PackageDef): Option[(
        List[l.Modifier],
        l.TermName,
        l.Template
    )] = {
      tree.stats match {
        case PackageObject(lstats) :: Nil =>
          val lself = l.Self(l.AnonymousName(), g.TypeTree())
          Some((l.Modifiers(tree), l.TermName(tree), l.Template(Nil, Nil, lself, Some(lstats))))
        case _ => None
      }
    }
  }

  // ============ CTORS ============

  trait CtorDef extends Tree

  private object CtorDef {
    def unapply(tree: g.DefDef): Option[(
        List[l.Modifier],
        l.CtorName,
        List[List[l.TermParamDef]],
        List[g.Tree]
    )] = {
      val g.DefDef(_, _, _, vparamss, _, body) = tree
      val lname = l.CtorName(tree)
      val lparamss = mkVparamss(tree.vparamss)
      val g.Block(binit, UnitConstant()) = body
      val lstats = binit.dropWhile {
        case g.pendingSuperCall => true
        case _: DefnVal => true
        case _ => false
      }
      Some((l.Modifiers(tree), lname, lparamss, lstats))
    }
  }

  case class PrimaryCtorDef(mods: List[l.Modifier],
                            name: l.CtorName,
                            paramss: List[List[l.TermParamDef]])
      extends CtorDef

  object PrimaryCtorDef {
    def apply(tree: g.ClassDef): l.PrimaryCtorDef = {
      val CtorDef(mods, name, paramss, stats) = tree.primaryCtor
      // NOTE. scala.reflect desugars `class X(x: Int)(val y: Int)(var x: String)` into
      // class X extends scala.AnyRef {
      // <paramaccessor> private[this] val x: Int = _;
      // <paramaccessor> val y: Int = _;
      // <paramaccessor> var z: String = _;
      //   def <init>(x: Int)(y: Int)(z: String) = {
      //     super.<init>();
      //     ()
      //   }
      // }
      // The curried structure of the primary constructor is extracted from the <init>
      // constructor, but the parameters to <init> are missing `val`/`var` modifiers.
      // paramAccessorMods extracts the `val`/`var` modifiers from the
      // <paramaccessor> statements inside the class body.
      val paramAccessorMods: Map[String, List[l.Modifier]] = tree.impl.body.collect {
        case t @ g.ValDef(gmods, g.TermName(gname), _, _) if gmods.hasFlag(Gflags.PARAMACCESSOR) =>
          // gmods.positions is non-empty if the user explicitly typed redundant
          // modifiers such as `private[this]`.
          val noExplicitModifiers: Boolean = gmods.positions.isEmpty
          val extraMods: List[l.Modifier] =
            if (noExplicitModifiers) Nil
            else {
              val valOrValModifier: l.Modifier =
                if (gmods.hasFlag(Gflags.MUTABLE)) l.VarParam()
                else l.ValParam()
              l.Modifiers(t) :+ valOrValModifier
            }
          gname -> extraMods
      }.toMap
      val lparamss = paramss match {
        case List(List()) if !tree.mods.hasFlag(Gflags.CASE) => Nil
        case paramss =>
          // consolidate paramAccessorMods with <init> constructor parameters.
          paramss.map(_.map { param =>
            val newMods = paramAccessorMods.getOrElse(param.name.displayName, Nil)
            param.copy(mods = param.mods ++ newMods)
          })
      }
      require(stats.isEmpty)
      PrimaryCtorDef(mods, name, lparamss)
    }
  }

  case class SecondaryCtorDef(mods: List[l.Modifier],
                              name: l.CtorName,
                              paramss: List[List[l.TermParamDef]],
                              body: g.Tree)
      extends CtorDef

  object SecondaryCtorDef {
    def replaceInitWithCtorName(tree: g.Tree): g.Tree = tree match {
      case g.Ident(nme.CONSTRUCTOR) => l.CtorName("this")
      case g.Apply(fun, args) => g.Apply(replaceInitWithCtorName(fun), args)
      case _ => tree
    }
    def apply(tree: g.DefDef): l.SecondaryCtorDef = {
      val CtorDef(lmods, lname, lparamss, gbody :: _) = tree
      val lbody = replaceInitWithCtorName(gbody)
      SecondaryCtorDef(lmods, lname, lparamss, lbody)
    }
  }

  case class CtorName(value: String) extends Name
  object CtorName {
    def apply(ctorDef: g.DefDef): l.CtorName = {
      l.CtorName("this")
    }
  }

  case class CtorIdent(name: l.CtorName) extends Tree
  object CtorIdent {
    def apply(classRef: g.RefTree): l.CtorIdent = {
      val lname = l.CtorName(classRef.displayName)
      l.CtorIdent(lname)
    }
  }

  // ============ TEMPLATES ============

  case class Template(early: List[g.Tree],
                      parents: List[g.Tree],
                      self: l.Self,
                      stats: Option[List[g.Tree]])
      extends Tree
  object Template {
    def apply(tree: g.ImplDef): l.Template = {
      def removeSyntheticParents(parents: List[g.Tree]): List[g.Tree] = parents match {
        case List(anyRef) if anyRef.toString == "scala.AnyRef" =>
          Nil
        case parents :+ product :+ serializable
            if tree.mods.hasFlag(Gflags.CASE) &&
              product.toString == "scala.Product" &&
              serializable.toString == "scala.Serializable" =>
          parents
        case other =>
          other
      }
      val template =
        g.Template(removeSyntheticParents(tree.impl.parents), tree.impl.self, tree.impl.body)
      def indexOfFirstCtor(trees: List[g.Tree]) = trees.indexWhere {
        case LowlevelCtor(_, _, _) => true; case _ => false
      }
      object LowlevelCtor {
        def unapply(tree: g.Tree): Option[(
            List[List[g.ValDef]],
            List[g.Tree],
            List[List[g.Tree]]
        )] = {
          tree match {
            case g.DefDef(_, nme.MIXIN_CONSTRUCTOR, _, _, _, SyntacticBlock(lvdefs :+ _)) =>
              Some((Nil, lvdefs, Nil))
            case g.DefDef(_,
                          nme.CONSTRUCTOR,
                          Nil,
                          vparamss,
                          _,
                          SyntacticBlock(lvdefs :+ Applied(superCtor, _, superArgss) :+ _)) =>
              Some((vparamss, lvdefs, superArgss))
            case _ =>
              None
          }
        }
      }
      val g.Template(parents, self, stats) = template
      val lself = l.Self(self)
      val (rawEdefs, rest) = stats.span(isEarlyDef)
      val (gvdefs, etdefs) = rawEdefs.partition(isEarlyValDef)
      val (evdefs, userDefinedStats) = rest.splitAt(indexOfFirstCtor(rest)) match {
        // TODO: superArgss are non-empty only in semantic mode
        case (fieldDefs, ctor @ LowlevelCtor(_, lvdefs, superArgss) :: body) =>
          val bodyWithSecondaryCtors = body.map {
            case t @ g.DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => l.SecondaryCtorDef.apply(t)
            case x => x
          }
          (gvdefs.zip(lvdefs).map {
            case (gvdef @ g.ValDef(_, _, tpt, _), g.ValDef(_, _, _, rhs)) =>
              copyValDef(gvdef)(tpt = tpt, rhs = rhs)
            case _ =>
              unreachable
          }, bodyWithSecondaryCtors)
        case (Nil, body) if body.forall(isInterfaceMember) =>
          (Nil, body)
        case _ => (Nil, Nil)
      }
      val edefs = evdefs ::: etdefs
      val lparents = parents.zipWithIndex.map {
        case (parent, i) =>
          val applied = dissectApplied(parent)
          val syntacticArgss = applied.argss
          val argss =
            if (syntacticArgss.isEmpty)
              List(List()) // todo dveim don't forget semanticArgss later
            else syntacticArgss
          l.Parent(argss.foldLeft(applied.callee)((curr, args) => g.Apply(curr, args)))
      }
      val lstats = if (userDefinedStats.nonEmpty) Some(templateStats(userDefinedStats)) else None
      l.Template(edefs, lparents, lself, lstats)
    }
  }

  case class Parent(tpt: g.Tree, ctor: l.CtorIdent, argss: List[List[g.Tree]]) extends Tree

  object Parent {
    def apply(tree: g.Tree): l.Parent = {
      val applied = dissectApplied(tree)
      (applied.callee, applied.core, applied.argss) match {
        case (tpt, classRef: g.RefTree, argss) =>
          l.Parent(tpt, l.CtorIdent(classRef), argss)
        case _ =>
          unreachable
      }
    }
  }

  case class Self(name: l.TermParamName, tpt: g.Tree) extends Tree

  object Self {
    def apply(tree: g.ValDef): l.Self = {
      val isAnonymous = tree.name == g.nme.WILDCARD || tree.name.startsWith(
          g.nme.FRESH_TERM_NAME_PREFIX)
      val lvalue = if (isAnonymous) "this" else tree.displayName
      val lname = if (isAnonymous) l.AnonymousName() else l.TermName(lvalue)
      Self(lname, tree.tpt)
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
  case class Inline() extends Modifier

  object Modifiers {
    def apply(tree: g.MemberDef): List[l.Modifier] = {
      val mods @ g.Modifiers(flags, privateWithin, _) = tree.mods

      case class ClassParameterInfo(param: g.ValDef,
                                    field: Option[g.ValDef],
                                    ctor: g.DefDef,
                                    cdef: g.ClassDef)
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

      object InlineExtractor {
        def unapply(tree: g.Tree): Option[l.Inline] = tree match {
          case g.Apply(Select(New(TypeTree(tpt)), _), Nil)
              if tpt.typeSymbol == rootMirror.staticClass("scala.meta.internal.inline.inline") =>
            Some(l.Inline())
          case _ => None
        }
      }

      // We have to keep this separate from annots
      // as we don't want it wrapped in Annotation()
      val linlineMods: List[l.Modifier] = {
        tree.mods.annotations
          .collect({
            case InlineExtractor(i) => i
          })
          .headOption
          .toList
      }

      val lannotationMods: List[l.Modifier] = {
        val trimmedAnnots = tree.mods.annotations
          .filter(InlineExtractor.unapply(_).isEmpty)
          .map({
            case tree @ g.Apply(_: g.Apply, _) => tree
            case g.Apply(tpt, Nil) => tpt
            case annot => annot
          })
        trimmedAnnots.map(Annotation.apply)
      }

      val laccessQualifierMods: List[l.Modifier] = {
        if (mods.hasFlag(Gflags.SYNTHETIC) && mods.hasFlag(Gflags.ARTIFACT)) {
          // NOTE: some sick artifact vals produced by mkPatDef can be private to method (whatever that means)
          Nil
        } else if (mods.hasFlag(Gflags.LOCAL)) {
          val lprivateWithin = g.This(privateWithin.toTypeName)
          if (mods.hasFlag(Gflags.PROTECTED)) List(l.Protected(lprivateWithin))
          else if (mods.hasFlag(Gflags.PRIVATE)) List(l.Private(lprivateWithin))
          else unreachable(debug(mods))
        } else if (mods.hasAccessBoundary && privateWithin != g.tpnme.EMPTY) {
          // TODO: `private[pkg] class C` doesn't have PRIVATE in its flags
          // so we need to account for that!
          val lprivateWithin = l.IndeterminateName(privateWithin.toString)
          if (mods.hasFlag(Gflags.PROTECTED)) List(l.Protected(lprivateWithin))
          else List(l.Private(lprivateWithin))
        } else {
          val lprivateWithin = l.AnonymousName()
          if (mods.hasFlag(Gflags.PROTECTED)) List(l.Protected(lprivateWithin))
          else if (mods.hasFlag(Gflags.PRIVATE)) List(l.Private(lprivateWithin))
          else Nil
        }
      }

      val lotherMods: List[l.Modifier] = {
        val lmods = scala.collection.mutable.ListBuffer[l.Modifier]()
        if (mods.hasFlag(Gflags.IMPLICIT)) lmods += l.Implicit()
        if (mods.hasFlag(Gflags.FINAL)) lmods += l.Final()
        if (mods.hasFlag(Gflags.SEALED)) lmods += l.Sealed()
        if (mods.hasFlag(Gflags.OVERRIDE)) lmods += l.Override()
        if (mods.hasFlag(Gflags.CASE)) lmods += l.Case()
        if (mods.hasFlag(Gflags.ABSTRACT) && tree.isInstanceOf[g.ClassDef] && !mods.hasFlag(Gflags.TRAIT))
          lmods += l.Abstract()
        if (mods.hasFlag(Gflags.ABSOVERRIDE)) { lmods += l.Abstract(); lmods += l.Override() }
        if (mods.hasFlag(Gflags.COVARIANT) && tree.isInstanceOf[g.TypeDef]) lmods += l.Covariant()
        if (mods.hasFlag(Gflags.CONTRAVARIANT) && tree.isInstanceOf[g.TypeDef])
          lmods += l.Contravariant()
        if (mods.hasFlag(LAZY)) lmods += l.Lazy()
        lmods.toList
      }

      val lvalVarParamMods: List[l.Modifier] = {
        val lmods = scala.collection.mutable.ListBuffer[l.Modifier]()
        val field = cpinfo.flatMap(_.field)
        val isImmutableField = field.map(!_.mods.hasFlag(Gflags.MUTABLE)).getOrElse(false)
        val isMutableField = field.map(_.mods.hasFlag(Gflags.MUTABLE)).getOrElse(false)
        val inCaseClass = cpinfo.map(_.cdef).map(_.mods.hasFlag(Gflags.CASE)).getOrElse(false)
        if (isMutableField) lmods += l.VarParam()
        if (isImmutableField && !inCaseClass) lmods += l.ValParam()
        lmods.toList
      }

      val result = linlineMods ++ lannotationMods ++ laccessQualifierMods ++ lotherMods ++ lvalVarParamMods

      // NOTE: we can't discern `class C(x: Int)` and `class C(private[this] val x: Int)`
      // so let's err on the side of the more popular option
      if (cpinfo.nonEmpty) result.filter({
        case l.Private(g.This(_)) => false; case _ => true
      })
      else result
    }
  }

  case class Annotation(tree: g.Tree) extends Modifier

  // ============ ODDS & ENDS ============

  trait Importee extends Tree
  case class ImporteeWildcard() extends Importee
  case class ImporteeName(value: l.IndeterminateName) extends Importee
  case class ImporteeRename(from: l.IndeterminateName, to: l.IndeterminateName) extends Importee
  case class ImporteeUnimport(name: l.IndeterminateName) extends Importee

  object Importer {
    def unapply(tree: g.Import): Option[(g.Tree, List[l.Importee])] = {
      val g.Import(expr, selectors) = tree
      val lname = expr
      val lselectors = selectors.map {
        case g.ImportSelector(nme.WILDCARD, _, _, _) =>
          l.ImporteeWildcard()
        case g.ImportSelector(name, _, nme.WILDCARD, _) =>
          l.ImporteeUnimport(l.IndeterminateName(name.displayName))
        case g.ImportSelector(name, _, rename, _) if name == rename =>
          l.ImporteeName(l.IndeterminateName(name.displayName))
        case g.ImportSelector(name, _, rename, _) =>
          l.ImporteeRename(l.IndeterminateName(name.displayName),
                           l.IndeterminateName(rename.displayName))
      }
      Some(lname, lselectors)
    }
  }

  object CaseDef {
    def unapply(tree: g.CaseDef): Option[(g.Tree, Option[g.Tree], g.Tree)] = {
      val g.CaseDef(pat, guard, body) = tree
      Some((pat, if (guard.nonEmpty) Some(guard) else None, body))
    }
  }

  // ============ HELPERS ============

  private def mkTparams(tparams: List[g.TypeDef],
                        paramss: List[List[g.ValDef]]): List[l.TypeParamDef] = {
    def tparam(targ: Tree): Option[g.TypeDef] =
      tparams
        .filter(tparam => {
          if (tparam.symbol != g.NoSymbol) tparam.symbol == targ.symbol
          else targ match { case g.Ident(name) => name == tparam.name; case _ => false }
        })
        .headOption

    object ViewBound {
      def unapply(tree: g.ValDef): Option[(g.TypeDef, g.Tree)] = tree match {
        case g.ValDef(_, _, tpt @ g.TypeTree(), _) =>
          val tycon = if (tpt.tpe != null) tpt.tpe.typeSymbolDirect else g.NoSymbol
          val targs = if (tpt.tpe != null) tpt.tpe.typeArgs else Nil
          val (fromTpe, toTpe) = targs match {
            case List(from, to) => (from, to); case _ => (g.NoType, g.NoType)
          }
          val fromSym = fromTpe.typeSymbol
          val tyconMatches = tycon == g.definitions.FunctionClass(1)
          if (tyconMatches) tparam(Ident(fromSym)).map(tparam => (tparam, g.TypeTree(toTpe)))
          else None
        case g.ValDef(_, _, g.AppliedTypeTree(tycon, from :: to :: Nil), _) =>
          val tyconMatches = tycon match {
            // NOTE: this doesn't handle every possible case (e.g. an Ident binding to renamed import),
            // but it should be good enough for 95% of the situations
            case g.Select(pre, g.TypeName("Function1")) =>
              pre.symbol == g.definitions.ScalaPackage
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
          val tycon =
            if (tpt.tpe != null) tpt.tpe.typeSymbolDirect else g.NoSymbol
          val targs = if (tpt.tpe != null) tpt.tpe.typeArgs else Nil
          val targ = targs.map(_.typeSymbol) match {
            case List(sym) => sym; case _ => g.NoSymbol
          }
          tparam(Ident(targ)).map(tparam => (tparam, Ident(tycon)))
        case g.ValDef(_, _, g.AppliedTypeTree(tycon, targ :: Nil), _) =>
          tparam(targ).map(tparam => (tparam, tycon))
        case _ =>
          None
      }
    }

    val (implicitss, explicitss) = paramss.partition(_.exists(_.mods.hasFlag(IMPLICIT)))
    val (bounds, implicits) =
      implicitss.flatten.partition(_.name.startsWith(nme.EVIDENCE_PARAM_PREFIX))
    tparams.map(tparam => {
      val vbounds = bounds.flatMap(ViewBound.unapply).filter(_._1.name == tparam.name).map(_._2)
      val cbounds =
        bounds.flatMap(ContextBound.unapply).filter(_._1.name == tparam.name).map(_._2)
      l.TypeParamDef(tparam, vbounds, cbounds)
    })
  }

  private def mkVparamss(paramss: List[List[g.ValDef]]): List[List[l.TermParamDef]] = {
    if (paramss.isEmpty) return Nil
    val init :+ last = paramss
    val hasImplicits = last.exists(_.mods.hasFlag(IMPLICIT))
    val explicitss = if (hasImplicits) init else init :+ last
    val implicits = if (hasImplicits) last else Nil
    val limplicits = implicits.filter(!_.name.startsWith(nme.EVIDENCE_PARAM_PREFIX))
    val lparamss = if (limplicits.nonEmpty) explicitss :+ limplicits else explicitss
    lparamss.map(_.map(l.TermParamDef.apply))
  }

  private def templateStats(stats: List[g.Tree]): List[g.Tree] = {
    // TODO: relevant synthetics are described in:
    // * subclasses of MultiEnsugarer: DefaultGetter, VanillaAccessor, AbstractAccessor, LazyAccessor
    // * ToMtree.mstats: PatDef, InlinableHoistedTemporaryVal
    val lresult = mutable.ListBuffer[g.Tree]()
    var i = 0
    while (i < stats.length) {
      val stat = stats(i)
      i += 1
      stat match {
        case g.DefDef(_, nme.MIXIN_CONSTRUCTOR, _, _, _, _) => // and this
        case g.ValDef(mods, _, _, _) if mods.hasFlag(Gflags.PARAMACCESSOR) => // and this
        case g.EmptyTree => // and this
        case _ => lresult += stat
      }
    }
    undoValDefDesugarings(lresult).toList
  }

  // Extracts "2" from patterns like this: `2: @scala.unchecked`
  // This patterns appears in all val/var with patterns on the left hand side.
  private object AnnotatedUnchecked {
    def unapply(tree: g.Tree): Option[g.Tree] = {
      tree match {
        case g.Annotated(
            g.Apply(g.Select(g.New(g.Select(g.Ident(nme.scala_), g.TypeName("unchecked"))),
                             termNames.CONSTRUCTOR),
                    Nil),
            arg
            ) =>
          Some(arg)
        case _ => None
      }
    }
  }

  // Extracts (Foo(a), 2, Some(Bar)) from patterns like this:
  //  <synthetic> <artifact> private[this] val x$1 = (2: @scala.unchecked: Bar) match {
  //    case Foo(a) => scala.Tuple2(a, b)
  //  };
  private object ValDesugaredPattern {
    def unapply(tree: g.Tree): Option[(g.Tree, g.Tree, Option[g.Tree])] = {
      tree match {
        // case: val Foo(a) = 2
        case g.Match(AnnotatedUnchecked(body), List(g.CaseDef(pat, g.EmptyTree, _))) =>
          Some((pat, body, None))
        // case: val Foo(a): Int = 2
        case g.Match(g.Typed(AnnotatedUnchecked(rhs), tpt),
                     List(g.CaseDef(pat, g.EmptyTree, _))) =>
          Some((pat, rhs, Some(tpt)))
        case _ => None
      }
    }
  }

  private object UnitConstant {
    def unapply(tree: g.Tree): Boolean = tree match {
      case g.Literal(g.Constant(())) => true
      case _ => false
    }
  }

  private def blockStats(stats: List[g.Tree]): List[g.Tree] = {
    undoValDefDesugarings(stats).toList
  }

  // Transforms g.ValDef into l.ValOrValDef. See l.ValOrVarDef for more explanations.
  def undoValDefDesugarings(stats: Seq[g.Tree]): Seq[g.Tree] = {
    val toCollapse = mutable.Set.empty[g.Tree]
    val valDefFingerprints = mutable.Map.empty[g.Tree, (g.Position, String)]
    def addFingerprint(valDefPattern: g.Tree, mods: g.Modifiers, rhs: g.Tree): Unit = {
      mods.positions.headOption.foreach {
        case (_, pos) =>
          valDefFingerprints += (valDefPattern -> (pos, rhs.toString()))
      }
    }

    def fingerprintsMatch(pat1: g.Tree, pat2: g.Tree): Boolean = {
      valDefFingerprints.get(pat1).exists(valDefFingerprints.get(pat2).contains)
    }

    val allValOrValDefs: Seq[g.Tree] = stats.zipWithIndex.collect {
      // case: val Foo(a, b) = rhs
      case (origin @ g.ValDef(syntheticMods, name, _, ValDesugaredPattern(pat, rhs, tpt)), i)
          if syntheticMods.hasFlag(Gflags.PRIVATE | Gflags.LOCAL | Gflags.SYNTHETIC | Gflags.ARTIFACT) =>
        val statsToRemove = stats.drop(i + 1).takeWhile {
          case g.ValDef(_, _, _, g.Select(g.Ident(`name`), _)) => true
          case UnitConstant() => true
          case _ => false
        }
        addFingerprint(pat, syntheticMods, rhs)
        toCollapse ++= statsToRemove
        val mods: g.Modifiers = statsToRemove.headOption match {
          case Some(g.ValDef(mods, _, _, _)) => mods
          case _ => g.Modifiers()
        }
        l.ValOrVarDefs(origin.copy(mods = mods | syntheticMods.flags), List(pat), tpt, rhs)
      // case: val Foo(a) = rhs
      case (origin @ g.ValDef(mods, name, _, ValDesugaredPattern(pat, rhs, tpt)), i)
          if !toCollapse(origin) =>
        addFingerprint(pat, mods, rhs)
        stats.drop(i + 1).headOption match {
          case Some(remove @ UnitConstant()) => toCollapse += remove
          case _ =>
        }
        l.ValOrVarDefs(origin, List(pat), tpt, rhs)
      // case: val a = rhs
      case (origin @ g.ValDef(mods, name, tpt, rhs), _) if !toCollapse(origin) =>
        val pat: g.Tree =
          if (name == nme.WILDCARD) l.ValPatWildcard
          else l.PatVarTerm(origin)
        addFingerprint(pat, mods, rhs)
        l.ValOrVarDefs(origin, List(pat), Some(tpt), rhs)
      case (t, _) if !t.isEmpty && !toCollapse(t) =>
        t
    }
    // collapses multiple ValOrValDefs that came from same position into single ValOrVarDef.
    // Example: val a, b = 2 is two statements in `allValOrValDefs` but only one statement
    // in collapsedValOrVarDefs.
    val collapsedValOrVarDefs = allValOrValDefs.zipWithIndex.collect {
      case (v1 @ ValOrVarDefs(pat1), i) if !toCollapse(v1) =>
        val toMerge = allValOrValDefs.drop(i + 1).takeWhile {
          case ValOrVarDefs(pat2) if fingerprintsMatch(pat1, pat2) => true
          case l.UnitConstant() => true
          case _ => false
        }
        toCollapse ++= toMerge
        val newPatterns = toMerge.collect { case v2: l.ValOrVarDefs => v2.pats }.flatten
        v1 match {
          case t: l.DefnVal => t.copy(pats = t.pats ++ newPatterns)
          case t: l.DeclVal => t.copy(pats = t.pats ++ newPatterns)
          case t: l.DefnVar => t.copy(pats = t.pats ++ newPatterns)
          case t: l.DeclVar => t.copy(pats = t.pats ++ newPatterns)
        }
      case (t, _) if !toCollapse(t) => t
    }
    collapsedValOrVarDefs
  }

  // Reflect trees represent flat Seq[Mod.Annot] as nested Mod.Annot(Mod.Annot(...term), annot).
  // We flatten the nested structure to match the meta representation for the most common case.
  // Unfortunately, this means we might flatten the structure when the original syntax used nesting.
  @tailrec
  private def flattenAnnotated(gtree: g.Tree, accum: List[g.Tree] = Nil): (g.Tree, List[g.Tree]) = {
    gtree match {
      case g.Annotated(annot, arg) => flattenAnnotated(arg, annot :: accum)
      case tree => (tree, accum)
    }
  }
}
