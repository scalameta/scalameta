package scala.meta
package internal
package ast

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.compat.Platform.EOL
import scala.reflect.{classTag, ClassTag}
import scala.meta.internal.ast._
import scala.meta.internal.ast.Helpers._
import scala.meta.internal.semantic._
import scala.meta.internal.semantic.RuntimeConverters._
import scala.meta.internal.prettyprinters._
import org.scalameta.collections._
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.debug._
import scala.meta.internal.{ast => m}
import scala.meta.prettyprinters._

// NOTE: This facility is used to merge syntactic trees (carrying perfect syntactic information)
// and semantic trees (carrying comprehensive set of semantic attributes for themselves and their subnodes).
// "sy-" stands for "syntactic", "se-" stands for "semantic", "me-" stands for "merged".
//
// The main difficulty here is supporting different desugarings that Scala typecheckers may perform.
// Since mergeTrees is an essential part of deserialization from TASTY, this needs to be platform-independent,
// so we have to support both scalac and dotc (also, as discussed with @smarter, it's possible to make mergeTrees
// platform-dependent, but we'll have to pay the price of user convenience).
//
// The list of supported desugarings can be found at:
// https://github.com/scalameta/scalameta/blob/master/docs/converters.md.
object mergeTrees {
  // NOTE: Much like in LogicalTrees and in ToMtree, cases here must be ordered according
  // to the order of appearance of the corresponding AST nodes in Trees.scala.
  // TODO: I think we could hardcode this traversal into the @ast infrastructure.
  // That will let us save time on: 1) eager recreation of entire trees, 2) eager computation of tokens for withTokens.
  def apply[T <: Tree : ClassTag](sy: T, se: Tree): T = {
    object loop {
      def apply[T <: Tree : ClassTag](sy1: T, sE2: Tree): T = {
        // NOTE: This roundabout way of recursing is here only for error reporting.
        // If we end up in, say, `apply(syss, sess)`, and we have `List()` vs `List(List())`,
        // then we will have troubles providing a nice traceback to the user (because we don't have parents at hand).
        // There are two solutions to this, as far as I can see:
        // 1) Pass parents explicitly to all calls to `loop` that deal with collections.
        // 2) Have `loop` definitions be nested within `mergeTrees.apply`, so that everyone can access `sy` and `se`.
        // I tried #1, and it's way too much hassle, which is why I settled down on #2.
        if ((sy1 ne sy) || (sE2 ne se)) mergeTrees(sy1, sE2)
        else {
          require(se.isTypechecked)

          val partialMe = (sy, se) match {
            // ============ NAMES ============

            case (sy: m.Name.Anonymous, se: m.Name.Anonymous) =>
              sy.copy()
            case (sy: m.Name.Indeterminate, se: m.Name.Indeterminate) =>
              if (sy.value != se.value) failCorrelate(sy, se, "incompatible names")
              sy.copy()

            // ============ TERMS ============

            case (sy: m.Term.This, se: m.Term.This) =>
              sy.copy(loop(sy.qual, se.qual))
            case (sy: m.Term.Name, se: m.Term.Name) =>
              if (sy.value != se.value && sy.isBinder) failCorrelate(sy, se, "incompatible definitions")
              sy.copy() // (E2)
            case (sy: m.Term.Name, se: m.Term.Select) =>
              sy.inheritAttrs(se.name).withExpansion(se) // (E1, E2)
            case (sy: m.Term.Name, seOuter @ m.Term.Apply(seInner: m.Term.Name, Nil)) =>
              val me = loop(sy, seInner).resetTypechecked
              val expansionOuter = seOuter.copy(fun = me).inheritAttrs(seOuter)
              me.withExpansion(expansionOuter) // (E7)
            case (sy: m.Term.Name, seOuter @ m.Term.Apply(seMid @ m.Term.Select(_, seInner), Nil)) =>
              val me = loop(sy, seInner).resetTypechecked
              val expansionMid = seMid.copy(name = me).inheritAttrs(seMid)
              val expansionOuter = seOuter.copy(fun = expansionMid).inheritAttrs(seOuter)
              me.withExpansion(expansionOuter) // (E7)
            case (sy: m.Term.Select, se: m.Term.Select) =>
              if (sy.name.value != se.name.value) failCorrelate(sy, se, "incompatible names")
              sy.copy(loop(sy.qual, se.qual), loop(sy.name, se.name))
            case (sy: m.Term.Select, seOuter @ m.Term.Apply(seInner: m.Term.Select, Nil)) =>
              val me = loop(sy, seInner).resetTypechecked
              val expansionOuter = seOuter.copy(fun = me).inheritAttrs(seOuter)
              me.withExpansion(expansionOuter) // (E7)
            case (sy: m.Term.Apply, se: m.Term.Apply) =>
              sy.copy(loop(sy.fun, se.fun), loop(sy.args, se.args))
            case (sy: m.Term.ApplyInfix, se @ m.Term.Apply(sefun, seargs)) =>
              val (selhs, seop, setargs) = sefun match {
                case m.Term.Select(selhs, seop) => (selhs, seop, Nil)
                case m.Type.Apply(m.Term.Select(selhs, seop), setargs) => (selhs, seop, setargs)
              }
              require(seop.isLeftAssoc && debug(sy, se)) // TODO: right-associative operators aren't supported yet
              sy.copy(loop(sy.lhs, selhs), loop(sy.op, seop), loop(sy.targs, setargs), loop(sy.args, seargs))
            case (sy: m.Term.ApplyInfix, se: m.Term.ApplyInfix) =>
              sy.copy(loop(sy.lhs, se.lhs), loop(sy.op, se.op), loop(sy.targs, se.targs), loop(sy.args, se.args))
            case (sy: m.Term.ApplyUnary, se: m.Term.Select) =>
              require(se.name.value.startsWith("unary_"))
              val meArg = loop(sy.arg, se.qual).resetTypechecked
              val me = sy.copy(loop(sy.op, se.name), meArg).inheritAttrs(se)
              val expansion = se.copy(qual = meArg).inheritAttrs(se)
              me.withExpansion(expansion) // (E8)
            case (sy: m.Term.ApplyUnary, se: m.Term.ApplyUnary) =>
              sy.copy(loop(sy.op, se.op), loop(sy.arg, se.arg))
            case (sy: m.Term.ApplyType, se: m.Term.ApplyType) =>
              sy.copy(loop(sy.fun, se.fun), loop(sy.targs, se.targs))
            case (sy: m.Term.Block, se: m.Term.Block) =>
              val mestats = (sy.stats, se.stats) match {
                case (systats, sestats :+ m.Lit(())) if systats.length == sestats.length => loop(systats, sestats) // (E4)
                case _ => loop(sy.stats, se.stats)
              }
              sy.copy(mestats)
            case (sy @ m.Term.Block(Seq(syStat)), seStat: m.Stat) =>
              val syTyping = seStat match { case seStat: Term => seStat.typing; case _ => typingUnit }
              sy.copy(Seq(loop(syStat, seStat))).withAttrs(syTyping) // (E3)
            case (sy: m.Term.If, se: m.Term.If) =>
              sy.copy(loop(sy.cond, se.cond), loop(sy.thenp, se.thenp), loop(se.thenp, se.thenp))
            case (sy: m.Term.Param, se: m.Term.Param) =>
              sy.copy(loop(sy.mods, se.mods), loop(sy.name, se.name), loop(sy.decltpe, se.decltpe), loop(sy.default, se.default))

            // ============ TYPES ============

            case (sy: m.Type.Name, se: m.Type.Name) =>
              if (sy.value != se.value && sy.isBinder) failCorrelate(sy, se, "incompatible definitions")
              sy.copy() // (E2)
            case (sy: m.Type.Select, se: m.Type.Select) =>
              sy.copy(loop(sy.qual, se.qual), loop(sy.name, se.name))
            case (sy: m.Type.Apply, se: m.Type.Apply) =>
              sy.copy(loop(sy.tpe, se.tpe), loop(sy.args, se.args))
            case (sy: m.Type.Bounds, se: m.Type.Bounds) =>
              val melo = (sy.lo, se.lo) match {
                case (None, Some(nothing: Type.Name)) if nothing.refersTo(denotNothing) => None // (M9)
                case (sylo, selo) => loop(sylo, selo)
              }
              val mehi = (sy.hi, se.hi) match {
                case (None, Some(any: Type.Name)) if any.refersTo(denotAny) => None // (M9)
                case (syhi, sehi) => loop(syhi, sehi)
              }
              sy.copy(melo, mehi)
            case (sy: m.Type.Param, se: m.Type.Param) =>
              sy.copy(loop(sy.mods, se.mods), loop(sy.name, se.name), loop(sy.tparams, se.tparams),
                loop(sy.typeBounds, se.typeBounds), loop(sy.viewBounds, se.viewBounds), loop(sy.contextBounds, se.contextBounds))

            // ============ PATTERNS ============

            case (sy: m.Pat.Var.Term, se: m.Pat.Var.Term) =>
              sy.copy(loop(sy.name, se.name))

            // ============ LITERALS ============

            case (sy: m.Lit, se: m.Lit) =>
              sy.copy()

            // ============ DECLS ============

            // ============ DEFNS ============

            case (sy: m.Defn.Val, se: m.Defn.Val) =>
              val medecltpe = (sy.decltpe, se.decltpe) match { // (M1)
                case (None, Some(se)) => None
                case (sy, se) => loop(sy, se)
              }
              sy.copy(loop(sy.mods, se.mods), loop(sy.pats, se.pats), medecltpe, loop(sy.rhs, se.rhs))
            case (sy: m.Defn.Def, se: m.Defn.Def) =>
              val medecltpe = (sy.decltpe, se.decltpe) match { // (M1)
                case (None, Some(se)) => None
                case (sy, se) => loop(sy, se)
              }
              sy.copy(loop(sy.mods, se.mods), loop(sy.name, se.name), loop(sy.tparams, se.tparams), loop(sy.paramss, se.paramss), medecltpe, loop(sy.body, se.body))
            case (sy: m.Defn.Class, se: m.Defn.Class) =>
              sy.copy(loop(sy.mods, se.mods), loop(sy.name, se.name), loop(sy.tparams, se.tparams), loop(sy.ctor, se.ctor), loop(sy.templ, se.templ))
            case (sy: m.Defn.Trait, se: m.Defn.Trait) =>
              sy.copy(loop(sy.mods, se.mods), loop(sy.name, se.name), loop(sy.tparams, se.tparams), loop(sy.ctor, se.ctor), loop(sy.templ, se.templ))
            case (sy: m.Defn.Object, se: m.Defn.Object) =>
              sy.copy(loop(sy.mods, se.mods), loop(sy.name, se.name), loop(sy.templ, se.templ))

            // ============ PKGS ============

            case (sy: m.Source, se: m.Source) =>
              sy.copy(loop(sy.stats, se.stats))
            case (sy: m.Pkg, se: m.Pkg) =>
              if (sy.ref.toString != se.ref.toString) failCorrelate(sy, se, "incompatible definitions")
              sy.copy(loop(sy.ref, se.ref), loop(sy.stats, se.stats))

            // ============ CTORS ============

            case (sy: m.Ctor.Primary, se: m.Ctor.Primary) =>
              val meparamss = (sy.paramss, se.paramss) match { // (M2)
                case (Seq(), Seq(Seq())) => List()
                case (syss, sess) => loop(syss, sess)
              }
              sy.copy(loop(sy.mods, se.mods), loop(sy.name, se.name), meparamss)
            case (sy: m.Ctor.Ref.Name, se: m.Ctor.Ref.Name) =>
              // TODO: This is a weird corner case in how we represent constructors.
              // In constructor definitions, we have Ctor.Name("this"),
              // whereas in constructor references, we have Ctor.Name(<classname>).
              if (sy.value != "this" && sy.value != se.value && sy.isBinder) failCorrelate(sy, se, "incompatible definitions")
              sy.copy()
            case (sy: m.Ctor.Ref.Select, se: m.Ctor.Ref.Select) =>
              sy.copy(loop(sy.qual, se.qual), loop(sy.name, se.name))

            // ============ TEMPLATES ============

            case (sy: m.Template, se: m.Template) =>
              def mergeParents(sys: Seq[m.Ctor.Call], ses: Seq[m.Ctor.Call]): Seq[m.Ctor.Call] = {
                if (sys.length != ses.length) failCorrelate(sy, se, sys, ses)
                sys.zip(ses).map({ // (M8)
                  case (sy, m.Term.Apply(se, Nil)) => loop(sy, se)
                  case (sy, se) => loop(sy, se)
                })
              }
              val meparents = (sy.parents, se.parents) match {
                case (Seq(), Seq(m.Term.Apply(anyRef: m.Ctor.Ref, Nil))) if anyRef.refersTo(denotObjectInit) => // (M3)
                  List()
                case (syss, sess) =>
                  mergeParents(syss, sess)
              }
              val mestats = (sy.stats, se.stats) match {
                case (None, Some(Nil)) => None
                case (sys, ses) => loop(sys, ses)
              }
              sy.copy(loop(sy.early, se.early), meparents, loop(sy.self, se.self), mestats)

            // ============ MODIFIERS ============

            // ============ ODDS & ENDS ============

            case _ =>
              failCorrelate(sy, se, "unexpected trees")
          }

          // NOTE: We have just created a copy of sy, which means that we got no tokens now.
          // Therefore we have to inherit sy's tokens explicitly.
          val syntacticMe = partialMe.inheritTokens(sy)

          // NOTE: In most cases, the partial result will not be attributed and will be meant to inherit attrs from se.
          // However, when we deal with desugarings, we might pre-attribute the result in advance,
          // and therefore we should avoid calling inheritAttrs if that was the case,
          // because we might've decided to assign different attrs to the partial result.
          val attributedMe = {
            if (syntacticMe.isUnattributed) syntacticMe.inheritAttrs(se)
            else syntacticMe
          }

          // NOTE: Explicitly specified expansion (already present in the semantic tree) takes precedence,
          // because the converter really knows better in cases like constant folding, macro expansion, etc.
          val inferredExpansion = attributedMe.internalExpansion
          val explicitExpansion = se.internalExpansion
          val expansion = explicitExpansion match {
            case Some(_: Expansion.Desugaring) => explicitExpansion
            case Some(Expansion.Zero | Expansion.Identity) => inferredExpansion
            case None => require(inferredExpansion.isEmpty && debug(sy, se)); None
          }
          val expandedMe = (attributedMe, expansion) match {
            case (attributedMe: Term, Some(expansion: Expansion)) => attributedMe.withExpansion(expansion)
            case _ => attributedMe
          }

          val me = expandedMe.setTypechecked
          if (sy.parent.isEmpty) {
            Debug.logMerge {
              println("======= SYNTACTIC TREE =======")
              println(sy)
              println(sy.show[Attributes])
              println("======== SEMANTIC TREE ========")
              println(se)
              println(se.show[Attributes])
              println("======== MERGED TREE ========")
              println(me)
              println(me.show[Attributes])
              println("=================================")
            }
          }
          if (classTag[T].runtimeClass.isAssignableFrom(me.getClass)) me.asInstanceOf[T]
          else failExpected(sy, se, classTag[T].runtimeClass, me)
        }
      }

      def apply[T <: Tree : ClassTag](syopt: Option[T], seopt: Option[Tree])(implicit hack1: OverloadHack1): Option[T] = (syopt, seopt) match {
        case (Some(sy), Some(se)) => Some(loop(sy, se))
        case (None, None) => None
        case _ => failCorrelate(sy, se, syopt.toList, seopt.toList)
      }

      def apply[T <: Tree : ClassTag](sysopt: Option[Seq[T]], sesopt: Option[Seq[Tree]])(implicit hack2: OverloadHack2): Option[Seq[T]] = (sysopt, sesopt) match {
        case (Some(sys), Some(ses)) => Some(loop(sys, ses))
        case (None, None) => None
        case _ => failCorrelate(sy, se, sysopt.toList, sesopt.toList)
      }

      def apply[T <: Tree : ClassTag](sys: Seq[T], ses: Seq[Tree])(implicit hack1: OverloadHack1): Seq[T] = {
        if (sys.length != ses.length) failCorrelate(sy, se, sys, ses)
        sys.zip(ses).map({ case (sy, se) => loop(sy, se) })
      }

      def apply[T <: Tree : ClassTag](syss: Seq[Seq[T]], sess: Seq[Seq[Tree]])(implicit hack2: OverloadHack2): Seq[Seq[T]] = {
        if (syss.length != sess.length) failCorrelate(sy, se, syss, sess)
        syss.zip(sess).map({ case (sys, ses) => loop(sys, ses) })
      }
    }
    loop(sy, se)
  }

  private lazy val denotObjectInit = denot(typeOf[Object].member(u.TermName("<init>")))
  private lazy val denotUnit = denot(symbolOf[Unit])
  private lazy val typingUnit = Typing.Nonrecursive(Type.Name("Unit").withAttrs(denotUnit).setTypechecked)
  private lazy val denotNothing = denot(symbolOf[Nothing])
  private lazy val denotAny = denot(symbolOf[Any])

  // NOTE: We can't use =:= here, because it requires a semantic context,
  // and we can't have a semantic context in MergeTrees.
  // Why's that? Because merging trees is an essential part of deserializing from TASTY,
  // and deserializing from TASTY is a prerequisite for building a semantic context.
  private implicit class XtensionRefersTo(ref: Ref) {
    def refersTo(denot: Denotation): Boolean = ref match {
      case name: m.Name => name.denot == denot
      case tree: m.Term.Select => tree.name.refersTo(denot)
      case tree: m.Type.Select => tree.name.refersTo(denot)
      case tree: m.Type.Project => tree.name.refersTo(denot)
      case tree: m.Type.Singleton => tree.ref.refersTo(denot)
      case tree: m.Pat.Type.Project => tree.name.refersTo(denot)
      case tree: m.Ctor.Ref.Select => tree.name.refersTo(denot)
      case tree: m.Ctor.Ref.Project => tree.name.refersTo(denot)
      case tree: m.Ctor.Ref.Function => tree.name.refersTo(denot)
      case _ => false
    }
  }

  private def failCorrelate(sy: Tree, se: Tree, diagnostics: String): Nothing = {
    val details = s"${sy.show[Structure]}$EOL${se.show[Structure]}"
    fail(sy, se, s"encountered $diagnostics during syntactic + semantic merge:$EOL$details")
  }

  private def failCorrelate(syparent: Tree, separent: Tree, sys: Seq[_], ses: Seq[_]): Nothing = {
    val summary = s"syntactic = ${sys.length} element(s), semantic = ${ses.length} element(s)"
    fail(syparent, separent, s"encountered incompatible collections during syntactic + semantic merge: $summary")
  }

  private def failExpected(sy: Tree, se: Tree, expected: Class[_], actual: Tree): Nothing = {
    var expectedAbbrev = expected.getName
    expectedAbbrev = expectedAbbrev.stripPrefix("scala.meta.internal.ast.").stripPrefix("scala.meta.")
    expectedAbbrev = expectedAbbrev.stripSuffix("$Impl")
    expectedAbbrev = expectedAbbrev.replace("$", ".")
    val actualAbbrev = actual.productPrefix
    val summary = s"expected = $expectedAbbrev, actual = $actualAbbrev"
    val details = actual.show[Structure]
    fail(sy, se, s"obtained an unexpected result during syntactic + semantic merge: $summary$EOL$details")
  }

  private def fail(sy: Tree, se: Tree, diagnostics: String): Nothing = {
    def traceback(sy: Tree, se: Tree): List[String] = {
      def summary(tree: Tree): String = {
        val prefix = tree.productPrefix
        var details = tree.toString.replace("\n", " ")
        if (details.length > 60) details = details.take(60) + "..."
        s"($prefix) $details"
      }
      val tail = (sy.parent, se.parent) match {
        case (Some(sy), Some(se)) => traceback(sy.require[Tree], se.require[Tree])
        case (Some(sy), None) => List("<-...$EOL->")
        case (None, Some(se)) => List("<-$EOL->...")
        case (None, None) => Nil
      }
      s"<-${summary(sy)}$EOL->${summary(se)}" +: tail
    }
    throw new MergeException(List(sy, se), s"$diagnostics$EOL${traceback(sy, se).mkString(EOL)}")
  }
}