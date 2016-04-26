package scala.meta
package internal
package ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import scala.meta.internal.ast.{Reflection => AstReflection}
import org.scalameta.internal.MacroHelpers

// @branch is a specialized version of @org.scalameta.adt.branch for scala.meta ASTs.
// TODO: The amount of cruft that's accumulated over dozens of prototype milestones is staggering.
class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BranchNamerMacros.impl
}

class BranchNamerMacros(val c: Context) extends AstReflection with MacroHelpers {
  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror
  import c.universe._
  import Flag._

  val Tree = tq"_root_.scala.meta.Tree"
  val Flags = tq"_root_.scala.meta.internal.flags.`package`.Flags"
  val TYPECHECKED = q"_root_.scala.meta.internal.flags.`package`.TYPECHECKED"
  val ZERO = q"_root_.scala.meta.internal.flags.`package`.ZERO"
  val Tokens = tq"_root_.scala.meta.tokens.Tokens"
  val Environment = tq"_root_.scala.meta.internal.semantic.Environment"
  val Denotation = tq"_root_.scala.meta.internal.semantic.Denotation"
  val Typing = tq"_root_.scala.meta.internal.semantic.Typing"
  val SemanticInternal = q"_root_.scala.meta.internal.semantic"
  val FlagsPackage = q"_root_.scala.meta.internal.flags.`package`"
  val ArrayClassMethod = q"_root_.scala.meta.internal.ast.Helpers.arrayClass"

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      def fullName = c.internal.enclosingOwner.fullName.toString + "." + cdef.name.toString
      def abbrevName = fullName.stripPrefix("scala.meta.")
      def is(abbrev: String) = abbrevName == abbrev
      def isQuasi = cdef.name.toString == "Quasi"
      def isLit = is("Lit")
      def isName = is("Name") || is("Term.Param.Name") || is("Type.Param.Name")
      def isTerm = is("Term") || is("Lit") || is("Term.Ref") || is("Ctor.Ref") || is("Ctor.Call")
      def isMember = is("Term.Param") || is("Type.Param") || is("Pat.Var.Term") || is("Pat.Var.Type") ||
                     is("Member") || is("Member.Term") || is("Member.Type") ||
                     is("Ctor") || is("Ctor.Primary") || is("Ctor.Secondary")
      val ClassDef(mods @ Modifiers(flags, privateWithin, anns), name, tparams, Template(parents, self, stats)) = cdef
      val ModuleDef(mmods, mname, Template(mparents, mself, mstats)) = mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      // NOTE: sealedness is turned off because we can't have @ast hierarchy sealed anymore
      // hopefully, in the future we'll find a way to restore sealedness
      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "@branch traits cannot be sealed")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@branch traits cannot be final")
      val flags1 = flags // TODO: flags | SEALED
      mstats1 += q"$AstTyperMacrosModule.hierarchyCheck[$name]"
      val anns1 = anns :+ q"new $AdtMetadataModule.branch" :+ q"new $AstMetadataModule.branch"

      if (!isQuasi) {
        // NOTE: we need to have this bunch of boilerplate here because we removed ThisType
        stats1 += q"override def withTokens(tokens: $Tokens): $name"
        stats1 += q"override def inheritTokens(other:  $Tree): $name"
        if (isName && !is("Name")) { // signatures written manually for Name
          stats1 += q"private[meta] override def withEnv(env: $SemanticInternal.Environment): $name"
          stats1 += q"private[meta] override def withAttrs(denot: $SemanticInternal.Denotation): $name"
        }
        if (isTerm && !is("Term")) { // signatures written manually for Term
          stats1 += q"private[meta] override def withEnv(env: $SemanticInternal.Environment): $name"
          stats1 += q"private[meta] override def withAttrs(typingLike: $SemanticInternal.TypingLike): $name"
        }
        if (isName || isTerm) {
          stats1 += q"private[meta] override def inheritAttrs(tree: Tree): $name"
        }
        stats1 += q"protected override def privateWithFlags(flags: $Flags): $name"
        stats1 += q"protected override def privatePrototype: $name"
        stats1 += q"""
          private[meta] override def privateCopy(
            flags: $Flags = $ZERO,
            prototype: $Tree = this,
            parent: $Tree = privateParent,
            tokens: $Tokens = privateTokens,
            env: $Environment = privateEnv,
            denot: $Denotation = privateDenot,
            typing: $Typing = privateTyping): $name
        """

        val qmods = Modifiers(NoFlags, TypeName("meta"), List(q"new _root_.scala.meta.internal.ast.ast"))
        val qname = TypeName("Quasi")
        val qparents = tq"$name" +: tq"_root_.scala.meta.internal.ast.Quasi" +: parents.map({
          case Ident(name) => Select(Ident(name.toTermName), TypeName("Quasi"))
          case Select(qual, name) => Select(Select(qual, name.toTermName), TypeName("Quasi"))
          case unsupported => c.abort(unsupported.pos, "implementation restriction: unsupported parent")
        })
        def quasigetter(mods: Modifiers, name: String) = {
          val unsupportedUnquotingPosition = "unsupported unquoting position"
          val unsupportedSplicingPosition = "unsupported splicing position"
          val message = q"if (this.rank == 0) $unsupportedUnquotingPosition else $unsupportedSplicingPosition"
          val impl = q"throw new _root_.scala.`package`.UnsupportedOperationException($message)"
          val Modifiers(flags, privateWithin, anns) = mods
          val mods1 = Modifiers(flags | OVERRIDE, privateWithin, anns)
          q"$mods1 def ${TermName(name)}: _root_.scala.Nothing = $impl"
        }
        def quasisetter(mods: Modifiers, name: String, params: ValDef*) = {
          val DefDef(mods1, termName, tparams, _, tpt, rhs) = quasigetter(mods, name)
          DefDef(mods1, termName, tparams, List(params.toList), tpt, rhs)
        }
        var qstats = List(q"def pt: _root_.java.lang.Class[_] = $ArrayClassMethod(_root_.scala.Predef.classOf[$name], this.rank)")
        if (isLit) {
          qstats :+= quasigetter(NoMods, "value")
        }
        if (isMember) {
          qstats :+= quasigetter(NoMods, "name")
        }
        if (isName) {
          qstats :+= quasigetter(NoMods, "value")
          qstats :+= quasigetter(PrivateMeta, "env")
          qstats :+= quasigetter(PrivateMeta, "denot")
          qstats :+= quasisetter(PrivateMeta, "withEnv", q"val env: $SemanticInternal.Environment")
          qstats :+= quasisetter(PrivateMeta, "withAttrs", q"val denot: $SemanticInternal.Denotation")
        }
        if (isTerm) {
          qstats :+= quasigetter(PrivateMeta, "env")
          qstats :+= quasigetter(PrivateMeta, "typing")
          qstats :+= quasisetter(PrivateMeta, "withEnv", q"val env: $SemanticInternal.Environment")
          qstats :+= quasisetter(PrivateMeta, "withAttrs", q"val typingLike: $SemanticInternal.TypingLike")
        }
        qstats :+= q"protected def privateEnv: $SemanticInternal.Environment = null"
        qstats :+= q"protected def privateDenot: $SemanticInternal.Denotation = null"
        qstats :+= q"protected def privateTyping: $SemanticInternal.Typing = null"
        mstats1 += q"$qmods class $qname(rank: _root_.scala.Int, tree: _root_.scala.Any) extends ..$qparents { ..$qstats }"
      }

      val cdef1 = ClassDef(Modifiers(flags1, privateWithin, anns1), name, tparams, Template(parents, self, stats1.toList))
      val mdef1 = ModuleDef(mmods, mname, Template(mparents, mself, mstats1.toList))
      List(cdef1, mdef1)
    }
  })
}