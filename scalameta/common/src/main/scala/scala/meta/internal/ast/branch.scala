package scala.meta
package internal
package ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import scala.meta.internal.ast.{Reflection => AstReflection}

// @branch is a specialized version of @org.scalameta.adt.branch for scala.meta ASTs.
class branch extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BranchNamerMacros.impl
}

class BranchNamerMacros(val c: Context) extends AstReflection with CommonNamerMacros {
  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror
  import c.universe._
  import Flag._

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      def isQuasi = cdef.name.toString == "Quasi"
      val q"${mods @ Modifiers(flags, privateWithin, anns)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "@branch traits cannot be sealed")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@branch traits cannot be final")
      val flags1 = flags // TODO: flags | SEALED
      mstats1 += q"$CommonTyperMacrosModule.hierarchyCheck[$name]"
      val anns1 = anns :+ q"new $AdtMetadataModule.branch" :+ q"new $AstMetadataModule.branch"
      mstats1 ++= mkClassifier(name)
      if (!isQuasi) mstats1 += mkQuasi(name, parents, Nil, "value", "name") // NOTE: this accounts for Name.value and Member.name

      val cdef1 = q"${Modifiers(flags1, privateWithin, anns1)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      val mdef1 = q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
  })
}