package scala.meta
package internal
package ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import scala.meta.internal.ast.{Reflection => AstReflection}

// @root is a specialized version of @org.scalameta.adt.root for scala.meta ASTs.
class root extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RootNamerMacros.impl
}

class RootNamerMacros(val c: Context) extends AstReflection with CommonNamerMacros {
  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror
  import c.universe._
  import Flag._

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"${mods @ Modifiers(flags, privateWithin, anns)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val mstats1 = ListBuffer[Tree]() ++ mstats

      if (mods.hasFlag(SEALED)) c.abort(cdef.pos, "@root traits cannot be sealed")
      if (mods.hasFlag(FINAL)) c.abort(cdef.pos, "@root traits cannot be final")
      val flags1 = flags // TODO: flags | SEALED
      mstats1 += q"$CommonTyperMacrosModule.hierarchyCheck[$name]"
      val anns1 = anns :+ q"new $AdtMetadataModule.root" :+ q"new $AstMetadataModule.root"
      val parents1 = parents :+ tq"$AstMetadataModule.Ast" :+ tq"$ProductClass" :+ tq"$SerializableClass"
      mstats1 ++= mkClassifier(name)
      mstats1 += mkQuasi(name, Nil, Nil)

      val cdef1 = q"${Modifiers(flags1, privateWithin, anns1)} trait $name[..$tparams] extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
      val mdef1 = q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
  })
}