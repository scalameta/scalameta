package org.scalameta.ast

import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import org.scalameta.ast.{Reflection => AstReflection}

class registry extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RegistryMacros.impl
}

class RegistryMacros(val c: Context) extends AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._
  import Flag._
  val AstInternal = q"_root_.org.scalameta.ast.internal"
  val ImmutableList = q"_root_.scala.collection.immutable.List"
  val RoughAstMetadata = q"_root_.org.scalameta.ast.internal.RoughAstMetadata"
  def impl(annottees: Tree*): Tree = {
    def transform(mdef: ModuleDef): ModuleDef = {
      val ModuleDef(mods @ Modifiers(flags, privateWithin, anns), name, Template(parents, self, stats)) = mdef
      // TODO: works around the deprecation warning
      val enclosingUnit = c.asInstanceOf[{ def enclosingUnit: { def body: Tree } }].enclosingUnit.body
      val anns1 = anns :+ q"new $AstInternal.registry(${enclosingUnit.detectAst})"
      ModuleDef(Modifiers(flags, privateWithin, anns1), name, Template(parents, self, stats))
    }
    val expanded = annottees match {
      case (mdef: ModuleDef) :: rest => transform(mdef) +: rest
      case annottee :: rest => c.abort(annottee.pos, "only objects can be @branch")
    }
    q"{ ..$expanded; () }"
  }
}