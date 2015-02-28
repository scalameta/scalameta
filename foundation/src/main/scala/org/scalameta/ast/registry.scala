package org.scalameta.ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context

class registry extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RegistryMacros.impl
}

class RegistryMacros(val c: Context) {
  import c.universe._
  import Flag._
  def impl(annottees: Tree*): Tree = {
    def expectedName = "scala.meta.internal.ast.Registry"
    def fail(annottee: Tree) = c.abort(annottee.pos, s"only the $expectedName module can be @registry")
    def transform(mdef: ModuleDef): ModuleDef = {
      if (c.internal.enclosingOwner.fullName + "." + mdef.name != expectedName) fail(mdef)
      val ModuleDef(mods @ Modifiers(flags, privateWithin, anns), name, Template(parents, self, stats)) = mdef
      val AstInternal = q"_root_.org.scalameta.ast.internal"
      val annRegistry = q"new $AstInternal.registry($AstInternal.buildRegistry[$name.type])"
      val defRegistry = q"@$annRegistry def all: _root_.scala.List[_root_.scala.Predef.String] = $AstInternal.buildRegistry[$name.type]"
      val stats1 = defRegistry +: stats
      val privateWithin1 = TypeName("meta")
      ModuleDef(Modifiers(flags, privateWithin1, anns), name, Template(parents, self, stats1))
    }
    val expanded = annottees match {
      case (mdef: ModuleDef) :: rest => transform(mdef) :: rest
      case annottee :: rest => fail(annottee)
    }
    q"{ ..$expanded; () }"
  }
}