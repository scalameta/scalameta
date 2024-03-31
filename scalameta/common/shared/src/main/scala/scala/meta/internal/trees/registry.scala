package scala.meta
package internal
package trees

import org.scalameta.internal.MacroHelpers
import scala.meta.internal.trees.{Reflection => AstReflection}

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros.whitebox.Context

// Detects scala.meta ASTs defined in the current compilation unit
// and then saves them in a runtime annotation on the annottee.
class registry extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro RegistryMacros.impl
}

class RegistryMacros(val c: Context) extends AstReflection with MacroHelpers {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._
  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformModule(mdef: ModuleDef): ModuleDef = {
      val ModuleDef(
        mods @ Modifiers(flags, privateWithin, anns),
        name,
        Template(parents, self, stats)
      ) = mdef
      val enclosingUnit = c.asInstanceOf[{ def enclosingUnit: { def body: Tree } }].enclosingUnit
        .body
      val anns1 = anns :+ q"new $AstMetadataModule.registry(${enclosingUnit.detectAst})"
      ModuleDef(Modifiers(flags, privateWithin, anns1), name, Template(parents, self, stats))
    }
  })
}
