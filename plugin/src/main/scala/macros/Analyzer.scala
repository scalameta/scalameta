package scala.reflect.hosts
package scalacompiler
package macros

import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}
import scala.reflect.hosts.scalacompiler.{Plugin => PalladiumPlugin}

// TODO: it's a shame that we have to hijack the analyzer to override isBlackbox
// instead we should extend the MacroPlugin API with `pluginsIsBlackbox(macroDef: Symbol): Boolean`
trait Analyzer extends NscAnalyzer { self =>
  import global._
  import definitions._

  // TODO: welcome to the path-dependent happy meal
  val plugin: PalladiumPlugin
  object PalladiumSignature {
    def unapply(tree: Tree): Option[(Boolean, DefDef)] = {
      val result = plugin.palladiumMacroPlugin.PalladiumSignature.unapply(tree.asInstanceOf[plugin.global.Tree])
      result.map { case (isBlackbox, implDdef) => (isBlackbox, implDdef.asInstanceOf[DefDef]) }
    }
  }

  override def isBlackbox(macroDef: Symbol): Boolean = {
    val macroSignatures = macroDef.annotations.filter(_.atp.typeSymbol == MacroImplAnnotation)
    macroSignatures match {
      case _ :: AnnotationInfo(_, List(PalladiumSignature(isBlackbox, _)), _) :: Nil => isBlackbox
      case _ => super.isBlackbox(macroDef)
    }
  }
}
