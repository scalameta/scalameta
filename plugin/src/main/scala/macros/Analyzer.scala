package scala.meta
package internal.hosts.scalacompiler
package macros

import scala.tools.nsc.typechecker.{Analyzer => NscAnalyzer}
import scala.meta.internal.hosts.scalacompiler.{Plugin => PalladiumPlugin}

// TODO: it's a shame that we have to hijack the analyzer to override isBlackbox
// instead we should extend the MacroPlugin API with `pluginsIsBlackbox(macroDef: Symbol): Boolean`
trait Analyzer extends NscAnalyzer with Common { self =>
  import global._
  import definitions._

  override def isBlackbox(macroDef: Symbol): Boolean = {
    val macroSignatures = macroDef.annotations.filter(_.atp.typeSymbol == MacroImplAnnotation)
    macroSignatures match {
      case _ :: AnnotationInfo(_, List(PalladiumSignature(isBlackbox, _)), _) :: Nil => isBlackbox
      case _ => super.isBlackbox(macroDef)
    }
  }
}
