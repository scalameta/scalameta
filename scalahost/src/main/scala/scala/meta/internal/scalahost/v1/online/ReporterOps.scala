package scala.meta.internal
package scalahost
package v1
package online

import scala.meta.semantic.v1.CompilerMessage
import scala.meta.semantic.v1.Severity
import scala.tools.nsc.reporters.StoreReporter

trait ReporterOps { self: Mirror =>

  private def toCompilerMessage(r: StoreReporter): Seq[CompilerMessage] =
    r.infos
      .collect {
        case i if i.pos.isRange =>
          val severity = i.severity.id match {
            case 0 => Severity.Info
            case 1 => Severity.Warning
            case 2 => Severity.Error
            case unknown => new Severity.Unknown(unknown)
          }
          new CompilerMessage(i.pos.toSemantic, severity, i.msg)
      }
      .to[Seq]

  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {
    def messages: Seq[CompilerMessage] = {
      g.reporter match {
        case r: StoreReporter =>
          val path = unit.body.pos.toSemantic.addr.syntax
          toCompilerMessage(r).filter(_.location.addr.syntax == path)
        case els => Nil
      }
    }
  }
}
