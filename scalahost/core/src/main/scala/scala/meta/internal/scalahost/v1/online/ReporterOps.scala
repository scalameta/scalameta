package scala.meta.internal
package scalahost
package v1
package online

import scala.meta.semantic.v1.CompilerMessage
import scala.meta.semantic.v1.Location
import scala.meta.semantic.v1.Severity
import scala.tools.nsc.reporters.StoreReporter

trait ReporterOps { self: Mirror =>

  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {
    def hijackedMessages: Seq[CompilerMessage] = {
      g.reporter match {
        case r: StoreReporter =>
          val path = unit.source.toAbsolutePath
          object Message {
            def unapply(info: r.Info): Option[(Location, Int, String)] =
              if (!info.pos.isRange) None
              else Some((info.pos.toSemantic, info.severity.id, info.msg))
          }
          r.infos
            .collect {
              case Message(location, severityId, msg) if location.path == path =>
                val severity = severityId match {
                  case 0 => Severity.Info
                  case 1 => Severity.Warning
                  case 2 => Severity.Error
                  case unknown => new Severity.Unknown(unknown)
                }
                new CompilerMessage(location, severity, msg)
            }
            .to[Seq]
        case _ =>
          Nil
      }
    }
  }
}
