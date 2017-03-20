package scala.meta.internal
package scalahost
package v1
package online

import scala.meta.semantic.v1.CompilerMessage
import scala.meta.semantic.v1.Location
import scala.meta.semantic.v1.Severity
import scala.tools.nsc.reporters.StoreReporter

trait ReporterOps { self: Mirror =>

  private def toCompilerMessage(r: StoreReporter, addressSyntax: String): Seq[CompilerMessage] = {
    object Message {
      def unapply(info: r.Info): Option[(Location, Int, String)] =
        if (!info.pos.isRange) None
        else Some((info.pos.toSemantic, info.severity.id, info.msg))
    }
    r.infos
      .collect {
        case Message(location, severityId, msg) if location.addr.syntax == addressSyntax =>
          val severity = severityId match {
            case 0 => Severity.Info
            case 1 => Severity.Warning
            case 2 => Severity.Error
            case unknown => new Severity.Unknown(unknown)
          }
          new CompilerMessage(location, severity, msg)
      }
      .to[Seq]
  }

  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {
    def reportedMessages: Seq[CompilerMessage] = {
      g.reporter match {
        case r: StoreReporter =>
          val path = unit.body.pos.toSemantic.addr.syntax
          toCompilerMessage(r, path)
        case _ => Nil
      }
    }
  }
}
