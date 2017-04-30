package scala.meta.internal
package scalahost
package databases

import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.{meta => m}
import scala.tools.nsc.reporters.StoreReporter

trait ReporterOps { self: DatabaseOps =>

  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {
    def hijackedMessages: Seq[m.Message] = {
      g.reporter match {
        case r: StoreReporter =>
          val path = unit.source.toAbsolutePath
          object Message {
            def unapply(info: r.Info): Option[(m.Anchor, Int, String)] =
              if (!info.pos.isRange) None
              else Some((info.pos.toAnchor, info.severity.id, info.msg))
          }
          r.infos
            .collect {
              case Message(location, severityId, msg) if location.path == path =>
                val severity = severityId match {
                  case 0 => m.Severity.Info
                  case 1 => m.Severity.Warning
                  case 2 => m.Severity.Error
                  case _ => unreachable
                }
                m.Message(location, severity, msg)
            }
            .to[Seq]
        case _ =>
          Nil
      }
    }
  }
}
