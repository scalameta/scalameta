package scala.meta.internal
package semantic

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
          object RelevantMessage {
            def unapply(info: r.Info): Option[(m.Position, Int, String)] = {
              val unitPath = unit.source.toAbsolutePath
              val infoPath = info.pos.source.toAbsolutePath
              if (!info.pos.isRange) return None
              if (infoPath != unitPath) return None
              Some((info.pos.toMeta, info.severity.id, info.msg))
            }
          }
          r.infos
            .collect {
              case RelevantMessage(pos, severityId, msg) =>
                val severity = severityId match {
                  case 0 => m.Severity.Info
                  case 1 => m.Severity.Warning
                  case 2 => m.Severity.Error
                  case _ => unreachable
                }
                m.Message(pos, severity, msg)
            }
            .to[Seq]
        case _ =>
          Nil
      }
    }
  }
}
