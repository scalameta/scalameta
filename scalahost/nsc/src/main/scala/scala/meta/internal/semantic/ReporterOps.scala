package scala.meta.internal
package semantic

import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.{meta => m}
import scala.tools.nsc.reporters.StoreReporter

trait ReporterOps { self: DatabaseOps =>

  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {
    def hijackedMessages(mstarts: mutable.Map[Int, m.Name]): Seq[m.Message] = {
      def metaPosition(pos: g.Position, msg: String): m.Position = msg match {
        // NOTE: The caret in unused import warnings points to Importee.pos, but
        // the message position start/end point to the enclosing Import.pos.
        // See https://github.com/scalameta/scalameta/issues/839
        case "Unused import" if mstarts.contains(pos.point) => mstarts(pos.point).pos
        case _ => pos.toMeta
      }
      g.reporter match {
        case r: StoreReporter =>
          object RelevantMessage {
            def unapply(info: r.Info): Option[(m.Position, Int, String)] = {
              val unitPath = unit.source.toAbsolutePath
              val infoPath = info.pos.source.toAbsolutePath
              if (!info.pos.isRange) return None
              if (infoPath != unitPath) return None
              Some((metaPosition(info.pos, info.msg), info.severity.id, info.msg))
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
