package scala.meta.internal
package semanticdb

import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.internal.util.{Position => gPosition}

trait ReporterOps { self: DatabaseOps =>

  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {
    def hijackedMessages: List[(gPosition, Int, String)] = {
      g.reporter match {
        case r: StoreReporter =>
          object RelevantMessage {
            def unapply(info: r.Info): Option[(gPosition, Int, String)] = {
              if (!info.pos.isRange) return None
              if (info.pos.source != unit.source) return None
              Some((info.pos, info.severity.id, info.msg))
            }
          }
          r.infos
            .collect {
              case RelevantMessage(pos, severity, msg) =>
                ((pos, severity, msg))
            }
            .to[List]
        case _ =>
          Nil
      }
    }
  }
}
