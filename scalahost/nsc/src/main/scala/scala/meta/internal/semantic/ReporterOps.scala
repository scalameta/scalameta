package scala.meta.internal
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.internal.util.{Position => gPosition}

trait ReporterOps { self: DatabaseOps =>

  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {
    def hijackedMessages: Seq[(gPosition, Int, String)] = {
      g.reporter match {
        case r: StoreReporter =>
          object RelevantMessage {
            def unapply(info: r.Info): Option[(gPosition, Int, String)] = {
              val unitPath = unit.source.toAbsolutePath
              val infoPath = info.pos.source.toAbsolutePath
              if (!info.pos.isRange) return None
              if (infoPath != unitPath) return None
              Some((info.pos, info.severity.id, info.msg))
            }
          }
          r.infos
            .collect {
              case RelevantMessage(pos, severity, msg) =>
                ((pos, severity, msg))
            }
            .to[Seq]
        case _ =>
          Nil
      }
    }
  }
}
