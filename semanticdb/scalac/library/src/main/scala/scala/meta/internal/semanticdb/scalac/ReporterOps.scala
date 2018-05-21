package scala.meta.internal.semanticdb.scalac

import scala.collection.mutable
import scala.tools.nsc.reporters.StoreReporter
import scala.reflect.internal.util.{Position => gPosition}

trait ReporterOps { self: SemanticdbOps =>
  // Hack, keep track of how many messages we have returns for each path to avoid
  // duplicate messages. The key is System.identityHashCode to keep memory usage low.
  private val returnedMessagesByPath = mutable.Map.empty[g.CompilationUnit, Int]
  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {
    def hijackedDiagnostics: List[(gPosition, Int, String)] = {
      g.reporter match {
        case r: StoreReporter =>
          object RelevantMessage {
            def unapply(info: r.Info): Option[(gPosition, Int, String)] = {
              if (info.pos.source != unit.source) return None
              Some((info.pos, info.severity.id, info.msg))
            }
          }
          val infos = r.infos
          val toDrop = returnedMessagesByPath.getOrElse(unit, 0)
          returnedMessagesByPath.put(unit, infos.size)
          infos.iterator
            .drop(toDrop) // drop messages that have been reported before.
            .collect {
              case RelevantMessage(pos, severity, msg) =>
                (pos, severity, msg)
            }
            .to[List]
        case _ =>
          Nil
      }
    }
  }
}
