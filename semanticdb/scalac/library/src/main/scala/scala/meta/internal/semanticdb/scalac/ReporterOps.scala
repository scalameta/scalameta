package scala.meta.internal.semanticdb.scalac

import scala.reflect.internal.util.{Position => gPosition}
import scala.tools.nsc.reporters.StoreReporter

trait ReporterOps {
  self: ReflectionToolkit =>

  /** the number of messages that the reporter holds, or 0 if it does not store them */
  private[meta] def storedMessages: Int = g.reporter match {
    case r: StoreReporter => r.infos.size
    case _ => 0
  }

  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {

    /** the messages at the unit's source, after the first `skip` messages of the reporter */
    def hijackedDiagnostics(skip: Int): Iterator[(gPosition, Int, String)] = g.reporter match {
      case r: StoreReporter => r.infos.iterator.drop(skip).filter(_.pos.source == unit.source)
          .map(info => (info.pos, info.severity.id, info.msg))
      case _ => Iterator.empty
    }

  }
}
