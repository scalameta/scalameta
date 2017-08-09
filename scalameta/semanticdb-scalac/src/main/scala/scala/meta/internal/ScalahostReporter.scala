package scala.meta.internal

import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.reporters.StoreReporter

class SemanticdbReporter(underlying: Reporter) extends StoreReporter {
  override protected def info0(
      pos: Position,
      msg: String,
      severity: Severity,
      force: Boolean): Unit = {
    super.info0(pos, msg, severity, force)
    severity.id match {
      case 0 => underlying.info(pos, msg, force)
      case 1 => underlying.warning(pos, msg)
      case 2 => underlying.error(pos, msg)
      case _ =>
    }
  }
}
