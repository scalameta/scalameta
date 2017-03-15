package scala.meta.internal.scalahost

import scala.meta.semantic.v1.CompilerMessage
import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.reporters.StoreReporter

/** Reporter that forwards messages to underlying reporter AND stores messages. */
class ScalahostReporter(underlying: Reporter) extends StoreReporter {
  override protected def info0(pos: Position,
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

/** Replaces g.reporter with a ScalahostReporter */
trait HijackReporter { self: ScalahostPlugin =>
  def hijackReporter(): Unit = {
    val scalahostReporter = new ScalahostReporter(g.reporter)
    g.reporter = scalahostReporter
  }
}
