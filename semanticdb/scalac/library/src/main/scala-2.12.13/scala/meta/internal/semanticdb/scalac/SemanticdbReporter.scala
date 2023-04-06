package scala.meta.internal.semanticdb.scalac

import scala.reflect.internal.util.Position
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.{Reporter, StoreReporter}

class SemanticdbReporter(underlying: Reporter)
    extends StoreReporter(SemanticdbReporter.defaultSettings()) {
  override protected def info0(
      pos: Position,
      msg: String,
      severity: Severity,
      force: Boolean
  ): Unit = {
    super.info0(pos, msg, severity, force)
    severity.id match {
      case 0 => underlying.info(pos, msg, force)
      case 1 => underlying.warning(pos, msg)
      case 2 => underlying.error(pos, msg)
      case _ =>
    }
  }

  override def hasErrors: Boolean = underlying.hasErrors

  override def hasWarnings: Boolean = underlying.hasWarnings


  // overriding increment is enough so make sure that error/warning
  // counts are the same as in underlying reporter
   override def increment(severity: Severity): Unit = {
    super.increment(severity)
    underlying.increment(severity)
  }
}
object SemanticdbReporter {
  def defaultSettings(): Settings = {
    val s = new Settings()
    s.processArguments(List("-Xmaxwarns", "-1", "-Xmaxerrs", "-1"), true)
    s
  }
}
