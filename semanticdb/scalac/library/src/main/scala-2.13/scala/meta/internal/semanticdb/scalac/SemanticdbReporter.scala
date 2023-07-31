package scala.meta.internal.semanticdb.scalac

import scala.reflect.internal.{Reporter => R}
import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.{FilteringReporter, StoreReporter}
import scala.tools.nsc.Settings

class SemanticdbReporter(underlying: FilteringReporter, minSeverity: FailureMode)
    extends StoreReporter(SemanticdbReporter.defaultSettings(underlying.settings)) {

  private val minSeverityId = minSeverity match {
    case FailureMode.Error => R.ERROR.id
    case FailureMode.Warning => R.WARNING.id
    case FailureMode.Info => R.INFO.id
    case FailureMode.Ignore => Int.MaxValue
  }

  override def doReport(pos: Position, msg: String, severity: Severity): Unit = {
    super.doReport(pos, msg, severity)
    if (severity.id >= minSeverityId) underlying.doReport(pos, msg, severity)
  }

  // overriding increment is enough so make sure that error/warning
  // counts are the same as in underlying reporter

  override def increment(severity: Severity): Unit = {
    super.increment(severity)
    underlying.increment(severity)
  }
}
object SemanticdbReporter {
  def defaultSettings(s: Settings): Settings = {
    s.processArguments(List("-Xmaxwarns", "-1", "-Xmaxerrs", "-1"), true)
    s
  }
}
