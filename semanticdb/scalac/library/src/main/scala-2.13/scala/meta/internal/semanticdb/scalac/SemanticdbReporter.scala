package scala.meta.internal.semanticdb.scalac

import scala.reflect.internal.util.CodeAction
import scala.reflect.internal.util.Position
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.FilteringReporter
import scala.tools.nsc.reporters.StoreReporter

class SemanticdbReporter(underlying: FilteringReporter)
    extends StoreReporter(SemanticdbReporter.defaultSettings(underlying.settings)) {

  override def doReport(
      pos: Position,
      msg: String,
      severity: Severity,
      actions: List[CodeAction]
  ): Unit = {
    super.doReport(pos, msg, severity, actions)
    underlying.doReport(pos, msg, severity, actions)
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
