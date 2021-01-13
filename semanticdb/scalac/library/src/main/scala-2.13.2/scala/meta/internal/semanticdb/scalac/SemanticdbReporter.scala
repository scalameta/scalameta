package scala.meta.internal.semanticdb.scalac

import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.{FilteringReporter, StoreReporter}

class SemanticdbReporter(underlying: FilteringReporter) extends StoreReporter {
  override def doReport(pos: Position, msg: String, severity: Severity): Unit = {
    super.doReport(pos, msg, severity)
    underlying.doReport(pos, msg, severity)
  }

  // overriding increment and filter is enough so make sure that error/warning
  // counts are the same as in underlying reporter

  override def increment(severity: Severity): Unit = {
    super.increment(severity)
    underlying.increment(severity)
  }

  override def filter(pos: Position, msg: String, severity: Severity): Int =
    underlying.filter(pos, msg, severity)
}
