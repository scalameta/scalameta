package scala.meta.internal.semanticdb.scalac

import scala.reflect.internal.util.Position
import scala.tools.nsc.reporters.{Reporter, StoreReporter}

class SemanticdbReporter(underlying: Reporter, minSeverity: FailureMode) extends StoreReporter {
  override protected def info0(
      pos: Position,
      msg: String,
      severity: Severity,
      force: Boolean
  ): Unit = {
    super.info0(pos, msg, severity, force)
    severity.id match {
      case 0 if minSeverity.level <= FailureMode.Info.level => underlying.info(pos, msg, force)
      case 1 if minSeverity.level <= FailureMode.Warning.level => underlying.warning(pos, msg)
      case 2 if minSeverity.level <= FailureMode.Error.level => underlying.error(pos, msg)
      case _ =>
    }

  }

  override def hasErrors: Boolean = underlying.hasErrors

  override def hasWarnings: Boolean = underlying.hasWarnings

}
