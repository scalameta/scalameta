package scala.meta.internal.metap

import scala.math.Ordering
import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.Diagnostic._
import scala.meta.internal.semanticdb.Diagnostic.Severity._

trait DiagnosticPrinter extends BasePrinter with RangePrinter {
  def pprint(diag: Diagnostic): Unit = {
    opt(diag.range, " ")(pprint)
    diag.severity match {
      case ERROR => out.print("[error] ")
      case WARNING => out.print("[warning] ")
      case INFORMATION => out.print("[info] ")
      case HINT => out.print("[hint] ")
      case UNKNOWN_SEVERITY | Severity.Unrecognized(_) => out.print("[<?>] ")
    }
    out.println(diag.message)
  }

  implicit def diagOrder: Ordering[Diagnostic] = {
    Ordering.by(d => (d.range, d.severity.value, d.message))
  }
}
