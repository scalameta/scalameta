package scala.meta.internal.metap

import scala.math.Ordering
import scala.meta.internal.semanticdb3._

trait SyntheticPrinter extends BasePrinter with OccurrencePrinter {
  def pprint(synth: Synthetic): Unit = {
    opt(synth.range)(pprint)
    opt(": ", doc.substring(synth.range))(out.print)
    out.print(" => ")
    synth.text match {
      case Some(text) =>
        out.println(text.text)
        rep("  ", text.occurrences.sorted, "  ") { occ =>
          opt(occ.range)(pprint)
          opt(": ", text.substring(occ.range))(out.print)
          pprint(occ.role)
          out.println(occ.symbol)
        }
      case _ =>
        out.println("<?>")
    }
  }

  implicit def synthOrder: Ordering[Synthetic] = {
    Ordering.by(s => (s.range, s.text.map(_.text)))
  }
}
