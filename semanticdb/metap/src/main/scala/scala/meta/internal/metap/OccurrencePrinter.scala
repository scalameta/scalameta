package scala.meta.internal.metap

import scala.math.Ordering
import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.SymbolOccurrence._
import scala.meta.internal.semanticdb.SymbolOccurrence.Role._

trait OccurrencePrinter extends BasePrinter with RangePrinter {
  def pprint(occ: SymbolOccurrence): Unit = {
    opt(occ.range)(pprint)
    opt(": ", doc.substring(occ.range))(out.print)
    pprint(occ.role)
    out.println(occ.symbol)
  }

  def pprint(role: Role): Unit = {
    role match {
      case REFERENCE => out.print(" => ")
      case DEFINITION => out.print(" <= ")
      case UNKNOWN_ROLE | Role.Unrecognized(_) => out.print(" <?> ")
    }
  }

  implicit def occOrder: Ordering[SymbolOccurrence] = {
    Ordering.by(o => (o.range, o.symbol, o.role.value))
  }
}
