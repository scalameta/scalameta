package scala.meta.internal.symtab

import scala.meta.internal.semanticdb.SymbolInformation

/** An aggregate of a list of symbol tables. */
final class AggregateSymbolTable private (private val symtabs: List[SymbolTable])
    extends SymbolTable {
  override def toString: String = s"AggregateSymbolTable($symtabs)"
  def info(symbol: String): Option[SymbolInformation] = {
    def loop(ss: List[SymbolTable]): Option[SymbolInformation] = ss match {
      case Nil =>
        None
      case head :: tail =>
        head.info(symbol) match {
          case Some(x) => Some(x)
          case None => loop(tail)
        }
    }
    loop(symtabs)
  }
}

object AggregateSymbolTable {
  def apply(symtabs: Iterable[SymbolTable]): SymbolTable = {
    val buf = List.newBuilder[SymbolTable]
    symtabs.foreach {
      case agg: AggregateSymbolTable =>
        buf ++= agg.symtabs
      case symtab =>
        buf += symtab
    }
    new AggregateSymbolTable(buf.result())
  }
}
