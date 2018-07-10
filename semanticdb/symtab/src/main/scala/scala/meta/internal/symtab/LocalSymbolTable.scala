package scala.meta.internal.symtab

import scala.meta.internal.semanticdb.SymbolInformation

/** An eager in-memory symbol table for local symbols. */
final class LocalSymbolTable private (locals: Map[String, SymbolInformation]) extends SymbolTable {
  override def toString: String = s"LocalSymbolTable(${locals.size} entries)"
  override def info(symbol: String): Option[SymbolInformation] = {
    locals.get(symbol)
  }
}

object LocalSymbolTable {
  def apply(infos: Iterable[SymbolInformation]): SymbolTable = {
    new LocalSymbolTable(infos.iterator.map(info => info.symbol -> info).toMap)
  }
}
