package scala.meta.internal.symtab

import scala.meta.internal.semanticdb.SymbolInformation

trait SymbolTable {

  /** Returns the SymbolInformation for the given symbol, or None if the symbol is missing. */
  def info(symbol: String): Option[SymbolInformation]

}
