package scala.meta.internal.symtab

import scala.meta.internal.semanticdb.Scope
import scala.meta.internal.semanticdb.SymbolInformation

trait SymbolTable {
  def info(symbol: String): Option[SymbolInformation]
  def withScope(scope: Scope): SymbolTable
}
