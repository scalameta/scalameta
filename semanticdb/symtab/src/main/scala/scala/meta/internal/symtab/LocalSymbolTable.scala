package scala.meta.internal.symtab

import scala.meta.internal.semanticdb.Scope
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.semanticdb.Scala._

final class LocalSymbolTable private (
    underlying: SymbolTable,
    locals: Map[String, SymbolInformation])
    extends SymbolTable {
  override def toString: String = s"LocalSymbolTable($underlying, Map(${locals.size} entries))"
  override def info(symbol: String): Option[SymbolInformation] = {
    if (symbol.isLocal) {
      locals.get(symbol)
    } else {
      underlying.info(symbol)
    }
  }

  override def withScope(scope: Scope): SymbolTable =
    LocalSymbolTable(underlying, scope, locals)
}

object LocalSymbolTable {
  def apply(underlying: SymbolTable, scope: Scope): SymbolTable = {
    LocalSymbolTable(underlying, scope, Map.empty)
  }
  def apply(
      underlying: SymbolTable,
      scope: Scope,
      locals: Map[String, SymbolInformation]): SymbolTable = {
    if (scope.hardlinks.isEmpty) {
      if (locals.isEmpty) underlying
      else new LocalSymbolTable(underlying, locals)
    } else {
      val newLocals = locals ++ scope.hardlinks.map(s => s.symbol -> s)
      new LocalSymbolTable(underlying, newLocals)
    }
  }
}
