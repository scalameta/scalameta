package scala.meta.internal.metap

import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.semanticdb.TextDocument

// Identical to symtab.SymbolTable but copied since symtab is JVM-only.
trait PrinterSymtab {
  def info(symbol: String): Option[SymbolInformation]
}

object PrinterSymtab {
  def fromTextDocument(doc: TextDocument): PrinterSymtab = {
    val map = doc.symbols.map(info => (info.symbol, info)).toMap
    new PrinterSymtab {
      override def info(symbol: String): Option[SymbolInformation] = map.get(symbol)
    }
  }
}
