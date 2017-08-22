package lang.meta
package semanticdb

final case class ResolvedSymbol(symbol: Symbol, denot: Denotation) {
  def syntax = s"${symbol.syntax} => ${denot.syntax}"
  def structure = s"""ResolvedSymbol(${symbol.structure}, ${denot.structure})"""
  override def toString = syntax
}
