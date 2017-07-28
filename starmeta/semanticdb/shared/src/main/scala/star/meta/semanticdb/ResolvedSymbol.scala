package star.meta
package semanticdb

final case class ResolvedSymbol(sym: Symbol, denot: Denotation) {
  def syntax = s"${sym.syntax} => ${denot.syntax}"
  def structure = s"""ResolvedSymbol(${sym.structure}, ${denot.structure})"""
  override def toString = syntax
}
