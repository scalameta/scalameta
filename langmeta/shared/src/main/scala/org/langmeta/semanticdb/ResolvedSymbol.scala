package org.langmeta
package semanticdb

final case class ResolvedSymbol(symbol: Symbol, denotation: Denotation) {
  def syntax = s"${symbol.syntax} => ${denotation.syntax}"
  def structure = s"""ResolvedSymbol(${symbol.structure}, ${denotation.structure})"""
  override def toString = syntax
}
