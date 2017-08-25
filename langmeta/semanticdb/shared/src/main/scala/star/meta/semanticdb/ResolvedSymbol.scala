package org.langmeta
package semanticdb

final case class ResolvedSymbol(symbol: Symbol, definition: Definition) {
  def syntax = s"${symbol.syntax} => ${definition.syntax}"
  def structure = s"""ResolvedSymbol(${symbol.structure}, ${definition.structure})"""
  override def toString = syntax
}
