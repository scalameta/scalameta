package org.langmeta
package semanticdb

final case class ResolvedSymbol(symbol: Symbol, denotation: Denotation) {
  def input: Input = Input.Denotation(denotation.signature, symbol)
  def structure = s"""ResolvedSymbol(${symbol.structure}, ${denotation.structure})"""
  def syntax: String = s"${symbol.syntax} => ${denotation.syntax}"
}

