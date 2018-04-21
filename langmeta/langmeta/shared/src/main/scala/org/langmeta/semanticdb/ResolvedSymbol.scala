package org.langmeta
package semanticdb

import org.langmeta.internal.semanticdb.DeprecationMessage

@deprecated(DeprecationMessage, "3.8.0")
final case class ResolvedSymbol(symbol: Symbol, denotation: Denotation) {
  def input: Input = Input.Denotation(denotation.signature, symbol)
  def syntax = s"${symbol.syntax} => ${denotation.syntax}"
  def structure = s"""ResolvedSymbol(${symbol.structure}, ${denotation.structure})"""
  override def toString = syntax
}
