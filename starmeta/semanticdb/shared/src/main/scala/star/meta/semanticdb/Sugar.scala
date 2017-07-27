package star.meta
package semanticdb

import star.meta.inputs._

final case class Sugar(
    input: Input.Sugar,
    names: Map[Position, Symbol]
) {
  def syntax = input.value
  def structure = s"""Sugar("$syntax")"""
}
