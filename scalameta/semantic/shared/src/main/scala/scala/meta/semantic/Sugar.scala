package scala.meta
package semantic

import org.scalameta.data._
import scala.meta.inputs._

@data class Sugar(
    input: Input.Sugar,
    names: List[(Position, Symbol)]
) {
  def syntax = input.value
  def structure = s"""Sugar("$syntax")"""
}
