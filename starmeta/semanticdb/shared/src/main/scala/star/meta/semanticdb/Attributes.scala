package star.meta
package semanticdb

import star.meta.inputs._
import star.meta.internal.inputs._

final case class Attributes(
  input: Input,
  language: String,
  names: List[(Position, Symbol)],
  messages: List[Message],
  denotations: List[(Symbol, Denotation)],
  sugars: Map[Position, Sugar]
) {
  def syntax: String = star.meta.internal.semanticdb.AttributesSyntax(this)
  def structure: String = {
    val s_input = input.structure
    val s_language = "\"" + language + "\""
    val s_names = names.map{ case (pos, sym) => s"""${pos.structure} -> ${sym.structure}""" }.mkString(", ")
    val s_messages = messages.map(_.structure).mkString(", ")
    val s_denots = denotations.map { case (sym, denot) => s"""${sym.structure} -> ${denot.structure}""" }.mkString(", ")
    val s_sugars = sugars.map { case (pos, sugar) => s"""${pos.structure} -> "${sugar.structure}"""" }.mkString(", ")
    s"Attributes($s_input, $s_language, List($s_names), List($s_messages), List($s_denots), List($s_sugars))"
  }
  override def toString: String = syntax
}
