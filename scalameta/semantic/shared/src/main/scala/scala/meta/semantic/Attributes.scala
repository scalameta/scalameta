package scala.meta
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.data._
import scala.meta.inputs._
import scala.meta.internal.inputs._

@data class Attributes(
  dialect: Dialect,
  names: Seq[(Position, Symbol)],
  messages: Seq[Message],
  denotations: Seq[(Symbol, Denotation)],
  sugars: Seq[(Position, String)]
) {
  def syntax: String = scala.meta.internal.semantic.AttributesSyntax(this)
  def structure: String = {
    val s_names = names.map{ case (pos, sym) => s"""${pos.structure} -> ${sym.structure}""" }.mkString(", ")
    val s_messages = messages.map(_.structure).mkString(", ")
    val s_denots = denotations.map { case (sym, denot) => s"""${sym.structure} -> ${denot.structure}""" }.mkString(", ")
    val s_sugars = sugars.map { case (pos, sugar) => s"""${pos.structure} -> "$sugar"""" }.mkString(", ")
    s"Attributes(Seq($s_names), Seq($s_messages), Seq($s_denots), Seq($s_sugars))"
  }
  override def toString: String = syntax
}
