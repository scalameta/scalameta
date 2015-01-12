package scala.meta
package ui

final case class Exception(msg: String, pos: scala.meta.syntactic.Token) extends scala.Exception(msg)
