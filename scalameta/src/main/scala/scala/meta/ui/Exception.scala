package scala.meta
package ui

// TODO: also have an optional Position parameter
final case class Exception(msg: String) extends scala.Exception(msg)
