package scala.meta
package semantic

import scala.meta.inputs._

private[meta] trait Api extends Flags {
  implicit class XtensionPositionAnchor(pos: Position) {
    def toAnchor: Anchor = pos.input match {
      case scala.meta.inputs.Input.File(path, _) =>
        Anchor(path, pos.start.offset, pos.end.offset)
      case other =>
        sys.error(s"unsupported input " + other)
    }
  }

  implicit class XtensionRefSymbol(ref: Ref)(implicit m: Mirror) {
    def symbol: Symbol = m.symbol(ref).get
  }

  implicit class XtensionSymbolDenotation(sym: Symbol)(implicit m: Mirror) extends HasFlags {
    def hasFlag(flag: Long): Boolean = (m.denot(sym).get.flags & flag) == flag
    def info: String = m.denot(sym).get.info
  }
}

private[meta] trait Aliases {
  type Mirror = scala.meta.semantic.Mirror
  val Mirror = scala.meta.semantic.Mirror

  type Database = scala.meta.semantic.Database
  val Database = scala.meta.semantic.Database

  type AttributedSource = scala.meta.semantic.AttributedSource
  val AttributedSource = scala.meta.semantic.AttributedSource

  type Anchor = scala.meta.semantic.Anchor
  val Anchor = scala.meta.semantic.Anchor

  type Symbol = scala.meta.semantic.Symbol
  val Symbol = scala.meta.semantic.Symbol

  type Signature = scala.meta.semantic.Signature
  val Signature = scala.meta.semantic.Signature

  type Message = scala.meta.semantic.Message
  val Message = scala.meta.semantic.Message

  type Severity = scala.meta.semantic.Severity
  val Severity = scala.meta.semantic.Severity

  type Denotation = scala.meta.semantic.Denotation
  val Denotation = scala.meta.semantic.Denotation

  type Completed[+T] = scala.meta.semantic.Completed[T]
  lazy val Completed = scala.meta.semantic.Completed

  type SemanticException = scala.meta.semantic.SemanticException
  lazy val SemanticException = scala.meta.semantic.SemanticException
}
