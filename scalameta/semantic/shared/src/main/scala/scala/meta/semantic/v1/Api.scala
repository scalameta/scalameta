package scala.meta
package semantic
package v1

import scala.meta.inputs._

// NOTE: This is an initial take on the semantic API.
// Instead of immediately implementing the full vision described in my dissertation,
// we will first deliver the low-hanging fruit (https://github.com/scalameta/scalameta/issues/604),
// and only then will approach really tricky tasks (https://github.com/scalameta/scalameta/issues/623).

private[meta] trait Api extends Flags {
  implicit class XtensionPositionLocation(pos: Position) {
    def toLocation: Location = pos.input match {
      case scala.meta.inputs.Input.File(path, _) =>
        Location(path, pos.start.offset, pos.end.offset)
      case other =>
        sys.error(s"unsupported input " + other)
    }
  }

  implicit class XtensionSemanticEquality(tree1: Tree)(implicit m: Mirror) {
    def ===(tree2: Tree): Boolean = scala.meta.internal.semantic.v1.equality.equals(tree1, tree2)
    def =!=(tree2: Tree): Boolean = !(tree1 === tree2)
  }

  implicit class XtensionRefSymbol(ref: Ref)(implicit m: Mirror) {
    def symbol: Symbol = m.symbol(ref).get
  }

  implicit class XtensionSymbolFlags(sym: Symbol)(implicit m: Mirror) extends HasFlags {
    def hasFlag(flag: Long): Boolean = (m.denot(sym).get.flags & flag) == flag
  }
}

private[meta] trait Aliases {
  type Database = scala.meta.semantic.v1.Database
  val Database = scala.meta.semantic.v1.Database

  type Mirror = scala.meta.semantic.v1.Mirror
  val Mirror = scala.meta.semantic.v1.Mirror

  type Location = scala.meta.semantic.v1.Location
  val Location = scala.meta.semantic.v1.Location

  type Symbol = scala.meta.semantic.v1.Symbol
  val Symbol = scala.meta.semantic.v1.Symbol

  type Signature = scala.meta.semantic.v1.Signature
  val Signature = scala.meta.semantic.v1.Signature

  type CompilerMessage = scala.meta.semantic.v1.CompilerMessage
  val CompilerMessage = scala.meta.semantic.v1.CompilerMessage

  type Severity = scala.meta.semantic.v1.Severity
  val Severity = scala.meta.semantic.v1.Severity

  type Denotation = scala.meta.semantic.v1.Denotation
  val Denotation = scala.meta.semantic.v1.Denotation

  type Completed[+T] = scala.meta.semantic.v1.Completed[T]
  lazy val Completed = scala.meta.semantic.v1.Completed

  type SemanticException = scala.meta.semantic.v1.SemanticException
  lazy val SemanticException = scala.meta.semantic.v1.SemanticException
}
