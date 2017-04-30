package scala.meta
package semantic

import org.scalameta.unreachable
import org.scalameta.debug
import scala.compat.Platform.EOL
import scala.meta.internal.ast.Helpers._
import scala.meta.inputs._
import scala.meta.prettyprinters._

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
    def symbol: Symbol = (apiBoundary {
      def relevantPosition(tree: Tree): Position = tree match {
        case name1: Name => name1.pos
        case _: Term.This => ???
        case _: Term.Super => ???
        case Term.Select(_, name1) => name1.pos
        case Term.ApplyUnary(_, name1) => name1.pos
        case Type.Select(_, name1) => name1.pos
        case Type.Project(_, name1) => name1.pos
        case Type.Singleton(ref1) => relevantPosition(ref1)
        case Ctor.Ref.Select(_, name1) => name1.pos
        case Ctor.Ref.Project(_, name1) => name1.pos
        case Ctor.Ref.Function(name1) => ???
        case _: Importee.Wildcard => ???
        case Importee.Name(name1) => name1.pos
        case Importee.Rename(name1, _) => name1.pos
        case Importee.Unimport(name1) => name1.pos
        case _ => unreachable(debug(tree.syntax, tree.structure))
      }
      val position = relevantPosition(ref)
      val anchor = position.toAnchor
      m.database.names.getOrElse(anchor, sys.error(s"semantic DB doesn't contain $ref"))
    }).get
  }

  implicit class XtensionSymbolDenotation(sym: Symbol)(implicit m: Mirror) extends HasFlags {
    def denot: Completed[Denotation] = apiBoundary(m.database.denotations.getOrElse(sym, sys.error(s"semantic DB doesn't contain $sym")))
    // NOTE: isXXX methods are added here via `extends HasFlags`
    def hasFlag(flag: Long): Boolean = (denot.get.flags & flag) == flag
    def info: String = denot.get.info
  }

  private def apiBoundary[T](op: => T): Completed[T] = {
    try {
      val result = op
      Completed.Success(result)
    } catch {
      case ex: SemanticException =>
        Completed.Error(ex)
      case ex: Exception =>
        var message = s"fatal error: ${ex.getMessage}$EOL"
        message += "This is a bug; please report it via https://github.com/scalameta/scalameta/issues/new."
        Completed.Error(new SemanticException(Position.None, message, Some(ex)))
    }
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
