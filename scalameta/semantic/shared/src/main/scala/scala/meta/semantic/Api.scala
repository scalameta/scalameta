package scala.meta
package semantic

import org.scalameta.unreachable
import org.scalameta.debug
import scala.compat.Platform.EOL
import scala.meta.internal.ast.Helpers._
import scala.meta.inputs._
import scala.meta.prettyprinters._

private[meta] trait Api extends Flags {
  // TODO: this is a temporary stub that should be replaced with something more principled
  implicit class XtensionPosition(pos: Position) {
    private def path = pos.input match {
      case Input.File(path, _) => path
      case Input.LabeledString(label, _) => label
      case _ => "<input>"
    }
    def syntax: String = {
      s"$path@${pos.start.offset}..${pos.end.offset}"
    }
    def structure: String = {
      val s_start = s"Point.Offset(${pos.start.input}, ${pos.start.offset})"
      val s_end = s"Point.Offset(${pos.end.input}, ${pos.end.offset})"
      s"Position.Range(${pos.input}, $s_start, $s_end)"
    }
  }

  implicit class XtensionRefSymbol(ref: Ref)(implicit m: Mirror) {
    def symbol: Symbol = {
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
      m.database.names.getOrElse(position, sys.error(s"semantic DB doesn't contain $ref"))
    }
  }

  implicit class XtensionSymbolDenotation(sym: Symbol)(implicit m: Mirror) extends HasFlags {
    def denot: Denotation = m.database.denotations.getOrElse(sym, sys.error(s"semantic DB doesn't contain $sym"))
    // NOTE: isXXX methods are added here via `extends HasFlags`
    def hasFlag(flag: Long): Boolean = (denot.flags & flag) == flag
    def info: String = denot.info
  }
}

private[meta] trait Aliases {
  type Mirror = scala.meta.semantic.Mirror
  val Mirror = scala.meta.semantic.Mirror

  type Database = scala.meta.semantic.Database
  val Database = scala.meta.semantic.Database

  type AttributedSource = scala.meta.semantic.AttributedSource
  val AttributedSource = scala.meta.semantic.AttributedSource

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
}
