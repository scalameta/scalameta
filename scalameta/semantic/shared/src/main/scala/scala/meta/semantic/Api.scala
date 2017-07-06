package scala.meta
package semantic

import org.scalameta.unreachable
import org.scalameta.debug
import scala.compat.Platform.EOL
import scala.meta.internal.trees._
import scala.meta.inputs._
import scala.meta.prettyprinters._
import scala.meta.parsers.{XtensionParsersDialectInput, XtensionParseDialectInput}

private[meta] trait Api extends Flags {
  implicit class XtensionMirrorSources(mirror: Mirror) {
    def sources: List[Source] = mirror.database.entries.map { attrs => attrs.dialect(attrs.input).parse[Source].get }
  }

  implicit class XtensionRefSymbol(ref: Ref)(implicit m: Mirror) {
    def symbol: Symbol = {
      def relevantPosition(tree: Tree): Position = tree match {
        case name: Name => name.pos
        case _: Term.This => ???
        case _: Term.Super => ???
        case Term.Select(_, name) => name.pos
        case Term.ApplyUnary(_, name) => name.pos
        case Type.Select(_, name) => name.pos
        case Type.Project(_, name) => name.pos
        case Type.Singleton(ref) => relevantPosition(ref)
        case Init(_, name, _) => name.pos
        case _: Importee.Wildcard => ???
        case Importee.Name(name) => name.pos
        case Importee.Rename(name, _) => name.pos
        case Importee.Unimport(name) => name.pos
        case _ => unreachable(debug(tree.syntax, tree.structure))
      }
      val position = relevantPosition(ref)
      m.database.names.getOrElse(position, sys.error(s"semantic DB doesn't contain $ref"))
    }
  }

  implicit class XtensionSymbolDenotation(sym: Symbol)(implicit m: Mirror) extends HasFlags {
    def denot: Denotation = m.database.denotations.getOrElse(sym, sys.error(s"semantic DB doesn't contain $sym"))
    // NOTE: hasFlag/isXXX methods are added here via `extends HasFlags`
    def flags: Long = denot.flags
    def info: String = denot.info
  }
}

private[meta] trait Aliases {
  type Mirror = scala.meta.semantic.Mirror
  val Mirror = scala.meta.semantic.Mirror

  type Database = scala.meta.semantic.Database
  val Database = scala.meta.semantic.Database

  type Attributes = scala.meta.semantic.Attributes
  val Attributes = scala.meta.semantic.Attributes

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
