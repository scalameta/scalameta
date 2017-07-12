package scala.meta
package semantic

import org.scalameta.unreachable
import org.scalameta.debug
import scala.meta.inputs._
import scala.meta.prettyprinters._
import scala.meta.parsers.{XtensionParsersDialectInput, XtensionParseDialectInput}

private[meta] trait Api extends Flags {
  implicit class XtensionMirrorSources(mirror: Mirror) {
    def sources: Seq[Source] = mirror.database.entries.map(_.source)
  }

  implicit class XtensionAttributesSource(attributes: Attributes) {
    def source: Source = attributes.dialect(attributes.input).parse[Source].get
  }

  implicit class XtensionTermSugar(term: Term)(implicit m: Mirror) {
    def sugar: Option[Term] =
      for {
        sugar <- m.database.sugars.get(term.pos)
        // Would be nice if there was a less hacky way to get the underlying dialect
        // of a Tree.
        dialect <- term.tokens.headOption.map(_.dialect)
      } yield dialect(sugar.input).parse[Term].get
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

  type Sugar = scala.meta.semantic.Sugar
  val Sugar = scala.meta.semantic.Sugar

  type Symbol = scala.meta.semantic.Symbol
  object Symbol {
    val None = scala.meta.semantic.Symbol.None

    type Local = scala.meta.semantic.Symbol.Local
    val Local = scala.meta.semantic.Symbol.Local

    type Global = scala.meta.semantic.Symbol.Global
    val Global = scala.meta.semantic.Symbol.Global

    type Multi = scala.meta.semantic.Symbol.Multi
    val Multi = scala.meta.semantic.Symbol.Multi

    def apply(s: String): Symbol = scala.meta.semantic.Symbol.apply(s)
    def unapply(sym: String): Option[Symbol] = scala.meta.semantic.Symbol.unapply(sym)
  }

  type Signature = scala.meta.semantic.Signature
  object Signature {
    type Type = scala.meta.semantic.Signature.Type
    val Type = scala.meta.semantic.Signature.Type

    type Term = scala.meta.semantic.Signature.Term
    val Term = scala.meta.semantic.Signature.Term

    type Method = scala.meta.semantic.Signature.Method
    val Method = scala.meta.semantic.Signature.Method

    type TypeParameter = scala.meta.semantic.Signature.TypeParameter
    val TypeParameter = scala.meta.semantic.Signature.TypeParameter

    type TermParameter = scala.meta.semantic.Signature.TermParameter
    val TermParameter = scala.meta.semantic.Signature.TermParameter

    type Self = scala.meta.semantic.Signature.Self
    val Self = scala.meta.semantic.Signature.Self
  }

  type Message = scala.meta.semantic.Message
  val Message = scala.meta.semantic.Message

  type Severity = scala.meta.semantic.Severity
  val Severity = scala.meta.semantic.Severity

  type Denotation = scala.meta.semantic.Denotation
  val Denotation = scala.meta.semantic.Denotation
}
