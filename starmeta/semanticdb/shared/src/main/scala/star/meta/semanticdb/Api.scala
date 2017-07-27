package star.meta
package semanticdb

import star.meta.inputs._

private[meta] trait Api extends Flags {
}

private[meta] trait Aliases {
  type Database = star.meta.semanticdb.Database
  val Database = star.meta.semanticdb.Database

  type Attributes = star.meta.semanticdb.Attributes
  val Attributes = star.meta.semanticdb.Attributes

  type Sugar = star.meta.semanticdb.Sugar
  val Sugar = star.meta.semanticdb.Sugar

  type Symbol = star.meta.semanticdb.Symbol
  object Symbol {
    val None = star.meta.semanticdb.Symbol.None

    type Local = star.meta.semanticdb.Symbol.Local
    val Local = star.meta.semanticdb.Symbol.Local

    type Global = star.meta.semanticdb.Symbol.Global
    val Global = star.meta.semanticdb.Symbol.Global

    type Multi = star.meta.semanticdb.Symbol.Multi
    val Multi = star.meta.semanticdb.Symbol.Multi

    def apply(s: String): Symbol = star.meta.semanticdb.Symbol.apply(s)
    def unapply(sym: String): Option[Symbol] = star.meta.semanticdb.Symbol.unapply(sym)
  }

  type Signature = star.meta.semanticdb.Signature
  object Signature {
    type Type = star.meta.semanticdb.Signature.Type
    val Type = star.meta.semanticdb.Signature.Type

    type Term = star.meta.semanticdb.Signature.Term
    val Term = star.meta.semanticdb.Signature.Term

    type Method = star.meta.semanticdb.Signature.Method
    val Method = star.meta.semanticdb.Signature.Method

    type TypeParameter = star.meta.semanticdb.Signature.TypeParameter
    val TypeParameter = star.meta.semanticdb.Signature.TypeParameter

    type TermParameter = star.meta.semanticdb.Signature.TermParameter
    val TermParameter = star.meta.semanticdb.Signature.TermParameter

    type Self = star.meta.semanticdb.Signature.Self
    val Self = star.meta.semanticdb.Signature.Self
  }

  type Message = star.meta.semanticdb.Message
  val Message = star.meta.semanticdb.Message

  type Severity = star.meta.semanticdb.Severity
  val Severity = star.meta.semanticdb.Severity

  type Denotation = star.meta.semanticdb.Denotation
  val Denotation = star.meta.semanticdb.Denotation
}
