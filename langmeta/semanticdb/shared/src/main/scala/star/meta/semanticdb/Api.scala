package lang.meta
package semanticdb

import lang.meta.inputs._

private[meta] trait Api extends Flags {
}

private[meta] trait Aliases {
  type Database = lang.meta.semanticdb.Database
  val Database = lang.meta.semanticdb.Database

  type Attributes = lang.meta.semanticdb.Attributes
  val Attributes = lang.meta.semanticdb.Attributes

  type Synthetic = lang.meta.semanticdb.Synthetic
  val Synthetic = lang.meta.semanticdb.Synthetic

  type Symbol = lang.meta.semanticdb.Symbol
  object Symbol {
    val None = lang.meta.semanticdb.Symbol.None

    type Local = lang.meta.semanticdb.Symbol.Local
    val Local = lang.meta.semanticdb.Symbol.Local

    type Global = lang.meta.semanticdb.Symbol.Global
    val Global = lang.meta.semanticdb.Symbol.Global

    type Multi = lang.meta.semanticdb.Symbol.Multi
    val Multi = lang.meta.semanticdb.Symbol.Multi

    def apply(s: String): Symbol = lang.meta.semanticdb.Symbol.apply(s)
    def unapply(sym: String): Option[Symbol] = lang.meta.semanticdb.Symbol.unapply(sym)
  }

  type Signature = lang.meta.semanticdb.Signature
  object Signature {
    type Type = lang.meta.semanticdb.Signature.Type
    val Type = lang.meta.semanticdb.Signature.Type

    type Term = lang.meta.semanticdb.Signature.Term
    val Term = lang.meta.semanticdb.Signature.Term

    type Method = lang.meta.semanticdb.Signature.Method
    val Method = lang.meta.semanticdb.Signature.Method

    type TypeParameter = lang.meta.semanticdb.Signature.TypeParameter
    val TypeParameter = lang.meta.semanticdb.Signature.TypeParameter

    type TermParameter = lang.meta.semanticdb.Signature.TermParameter
    val TermParameter = lang.meta.semanticdb.Signature.TermParameter

    type Self = lang.meta.semanticdb.Signature.Self
    val Self = lang.meta.semanticdb.Signature.Self
  }

  type Message = lang.meta.semanticdb.Message
  val Message = lang.meta.semanticdb.Message

  type Severity = lang.meta.semanticdb.Severity
  val Severity = lang.meta.semanticdb.Severity

  type Denotation = lang.meta.semanticdb.Denotation
  val Denotation = lang.meta.semanticdb.Denotation

  type ResolvedName = lang.meta.semanticdb.ResolvedName
  val ResolvedName = lang.meta.semanticdb.ResolvedName

  type ResolvedSymbol = lang.meta.semanticdb.ResolvedSymbol
  val ResolvedSymbol = lang.meta.semanticdb.ResolvedSymbol
}
