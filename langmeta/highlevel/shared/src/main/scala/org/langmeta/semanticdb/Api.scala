package org.langmeta
package semanticdb

import org.langmeta.inputs._
import org.langmeta

private[langmeta] trait Api extends Flags {
}

private[langmeta] trait Aliases {
  type Database = org.langmeta.semanticdb.Database
  val Database = org.langmeta.semanticdb.Database

  type Document = org.langmeta.semanticdb.Document
  val Document = org.langmeta.semanticdb.Document

  type Synthetic = org.langmeta.semanticdb.Synthetic
  val Synthetic = org.langmeta.semanticdb.Synthetic

  type Symbol = org.langmeta.semanticdb.Symbol
  object Symbol {
    val None = org.langmeta.semanticdb.Symbol.None

    type Local = org.langmeta.semanticdb.Symbol.Local
    val Local = org.langmeta.semanticdb.Symbol.Local

    type Global = org.langmeta.semanticdb.Symbol.Global
    val Global = org.langmeta.semanticdb.Symbol.Global

    type Multi = org.langmeta.semanticdb.Symbol.Multi
    val Multi = org.langmeta.semanticdb.Symbol.Multi

    def apply(s: String): Symbol = org.langmeta.semanticdb.Symbol.apply(s)
    def unapply(sym: String): Option[Symbol] = org.langmeta.semanticdb.Symbol.unapply(sym)
  }

  type Signature = org.langmeta.semanticdb.Signature
  object Signature {
    type Type = org.langmeta.semanticdb.Signature.Type
    val Type = org.langmeta.semanticdb.Signature.Type

    type Term = org.langmeta.semanticdb.Signature.Term
    val Term = org.langmeta.semanticdb.Signature.Term

    type Method = org.langmeta.semanticdb.Signature.Method
    val Method = org.langmeta.semanticdb.Signature.Method

    type TypeParameter = org.langmeta.semanticdb.Signature.TypeParameter
    val TypeParameter = org.langmeta.semanticdb.Signature.TypeParameter

    type TermParameter = org.langmeta.semanticdb.Signature.TermParameter
    val TermParameter = org.langmeta.semanticdb.Signature.TermParameter

    type Self = org.langmeta.semanticdb.Signature.Self
    val Self = org.langmeta.semanticdb.Signature.Self
  }

  type Message = org.langmeta.semanticdb.Message
  val Message = org.langmeta.semanticdb.Message

  type Severity = org.langmeta.semanticdb.Severity
  val Severity = org.langmeta.semanticdb.Severity

  type Denotation = org.langmeta.semanticdb.Denotation
  val Denotation = org.langmeta.semanticdb.Denotation

  type ResolvedName = org.langmeta.semanticdb.ResolvedName
  val ResolvedName = org.langmeta.semanticdb.ResolvedName

  type ResolvedSymbol = org.langmeta.semanticdb.ResolvedSymbol
  val ResolvedSymbol = org.langmeta.semanticdb.ResolvedSymbol
}
