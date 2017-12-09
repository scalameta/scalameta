package org.langmeta.highlevel.semanticdb

import org.langmeta.highlevel.inputs._

private[langmeta] trait Api extends Flags {
}

private[langmeta] trait Aliases {
  type Database = org.langmeta.highlevel.semanticdb.Database
  val Database = org.langmeta.highlevel.semanticdb.Database

  type Document = org.langmeta.highlevel.semanticdb.Document
  val Document = org.langmeta.highlevel.semanticdb.Document

  type Synthetic = org.langmeta.highlevel.semanticdb.Synthetic
  val Synthetic = org.langmeta.highlevel.semanticdb.Synthetic

  type Symbol = org.langmeta.highlevel.semanticdb.Symbol
  object Symbol {
    val None = org.langmeta.highlevel.semanticdb.Symbol.None

    type Local = org.langmeta.highlevel.semanticdb.Symbol.Local
    val Local = org.langmeta.highlevel.semanticdb.Symbol.Local

    type Global = org.langmeta.highlevel.semanticdb.Symbol.Global
    val Global = org.langmeta.highlevel.semanticdb.Symbol.Global

    type Multi = org.langmeta.highlevel.semanticdb.Symbol.Multi
    val Multi = org.langmeta.highlevel.semanticdb.Symbol.Multi

    def apply(s: String): Symbol = org.langmeta.highlevel.semanticdb.Symbol.apply(s)
    def unapply(sym: String): Option[Symbol] = org.langmeta.highlevel.semanticdb.Symbol.unapply(sym)
  }

  type Signature = org.langmeta.highlevel.semanticdb.Signature
  object Signature {
    type Type = org.langmeta.highlevel.semanticdb.Signature.Type
    val Type = org.langmeta.highlevel.semanticdb.Signature.Type

    type Term = org.langmeta.highlevel.semanticdb.Signature.Term
    val Term = org.langmeta.highlevel.semanticdb.Signature.Term

    type Method = org.langmeta.highlevel.semanticdb.Signature.Method
    val Method = org.langmeta.highlevel.semanticdb.Signature.Method

    type TypeParameter = org.langmeta.highlevel.semanticdb.Signature.TypeParameter
    val TypeParameter = org.langmeta.highlevel.semanticdb.Signature.TypeParameter

    type TermParameter = org.langmeta.highlevel.semanticdb.Signature.TermParameter
    val TermParameter = org.langmeta.highlevel.semanticdb.Signature.TermParameter

    type Self = org.langmeta.highlevel.semanticdb.Signature.Self
    val Self = org.langmeta.highlevel.semanticdb.Signature.Self
  }

  type Message = org.langmeta.highlevel.semanticdb.Message
  val Message = org.langmeta.highlevel.semanticdb.Message

  type Severity = org.langmeta.highlevel.semanticdb.Severity
  val Severity = org.langmeta.highlevel.semanticdb.Severity

  type Denotation = org.langmeta.highlevel.semanticdb.Denotation
  val Denotation = org.langmeta.highlevel.semanticdb.Denotation

  type ResolvedName = org.langmeta.highlevel.semanticdb.ResolvedName
  val ResolvedName = org.langmeta.highlevel.semanticdb.ResolvedName

  type ResolvedSymbol = org.langmeta.highlevel.semanticdb.ResolvedSymbol
  val ResolvedSymbol = org.langmeta.highlevel.semanticdb.ResolvedSymbol
}
