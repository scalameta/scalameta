package scala.meta.semanticdb

private[meta] trait Api {}

private[meta] trait Aliases {
  type Symbol = scala.meta.semanticdb.Symbol
  object Symbol {
    val None = scala.meta.semanticdb.Symbol.None

    type Local = scala.meta.semanticdb.Symbol.Local
    val Local = scala.meta.semanticdb.Symbol.Local

    type Global = scala.meta.semanticdb.Symbol.Global
    val Global = scala.meta.semanticdb.Symbol.Global

    type Multi = scala.meta.semanticdb.Symbol.Multi
    val Multi = scala.meta.semanticdb.Symbol.Multi

    def apply(s: String): Symbol = scala.meta.semanticdb.Symbol.apply(s)
    def unapply(sym: String): Option[Symbol] = scala.meta.semanticdb.Symbol.unapply(sym)
  }

  type Signature = scala.meta.semanticdb.Signature
  object Signature {
    type Type = scala.meta.semanticdb.Signature.Type
    val Type = scala.meta.semanticdb.Signature.Type

    type Term = scala.meta.semanticdb.Signature.Term
    val Term = scala.meta.semanticdb.Signature.Term

    type Method = scala.meta.semanticdb.Signature.Method
    val Method = scala.meta.semanticdb.Signature.Method

    type TypeParameter = scala.meta.semanticdb.Signature.TypeParameter
    val TypeParameter = scala.meta.semanticdb.Signature.TypeParameter

    type TermParameter = scala.meta.semanticdb.Signature.TermParameter
    val TermParameter = scala.meta.semanticdb.Signature.TermParameter

    type Self = scala.meta.semanticdb.Signature.Self
    val Self = scala.meta.semanticdb.Signature.Self
  }

}
