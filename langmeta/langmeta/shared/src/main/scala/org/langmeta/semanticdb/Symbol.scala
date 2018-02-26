package org.langmeta
package semanticdb

import scala.compat.Platform.EOL

sealed trait Symbol extends Product {
  def syntax: String
  def structure: String
}

object Symbol {
  case object None extends Symbol {
    override def toString = syntax
    override def syntax = ""
    override def structure = s"""Symbol.None"""
  }

  final case class Local(id: String) extends Symbol {
    override def toString = syntax
    override def syntax = id
    override def structure = s"""Symbol.Local("$id")"""
  }

  final case class Global(owner: Symbol, signature: Signature) extends Symbol {
    override def toString = syntax
    override def syntax = s"${owner.syntax}${signature.syntax}"
    override def structure = s"""Symbol.Global(${owner.structure}, ${signature.structure})"""
  }

  final case class Multi(symbols: List[Symbol]) extends Symbol {
    override def toString = syntax
    override def syntax = symbols.map(_.syntax).sorted.mkString(";")
    override def structure = s"""Symbol.Multi(${symbols.map(_.structure).mkString(", ")})"""
  }

  import fastparse.all._
  private def charsExcept(chars: String) =
    CharsWhile(c => ! chars.contains(c))

  private val quotedName =
    P("`" ~/ charsExcept("`").! ~ "`")
  private val unquotedName =
    P(CharPred(Character.isJavaIdentifierStart) ~ CharsWhile(Character.isJavaIdentifierPart, min = 0)).!
  private val name =
    P(quotedName | unquotedName)

  private val method =
    P(("(" ~/ (quotedName | charsExcept("`)")).rep(min = 0) ~ ")").! ~ ".")

  private val signature = P {
    ("[" ~/ name ~/ "]").map(Signature.TypeParameter) |
    ("(" ~/ name ~/ ")").map(Signature.TermParameter) |
    (name ~/ (
      "#" ~/ PassWith(Signature.Type) |
      "." ~/ PassWith(Signature.Term) |
      method.map(sig => Signature.Method(_: String, sig)) |
      ("=>" ~/ PassWith(Signature.Self)))
    ).map { case (n, f) => f(n) }
  }

  private val global = P {
    signature.rep(min = 0).map {
      _.foldLeft(Symbol.None: Symbol)(Symbol.Global)
    }
  }

  private val local: Parser[Symbol] = P {
    name.filter(_.startsWith("local")).map(Symbol.Local)
  }

  private val multi = P {
    ((&("_") ~/ global) | local).rep(min = 0, sep = ";").map {
      case Seq() => Symbol.None
      case Seq(symbol) => symbol
      case syms => Symbol.Multi(syms.sortBy(_.syntax).toList)
    } ~ End
  }

  def apply(s: String): Symbol = multi.parse(s).get.value

  def unapply(sym: String): Option[Symbol] = multi.unapply(sym)
}

sealed trait Signature {
  def name: String
  def syntax: String
  def structure: String
}

object Signature {
  final case class Type(name: String) extends Signature {
    override def syntax = s"${encodeName(name)}#"
    override def structure = s"""Signature.Type("$name")"""
    override def toString = syntax
  }

  final case class Term(name: String) extends Signature {
    override def syntax = s"${encodeName(name)}."
    override def structure = s"""Signature.Term("$name")"""
    override def toString = syntax
  }

  final case class Method(name: String, disambiguator: String) extends Signature {
    @deprecated("Use `disambiguator` instead.", "3.3.0")
    def jvmSignature: String = disambiguator
    override def syntax = s"${encodeName(name)}${disambiguator}."
    override def structure = s"""Signature.Method("$name", "$disambiguator")"""
    override def toString = syntax
  }

  final case class TypeParameter(name: String) extends Signature {
    override def syntax = s"[${encodeName(name)}]"
    override def structure = s"""Signature.TypeParameter("$name")"""
    override def toString = syntax
  }

  final case class TermParameter(name: String) extends Signature {
    override def syntax = s"(${encodeName(name)})"
    override def structure = s"""Signature.TermParameter("$name")"""
    override def toString = syntax
  }

  final case class Self(name: String) extends Signature {
    override def syntax = s"${encodeName(name)}=>"
    override def structure = s"""Signature.Self("$name")"""
    override def toString = syntax
  }

  private def encodeName(name: String): String = {
    val headOk = Character.isJavaIdentifierStart(name.head)
    val tailOk = name.tail.forall(Character.isJavaIdentifierPart)
    if (headOk && tailOk) name else "`" + name + "`"
  }
}
