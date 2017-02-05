package scala.meta
package semantic
package v1

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import org.scalameta.adt._
import scala.meta.common._
import scala.meta.internal.semantic.v1._

// NOTE: This is an initial take on the semantic API.
// Instead of immediately implementing the full vision described in my dissertation,
// we will first deliver the low-hanging fruit (https://github.com/scalameta/scalameta/issues/604),
// and only then will approach really tricky tasks (https://github.com/scalameta/scalameta/issues/623).

@root trait Symbol extends Optional {
  def syntax: String
  def structure: String
}

object Symbol {
  @none object None extends Symbol {
    override def syntax = s""
    override def structure = s"""Symbol.None"""
    override def toString = syntax
  }

  @leaf class Local(addr: Address, start: Int, end: Int) extends Symbol {
    override def syntax = s"${addr.syntax}@$start..$end"
    override def structure = s"""Symbol.Local(${addr.structure}, $start, $end)"""
    override def toString = syntax
  }

  @leaf class Global(owner: Symbol, signature: Signature) extends Symbol {
    override def syntax = s"${owner.syntax}${signature.syntax}"
    override def structure = s"""Symbol.Global(${owner.structure}, ${signature.structure})"""
    override def toString = syntax
  }

  @leaf class Multi(ids: Seq[Symbol]) extends Symbol {
    override def syntax = ids.map(_.syntax).mkString(";")
    override def structure = s"""Symbol.Multi(${ids.map(_.structure).mkString(", ")})"""
    override def toString = syntax
  }

  // TODO: This is obviously a very naive implementation.
  // It'll do for prototyping, but in the future we'll have to replace it.
  // upd. Ugh, I should've started with fastparse in the first place!!
  def apply(s: String): Symbol = {
    if (s == "") {
      Symbol.None
    } else if (s.startsWith("_")) {
      var i = 0

      var currChar = '\u0000'
      def readChar(): Char = {
        if (i >= s.length) sys.error(s"invalid format: $s")
        currChar = s(i)
        i += 1
        currChar
      }

      def readName(): String = {
        val buf = new StringBuilder()
        if (currChar == '`') {
          while (readChar() != '`') buf += currChar
          readChar()
        } else {
          if (!Character.isJavaIdentifierStart(currChar)) sys.error(s"invalid format: $s")
          buf += currChar
          while (Character.isJavaIdentifierPart(readChar())) buf += currChar
        }
        buf.toString
      }

      def parse(curr: Symbol): Symbol = {
        if (i == s.length) return curr
        readChar()

        if (currChar == '[') {
          readChar()
          val name = readName()
          if (currChar != ']') sys.error(s"invalid format: $s")
          parse(Symbol.Global(curr, Signature.TypeParameter(name)))
        } else if (currChar == '(') {
          readChar()
          val name = readName()
          if (currChar != ')') sys.error(s"invalid format: $s")
          parse(Symbol.Global(curr, Signature.TermParameter(name)))
        } else {
          val name = readName()
          if (currChar == '#') {
            parse(Symbol.Global(curr, Signature.Type(name)))
          } else if (currChar == '.') {
            parse(Symbol.Global(curr, Signature.Term(name)))
          } else if (currChar == '(') {
            val buf = new StringBuilder()
            buf += currChar
            while (readChar() != '.') buf += currChar
            parse(Symbol.Global(curr, Signature.Method(name, buf.toString)))
          } else if (currChar == '=') {
            readChar()
            if (currChar != '>') sys.error(s"invalid format: $s")
            parse(Symbol.Global(curr, Signature.Self(name)))
          } else {
            sys.error(s"invalid format: $s")
          }
        }
      }

      parse(Symbol.None)
    } else {
      val LocalSymbol = """^(.*?)@(\d+)\.\.(\d+)$""".r
      s match {
        case LocalSymbol(s_addr, s_start, s_end) =>
          Symbol.Local(Address(s_addr), s_start.toInt, s_end.toInt)
        case _ =>
          sys.error(s"invalid format: $s")
      }
    }
  }
}

@root trait Signature {
  def name: String
  def syntax: String
  def structure: String
}

object Signature {
  @leaf class Type(name: String) extends Signature {
    override def syntax = s"${encodeName(name)}#"
    override def structure = s"""Signature.Type("$name")"""
    override def toString = syntax
  }

  @leaf class Term(name: String) extends Signature {
    override def syntax = s"${encodeName(name)}."
    override def structure = s"""Signature.Term("$name")"""
    override def toString = syntax
  }

  @leaf class Method(name: String, jvmSignature: String) extends Signature {
    override def syntax = s"${encodeName(name)}$jvmSignature."
    override def structure = s"""Signature.Method("$name", "$jvmSignature")"""
    override def toString = syntax
  }

  @leaf class TypeParameter(name: String) extends Signature {
    override def syntax = s"[${encodeName(name)}]"
    override def structure = s"""Signature.TypeParameter("$name")"""
    override def toString = syntax
  }

  @leaf class TermParameter(name: String) extends Signature {
    override def syntax = s"(${encodeName(name)})"
    override def structure = s"""Signature.TermParameter("$name")"""
    override def toString = syntax
  }

  @leaf class Self(name: String) extends Signature {
    override def syntax = s"${encodeName(name)}=>"
    override def structure = s"""Signature.Self("$name")"""
    override def toString = syntax
  }

  private def encodeName(name: String): String = {
    if (name.forall(Character.isJavaIdentifierStart)) name else "`" + name + "`"
  }
}