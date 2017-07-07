package scala.meta
package semantic

import scala.compat.Platform.EOL
import org.scalameta.adt._
import org.scalameta.invariants._
import scala.meta.common._

@root trait Symbol extends Optional {
  def syntax: String
  def structure: String
  def name: Name
  def fullName: Ref
}

object Symbol {
  private def unsupported(sym: Symbol, op: String) = {
    val receiver = if (sym == None) "Symbol.None" else sym.syntax
    sys.error(s"Symbol.${op} not supported for $receiver")
  }

  @none object None extends Symbol {
    override def toString = syntax
    override def syntax = s""
    override def structure = s"""Symbol.None"""
    override def name: Name = unsupported(this, "name")
    override def fullName: Ref = unsupported(this, "fullName")
  }

  @leaf class Local(id: String) extends Symbol {
    override def toString = syntax
    override def syntax = id
    override def structure = s"""Symbol.Local("$id")"""
    override def name: Name = ???
    override def fullName: Ref = ???
  }

  @leaf class Global(owner: Symbol, signature: Signature) extends Symbol {
    override def toString = syntax
    override def syntax = s"${owner.syntax}${signature.syntax}"
    override def structure = s"""Symbol.Global(${owner.structure}, ${signature.structure})"""
    override def name: Name = {
      signature match {
        case _: Signature.Type | _: Signature.TypeParameter => Type.Name(signature.name)
        case _ => Term.Name(signature.name)
      }
    }
    override def fullName: Ref = {
      def pre: Option[Term.Ref] = {
        def ownerChain(sym: Symbol): List[Symbol] = sym match {
          case Symbol.Global(Symbol.None, _) => Nil
          case Symbol.Global(owner, _) => ownerChain(owner) :+ owner
          case _ => Nil
        }
        val nameChain = ownerChain(this).map {
          case Symbol.Global(_, Signature.Term(name)) => name
          case _ => return scala.None
        }
        def loop(names: List[String]): Option[Term.Ref] = names match {
          case Nil =>
            scala.None
          case init :+ last =>
            loop(init) match {
              case scala.None => scala.Some(Term.Name(last))
              case scala.Some(pre) => scala.Some(Term.Select(pre, Term.Name(last)))
            }
        }
        loop(nameChain)
      }
      (pre, signature) match {
        case (scala.Some(pre), Signature.Term(name)) => Term.Select(pre, Term.Name(name))
        case (scala.Some(pre), Signature.Type(name)) => Type.Select(pre, Type.Name(name))
        case _ => name
      }
    }
  }

  @leaf class Multi(symbols: List[Symbol] @nonEmpty) extends Symbol {
    override def toString = syntax
    override def syntax = symbols.map(_.syntax).mkString(";")
    override def structure = s"""Symbol.Multi(${symbols.map(_.structure).mkString(", ")})"""
    override def name: Name = ???
    override def fullName: Ref = ???
  }

  // TODO: This is obviously a very naive implementation.
  // It'll do for prototyping, but in the future we'll have to replace it.
  // upd. Ugh, I should've started with fastparse in the first place!!
  def apply(s: String): Symbol = {
    object naiveParser {
      var i = 0
      def fail() = {
        val message = "invalid symbol format"
        val caret = " " * (i - 1) + "^"
        sys.error(s"$message$EOL$s$EOL$caret")
      }

      val BOF = '\u0000'
      val EOF = '\u001A'
      var currChar = BOF
      def readChar(): Char = {
        if (i >= s.length) {
          if (i == s.length) {
            currChar = EOF
            i += 1
            currChar
          } else {
            fail()
          }
        } else {
          currChar = s(i)
          i += 1
          currChar
        }
      }

      def parseName(): String = {
        val buf = new StringBuilder()
        if (currChar == '`') {
          while (readChar() != '`') buf += currChar
          readChar()
        } else {
          if (!Character.isJavaIdentifierStart(currChar)) fail()
          buf += currChar
          while (Character.isJavaIdentifierPart(readChar())) buf += currChar
        }
        buf.toString
      }

      def parseGlobal(curr: Symbol): Symbol = {
        if (currChar == EOF) {
          curr
        } else if (currChar == ';') {
          curr
        } else if (currChar == '[') {
          readChar()
          val name = parseName()
          if (currChar != ']') fail()
          else readChar()
          parseGlobal(Symbol.Global(curr, Signature.TypeParameter(name)))
        } else if (currChar == '(') {
          readChar()
          val name = parseName()
          if (currChar != ')') fail()
          else readChar()
          parseGlobal(Symbol.Global(curr, Signature.TermParameter(name)))
        } else {
          val name = parseName()
          if (currChar == '#') {
            readChar()
            parseGlobal(Symbol.Global(curr, Signature.Type(name)))
          } else if (currChar == '.') {
            readChar()
            parseGlobal(Symbol.Global(curr, Signature.Term(name)))
          } else if (currChar == '(') {
            val buf = new StringBuilder()
            buf += currChar
            while (readChar() != '.') buf += currChar
            readChar()
            parseGlobal(Symbol.Global(curr, Signature.Method(name, buf.toString)))
          } else if (currChar == '=') {
            readChar()
            if (currChar != '>') fail()
            else readChar()
            parseGlobal(Symbol.Global(curr, Signature.Self(name)))
          } else {
            fail()
          }
        }
      }
      def parseLocal(): Symbol = {
        val start = i - 1
        while (readChar() != '@') {}
        while (Character.isDigit(readChar())) {}
        if (currChar != '.') fail()
        readChar()
        if (currChar != '.') fail()
        while (Character.isDigit(readChar())) {}
        val end = i - 1
        Symbol.Local(s.substring(start, end))
      }

      def parseMulti(symbols: List[Symbol]): Symbol = {
        if (currChar == EOF) {
          symbols match {
            case Nil => Symbol.None
            case List(symbol) => symbol
            case symbols => Symbol.Multi(symbols)
          }
        } else {
          val symbol = {
            if (currChar == '_') parseGlobal(Symbol.None)
            else parseLocal()
          }
          if (currChar == ';') {
            readChar()
            if (currChar == EOF) fail()
          }
          parseMulti(symbols :+ symbol)
        }
      }

      def entryPoint(): Symbol = {
        readChar()
        parseMulti(Nil)
      }
    }
    naiveParser.entryPoint()
  }
  def unapply(sym: String): Option[Symbol] = scala.util.Try(apply(sym)).toOption
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
    val headOk = Character.isJavaIdentifierStart(name.head)
    val tailOk = name.tail.forall(Character.isJavaIdentifierPart)
    if (headOk && tailOk) name else "`" + name + "`"
  }
}
