package scala.meta.internal.semanticdb3

import scala.annotation.tailrec
import scala.compat.Platform.EOL
import scala.meta.internal.semanticdb3.Scala.{Descriptor => d}
import scala.meta.internal.semanticdb3.Scala.{Names => n}
import scala.meta.internal.semanticdb3.Scala.{TypeDescriptor => td}

// NOTE: This API is not meant to be the only true way of interacting with symbols.
//
// Some producers may find it easier to work directly with strings.
// For example, in Javacp it's more convenient to go from JVM class names
// directly to class symbols, forgoing the tedium of repeated invocations of
// Symbol.Global. That's alright.
//
// Most consumers will find it easier to talk to their symbol index directly,
// since SymbolInformation contains most of the information provided here
// and then some more. That's alright as well.
//
// Instead, this API is meant to assist with reducing the hassle of string
// manipulation by abstracting away common tasks of constructing and deconstructing
// symbols. If you find yourself doing substrings for symbol analysis or
// concatenations / magic strings for symbol creation, you will most likely
// find this API helpful.

object Scala {
  object Symbols {
    val None: String = ""
    val RootPackage: String = "_root_."
    val EmptyPackage: String = "_empty_."
    def Global(owner: String, desc: Descriptor): String = Global(owner, desc.toString)
    def Global(owner: String, desc: String) = if (owner != RootPackage) owner + desc else desc
    def Local(id: Int): String = Local("local" + id.toString)
    def Local(symbol: String): String = symbol
  }

  implicit class ScalaSymbolOps(symbol: String) {
    def isNone: Boolean = symbol == Symbols.None
    def isRootPackage: Boolean = symbol == Symbols.RootPackage
    def isEmptyPackage: Boolean = symbol == Symbols.EmptyPackage
    def isGlobal: Boolean = !isNone && Descriptor.descriptorLasts.contains(symbol.last)
    def isLocal: Boolean = !isNone && !isGlobal
    def ownerChain: List[String] = {
      val buf = List.newBuilder[String]
      def loop(symbol: String): Unit = {
        if (!symbol.isNone) {
          loop(symbol.owner)
          buf += symbol
        }
      }
      loop(symbol)
      buf.result
    }
    def owner: String = {
      if (isGlobal) {
        if (isRootPackage) Symbols.None
        else {
          val rest = DescriptorParser(symbol)._2
          if (rest.nonEmpty) rest
          else Symbols.RootPackage
        }
      } else {
        Symbols.None
      }
    }
    def desc: Descriptor = DescriptorParser(symbol)._1
  }

  sealed trait Descriptor {
    def isNone: Boolean = this == d.None
    def isTerm: Boolean = this.isInstanceOf[d.Term]
    def isMethod: Boolean = this.isInstanceOf[d.Method]
    def isType: Boolean = this.isInstanceOf[d.Type]
    def isParameter: Boolean = this.isInstanceOf[d.Parameter]
    def isTypeParameter: Boolean = this.isInstanceOf[d.TypeParameter]
    def name: String
    override def toString: String = {
      this match {
        case d.None => sys.error("unsupported descriptor")
        case d.Term(name) => s"${name.encoded}."
        case d.Method(name, tpe, 0) => s"${name.encoded}(${tpe})."
        case d.Method(name, tpe, n) => s"${name.encoded}(${tpe}+${n})."
        case d.Type(name) => s"${name.encoded}#"
        case d.Parameter(name) => s"(${name.encoded})"
        case d.TypeParameter(name) => s"[${name.encoded}]"
      }
    }
  }
  object Descriptor {
    final case object None extends Descriptor { def name: String = n.None }
    final case class Term(name: String) extends Descriptor
    final case class Method(name: String, tpe: TypeDescriptor, index: Int) extends Descriptor
    final case class Type(name: String) extends Descriptor
    final case class Parameter(name: String) extends Descriptor
    final case class TypeParameter(name: String) extends Descriptor
    private[semanticdb3] val descriptorLasts = Set('.', '#', ')', ']')
  }

  implicit class ScalaNameOps(name: String) {
    def encoded: String = {
      if (name == n.None) {
        "``"
      } else {
        val (start, parts) = (name.head, name.tail)
        val isStartOk = Character.isJavaIdentifierStart(start)
        val isPartsOk = parts.forall(Character.isJavaIdentifierPart)
        if (isStartOk && isPartsOk) name
        else "`" + name + "`"
      }
    }
    def decoded: String = {
      name.stripPrefix("`").stripSuffix("`")
    }
  }

  object Names {
    val None: String = ""
    val RootPackage: String = "_root_"
    val EmptyPackage: String = "_empty_"
    val Constructor: String = "<init>"
    val Anonymous: String = "_"
  }

  sealed trait TypeDescriptor {
    override def toString: String = {
      this match {
        case td.Ref(name) => name.encoded
        case td.Method(params) => params.mkString(",")
        case td.Other => "?"
      }
    }
  }
  object TypeDescriptor {
    final case class Ref(name: String) extends TypeDescriptor
    final case class Method(params: List[TypeDescriptor]) extends TypeDescriptor
    final case object Other extends TypeDescriptor
  }

  private class DescriptorParser(s: String) {
    var i = s.length
    def fail() = {
      val message = "invalid symbol format"
      val caret = " " * i + "^"
      sys.error(s"$message$EOL$s$EOL$caret")
    }

    val BOF = '\u0000'
    val EOF = '\u001A'
    var currChar = EOF
    def readChar(): Char = {
      if (i <= 0) {
        if (i == 0) {
          i -= 1
          currChar = BOF
          currChar
        } else {
          fail()
        }
      } else {
        i -= 1
        currChar = s(i)
        currChar
      }
    }

    def parseName(): String = {
      val buf = new StringBuilder()
      if (currChar == '`') {
        while (readChar() != '`') buf += currChar
        readChar()
      } else {
        if (!Character.isJavaIdentifierPart(currChar)) fail()
        buf += currChar
        while (Character.isJavaIdentifierPart(readChar())) {
          if (currChar == BOF) return buf.reverse.toString
          buf += currChar
        }
      }
      buf.reverse.toString
    }

    def parseDisambiguator(): (TypeDescriptor, Int) = {
      def merge(typeDesc1: TypeDescriptor, typeDesc2: TypeDescriptor): TypeDescriptor = {
        typeDesc2 match {
          case null => typeDesc1
          case td.Method(typeDescs2) => td.Method(typeDesc1 +: typeDescs2)
          case _ => td.Method(typeDesc1 +: List(typeDesc2))
        }
      }
      @tailrec def loop(typeDesc: TypeDescriptor, index: Int): (TypeDescriptor, Int) = {
        if (currChar == '(') {
          if (typeDesc != null) (typeDesc, index)
          else (td.Method(Nil), index)
        } else if (currChar == ',') {
          readChar()
          loop(typeDesc, index)
        } else if (currChar == '?') {
          loop(merge(td.Other, typeDesc), index)
        } else {
          val backtick = currChar == '`'
          val name = parseName()
          if (currChar == '+') {
            if (!backtick && index == 0) {
              readChar()
              loop(typeDesc, name.toInt)
            } else {
              fail()
            }
          } else {
            loop(merge(td.Ref(name), typeDesc), index)
          }
        }
      }
      loop(null, 0)
    }

    def parseDescriptor(): Descriptor = {
      if (currChar == '.') {
        readChar()
        if (currChar == ')') {
          readChar()
          val (typeDesc, index) = parseDisambiguator()
          if (currChar != '(') fail()
          else readChar()
          d.Method(parseName(), typeDesc, index)
        } else {
          d.Term(parseName())
        }
      } else if (currChar == '#') {
        readChar()
        d.Type(parseName())
      } else if (currChar == ')') {
        readChar()
        val name = parseName()
        if (currChar != '(') fail()
        else readChar()
        d.Parameter(name)
      } else if (currChar == ']') {
        readChar()
        val name = parseName()
        if (currChar != '[') fail()
        else readChar()
        d.TypeParameter(name)
      } else {
        fail()
      }
    }

    def entryPoint(): (Descriptor, String) = {
      readChar()
      val desc = parseDescriptor()
      (desc, s.substring(0, i + 1))
    }
  }

  private object DescriptorParser {
    def apply(symbol: String): (Descriptor, String) = {
      val parser = new DescriptorParser(symbol)
      parser.entryPoint()
    }
  }
}
