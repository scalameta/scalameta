package scala.meta
package internal
package ui

import org.scalameta.adt._
import org.scalameta.show._
import org.scalameta.invariants._
import org.scalameta.unreachable
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.ast._
import scala.{meta => api}
import scala.meta.syntactic.Token
import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.meta.internal.semantic._
import scala.compat.Platform.EOL
import scala.language.implicitConversions
import scala.meta.internal.flags._

@implicitNotFound(msg = "don't know how to show[Attributes] for ${T}")
trait Attributes[T] extends Show[T]
object Attributes {
  def apply[T](f: T => Show.Result): Attributes[T] = new Attributes[T] { def apply(input: T) = f(input) }

  @root trait Recursion
  object Recursion {
    @leaf object Shallow extends Recursion
    @leaf implicit object Deep extends Recursion
  }

  @root trait Detalization
  object Detalization {
    @leaf object ExcludeFlags extends Detalization
    @leaf implicit object IncludeFlags extends Detalization
  }

  // TODO: would be nice to generate this with a macro for all tree nodes that we have
  implicit def attributesTree[T <: api.Tree](implicit recursion: Recursion, detalization: Detalization): Attributes[T] = new Attributes[T] {
    private def includeFlags = detalization == Detalization.IncludeFlags
    private def deep = recursion == Recursion.Deep

    def apply(x: T): Show.Result = {
      val bodyPart = body(x) // NOTE: body may side-effect on footnotes
      val footnotePart = footnotes.toString
      s(bodyPart, if (footnotePart.nonEmpty) EOL + footnotePart else footnotePart)
    }

    object footnotes {
      trait Footnote {
        def entity: Any
        def tag: Class[_]
        def prettyprint(): String
        final override def toString: String = s"Footnote($entity)"
        final override def equals(that: Any): Boolean = entity.equals(that)
        final override def hashCode: Int = entity.hashCode()
      }

      object Footnote {
        implicit def envFootnote(env: Environment): Footnote = new Footnote {
          def entity = env
          def tag = classOf[Environment]
          def prettyprint() = env match {
            case scala.meta.semantic.Environment.Zero => unreachable
          }
        }
        implicit def denotFootnote(denot: Denotation): Footnote = new Footnote {
          def entity = denot
          def tag = classOf[Denotation]
          def prettyprint() = {
            def prettyprintPrefix(pre: Prefix): String = {
              pre match {
                case Prefix.Zero => "0"
                case Prefix.Type(tpe) => if (deep) body(tpe) else tpe.show[Structure]
              }
            }
            def prettyprintSymbol(sym: Symbol): String = {
              def loop(sym: Symbol): String = sym match {
                case Symbol.Zero => "0"
                case Symbol.RootPackage => "_root_"
                case Symbol.EmptyPackage => "_empty_"
                case Symbol.Global(owner, name, Signature.Type) => loop(owner) + "#" + name
                case Symbol.Global(owner, name, Signature.Term) => loop(owner) + "." + name
                case Symbol.Global(owner, name, Signature.Method(jvmSignature)) => loop(owner) + "." + name + jvmSignature
                case Symbol.Global(owner, name, Signature.TypeParameter) => loop(owner) + "[" + name + "]"
                case Symbol.Global(owner, name, Signature.TermParameter) => loop(owner) + "(" + name + ")"
                case Symbol.Local(id) => "local#" + id
              }
              var result = loop(sym)
              if (result != "_root_") result = result.stripPrefix("_root_.")
              result
            }
            val symbol = denot.require[Denotation.Single].symbol
            prettyprintPrefix(denot.prefix) + "::" + prettyprintSymbol(symbol)
          }
        }
        implicit def typingFootnote(typing: Typing): Footnote = new Footnote {
          def entity = typing
          def tag = classOf[Typing]
          def prettyprint() = typing match {
            case Typing.Zero => unreachable
            case Typing.Recursive => unreachable
            case Typing.Nonrecursive(tpe) => if (deep) body(tpe) else tpe.show[Structure]
          }
        }
        implicit def statusExpansion(expansion: Expansion): Footnote = new Footnote {
          def entity = expansion
          def tag = classOf[Expansion]
          def prettyprint() = expansion match {
            case Expansion.Zero => unreachable
            case Expansion.Identity => unreachable
            case Expansion.Desugaring(term) => if (deep) body(term) else term.show[Structure]
          }
        }
      }

      private var size = 0
      private val repr = mutable.Map[Class[_], CustomMap[Any, (Int, Footnote)]]()
      def previewInsert[T <% Footnote](x: T): Int = {
        val footnote = implicitly[T => Footnote].apply(x)
        val miniRepr = repr.getOrElseUpdate(footnote.tag, CustomMap[Any, (Int, Footnote)]())
        val maxId = (miniRepr.values.map(_._1) ++ List(0)).max
        maxId + 1
      }
      def insert[T <% Footnote](x: T): Int = {
        val id = previewInsert(x)
        val footnote = implicitly[T => Footnote].apply(x)
        val miniRepr = repr.getOrElseUpdate(footnote.tag, CustomMap[Any, (Int, Footnote)]())
        if (!miniRepr.contains(new CustomWrapper(x))) size += 1
        miniRepr.getOrElseUpdate(new CustomWrapper(x), (id, footnote))._1
      }

      override def toString: String = {
        if (deep) {
          var prevSize = 0 // NOTE: prettyprint may side-effect on footnotes
          do {
            prevSize = size
            val stableMinis = repr.toList.sortBy(_._1.getName).map(_._2)
            val stableFootnotes = stableMinis.flatMap(_.toList.sortBy(_._2._1).map(_._2._2))
            stableFootnotes.foreach(_.prettyprint())
          } while (size != prevSize)
        }
        def byType(tag: Class[_], bracket1: String, bracket2: String): List[String] = {
          val miniRepr = repr.getOrElseUpdate(tag, CustomMap[Any, (Int, Footnote)]())
          val sortedMiniCache = miniRepr.toList.sortBy{ case (_, (id, footnote)) => id }
          sortedMiniCache.map{ case (_, (id, footnote)) => s"$bracket1$id$bracket2 ${footnote.prettyprint()}" }
        }
        (
          byType(classOf[Environment], "[", "]") ++
          byType(classOf[Denotation], "[", "]") ++
          byType(classOf[Typing], "{", "}") ++
          byType(classOf[Expansion], "<", ">")
        ).mkString(EOL)
      }
    }

    val recursions = CustomMap[Term, Int]()
    def body(x: api.Tree): String = {
      def whole(x: Any): String = x match {
        case x: String => enquote(x, DoubleQuotes)
        case x: api.Tree => body(x)
        case x: Nil.type => "Nil"
        case el @ List(List()) => "List(List())"
        case x: List[_] => "List(" + x.map(whole).mkString(", ") + ")"
        case x: None.type => "None"
        case x: Some[_] => "Some(" + whole(x.get) + ")"
        case x => x.toString
      }
      def contents(x: api.Tree): String = x match {
        case x: Lit.String => enquote(x.value, DoubleQuotes)
        case x: Lit => import scala.meta.dialects.Scala211; x.show[Syntax]
        case x => x.productIterator.map(whole).mkString(", ")
      }
      val syntax = x.productPrefix + "(" + contents(x) + ")"
      val attributes = {
        val envPart = x.maybeEnv.map({
          case env @ scala.meta.semantic.Environment.Zero =>
            ""
        }).getOrElse("")

        val denotPart = x.maybeDenot.map({
          case Denotation.Zero =>
            ""
          case denot @ Denotation.Single(prefix, symbol) =>
            s"[${footnotes.insert(denot)}]"
          case denot @ Denotation.Multi(prefix, symbols) =>
            val symbolFootnotes = symbols.map(symbol => footnotes.insert(Denotation.Single(prefix, symbol)))
            s"[${symbolFootnotes.mkString(", ")}]"
        }).getOrElse("")

        val typingPart = x.maybeTyping.map({
          case Typing.Zero =>
            ""
          case Typing.Recursive =>
            val xkey = new CustomWrapper(x.require[Term])
            if (recursions.contains(xkey)) {
              s"{${recursions(xkey)}}"
            } else {
              val typing = Typing.Nonrecursive(Type.Singleton(x.require[Term.Ref]))
              recursions(xkey) = footnotes.previewInsert(typing)
              s"{${footnotes.insert(typing)}}"
            }
          case typing @ Typing.Nonrecursive(tpe) =>
            s"{${footnotes.insert(typing)}}"
        }).getOrElse("")

        val expansionPart = x.maybeExpansion.map({
          case Expansion.Zero => ""
          case expansion @ Expansion.Identity => s"<=>"
          case expansion @ Expansion.Desugaring(term) => s"<${footnotes.insert(expansion)}>"
        }).getOrElse("")

        val typecheckedPart = {
          if (x.isTypechecked && includeFlags) "*"
          else ""
        }

        envPart + denotPart + typingPart + expansionPart + typecheckedPart
      }
      syntax + attributes
    }

    // NOTE: This is a map that does semantic comparisons of its keys.
    // Since we can't plug custom equality and hashcode implementations into the standard map,
    // we have to use custom keys instead.
    private type CustomMap[T, U] = mutable.Map[CustomWrapper[T], U]
    private def CustomMap[T, U]() = mutable.Map[CustomWrapper[T], U]()

    private class CustomWrapper[+T](val x: T) {
      override def equals(that: Any): Boolean = that match {
        case that: CustomWrapper[_] => customEquals(x, that.x)
        case _ => false
      }

      private def customEquals(x: Any, y: Any): Boolean = (x, y) match {
        case (x: Some[_], y: Some[_]) =>
          customEquals(x.get, y.get)
        case (x: None.type, y: None.type) =>
          true
        case (xs: Seq[_], ys: Seq[_]) =>
          xs.length == ys.length && xs.zip(ys).forall{ case (x, y) => customEquals(x, y) }
        case (x: Environment, y: Environment) =>
          x == y
        case (x: Prefix, y: Prefix) =>
          (x, y) match {
            case (Prefix.Type(x), Prefix.Type(y)) => customEquals(x, y)
            case _ => x == y
          }
        case (x: Denotation, y: Denotation) =>
          customEquals(x.prefix, y.prefix) && customEquals(x.symbols, y.symbols)
        case (x: Typing, y: Typing) =>
          (x, y) match {
            case (Typing.Nonrecursive(x), Typing.Nonrecursive(y)) => customEquals(x, y)
            case _ => x == y
          }
        case (x: Expansion, y: Expansion) =>
          (x, y) match {
            case (Expansion.Desugaring(x), Expansion.Desugaring(y)) => customEquals(x, y)
            case _ => x == y
          }
        case (x: Tree, y: Tree) =>
          def syntaxPart = x.productPrefix == y.productPrefix && customEquals(x.productIterator.toList, y.productIterator.toList)
          def envPart = customEquals(x.maybeEnv, y.maybeEnv)
          def denotPart = customEquals(x.maybeDenot, y.maybeDenot)
          def typingPart = customEquals(x.maybeTyping, y.maybeTyping)
          def expansionPart = customEquals(x.maybeExpansion, y.maybeExpansion)
          def typecheckedPart = includeFlags ==> customEquals(x.isTypechecked, y.isTypechecked)
          syntaxPart && envPart && denotPart && typingPart && expansionPart && typecheckedPart
        case _ =>
          x == y
      }

      override def hashCode: Int = customHashcode(x)

      def customHashcode(x: Any): Int = x match {
        case x: Option[_] =>
          x.map(customHashcode).getOrElse(0)
        case xs: Seq[_] =>
          xs.foldLeft(0)((acc, curr) => acc * 37 + customHashcode(curr))
        case x: Environment =>
          x.hashCode
        case x: Prefix =>
          x match {
            case Prefix.Zero => 0
            case Prefix.Type(tpe) => customHashcode(tpe)
          }
        case x: Denotation =>
          x match {
            case Denotation.Zero => 0
            case Denotation.Single(prefix, symbol) => customHashcode(prefix) * 37 + customHashcode(symbol)
            case Denotation.Multi(prefix, symbols) => customHashcode(prefix) * 37 + customHashcode(symbols)
          }
        case x: Typing =>
          x match {
            case Typing.Zero => 0
            case Typing.Recursive => 1
            case Typing.Nonrecursive(tpe) => customHashcode(tpe)
          }
        case x: Expansion =>
          x match {
            case Expansion.Zero => 0
            case Expansion.Identity => 1
            case Expansion.Desugaring(term) => customHashcode(term)
          }
        case x: Tree =>
          def syntaxPart = customHashcode(x.productPrefix) * customHashcode(x.productIterator.toList)
          def envPart = customHashcode(x.maybeEnv)
          def denotPart = customHashcode(x.maybeDenot)
          def typingPart = customHashcode(x.maybeTyping)
          def expansionPart = customHashcode(x.maybeExpansion)
          def typecheckedPart = x.isTypechecked.hashCode * (if (includeFlags) 1 else 0)
          customHashcode(List(syntaxPart, envPart, denotPart, typingPart, expansionPart, typecheckedPart))
        case _ =>
          x.hashCode
      }
    }
  }
}
