package scala.meta
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

@implicitNotFound(msg = "don't know how to show[Semantics] for ${T}")
trait Semantics[T] extends Show[T]
object Semantics {
  def apply[T](f: T => Show.Result): Semantics[T] = new Semantics[T] { def apply(input: T) = f(input) }

  @root trait Style
  object Style {
    @leaf object Shallow extends Style
    @leaf implicit object Deep extends Style
  }

  // TODO: would be nice to generate this with a macro for all tree nodes that we have
  implicit def semanticsTree[T <: api.Tree](implicit style: Style): Semantics[T] = new Semantics[T] {
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
        implicit def denotFootnote(denot: Denotation): Footnote = new Footnote {
          def entity = denot
          def tag = classOf[Denotation]
          def prettyprint() = {
            def prettyprintPrefix(pre: Prefix): String = {
              pre match {
                case Prefix.Zero => "0"
                case Prefix.Type(tpe) => if (style == Style.Deep) body(tpe) else tpe.show[Structure]
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
            case Typing.Specified(tpe) => if (style == Style.Deep) body(tpe) else tpe.show[Structure]
          }
        }
        implicit def statusExpansion(expansion: Expansion): Footnote = new Footnote {
          def entity = expansion
          def tag = classOf[Expansion]
          def prettyprint() = expansion match {
            case Expansion.Zero => unreachable
            case Expansion.Identity => unreachable
            case Expansion.Desugaring(term) => if (style == Style.Deep) body(term) else term.show[Structure]
          }
        }
      }
      private var size = 0
      private val repr = mutable.Map[Class[_], mutable.Map[Any, (Int, Footnote)]]()
      def insert[T <% Footnote](x: T): Int = {
        val footnote = implicitly[T => Footnote].apply(x)
        val miniRepr = repr.getOrElseUpdate(footnote.tag, mutable.Map[Any, (Int, Footnote)]())
        if (!miniRepr.contains(x)) size += 1
        val maxId = (miniRepr.values.map(_._1) ++ List(0)).max
        miniRepr.getOrElseUpdate(x, (maxId + 1, footnote))._1
      }
      override def toString: String = {
        if (style == Style.Deep) {
          var prevSize = 0 // NOTE: prettyprint may side-effect on footnotes
          do {
            prevSize = size
            val stableMinis = repr.toList.sortBy(_._1.getName).map(_._2)
            val stableFootnotes = stableMinis.flatMap(_.toList.sortBy(_._2._1).map(_._2._2))
            stableFootnotes.foreach(_.prettyprint())
          } while (size != prevSize)
        }
        def byType(tag: Class[_], bracket1: String, bracket2: String): List[String] = {
          val miniRepr = repr.getOrElseUpdate(tag, mutable.Map[Any, (Int, Footnote)]())
          val sortedMiniCache = miniRepr.toList.sortBy{ case (_, (id, footnote)) => id }
          sortedMiniCache.map{ case (_, (id, footnote)) => s"$bracket1$id$bracket2 ${footnote.prettyprint()}" }
        }
        (byType(classOf[Denotation], "[", "]") ++ byType(classOf[Typing], "{", "}") ++ byType(classOf[Expansion], "<", ">")).mkString(EOL)
      }
    }
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
      val semantics = {
        val denotPart = x match {
          case x: Name =>
            x.denot match {
              case Denotation.Zero =>
                ""
              case denot @ Denotation.Single(prefix, symbol) =>
                s"[${footnotes.insert(denot)}]"
              case denot @ Denotation.Multi(prefix, symbols) =>
                s"[${symbols.map(symbol => footnotes.insert(Denotation.Single(prefix, symbol))).mkString(", ")}]"
            }
          case _ =>
            ""
        }
        val statusPart = x match {
          case x: Term =>
            x.typing match {
              case Typing.Zero => ""
              case typing @ Typing.Specified(tpe) => s"{${footnotes.insert(typing)}}"
            }
          case _ =>
            ""
        }
        val expansionPart = x match {
          case x: Term =>
            x.expansion match {
              case Expansion.Zero => ""
              case expansion @ Expansion.Identity => s"<=>"
              case expansion @ Expansion.Desugaring(term) => s"<${footnotes.insert(expansion)}>"
            }
          case _ =>
            ""
        }
        denotPart + statusPart + expansionPart
      }
      syntax + semantics
    }
    def apply(x: T): Show.Result = {
      val bodyPart = body(x) // NOTE: body may side-effect on footnotes
      val footnotePart = footnotes.toString
      s(bodyPart, if (footnotePart.nonEmpty) EOL + footnotePart else footnotePart)
    }
  }
}
