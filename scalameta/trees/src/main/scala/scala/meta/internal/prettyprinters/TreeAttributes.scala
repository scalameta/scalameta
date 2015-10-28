package scala.meta
package internal
package prettyprinters

import org.scalameta.show.{Show, enquote, SingleQuotes, DoubleQuotes, TripleQuotes}
import org.scalameta.adt._
import org.scalameta.invariants._
import org.scalameta.unreachable
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.ast._
import scala.{meta => api}
import scala.meta.tokens._
import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.meta.internal.semantic._
import scala.meta.internal.{equality => e}
import scala.compat.Platform.EOL
import scala.language.implicitConversions
import scala.meta.prettyprinters._

@implicitNotFound(msg = "don't know how to show[Attributes] for ${T}")
trait Attributes[T] extends Show[T]
object Attributes {
  def apply[T](f: T => Show.Result): Attributes[T] = new Attributes[T] { def apply(input: T) = f(input) }

  @root trait Recursion
  trait LowPriorityRecursion {
    @leaf implicit object Shallow extends Recursion
  }
  object Recursion extends LowPriorityRecursion {
    @leaf implicit object Deep extends Recursion
  }

  @root trait Force
  trait LowPriorityForce {
    @leaf implicit object Never extends Force
  }
  object Force extends LowPriorityForce {
    @leaf implicit object Always extends Force
  }

  // TODO: would be nice to generate this with a macro for all tree nodes that we have
  implicit def attributesTree[T <: api.Tree](implicit recursion: Recursion, force: Force): Attributes[T] = new Attributes[T] {
    private def deep = recursion == Recursion.Deep
    private def forceTypes = force == Force.Always

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
            case Environment.Zero => unreachable
          }
        }
        implicit def denotFootnote(denot: Denotation): Footnote = new Footnote {
          def entity = denot
          def tag = classOf[Denotation]
          def prettyprint() = {
            def prettyprintPrefix(pre: Prefix): String = {
              pre match {
                case Prefix.Zero => "{0}"
                case Prefix.Type(tpe) => s"{${footnotes.insert(Typing.Nonrecursive(tpe))}}"
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
                case Symbol.Global(owner, name, Signature.Self) => loop(owner) + ".this"
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
        val existingId = miniRepr.get(new CustomWrapper(x)).map(_._1)
        existingId.getOrElse((miniRepr.values.map(_._1) ++ List(0)).max + 1)
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
        case el @ Seq(Seq()) => "Seq(Seq())"
        case x: Seq[_] => "Seq(" + x.map(whole).mkString(", ") + ")"
        case x: None.type => "None"
        case x: Some[_] => "Some(" + whole(x.get) + ")"
        case x => x.toString
      }
      def contents(x: api.Tree): String = x match {
        case x @ Lit(s: String) => enquote(s, DoubleQuotes)
        case x @ Lit(_) => import scala.meta.dialects.Scala211; x.show[Syntax]
        case x => x.productIterator.map(whole).mkString(", ")
      }
      val syntax = x.productPrefix + "(" + contents(x) + ")"
      val attributes = {
        val envPart = x.internalEnv.map({
          case env @ Environment.Zero =>
            ""
        }).getOrElse("")

        val denotPart = x.internalDenot.map({
          case Denotation.Zero =>
            ""
          case denot @ Denotation.Single(prefix, symbol) =>
            s"[${footnotes.insert(denot)}]"
          case denot @ Denotation.Multi(prefix, symbols) =>
            val symbolFootnotes = symbols.map(symbol => footnotes.insert(Denotation.Single(prefix, symbol)))
            s"[${symbolFootnotes.mkString(", ")}]"
        }).getOrElse("")

        val typingPart = x.internalTyping.map({
          case Typing.Zero =>
            ""
          case Typing.Recursive =>
            val xkey = new CustomWrapper(x.require[Term])
            if (recursions.contains(xkey)) {
              s"{${recursions(xkey)}}"
            } else {
              if (x.isTypechecked) {
                val xsub = Type.Singleton(x.require[Term.Ref]).setTypechecked
                val typing = Typing.Nonrecursive(xsub)
                recursions(xkey) = footnotes.previewInsert(typing)
                s"{${footnotes.insert(typing)}}"
              } else {
                // NOTE: if x is not TYPECHECKED, then trying to insert it into a typing will crash.
                // It would be ideal if we could just print the type as is in typing footnotes,
                // but there's no easy way of doing that, so I'm going for something real simple.
                s"{}"
              }
            }
          case typing: Typing.Nonrecursive =>
            if (forceTypes || typing.isTpeLoaded) s"{${footnotes.insert(typing)}}"
            else s"{...}"
        }).getOrElse("")

        val expansionPart = x.internalExpansion.map({
          case Expansion.Zero =>
            ""
          case expansion @ Expansion.Identity =>
            s"<>"
          case expansion @ Expansion.Desugaring(term: Term) =>
            val isTpeLoaded = term.typing match {
              case typing: Typing.Nonrecursive => typing.isTpeLoaded
              case _ => true
            }
            if (forceTypes || isTpeLoaded) s"<${footnotes.insert(expansion)}>"
            else s"<...>"
        }).getOrElse("")

        val typecheckedPart = {
          if (!x.isTypechecked) "*"
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
        case that: CustomWrapper[_] => e.Semantic.equals(x, that.x)
        case _ => false
      }
      override def hashCode: Int = e.Semantic.hashCode(x)
      override def toString: String = s"CustomWrapper($x)"
    }
  }
}
