package scala.meta
package ui

import org.scalameta.show._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.ast._
import scala.{meta => api}
import scala.meta.internal.{ast => impl}
import scala.annotation.implicitNotFound
import scala.meta.syntactic.Token._

@implicitNotFound(msg = "don't know how to show[Raw] for ${T}")
trait Raw[T] extends Show[T]
object Raw {
  def apply[T](f: T => Show.Result): Raw[T] = new Raw[T] { def apply(input: T) = f(input) }

  // TODO: would be nice to generate this with a macro for all tree nodes that we have
  implicit def rawTree[T <: api.Tree]: Raw[T] = Raw(x => s(x.productPrefix, "(", x match {
    case x: impl.Ellipsis =>
      s(x.tree.show[Raw], ", classOf[", x.pt.getName, "]")
    case x: impl.Unquote =>
      s(x.tree.toString, ", classOf[", x.pt.getName, "]")
    case x: Lit.String =>
      s(enquote(x.value, DoubleQuotes))
    case x: Lit =>
      import scala.meta.dialects.Scala211 // show[Raw] is a debug printout, so it doesn't matter much which dialect we're using for that
      s(x.show[Code])
    case x =>
      def showRaw(x: Any): String = x match {
        case el: String => enquote(el, DoubleQuotes)
        case el: api.Tree => el.show[Raw]
        case el: Nil.type => "Nil"
        case el: List[_] => "List(" + el.map(showRaw).mkString(", ") + ")"
        case el: None.type => "None"
        case el: Some[_] => "Some(" + showRaw(el.get) + ")"
        case el => el.toString
      }
      r(x.productIterator.map(showRaw).toList, ", ")
  }, ")"))

  implicit def rawToken[T <: Token]: Raw[T] = Raw { x =>
    val prefix = (x: Token) match { case x: BOF => "BOF"; case x: EOF => "EOF"; case x: Dynamic => x.code; case x: Static => x.name }
    s(prefix, " (", x.start.toString, "..", x.end.toString, ")")
  }
}
