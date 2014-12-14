package scala.meta.syntactic
package show

import org.scalameta.show._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.ast._
import scala.{meta => api}

trait Raw[T] extends Show[T]
object Raw {
  def apply[T](f: T => Show.Result): Raw[T] = new Raw[T] { def apply(input: T) = f(input) }

  // TODO: would be nice to generate this with a macro for all tree nodes that we have
  implicit def rawTree[T <: api.Tree]: Raw[T] = Raw(x => s(x.productPrefix, "(", x match {
    case x: Lit.String => s(enquote(x.value, DoubleQuotes))
    case x: Lit => s(x.show[Code])
    case x =>
      def showRaw(x: Any): String = x match {
        case el: String => enquote(el, DoubleQuotes)
        case el: api.Tree => el.show[Raw]
        case el: Nil.type => "Nil"
        case el: List[_] => "List(" + el.map(showRaw).mkString(", ") + ")"
        case el => el.toString
      }
      r(x.productIterator.map(showRaw).toList, ", ")
  }, ")"))
}
