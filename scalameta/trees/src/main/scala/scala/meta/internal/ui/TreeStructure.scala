package scala.meta
package internal
package ui

import org.scalameta.show._
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.meta.internal.{ast => impl}
import scala.meta.ui.{Syntax, Structure}

object TreeStructure {
  def apply[T <: Tree]: Structure[T] = {
    Structure(x => s(x.productPrefix, "(", {
      def default = {
        def showRaw(x: Any): String = x match {
          case el: String => enquote(el, DoubleQuotes)
          case el: Tree => el.show[Structure]
          case el: Nil.type => "Nil"
          case el @ Seq(Seq()) => "Seq(Seq())"
          case el: Seq[_] => "Seq(" + el.map(showRaw).mkString(", ") + ")"
          case el: None.type => "None"
          case el: Some[_] => "Some(" + showRaw(el.get) + ")"
          case el => el.toString
        }
        r(x.productIterator.map(showRaw).toList, ", ")
      }
      x match {
        case x: impl.Quasi => default
        case x: impl.Lit.String => s(enquote(x.value, DoubleQuotes))
        case x: Lit => import scala.meta.dialects.Scala211; s(x.show[Syntax])
        case x => default
      }
    }, ")"))
  }
}