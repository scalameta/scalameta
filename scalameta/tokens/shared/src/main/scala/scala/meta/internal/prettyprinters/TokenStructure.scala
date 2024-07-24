package scala.meta
package internal
package prettyprinters

import scala.meta.internal.tokens.Chars
import scala.meta.prettyprinters.Show.{sequence => s}
import scala.meta.prettyprinters._
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

object TokenStructure {
  def apply[T <: Token]: Structure[T] = Structure[Token] { x =>
    implicit val dialect = x.dialect
    val text = x match {
      case t: Ident => t.value
      case t: Constant.Char => Chars.escape(t.value)
      case t: Constant.String => Chars.escape(t.value)
      case t: Constant.Symbol => Chars.escape(t.value.name)
      case t: NumericConstant[_] => t.value.toString
      case t: Interpolation.Id => Chars.escape(t.value)
      case t: Interpolation.Start => t.text
      case t: Interpolation.Part => Chars.escape(t.value)
      case t: Interpolation.End => t.text
      case t: Xml.Part => Chars.escape(t.value)
      case t: Comment => Chars.escape(t.value)
      case t: Ellipsis => t.rank.toString
      case t: Unquote => t.text
      case t: MultiHS => t.len.toString
      case t: MultiNL => t.tokens.length.toString
      case t: Invalid => t.error
      case _ => null
    }
    val label = {
      val name = x.getClass.getName
      val idx = name.lastIndexOf(".Token$")
      if (idx < 0) x.name else name.substring(idx + 7).replace("$", ".")
    }
    val syntax = if (text eq null) s() else s("(", text, ")")
    s(label, syntax, " [", x.start.toString, "..", x.end.toString, ")")
  }
}
