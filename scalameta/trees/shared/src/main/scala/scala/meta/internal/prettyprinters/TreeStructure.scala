package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{sequence => s, repeat => r}
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.internal.trees.Quasi

object TreeStructure {
  def apply[T <: Tree]: Structure[T] = {
    Structure {
      case _: Name.Anonymous =>
        s(s"""Name.Anonymous()""")
      case _: Name.This =>
        s(s"""Name.This()""")
      case _: Name.Placeholder =>
        s(s"""Name.Placeholder()""")
      case Name.Indeterminate(value) =>
        s("Name(", DoubleQuotes(value), ")")
      case x =>
        s(
          x.productPrefix,
          "(", {
            def default = {
              def anyStructure(x: Any): String = x match {
                case el: String => DoubleQuotes(el)
                case el: Tree => el.structure
                case None => "None"
                case Some(el) => "Some(" + anyStructure(el) + ")"
                case el: List[_] => iterableStructure(el, "List")
                case el: Seq[_] => iterableStructure(el, "Seq")
                case el => el.toString
              }
              def iterableStructure(xs: Iterable[_], cls: String): String =
                if (xs.isEmpty) "Nil" else xs.map(anyStructure).mkString(s"$cls(", ", ", ")")

              r(x.productIterator.map(anyStructure).toList, ", ")
            }
            x match {
              case _: Quasi =>
                default
              case Lit(value: String) =>
                s(DoubleQuotes(value))
              case _: Lit.Unit | _: Lit.Null =>
                s()
              case x: Lit.Double =>
                s(x.tokens.toString)
              case x: Lit.Float =>
                s(x.tokens.toString)
              case x: Lit =>
                s(x.tokens.filter(isRelevantToken).map(showToken).mkString)
              case _ =>
                default
            }
          },
          ")"
        )
    }
  }

  private def isRelevantToken(tok: Token) = tok match {
    case _: Literal => true
    case Ident("-") => true
    case _ => false
  }

  private def showToken(tok: Token) = tok match {
    case Constant.Long(v) => Show.Str(v.toString + "L")
    case _ => tok.syntax
  }
}
