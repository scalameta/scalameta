package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._
import Show.{repeat => r, sequence => s}
import scala.annotation.tailrec
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
              case x: Lit.String =>
                s(DoubleQuotes.orTriple(x.value))
              case _: Lit.Unit | _: Lit.Null =>
                s()
              case x: Lit.Double =>
                s(asFloat(x.format, 'd'))
              case x: Lit.Float =>
                s(asFloat(x.format, 'f'))
              case x: Lit.Long =>
                s(x.value.toString + 'L')
              case x: Lit =>
                s(x.value.toString)
              case _ =>
                default
            }
          },
          ")"
        )
    }
  }

  private def asFloat(value: String, suffix: Char): String = {
    val sb = new java.lang.StringBuilder()
    val end = value.length - 1
    val noSuffixEnd = if (Character.toLowerCase(value(end)) == suffix) end - 1 else end
    def appendNoSuffix = sb.append(value, 0, noSuffixEnd + 1)
    @tailrec def removeZerosAndDot(idx: Int): Unit =
      if (idx < 0) appendNoSuffix
      else
        value(idx) match {
          case '0' => removeZerosAndDot(idx - 1)
          case '.' => if (idx == 0) sb.append('0') else sb.append(value, 0, idx)
          case _ if value.lastIndexOf('.', idx - 1) < 0 => appendNoSuffix
          case _ => sb.append(value, 0, idx + 1)
        }
    removeZerosAndDot(noSuffixEnd)
    sb.append(suffix)
    sb.toString
  }
}
