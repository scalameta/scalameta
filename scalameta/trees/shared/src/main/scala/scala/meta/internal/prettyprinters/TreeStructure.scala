package scala.meta
package internal
package prettyprinters

import scala.meta.internal.trees.Quasi
import scala.meta.prettyprinters._

import scala.annotation.tailrec

object TreeStructure {
  import Show.{indent => i}
  import Show.{newline => n}
  import Show.{repeat => r}
  import Show.{sequence => s}

  def apply[T <: Tree]: Structure[T] = Structure(anyTree)

  private def anyStructure(x: Any): Show.Result = x match {
    case el: String => s(DoubleQuotes(el))
    case el: Tree => anyTree(el)
    case None => s("None")
    case Some(el) => s("Some(", i(anyStructure(el)), n(")"))
    case el: List[_] => iterableStructure(el, "List")
    case el: Seq[_] => iterableStructure(el, "Seq")
    case el => s(el.toString)
  }

  private def iterableStructure(xs: Seq[_], cls: String): Show.Result =
    if (xs.isEmpty) s("Nil") else s(s"$cls(", r(xs.map(x => i(anyStructure(x))), ","), n(")"))

  private def anyTree(tree: Tree) = tree match {
    case _: Name.Anonymous => s(s"""Name.Anonymous()""")
    case _: Name.This => s(s"""Name.This()""")
    case _: Name.Placeholder => s(s"""Name.Placeholder()""")
    case Name.Indeterminate(value) => s("Name(", DoubleQuotes(value), ")")
    case x =>
      val args: List[Show.Result] = {
        def default = x.productIterator.map(anyStructure).toList
        x match {
          case _: Quasi => default
          case x: Lit.String => DoubleQuotes.orTriple(x.value) :: Nil
          case x: Lit.Char => s"'${x.value}'" :: Nil
          case x: Lit.Symbol => s"""Symbol("${x.value.name}")""" :: Nil
          case _: Lit.Unit | _: Lit.Null => Nil
          case x: Lit.Double => asFloat(x.format, 'd') :: Nil
          case x: Lit.Float => asFloat(x.format, 'f') :: Nil
          case x: Lit.Long => x.value.toString + 'L' :: Nil
          case x: Lit => x.value.toString :: Nil
          case x: Term.ArgClause if x.mod.isEmpty => anyStructure(x.values) :: Nil
          case x: Term.ParamClause if x.mod.isEmpty => anyStructure(x.values) :: Nil
          case _ => default
        }
      }
      if (args.lengthCompare(1) > 0) s(x.productPrefix, "(", r(args.map(i), ","), n(")"))
      else s(x.productPrefix, "(", r(args), ")")
  }

  private def asFloat(value: String, suffix: Char): String = {
    val sb = new java.lang.StringBuilder()
    val end = value.length - 1
    val noSuffixEnd = if (Character.toLowerCase(value(end)) == suffix) end - 1 else end
    def appendNoSuffix = sb.append(value, 0, noSuffixEnd + 1)
    @tailrec
    def removeZerosAndDot(idx: Int): Unit =
      if (idx < 0) appendNoSuffix
      else value(idx) match {
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
