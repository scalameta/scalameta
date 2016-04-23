package scala.meta
package internal
package prettyprinters

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import scala.meta.prettyprinters._

class ShowMacros(val c: Context) {
  import c.universe._
  val ShowTpe = typeOf[Show[_]]
  val ShowObj = q"_root_.scala.meta.prettyprinters.Show"

  private def mkResults(xs: List[c.Tree]): List[c.Tree] = {
    xs.map { x =>
      if (x.tpe <:< typeOf[Show.Result])
        x
      else {
        val printer = c.inferImplicitValue(appliedType(ShowTpe, x.tpe :: Nil), silent = true)
        if (printer.nonEmpty)
          q"$printer($x)"
        else
          c.abort(x.pos, s"don't know how to print value of type ${x.tpe}")
      }
    }
  }

  def seq(xs: c.Tree*) = {
    val results = mkResults(xs.toList)
    if (xs.isEmpty) q"$ShowObj.None"
    else if (xs.length == 1) results.head
    else q"$ShowObj.Sequence(..$results)"
  }

  def meta(data: c.Tree, xs: c.Tree*) = {
    val results = mkResults(xs.toList)
    if (xs.isEmpty) q"$ShowObj.None"
    else if (xs.length == 1) q"$ShowObj.Meta($data, ${results.head})"
    else q"$ShowObj.Meta($data, $ShowObj.Sequence(..$results))"
  }
}
