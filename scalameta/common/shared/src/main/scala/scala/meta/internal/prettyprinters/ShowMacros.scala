package scala.meta
package internal
package prettyprinters

import scala.meta.prettyprinters._

import scala.reflect.macros.blackbox.Context

class ShowMacros(val c: Context) {
  import c.universe._
  val ShowClass = c.mirror.staticClass("scala.meta.prettyprinters.Show")
  val ShowObj = q"_root_.scala.meta.prettyprinters.Show"

  private def mkResults(xs: Seq[c.Tree]): Seq[c.Tree] = xs.map { x =>
    if (x.tpe <:< typeOf[Show.Result]) x
    else {
      val printer = c.inferImplicitValue(appliedType(ShowClass, x.tpe :: Nil), silent = true)
      if (printer.nonEmpty) q"$printer($x)"
      else c.abort(x.pos, s"don't know how to print value of type ${x.tpe}")
    }
  }

  def sequence(xs: c.Tree*) = q"$ShowObj.mkseq(..${mkResults(xs)})"
  def meta(data: c.Tree, xs: c.Tree*) = q"$ShowObj.meta($data, ${sequence(xs: _*)})"
}
