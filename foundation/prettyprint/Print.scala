package org.scalareflect
package prettyprint

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Print[T] { def apply(t: T): String }
object Print {
  def apply[T](f: T => String): Print[T] =
    new Print[T] { def apply(input: T): String = f(input) }
  def seq[T](xs: T*): String = macro PrintMacros.seq[T]
  def rep[T](xs: Seq[T], sep: String): String = macro PrintMacros.rep[T]
}

class PrintMacros(val c: Context) {
  import c.universe._
  val Print = typeOf[Print[_]]

  def seq[T](xs: c.Tree*) = {
    val strs = xs.map { x =>
      if (x.tpe <:< typeOf[String])
        x
      else {
        val printer = c.inferImplicitValue(appliedType(Print, x.tpe :: Nil), silent = true)
        if (printer.nonEmpty)
          q"$printer($x)"
        else
          c.abort(x.pos, s"don't know how to print value of type ${x.tpe}")
      }
    }
    if (xs.isEmpty) q""" "" """
    else if (xs.length == 1) strs.head
    else {
      val appends = strs.map { s => q"sb.append($s)" }
      q"""
        val sb = new StringBuilder
        ..$appends
        sb.toString
      """
    }
  }

  def rep[T: WeakTypeTag](xs: c.Tree, sep: c.Tree) = {
    val T = weakTypeOf[T]
    val printer = c.inferImplicitValue(appliedType(Print, T :: Nil), silent = true)
    if (printer.nonEmpty)
      q""" $xs.map($printer(_)).mkString("", $sep, "") """
    else
      c.abort(c.macroApplication.pos, s"don't know how to print $T")
  }
}

