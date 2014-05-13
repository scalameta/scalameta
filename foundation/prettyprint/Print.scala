package org.scalareflect
package prettyprint

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Print[-T] { def apply(t: T): Print.Result }
object Print {
  def apply[T](f: T => Result): Print[T] =
    new Print[T] { def apply(input: T): Result = f(input) }

  sealed abstract class Result {
    override def toString = {
      val sb = new StringBuilder
      def loop(result: Result): Unit = result match {
        case Empty              => ()
        case Str(value)         => sb.append(value)
        case Sequence(xs @ _*)  => xs.foreach(loop)
        case Repeated(Nil, sep) => ()
        case Repeated(init :+ last, sep) =>
          init.foreach { x => loop(x); sb.append(sep) }
          loop(last)
      }
      loop(this)
      sb.toString
    }
  }
  final case object Empty extends Result
  final case class Str(value: String) extends Result
  final case class Sequence(xs: Result*) extends Result
  final case class Repeated(xs: Seq[Result], sep: String) extends Result

  def seq[T](xs: T*): Result = macro PrintMacros.seq[T]
  def rep[T](xs: Seq[T], sep: String)(implicit print: Print[T]): Repeated =
    Repeated(xs.map(print(_)), sep)

  implicit val printResult: Print[Result] = Print(identity)
}

class PrintMacros(val c: Context) {
  import c.universe._
  val PrintTpe = typeOf[Print[_]]
  val PrintObj = q"_root_.org.scalareflect.prettyprint.Print"

  def seq[T](xs: c.Tree*) = {
    val results = xs.map { x =>
      if (x.tpe <:< typeOf[Print.Result])
        x
      else {
        val printer = c.inferImplicitValue(appliedType(PrintTpe, x.tpe :: Nil), silent = true)
        if (printer.nonEmpty)
          q"$printer($x)"
        else
          c.abort(x.pos, s"don't know how to print value of type ${x.tpe}")
      }
    }
    if (xs.isEmpty) q"$PrintObj.Empty"
    else if (xs.length == 1) results.head
    else q"$PrintObj.Sequence(..$results)"
  }
}

