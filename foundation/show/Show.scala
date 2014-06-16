package org.scalameta.show

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.higherKinds
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.convert._

trait Show[T] { def apply(t: T): Show.Result }
object Show {
  sealed abstract class Result {
    override def toString = {
      val sb = new StringBuilder
      var indentation = 0
      def nl(res: Result) = {
        sb.append("\n")
        sb.append("  " * indentation)
        loop(res)
      }
      def loop(result: Result): Unit = result match {
        case None => ()
        case Str(value) => sb.append(value)
        case Sequence(xs @ _*) => xs.foreach(loop)
        case Repeat(Nil, sep) => ()
        case Repeat(init :+ last, sep) =>
          init.foreach { x =>
            loop(x)
            sb.append(sep)
          }
          loop(last)
        case Indent(res) =>
          indentation += 1
          nl(res)
          indentation -= 1
        case Newline(res) =>
          nl(res)
      }
      loop(this)
      sb.toString
    }
  }
  final case object None extends Result
  final case class Str(value: String) extends Result
  final case class Sequence(xs: Result*) extends Result
  final case class Repeat(xs: Seq[Result], sep: String) extends Result
  final case class Indent(res: Result) extends Result
  final case class Newline(res: Result) extends Result

  def apply[T](f: T => Result): Show[T] =
    new Show[T] { def apply(input: T): Result = f(input) }

  def sequence[T](xs: T*): Result = macro ShowMacros.seq[T]

  def indent[T](x: T)(implicit show: Show[T]): Indent = Indent(show(x))

  def repeat[T](xs: Seq[T], sep: String = "")(implicit show: Show[T]): Repeat =
    Repeat(xs.map(show(_)), sep)

  def newline[T](x: T)(implicit show: Show[T]): Newline = Newline(show(x))

  implicit def printResult[R <: Result]: Show[R] = apply(identity)
  implicit def printString[T <: String]: Show[T] = apply(Show.Str(_))

  implicit def show2convert[T](show: Show[T]): Convert[T, String] = Convert(show(_).toString)
}

private[show] class ShowMacros(val c: Context) {
  import c.universe._
  val ShowTpe = typeOf[Show[_]]
  val ShowObj = q"_root_.org.scalameta.show.Show"

  def seq[T](xs: c.Tree*) = {
    val results = xs.map { x =>
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
    if (xs.isEmpty) q"$ShowObj.None"
    else if (xs.length == 1) results.head
    else q"$ShowObj.Sequence(..$results)"
  }
}

