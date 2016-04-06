package scala.meta
package prettyprinters

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.higherKinds
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.convert._
import scala.compat.Platform.EOL

trait Show[T] { def apply(t: T): Show.Result }
object Show {
  sealed abstract class Result {
    override def toString = {
      val sb = new StringBuilder
      var indentation = 0
      def nl(res: Result) = {
        sb.append(EOL)
        sb.append("  " * indentation)
        loop(res)
      }
      def loop(result: Result): Unit = result match {
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
        case Meta(_, res) =>
          loop(res)
        case Adorn(prefix, res, suffix, cond) =>
          // TODO: think of an effective implementation for this
          val s_res = res.toString
          if (cond(s_res)) sb.append(prefix)
          sb.append(s_res)
          if (cond(s_res)) sb.append(suffix)
        case Function(fn) =>
          sb.append(fn(sb))
      }
      loop(this)
      sb.toString
    }
  }
  final case class Str(value: String) extends Result
  final case class Sequence(xs: Result*) extends Result
  final case class Repeat(xs: Seq[Result], sep: String) extends Result
  final case class Indent(res: Result) extends Result
  final case class Newline(res: Result) extends Result
  final case class Meta(data: Any, res: Result) extends Result
  final case class Adorn(prefix: String, res: Result, suffix: String, cond: String => Boolean) extends Result
  final case class Function(fn: StringBuilder => Result) extends Result

  def apply[T](f: T => Result): Show[T] =
    new Show[T] { def apply(input: T): Result = f(input) }

  def sequence[T](xs: T*): Result = macro scala.meta.internal.prettyprinters.ShowMacros.seq

  def indent[T](x: T)(implicit show: Show[T]): Indent = Indent(show(x))

  def repeat[T](xs: Seq[T], sep: String = "")(implicit show: Show[T]): Repeat =
    Repeat(xs.map(show(_)), sep)

  def newline[T](x: T)(implicit show: Show[T]): Newline = Newline(show(x))

  def meta[T](data: Any, xs: T*): Meta = macro scala.meta.internal.prettyprinters.ShowMacros.meta

  def adorn[T](x: T, suffix: String)(implicit show: Show[T]): Adorn = Adorn("", show(x), suffix, _.nonEmpty)
  def adorn[T](x: T, suffix: String, cond: Boolean)(implicit show: Show[T]): Adorn = Adorn("", show(x), suffix, _ => cond)
  def adorn[T](prefix: String, x: T)(implicit show: Show[T]): Adorn = Adorn(prefix, show(x), "", _.nonEmpty)
  def adorn[T](prefix: String, x: T, cond: Boolean)(implicit show: Show[T]): Adorn = Adorn(prefix, show(x), "", _ => cond)
  def adorn[T](prefix: String, x: T, suffix: String)(implicit show: Show[T]): Adorn = Adorn(prefix, show(x), suffix, _.nonEmpty)
  def adorn[T](prefix: String, x: T, suffix: String, cond: Boolean)(implicit show: Show[T]): Adorn = Adorn(prefix, show(x), suffix, _ => cond)

  def function(fn: StringBuilder => Result): Function = Function(fn)

  implicit def printResult[R <: Result]: Show[R] = apply(identity)
  implicit def printString[T <: String]: Show[T] = apply(Show.Str(_))

  implicit def show2convert[T](show: Show[T]): Convert[T, String] = Convert(show(_).toString)
}
