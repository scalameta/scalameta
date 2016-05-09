package scala.meta
package prettyprinters

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.language.higherKinds
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.common._
import scala.compat.Platform.EOL

trait Show[T] {
  def apply(t: T): Show.Result
}

object Show {
  private[meta] sealed abstract class Result {
    override def toString = {
      val sb = new StringBuilder
      var indentation = 0
      def nl(res: Result) = {
        sb.append(EOL)
        sb.append("  " * indentation)
        loop(res)
      }
      def loop(result: Result): Unit = result match {
        case None => // do nothing
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
        case Wrap(prefix, res, suffix, cond) =>
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
  private[meta] final case object None extends Result
  private[meta] final case class Str(value: String) extends Result
  private[meta] final case class Sequence(xs: Result*) extends Result
  private[meta] final case class Repeat(xs: Seq[Result], sep: String) extends Result
  private[meta] final case class Indent(res: Result) extends Result
  private[meta] final case class Newline(res: Result) extends Result
  private[meta] final case class Meta(data: Any, res: Result) extends Result
  private[meta] final case class Wrap(prefix: String, res: Result, suffix: String, cond: String => Boolean) extends Result
  private[meta] final case class Function(fn: StringBuilder => Result) extends Result

  def apply[T](f: T => Result): Show[T] =
    new Show[T] { def apply(input: T): Result = f(input) }

  def sequence[T](xs: T*): Result = macro scala.meta.internal.prettyprinters.ShowMacros.seq

  def indent[T](x: T)(implicit show: Show[T]): Result = Indent(show(x))

  def repeat[T](xs: Seq[T], sep: String = "")(implicit show: Show[T]): Result =
    Repeat(xs.map(show(_)), sep)

  def newline[T](x: T)(implicit show: Show[T]): Result = Newline(show(x))

  def meta[T](data: Any, xs: T*): Result = macro scala.meta.internal.prettyprinters.ShowMacros.meta

  def wrap[T](x: T, suffix: String)(implicit show: Show[T]): Result = Wrap("", show(x), suffix, _.nonEmpty)
  def wrap[T](x: T, suffix: String, cond: Boolean)(implicit show: Show[T]): Result = Wrap("", show(x), suffix, _ => cond)
  def wrap[T](prefix: String, x: T)(implicit show: Show[T]): Result = Wrap(prefix, show(x), "", _.nonEmpty)
  def wrap[T](prefix: String, x: T, cond: Boolean)(implicit show: Show[T]): Result = Wrap(prefix, show(x), "", _ => cond)
  def wrap[T](prefix: String, x: T, suffix: String)(implicit show: Show[T]): Result = Wrap(prefix, show(x), suffix, _.nonEmpty)
  def wrap[T](prefix: String, x: T, suffix: String, cond: Boolean)(implicit show: Show[T]): Result = Wrap(prefix, show(x), suffix, _ => cond)

  def function(fn: StringBuilder => Result): Result = Function(fn)

  implicit def printResult[R <: Result]: Show[R] = apply(identity)
  implicit def printString[T <: String]: Show[T] = apply(Show.Str(_))
}
