package scala.meta
package prettyprinters

import scala.language.experimental.macros
import scala.language.implicitConversions
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
        case Repeat(head +: tail, sep) =>
          val sbLenInit = sb.length
          var sbLen = sbLenInit
          loop(head)
          tail.foreach { x =>
            if (sbLen != sb.length) {
              sb.append(sep)
              sbLen = sb.length
            }
            loop(x)
          }
          val sbLenLast = sb.length
          if (sbLenInit == sbLenLast) sb.setLength(sbLenInit)
          else if (sbLen == sbLenLast) sb.setLength(sbLenLast - sep.length)
        case _: Repeat => ()
        case Indent(res) =>
          indentation += 1
          nl(res)
          indentation -= 1
        case Newline(res) =>
          nl(res)
        case Meta(_, res) =>
          loop(res)
        case Wrap(prefix, res, suffix) =>
          sb.append(prefix)
          val sbLen = sb.length
          loop(res)
          if (sbLen != sb.length) sb.append(suffix)
          else sb.setLength(sbLen - prefix.length)
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
  private[meta] final case class Wrap(prefix: String, res: Result, suffix: String) extends Result
  private[meta] final case class Function(fn: StringBuilder => Result) extends Result

  def apply[T](f: T => Result): Show[T] =
    new Show[T] { def apply(input: T): Result = f(input) }

  def sequence[T](xs: T*): Result = macro scala.meta.internal.prettyprinters.ShowMacros.sequence

  def indent[T](x: T)(implicit show: Show[T]): Result = Indent(show(x))

  def repeat[T](xs: List[T], sep: String = "")(implicit show: Show[T]): Result =
    repeat(sep)(xs.map(show(_)): _*)
  def repeat[T](xs: List[T], prefix: String, sep: String, suffix: String)(
      implicit show: Show[T]
  ): Result = wrap(prefix, repeat(xs, sep), suffix)

  def repeat(xs: Result*): Result = repeat("")(xs: _*)
  def repeat(sep: String)(xs: Result*): Result = {
    val results = xs.filter(_ ne None)
    if (results.isEmpty) None else Repeat(results, sep)
  }
  def repeat(prefix: String, sep: String, suffix: String)(xs: Result*): Result =
    wrap(prefix, repeat(sep)(xs: _*), suffix)

  def newline[T](x: T)(implicit show: Show[T]): Result = Newline(show(x))

  def meta[T](data: Any, xs: T*): Result = macro scala.meta.internal.prettyprinters.ShowMacros.meta

  // wrap if non-empty
  def wrap[T](x: T, suffix: String)(implicit show: Show[T]): Result =
    wrap("", x, suffix)
  def wrap[T](prefix: String, x: T)(implicit show: Show[T]): Result =
    wrap(prefix, x, "")
  def wrap[T](prefix: String, x: T, suffix: String)(implicit show: Show[T]): Result = {
    val result = show(x)
    if (result eq None) result else Wrap(prefix, result, suffix)
  }

  // wrap if cond
  def wrap[T](x: T, suffix: String, cond: Boolean)(implicit show: Show[T]): Result =
    wrap("", x, suffix, cond)
  def wrap[T](prefix: String, x: T, cond: Boolean)(implicit show: Show[T]): Result =
    wrap(prefix, x, "", cond)
  def wrap[T](prefix: String, x: T, suffix: String, cond: Boolean)(
      implicit show: Show[T]
  ): Result = {
    val result = show(x)
    if (!cond || (result eq None)) result else Sequence(prefix, result, suffix)
  }

  def opt[T](x: Option[T])(implicit show: Show[T]): Result = x.fold[Result](None)(show(_))
  def opt[T](x: Option[T], suffix: String)(implicit show: Show[T]): Result =
    x.fold[Result](None)(x => Sequence(show(x), suffix))

  def function(fn: StringBuilder => Result): Result = Function(fn)

  implicit def printResult[R <: Result]: Show[R] = apply(identity)
  implicit def printString[T <: String]: Show[T] = apply(Show.Str(_))
  implicit def stringAsResult(value: String) = if (value.isEmpty) None else Show.Str(value)
  implicit def showAsResult[T](x: T)(implicit show: Show[T]): Result = show(x)
}
