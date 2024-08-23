package scala.meta
package prettyprinters

import org.scalameta.internal.ScalaCompat.EOL

import scala.language.experimental.macros
import scala.language.implicitConversions

trait Show[-T] {
  def apply(t: T): Show.Result
}

private[meta] object Show {
  sealed abstract class Result {
    def desc: String
    override def toString = {
      val sb = new StringBuilder
      var indentation = 0
      def nl(obj: Result) = {
        sb.append(EOL)
        sb.append("  " * indentation)
        loop(obj)
      }
      def loop(obj: Result): Unit = obj match {
        case None => // do nothing
        case obj: Str => sb.append(obj.value)
        case obj: Sequence => obj.xs.foreach(loop)
        case obj: Repeat =>
          val sep = obj.sep
          val sbLenInit = sb.length
          obj.xs.foreach { x =>
            val sbLen = sb.length
            loop(x)
            if (sbLen != sb.length) sb.append(sep)
          }
          val sbLenLast = sb.length
          if (sbLenInit != sbLenLast) sb.setLength(sbLenLast - sep.length)
        case obj: Indent => indentation += 1; nl(obj.res); indentation -= 1
        case obj: Newline => nl(obj.res)
        case obj: Meta => loop(obj.res)
        case obj: Wrap =>
          val sbLenInit = sb.length
          sb.append(obj.prefix)
          val sbLen = sb.length
          loop(obj.res)
          if (sbLen != sb.length) sb.append(obj.suffix) else sb.setLength(sbLenInit)
        case result: Function => loop(result.fn(sb))
      }
      loop(this)
      sb.toString
    }
    final def isEmpty: Boolean = this eq None
  }

  final case object None extends Result {
    override def desc: String = "None"
  }
  final case class Str(value: String) extends Result {
    override def desc: String = s"Str($value)"
  }
  final case class Sequence(xs: Result*) extends Result {
    override def desc: String = s"Sequence(#${xs.length})"
  }
  final case class Repeat(xs: Seq[Result], sep: String) extends Result {
    override def desc: String = s"Repeat(#${xs.length}, s=$sep)"
  }
  final case class Indent(res: Result) extends Result {
    override def desc: String = s"Indent(r=${res.desc})"
  }
  final case class Newline(res: Result) extends Result {
    override def desc: String = s"Newline(r=${res.desc})"
  }
  final case class Meta(data: Any, res: Result) extends Result {
    override def desc: String = s"Meta(d=$data, r=${res.desc})"
  }
  final case class Wrap(prefix: String, res: Result, suffix: String) extends Result {
    override def desc: String = s"Wrap(p=$prefix, r=${res.desc}, s=$suffix)"
  }
  final case class Function(fn: StringBuilder => Result) extends Result {
    override def desc: String = s"Function(...)"
  }

  def apply[T](f: T => Result): Show[T] = new Show[T] {
    def apply(input: T): Result = f(input)
  }

  def mkseq(xs: Result*): Result = xs.filter(_ ne None) match {
    case Seq() => None
    case Seq(head) => head
    case res => Sequence(res: _*)
  }
  def sequence[T](xs: T*): Result = macro scala.meta.internal.prettyprinters.ShowMacros.sequence

  def indent(res: Result): Result = if (res eq None) None else Indent(res)
  def indent[T](x: T)(implicit show: Show[T]): Result = indent(show(x))

  def repeat[T](xs: Seq[T], sep: String = "")(implicit show: Show[T]): Result =
    repeat(sep)(xs.map(show(_)): _*)
  def repeat[T](xs: Seq[T], prefix: String, sep: String, suffix: String)(implicit
      show: Show[T]
  ): Result = wrap(prefix, repeat(xs, sep), suffix)

  def repeat(xs: Result*): Result = repeat("")(xs: _*)
  def repeat(sep: String)(xs: Result*): Result = {
    val results = xs.filter(_ ne None)
    if (results.isEmpty) None else Repeat(results, sep)
  }
  def repeat(prefix: String, sep: String, suffix: String)(xs: Result*): Result =
    wrap(prefix, repeat(sep)(xs: _*), suffix)

  def newline(res: Result): Result = if (res eq None) None else Newline(res)
  def newline[T](x: T)(implicit show: Show[T]): Result = newline(show(x))

  def meta(data: Any, res: Result): Result = if (res eq None) None else Meta(data, res)
  def meta[T](data: Any, xs: T*): Result = macro scala.meta.internal.prettyprinters.ShowMacros.meta

  // wrap if non-empty
  def wrap[T](x: T, suffix: String)(implicit show: Show[T]): Result = wrap("", x, suffix)
  def wrap[T](prefix: String, x: T)(implicit show: Show[T]): Result = wrap(prefix, x, "")
  def wrap[T](prefix: => String, x: T, suffix: => String)(implicit show: Show[T]): Result =
    wrap(prefix, show(x), suffix)
  def wrap(prefix: => String, res: Result, suffix: => String): Result =
    if (res eq None) None else Wrap(prefix, res, suffix)

  // wrap if cond, even if value ends up being none
  def wrap[T](x: T, suffix: => String, cond: Boolean)(implicit show: Show[T]): Result =
    wrap("", x, suffix, cond)
  def wrap[T](prefix: => String, x: T, cond: Boolean)(implicit show: Show[T]): Result =
    wrap(prefix, x, "", cond)
  def wrap[T](prefix: => String, x: T, suffix: => String, cond: Boolean)(implicit
      show: Show[T]
  ): Result = if (!cond) x else mkseq(prefix, x, suffix)

  def opt[T](x: => T, cond: Boolean)(implicit show: Show[T]): Result = if (cond) x else None
  def opt[T](x: Option[T])(implicit show: Show[T]): Result = x.fold[Result](None)(show(_))
  def opt[T](x: Option[T], suffix: => Result)(implicit show: Show[T]): Result = x
    .fold[Result](None)(x => mkseq(x, suffix))
  def opt[T](prefix: => Result, x: Option[T])(implicit show: Show[T]): Result = x
    .fold[Result](None)(x => mkseq(prefix, x))
  def opt[T](prefix: => Result, x: Option[T], suffix: => Result)(implicit show: Show[T]): Result = x
    .fold[Result](None)(x => mkseq(prefix, x, suffix))

  def alt(a: Result, b: => Result): Result = if (a ne None) a else b
  def alt[A, B](a: A, b: => B)(implicit showA: Show[A], showB: Show[B]): Result =
    alt(showA(a), showB(b))

  def function(fn: StringBuilder => Result): Result = Function(fn)

  implicit def printResult[R <: Result]: Show[R] = apply(identity)
  implicit def printString[T <: String]: Show[T] = apply(Show.Str(_))
  implicit def stringAsResult(value: String): Result = if (value.isEmpty) None else Show.Str(value)
  implicit def showAsResult[T](x: T)(implicit show: Show[T]): Result = show(x)
}
