package scala.meta
package prettyprinters

import org.scalameta.internal.ScalaCompat.EOL

import scala.language.experimental.macros
import scala.language.implicitConversions

trait Show[-T] {
  def apply(t: T): Show.Result
}

private[meta] object Show {

  private class Serializer {
    private val sb = new StringBuilder
    private val indentation = new StringBuilder
    private var afterEOL = 0
    private var delay: CharSequence = _

    def result: String = sb.result()

    def wasNL: Boolean = afterEOL != 0

    def delay(value: CharSequence = null): Unit = delay = value

    def append(value: String): Unit = if (value.nonEmpty) {
      appendPrepare()
      appendImpl(value)
    }

    def appendAsIs(value: String): Unit = if (value.nonEmpty) {
      appendPrepare()
      sb.append(value)
    }

    private def appendPrepare(): Unit = {
      appendDelay()
      if (wasNL) {
        sb.append(indentation)
        afterEOL = 0
      }
    }

    private def appendDelay(): Unit = if (delay ne null) {
      if (delay.length() != 0) appendImpl(delay)
      delay = null
    }

    private def appendImpl(value: CharSequence): Unit = {
      val len = value.length
      var idx = 0
      while (idx < len) {
        value.charAt(idx) match {
          case '\r' =>
            sb.append(EOL)
            // now skip '\n'
            idx += 1
            if (idx < len && value.charAt(idx) != '\n') idx -= 1
          case '\n' => sb.append(EOL)
          case ch => sb.append(ch)
        }
        idx += 1
      }
    }

    def append(fn: CharSequence => Result): Unit = fn(sb).serialize(this)

    def appendNoDelay(value: String): Unit = if (delay eq null) append(value) else delay = null

    private def blank(newAfterEOL: Int): Unit = {
      appendDelay()
      sb.append(EOL)
      afterEOL = newAfterEOL
    }

    def blank(): Unit = blank(-1)

    def nl(): Unit = if (afterEOL <= 0) blank(1)

    def nlBefore(obj: Result): Unit = {
      nl()
      obj.serialize(this)
    }

    def indent(obj: Result): Unit = {
      val prev = indentation.length
      indentation.append("  ")
      nlBefore(obj)
      indentation.setLength(prev)
    }
  }

  sealed abstract class Result {
    def desc: String
    def serialize(implicit builder: Serializer): Unit
    override def toString: String = {
      implicit val builder: Serializer = new Serializer
      serialize
      builder.result
    }
    final def isEmpty: Boolean = this eq None
    def headChar: Option[Char]
  }

  final case object None extends Result {
    override def desc: String = "None"
    def headChar: Option[Char] = Option.empty
    override def serialize(implicit builder: Serializer): Unit = {} // do nothing
  }
  final case class AsIs(value: String) extends Result {
    override def desc: String = s"AsIs($value)"
    def headChar: Option[Char] = value.headOption
    override def serialize(implicit builder: Serializer): Unit = builder.appendAsIs(value)
  }
  final case class Str(value: String) extends Result {
    override def desc: String = s"Str($value)"
    def headChar: Option[Char] = value.headOption
    override def serialize(implicit builder: Serializer): Unit = builder.append(value)
  }
  final case class Sequence(xs: Result*) extends Result {
    override def desc: String = s"Sequence(#${xs.length})"
    def headChar: Option[Char] = xs.view.flatMap(_.headChar).headOption
    override def serialize(implicit builder: Serializer): Unit = xs.foreach(_.serialize)
  }
  final case class Repeat(xs: Seq[Result], sep: String) extends Result {
    override def desc: String = s"Repeat(#${xs.length}, s=$sep)"
    def headChar: Option[Char] = xs.view.flatMap(_.headChar).headOption
    override def serialize(implicit builder: Serializer): Unit = {
      xs.foreach { x =>
        x.serialize
        builder.delay(sep)
      }
      builder.delay(null)
    }
  }
  final case class Indent(res: Result) extends Result {
    override def desc: String = s"Indent(r=${res.desc})"
    def headChar: Option[Char] = res.headChar
    override def serialize(implicit builder: Serializer): Unit = builder.indent(res)
  }
  final case class Space(res: Result, space: String) extends Result {
    override def desc: String = s"NoSplit(r=${res.desc})"
    def headChar: Option[Char] = res.headChar
    override def serialize(implicit builder: Serializer): Unit =
      if (builder.wasNL) builder.indent(res)
      else {
        builder.append(space)
        res.serialize
      }
  }
  final case object Blank extends Result {
    override def desc: String = s"Blank()"
    def headChar: Option[Char] = Option.empty
    override def serialize(implicit builder: Serializer): Unit = builder.blank()
  }
  final case class Newline(res: Result) extends Result {
    override def desc: String = s"Newline(r=${res.desc})"
    def headChar: Option[Char] = Option.empty
    override def serialize(implicit builder: Serializer): Unit = builder.nlBefore(res)
  }
  final case class Meta(data: Any, res: Result) extends Result {
    override def desc: String = s"Meta(d=$data, r=${res.desc})"
    def headChar: Option[Char] = res.headChar
    override def serialize(implicit builder: Serializer): Unit = res.serialize
  }
  final case class Wrap(prefix: String, res: Result, suffix: String) extends Result {
    override def desc: String = s"Wrap(p=$prefix, r=${res.desc}, s=$suffix)"
    def headChar: Option[Char] = prefix.headOption.orElse(res.headChar)
    override def serialize(implicit builder: Serializer): Unit = {
      builder.delay(prefix)
      res.serialize
      builder.appendNoDelay(suffix)
    }
  }
  final case class Function(fn: CharSequence => Result) extends Result {
    override def desc: String = s"Function(...)"
    def headChar: Option[Char] = Option.empty
    override def serialize(implicit builder: Serializer): Unit = builder.append(fn)
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
  def repeat[T](xs: Seq[T], prefix: => Result, sep: String, suffix: => Result)(implicit
      show: Show[T]
  ): Result = wrap(prefix, repeat(xs, sep), suffix)

  def repeat(xs: Result*): Result = repeat("")(xs: _*)
  def repeat(sep: String)(xs: Result*): Result = xs.filter(_ ne None) match {
    case Seq() => None
    case Seq(head) => head
    case res => Repeat(res, sep)
  }
  def repeat(prefix: String, sep: String, suffix: String)(xs: Result*): Result =
    wrap(prefix, repeat(sep)(xs: _*), suffix)
  def repeat(prefix: => Result, sep: String, suffix: => Result)(xs: Result*): Result =
    wrap(prefix, repeat(sep)(xs: _*), suffix)

  def blank(): Result = Blank
  def blank(cond: Boolean): Result = if (cond) Blank else None

  def space(x: Result, space: String): Result = if (x.isEmpty) None else Space(x, space)
  def space[T](x: T)(implicit show: Show[T]): Result = space(show(x), " ")
  def nosplit[T](x: T)(implicit show: Show[T]): Result = space(show(x), "")

  def newline(): Result = Newline(None)
  def newline(res: Result): Result = if (res eq None) None else Newline(res)
  def newline[T](x: T)(implicit show: Show[T]): Result = newline(show(x))

  def meta(data: Any, res: Result): Result = if (res eq None) None else Meta(data, res)
  def meta[T](data: Any, xs: T*): Result = macro scala.meta.internal.prettyprinters.ShowMacros.meta

  // wrap if non-empty
  def wrap[T](x: T, suffix: String)(implicit show: Show[T]): Result = wrap("", x, suffix)
  def wrap[T](prefix: String, x: T)(implicit show: Show[T]): Result = wrap(prefix, x, "")
  def wrap[P, T, S](prefix: => P, x: T, suffix: => S)(implicit
      showP: Show[P],
      showT: Show[T],
      showS: Show[S]
  ): Result = wrap(showP(prefix), showT(x), showS(suffix))
  def wrap(prefix: => Result, res: Result, suffix: => Result): Result =
    if (res eq None) None else mkseq(prefix, res, suffix)

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

  def function(fn: CharSequence => Result): Result = Function(fn)

  def asis(value: String): Result = if (value.isEmpty) None else AsIs(value)

  implicit def printResult[R <: Result]: Show[R] = apply(identity)
  implicit def printString[T <: String]: Show[T] = apply(str)
  implicit def str(value: String): Result = if (value.isEmpty) None else Str(value)
  implicit def showAsResult[T](x: T)(implicit show: Show[T]): Result = show(x)
}
