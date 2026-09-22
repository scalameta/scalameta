package scala.meta
package prettyprinters

import org.scalameta.internal.ScalaCompat.EOL

import java.nio.CharBuffer

import scala.language.implicitConversions

trait Show[-T] {
  def apply(t: T): Show.Result
}

private[meta] object Show {

  private class Serializer {
    private val sb = new StringBuilder
    private val indentation = new StringBuilder
    private var afterEOL = 0
    // text that emits only if a character follows it
    private var delay: CharSequence = _
    private var stack: List[Result] = Nil

    /* A separator waiting for the next emission. A character takes it, and a
     * newline drops it. If the separator is scoped to the element it precedes,
     * a newline on either side indents that element instead. A separator on
     * top of another emits nothing of its own. */
    private final class Pending(val sep: String, val outer: Pending, val scoped: Boolean) {
      var prev: Int = -1
    }
    private var pending: Pending = _

    private def addPending(sep: String, scoped: Boolean): Pending = {
      val outer = pending
      val p = new Pending(if ((outer ne null) && outer.sep.nonEmpty) "" else sep, outer, scoped)
      pending = p
      p
    }
    private def closePending(p: Pending): Unit = {
      if (pending eq p) pending = p.outer
      if (p.prev >= 0) indentation.setLength(p.prev)
    }
    private def takePending(): Pending = {
      val p = pending
      pending = null
      p
    }
    private def emitPending(p: Pending): Unit = if (p ne null) {
      emitPending(p.outer)
      if (p.sep.nonEmpty) appendImpl(p.sep)
    }
    private def indentPending(p: Pending): Unit = if (p ne null)
      if (p.scoped) {
        p.prev = indentation.length
        indentation.append("  ")
      } else indentPending(p.outer)

    def result: String = sb.result()

    def wasNL: Boolean = afterEOL != 0

    def delay(value: CharSequence = null): Unit = delay = value

    def endTrimmed(value: String): Int = {
      var end = value.length
      while (end > 0 && value.charAt(end - 1) == ' ') end -= 1
      end
    }

    def appendTrimmed(value: String, beg: Int, end: Int): Unit = if (beg < end) {
      appendPrepare()
      appendImpl(CharBuffer.wrap(value, beg, end))
    }

    // trailing spaces are a separator, not text
    def append(value: String): Unit = {
      val len = value.length
      val end = endTrimmed(value)
      if (end > 0) appendTrimmed(value, 0, end)
      if (end < len) addPending(value.substring(end), scoped = false)
    }

    def appendAsIs(value: String): Unit = if (value.nonEmpty) {
      appendPrepare()
      sb.append(value)
    }

    private def appendPrepare(): Unit = {
      val p = takePending()
      if (wasNL) indentPending(p) else emitPending(p)
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

    private def blank(newAfterEOL: Int): Unit = {
      val p = takePending()
      if ((delay ne null) && delay.length() != 0) emitPending(p) else indentPending(p)
      appendDelay()
      sb.append(EOL)
      afterEOL = newAfterEOL
    }

    def blank(): Unit = blank(-1)

    def nl(): Unit = if (afterEOL <= 0) blank(1)

    private def taskRun(task: => Unit): Result = new Run(() => task)

    // enter an indent scope: indent, and newline, return the restore action
    private def taskIndent: Result = {
      val prev = indentation.length
      indentation.append("  ")
      nl()
      taskRun(indentation.setLength(prev))
    }

    // Iterative render: a deeply nested Result would overflow the stack if each
    // node recursed into its children's `serialize`. Instead, walk an explicit
    // task stack -- each task either emits a Result or runs a deferred action
    // (a child's trailing effect).
    def serialize(top: Result): Unit = {
      stack = top :: Nil
      while (stack.nonEmpty) {
        val task = stack.head
        stack = stack.tail
        task match {
          case None => // do nothing
          case AsIs(value) => appendAsIs(value)
          case Str(value) =>
            if (stack.isEmpty || stack.head.isInstanceOf[Run]) append(value)
            else {
              val end = endTrimmed(value)
              if (end < value.length) // a trailing space is scoped to what follows
                stack = SpaceOrIndent(stack.head, value.substring(end)) :: stack.tail
              appendTrimmed(value, 0, end)
            }
          case Blank => blank()
          case m: Deferred => stack = m.res() :: stack
          case Function(fn) =>
            if (!wasNL) emitPending(takePending())
            stack = fn(sb) :: stack
          case Newline(res) =>
            nl()
            stack = res :: stack
          case Indent(res) =>
            pending = null // the indent replaces the separator
            stack = res :: taskIndent :: stack
          case SpaceOrIndent(res, sep) =>
            val p = addPending(sep, scoped = true)
            stack = res :: taskRun(closePending(p)) :: stack
          case SpaceOrNewline(res, sep) =>
            addPending(sep, scoped = false)
            stack = res :: stack
          case Wrap(prefix, res, suffix) =>
            delay(prefix)
            stack = res :: taskRun(if (delay eq null) append(suffix) else delay = null) :: stack
          case Sequence(xs @ _*) =>
            val it = xs.reverseIterator
            while (it.hasNext) stack = it.next() :: stack
          case Repeat(xs, sep) if sep.forall(_ == ' ') =>
            // a space separator is scoped to the element it precedes, and skips an empty one
            var acc: List[Result] = Nil
            val it = xs.reverseIterator
            while (it.hasNext) {
              val x = it.next()
              acc = if (acc.isEmpty || sep.isEmpty) x :: acc else x :: Str(sep) :: acc
            }
            stack = acc ::: stack
          case Repeat(xs, sep) =>
            val sepRun = taskRun(delay(sep))
            stack = taskRun(delay(null)) :: stack
            val it = xs.reverseIterator // most of the time, walk over IndexedSeq
            while (it.hasNext) stack = it.next() :: sepRun :: stack
          case r: Run => r.run()
        }
      }
    }
  }

  sealed abstract class Result {
    def desc: String
    override def toString: String = {
      val builder = new Serializer
      builder.serialize(this)
      builder.result
    }
    final def isEmpty: Boolean = this eq None
  }

  final case object None extends Result {
    override def desc: String = "None"
  }
  final case class AsIs(value: String) extends Result {
    override def desc: String = s"AsIs($value)"
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
  final case class SpaceOrIndent(res: Result, sep: String) extends Result {
    override def desc: String = s"SpaceOrIndent(sep=$sep, r=${res.desc})"
  }
  final case class SpaceOrNewline(res: Result, sep: String) extends Result {
    override def desc: String = s"SpaceOrNewline(sep=$sep, r=${res.desc})"
  }
  final case object Blank extends Result {
    override def desc: String = s"Blank()"
  }
  final case class Newline(res: Result) extends Result {
    override def desc: String = s"Newline(r=${res.desc})"
  }
  sealed class Deferred(val res: () => Result) extends Result {
    override def desc: String = s"Deferred(...)"
  }
  // `data` can be consulted without materializing `res`
  final class Meta(val data: Any, res: () => Result) extends Deferred(res) {
    override def desc: String = s"Meta(d=$data, ...)"
  }
  final case class Wrap(prefix: String, res: Result, suffix: String) extends Result {
    override def desc: String = s"Wrap(p=$prefix, r=${res.desc}, s=$suffix)"
  }
  final case class Function(fn: CharSequence => Result) extends Result {
    override def desc: String = s"Function(...)"
  }

  private final class Run(val run: () => Unit) extends Result {
    override def desc: String = "Run(...)"
  }

  def apply[T](f: T => Result): Show[T] = new Show[T] {
    def apply(input: T): Result = f(input)
  }

  def sequence(xs: Result*): Result = xs.filter(_ ne None) match {
    case Seq() => None
    case Seq(head) => head
    case res => Sequence(res: _*)
  }

  def indent(res: Result): Result = if (res eq None) None else Indent(res)

  def repeat(sep: String)(xs: Result*): Result = xs.filter(_ ne None) match {
    case Seq() => None
    case Seq(head) => head
    case res => Repeat(res, sep)
  }
  def repeat(xs: Seq[Result], sep: String = ""): Result = repeat(sep)(xs: _*)
  def repeat(prefix: => Result, sep: String, suffix: => Result)(xs: Result*): Result =
    wrap(prefix, repeat(xs, sep), suffix)

  def blank(): Result = Blank
  def blank(cond: Boolean): Result = if (cond) Blank else None

  def nosplit[T: Show](x: T): Result = {
    val res = implicitly[Show[T]].apply(x)
    if (res.isEmpty) None else SpaceOrIndent(res, "")
  }
  def spacen(x: Result, sep: String): Result = if (x.isEmpty) None else SpaceOrNewline(x, sep)
  def spacen[T: Show](x: T): Result = spacen(x, " ")

  def newline(): Result = Newline(None)
  def newline(res: Result): Result = if (res eq None) None else Newline(res)

  // body by-name + no eager None-elision: the child render is deferred, and
  // serialize's `delay` mechanism elides empties dynamically instead.
  def meta(data: Any, res: => Result): Result = new Meta(data, () => res)

  def defer(res: => Result): Result = new Deferred(() => res)

  // wrap if non-empty
  def wrap(x: Result, suffix: => String): Result = if (x eq None) None else sequence(x, suffix)
  def wrap(prefix: => String, x: Result): Result = if (x eq None) None else sequence(prefix, x)
  def wrap(prefix: => Result, res: Result, suffix: => Result): Result =
    if (res eq None) None else sequence(prefix, res, suffix)

  // wrap if cond, even if value ends up being none
  def wrap(x: Result, suffix: => String, cond: Boolean): Result =
    if (cond) sequence(x, suffix) else x
  def wrap(prefix: => String, x: Result, cond: Boolean): Result =
    if (cond) sequence(prefix, x) else x
  def wrap(prefix: => Result, x: Result, suffix: => Result, cond: Boolean): Result =
    if (cond) sequence(prefix, x, suffix) else x

  def opt(x: => Result, cond: Boolean): Result = if (cond) x else None
  def opt[T](x: Option[T])(implicit show: Show[T]): Result = x.fold[Result](None)(show.apply)
  def opt[T: Show](x: Option[T], suffix: => Result): Result = x
    .fold[Result](None)(sequence(_, suffix))
  def opt[T: Show](prefix: => Result, x: Option[T]): Result = x
    .fold[Result](None)(sequence(prefix, _))
  def opt[T: Show](prefix: => Result, x: Option[T], suffix: => Result): Result = x
    .fold[Result](None)(sequence(prefix, _, suffix))

  def alt(a: Result, b: => Result): Result = if (a ne None) a else b

  def function(fn: CharSequence => Result): Result = Function(fn)

  def asis(value: String): Result = if (value.isEmpty) None else AsIs(value)

  implicit def printResult[R <: Result]: Show[R] = apply(identity)
  implicit def printString[T <: String]: Show[T] = apply(str)
  implicit def str(value: String): Result = if (value.isEmpty) None else Str(value)
  implicit def showAsResult[T](x: T)(implicit show: Show[T]): Result = show(x)
  implicit def seq[T](x: Seq[T])(implicit show: Show[T]): Seq[Result] = x.map(show.apply)
}
