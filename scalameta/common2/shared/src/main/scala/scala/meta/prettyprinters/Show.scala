package scala.meta
package prettyprinters

import org.scalameta.internal.ScalaCompat.EOL
import scala.meta.internal.tokens.Chars.{isIdentifierPart, isOperatorPart}

import java.nio.CharBuffer

import scala.annotation.tailrec
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

    /**
     * A separator waiting for the next emission. If followed by a newline, this separator is
     * dropped. A chain of separators emits one space, if any of them has one.
     * @param space
     *   whether to emit a space before the next character
     * @param outer
     *   the previous pending separator, if any
     * @param indented
     *   whether a newline on either side of the next element indents that element
     */
    private final class Pending(val space: Boolean, val outer: Pending, val indented: Boolean) {
      var prev: Int = -1
    }
    private var pending: Pending = _

    private def addIndent(space: Boolean): Pending = {
      val p = new Pending(space, pending, indented = true)
      pending = p
      p
    }
    private def addSpace(): Unit = if ((pending eq null) || pending.indented)
      pending = new Pending(space = true, pending, indented = false)
    private def closePending(p: Pending): Unit = {
      if (pending eq p) pending = p.outer
      if (p.prev >= 0) indentation.setLength(p.prev)
    }
    private def takePending(): Pending = {
      val p = pending
      pending = null
      p
    }
    // nothing precedes the start of the output, so no space separates it
    private def emitPending(p: Pending): Unit = if (sb.length != 0 && hasSpace(p)) sb.append(' ')
    @tailrec
    private def hasSpace(p: Pending): Boolean = (p ne null) && (p.space || hasSpace(p.outer))
    @tailrec
    private def indentPending(p: Pending): Unit = if (p ne null)
      if (p.indented) {
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

    // add the leading spaces before `end` as a separator; return where the text begins
    private def leadingPending(value: String, end: Int): Int = {
      var beg = 0
      while (beg < end && value.charAt(beg) == ' ') beg += 1
      if (beg > 0) addLeadingSpace()
      beg
    }
    // the delayed separator precedes the space, and a space that ends it is enough
    private def addLeadingSpace(): Unit =
      if (!hasDelay) addSpace()
      else {
        appendPrepare()
        if (sb.charAt(sb.length - 1) != ' ') addSpace()
      }

    // the spaces at the ends are separators; `space` adds one before the text
    private def appendStr(value: String, space: Boolean): Unit = {
      val end = endTrimmed(value)
      val last = stack.isEmpty || stack.head.isInstanceOf[Run]
      if (end < value.length && !last) // a trailing space is scoped to what follows
        push(SpaceOrIndent(pop(), space = true))
      var beg = 0
      while (beg < end && value.charAt(beg) == ' ') beg += 1
      if ((space || beg > 0) && end > 0) addLeadingSpace()
      appendTrimmed(value, beg, end)
      if (end < value.length && last) addSpace()
    }

    def appendTrimmed(value: String, beg: Int, end: Int): Unit = if (beg < end) {
      appendPrepare()
      appendImpl(CharBuffer.wrap(value, beg, end))
    }

    // leading and trailing spaces are a separator, not text
    def append(value: String): Unit = {
      val end = endTrimmed(value)
      if (end > 0) appendTrimmed(value, leadingPending(value, end), end)
      if (end < value.length) addSpace()
    }

    def appendAsIs(value: String): Unit = if (value.nonEmpty) {
      appendPrepare()
      sb.append(value)
    }

    private def appendPrepare(): Unit = {
      val p = takePending()
      if (wasNL) indentPending(p) else if (!hasDelay) emitPending(p) // the separator replaces it
      appendDelay()
      if (wasNL) {
        sb.append(indentation)
        afterEOL = 0
      }
    }

    private def hasDelay: Boolean = delay ne null

    private def appendDelay(): Unit = if (hasDelay) {
      appendImpl(delay)
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

    def blank(): Unit = nl(-1)

    private def nl(newAfterEOL: Int): Unit = {
      val p = takePending()
      if (hasDelay) emitPending(p) else indentPending(p)
      appendDelay()
      sb.append(EOL)
      afterEOL = newAfterEOL
    }
    def nl(): Unit = if (afterEOL <= 0) nl(1)

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
        val task = pop()
        task match {
          case None => // do nothing
          case AsIs(value) => appendAsIs(value)
          case Literal(value) => appendTrimmed(value, 0, value.length)
          case Str(value) => appendStr(value, space = false)
          case Blank => blank()
          case m: Deferred => maybePush(m.value)
          case Keyword(value) => appendStr(
              value,
              space = withPending {
                val len = sb.length
                isOperatorPart(value.charAt(0)) && len >= 1 && {
                  val last = sb.charAt(len - 1)
                  isOperatorPart(last) ||
                  last == '_' && len >= 2 && isIdentifierPart(sb.charAt(len - 2))
                }
              },
            )
          case Comment(res) => maybePush(res)
          case LeadingComments(res, breakAtLineStart) =>
            val atLineStart = withPending(sb.length == 0 || sb.charAt(sb.length - 1) == '\n')
            push(if (atLineStart && breakAtLineStart) newlineOnly else spaceOnly)
            maybePush(res)
          case Newline(res) =>
            nl()
            maybePush(res)
          case Indent(res) =>
            pending = null // the indent replaces the separator
            push(taskIndent)
            maybePush(res)
          case SpaceOrIndent(res, space) =>
            val p = addIndent(space)
            push(taskRun(closePending(p)))
            maybePush(res)
          case SpaceOrNewline(res, space) =>
            if (space) addSpace()
            maybePush(res)
          case Sequence(xs @ _*) =>
            val it = xs.reverseIterator
            while (it.hasNext) maybePush(it.next())
          case Repeat(xs, sep) if sep.forall(_ == ' ') =>
            // a space separator is scoped to the element it precedes
            val it = xs.reverseIterator
            if (sep.isEmpty) while (it.hasNext) maybePush(it.next())
            else if (it.hasNext) {
              var x = it.next() // last element
              while (it.hasNext) {
                push(SpaceOrIndent(x, space = true))
                x = it.next()
              }
              maybePush(x) // first element
            }
          case Repeat(xs, sep) =>
            val sepRun = taskRun(delay(sep))
            push(taskRun(delay(null)))
            val it = xs.reverseIterator // most of the time, walk over IndexedSeq
            while (it.hasNext) {
              push(sepRun)
              maybePush(it.next())
            }
          case r: Run => r.run()
        }
      }
    }

    // decide with the pending separator in the output, then take it out again
    private def withPending[A](res: => A): A = {
      val len = sb.length
      if (!wasNL) emitPending(pending)
      try res
      finally sb.setLength(len)
    }

    private def pop(): Result = {
      val res = stack.head
      stack = stack.tail
      res
    }
    private def push(res: Result): Unit = stack = res :: stack
    private def maybePush(res: Result): Unit = if (res ne None) push(res)

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
  // text with its own spaces, such as a part of an interpolation
  final case class Literal(value: String) extends Result {
    override def desc: String = s"Literal($value)"
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
  final case class SpaceOrIndent(res: Result, space: Boolean) extends Result {
    override def desc: String = s"SpaceOrIndent(space=$space, r=${res.desc})"
  }
  final case class SpaceOrNewline(res: Result, space: Boolean) extends Result {
    override def desc: String = s"SpaceOrNewline(space=$space, r=${res.desc})"
  }
  final case object Blank extends Result {
    override def desc: String = s"Blank()"
  }
  final case class Newline(res: Result) extends Result {
    override def desc: String = s"Newline(r=${res.desc})"
  }
  sealed class Deferred(res: () => Result) extends Result {
    lazy val value: Result = res()
    override def desc: String = s"Deferred(...)"
  }
  // `data` can be consulted without materializing `res`
  final class Meta(val data: Any, res: () => Result) extends Deferred(res) {
    override def desc: String = s"Meta(d=$data, ...)"
  }
  // a keyword, after a space if it would otherwise join the operator before it
  final case class Keyword(value: String) extends Result {
    override def desc: String = s"Keyword($value)"
  }
  final case class Comment(res: Result) extends Result {
    override def desc: String = s"Comment(r=${res.desc})"
  }
  // comments before a tree, then a newline if they start a line and the last ends it, else a space
  final case class LeadingComments(res: Result, breakAtLineStart: Boolean) extends Result {
    override def desc: String = s"LeadingComments(break=$breakAtLineStart, r=${res.desc})"
  }

  private final class Run(val run: () => Unit) extends Result {
    override def desc: String = "Run(...)"
  }

  def apply[T](f: T => Result): Show[T] = new Show[T] {
    def apply(input: T): Result = f(input)
  }

  def sequenceFiltered(xs: Result*): Result = xs match {
    case Seq() => None
    case Seq(head) => head
    case res => Sequence(res: _*)
  }
  def sequence(xs: Result*): Result = sequenceFiltered(xs.filter(_ ne None): _*)

  def indent(res: Result): Result = if (res eq None) None else Indent(res)
  def indent(res: Result, cond: Boolean): Result = if (cond) indent(res) else res

  def repeatFiltered(sep: String)(xs: Seq[Result]): Result = xs match {
    case Seq() => None
    case Seq(head) => head
    case res => Repeat(res, sep)
  }
  def repeat(sep: String)(xs: Result*): Result = repeatFiltered(sep)(xs.filter(_ ne None))
  def repeat(xs: Seq[Result], sep: String = ""): Result = repeat(sep)(xs: _*)
  def repeat(prefix: => Result, sep: String, suffix: => Result)(xs: Result*): Result =
    wrap(prefix, repeat(xs, sep), suffix)

  def blank(): Result = Blank
  def blank(cond: Boolean): Result = if (cond) Blank else None

  def nosplit[T: Show](x: T): Result = spacei(x, space = false)

  def spacei(x: Result, space: Boolean): Result = if (x.isEmpty) None else SpaceOrIndent(x, space)

  val spaceOnly: Result = Str(" ")
  val spaceOrNewline: Result = SpaceOrNewline(None, space = true)
  def spacen(): Result = spaceOrNewline
  def spacen(x: Result, space: Boolean): Result = if (x.isEmpty) None else SpaceOrNewline(x, space)
  def spacen[T: Show](x: T): Result = spacen(x, space = true)

  val newlineOnly: Result = Newline(None)
  def newline(): Result = newlineOnly
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

  def keyword(value: String): Result = Keyword(value)

  def asis(value: String): Result = if (value.isEmpty) None else AsIs(value)
  def literal(value: String): Result = if (value.isEmpty) None else Literal(value)

  implicit def printResult[R <: Result]: Show[R] = apply(identity)
  implicit def printString[T <: String]: Show[T] = apply(str)
  implicit def str(value: String): Result = if (value.isEmpty) None else Str(value)
  implicit def showAsResult[T](x: T)(implicit show: Show[T]): Result = show(x)
  implicit def seq[T](x: Seq[T])(implicit show: Show[T]): Seq[Result] = x.map(show.apply)

}
