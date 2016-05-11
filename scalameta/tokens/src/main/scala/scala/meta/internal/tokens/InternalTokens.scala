package scala.meta
package internal
package tokens

import org.scalameta._
import org.scalameta.invariants._
import scala.meta.classifiers._
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.tokens.Token._

trait InternalTokens {
  self: Tokens =>

  // NOTE: If this method changes, go and change the "freeform tokens" test.
  // TODO: I don't like the number of special cases and validations inside this method.
  private[meta] def translatePosition(pos: Position): TokenStreamPosition = {
    def fail(message: String) = throw new IllegalArgumentException("internal error: " + message)
    def failPositionEmpty() = fail("can't slice according to an empty position")
    def failMissingLetterbox() = fail("can't slice without the BOF .. EOF letterbox")
    def failEmptySyntax() = fail("can't slice empty syntax")
    def failDoesntLineUp() = fail(s"tokens in $this don't line up according to $pos")
    def failWrongInput(badInput: Input) = fail(s"tokens in $this have wrong input according to $pos: expected = ${pos.input}, actual = $badInput")
    pos match {
      case Position.Range(input, start, end) =>
        def validateTokens(): Unit = {
          if (this.length < 2 || !this.head.is[BOF] || !this.last.is[EOF]) failMissingLetterbox()
          if (this.forall(token => token.start == token.end)) failEmptySyntax()
        }
        def find(offset: Int, start: Boolean): Int = {
          def coord(idx: Int) = {
            if (start) this(idx).start else this(idx).end
          }
          def binarySearch(): Int = {
            // Bounds are inclusive
            var lo = 0
            var hi = this.length - 1
            while (lo <= hi) {
              val mid = (lo + hi) / 2
              if (offset < coord(mid)) hi = mid - 1
              else if (offset == coord(mid)) return mid
              else /* if (coord(mid) < offset) */ lo = mid + 1
            }
            return -1
          }
          def disambiguate(idx0: Int): Int = {
            def rangeCheck(idx: Int) = {
              0 <= idx && idx < this.length
            }
            def badToken(idx: Int): Boolean = {
              // These are tokens that are empty (i.e. of zero length)
              // and that can't be first/last tokens of an abstract syntax tree.
              this(idx).is[Interpolation.SpliceEnd] || this(idx).is[Xml.SpliceStart] || this(idx).is[Xml.SpliceEnd]
            }
            var idx = idx0
            if (badToken(idx)) {
              val step = if (start) +1 else -1
              while (rangeCheck(idx) && badToken(idx)) idx += step
              if (!rangeCheck(idx)) idx -= step
            } else {
              val step = if (start) -1 else +1
              while (rangeCheck(idx) && !badToken(idx) && coord(idx) == offset) idx += step
              idx -= step
            }
            if (badToken(idx)) failDoesntLineUp()
            idx
          }
          // Find a token that starts/ends at a given offset
          // and then disambiguate with other tokens that look the same.
          val idx = binarySearch()
          if (idx == -1) failDoesntLineUp()
          disambiguate(idx)
        }
        def validateResult(lo: Int, hi: Int): Unit = {
          require(start.offset == this(lo).start && debug(pos, lo, hi))
          require(end.offset == this(hi).end && debug(pos, lo, hi))
          var i = lo
          while (i <= hi) {
            if (pos.input != this(i).input) failWrongInput(this(i).input)
            i += 1
          }
        }
        validateTokens()
        var lo = find(start.offset, start = true)
        var hi = find(end.offset, start = false)
        // NOTE: need to cut out BOF/EOF if the position doesn't cover the entire range
        val wasEmpty = lo > hi
        def isEmpty = lo > hi
        if (hi == this.length - 1 && start.offset != 0) hi -= 1
        if (lo == 0 && end.offset != this(this.length - 1).end) lo += 1
        if (!wasEmpty && isEmpty) failDoesntLineUp()
        validateResult(lo, hi)
        TokenStreamPosition(pos.input, lo, hi + 1)
      case _ =>
        failPositionEmpty()
    }
  }
}