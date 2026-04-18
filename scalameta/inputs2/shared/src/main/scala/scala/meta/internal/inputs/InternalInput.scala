package scala.meta.internal
package inputs

import scala.meta.Dialect
import scala.meta.inputs._
import scala.meta.tokens.Tokens

import scala.collection.mutable

trait InternalInput {
  self: Input =>

  private[meta] lazy val tokenCache: mutable.Map[Dialect, Tokens] = Compat
    .newMutableMap[Dialect, Tokens]

  // NOTE: It's regrettable that we need to taint the pure abstraction of Input.
  // However, as #334 shows, we just can't redo offset -> line conversions over and over again.
  // This means that we gotta cache, and this instance is really the only place versatile enough.
  private lazy val cachedLineIndices: Array[Int] = {
    val chars = this.chars
    val buf = new mutable.ArrayBuffer[Int]
    buf += 0
    var i = 0
    var lastIsCR = false
    while (i < chars.length) {
      // we consider all `\n`, `\r\n` and `\r` as new line
      if (chars(i) == '\n') buf += i + 1 else if (lastIsCR) buf += i
      lastIsCR = chars(i) == '\r'
      i += 1
    }
    buf.toArray
  }

  private[meta] def lineToOffsetAndLength(line: Int): (Int, Int) = {
    val a = cachedLineIndices
    // NOTE: The length-1 part is not a typo, it's to accommodate the sentinel value.
    if (0 > line || line >= a.length) {
      val message = s"$line is not a valid line number, allowed [0..${a.length - 1})"
      throw new IllegalArgumentException(message)
    }
    val offset = a(line)
    val nextLine = line + 1
    (offset, (if (nextLine == a.length) chars.length else a(nextLine)) - offset)
  }

  private[meta] def offsetToLineAndColumn(offset: Int): (Int, Int) = {
    val chars = this.chars
    val a = cachedLineIndices
    // NOTE: We allow chars.length, because it's a valid value for an offset.
    if (0 > offset || offset > chars.length) {
      val message = s"$offset is not a valid offset, allowed [0..${chars.length}]"
      throw new IllegalArgumentException(message)
    }
    // NOTE: chars.length requires a really ugly special case.
    if (offset == chars.length) return (a.length - 1, offset - a(a.length - 1))
    var lo = 0
    var mid = 0
    var hi = a.length
    while ({
      mid = (hi + lo) / 2
      mid > lo
    }) {
      val midoff = a(mid)
      if (offset == midoff) return (mid, 0)
      if (offset < midoff) hi = mid else lo = mid
    }
    (lo, offset - a(lo))
  }
}
