package scala.meta
package inputs

import java.nio.charset.Charset
import scala.meta.convert._

trait Content extends Input {
  def chars: Array[Char]

  // TODO: It's regrettable that we need to taint the pure abstraction of Content.
  // However, as #334 shows, we just can't redo offset -> line conversions over and over again.
  // This means that we gotta cache, and this instance is really the only place versatile enough.
  private lazy val lineIndices: Array[Int] = {
    val chars = this.chars
    val buf = new scala.collection.mutable.ArrayBuffer[Int]
    buf += 0
    var i = 0
    while (i < chars.length) {
      if (chars(i) == '\n') buf += (i + 1)
      i += 1
    }
    if (buf.last != chars.length) buf += chars.length // sentinel value used for binary search
    buf.toArray
  }

  private[meta] def lineToOffset(line: Int): Int = {
    // NOTE: The length-1 part is not a typo, it's to accommodate the sentinel value.
    if (!(0 <= line && line <= lineIndices.length - 1)) {
      val message = s"$line is not a valid line number, allowed [0..${lineIndices.length - 1}]"
      throw new IllegalArgumentException(message)
    }
    lineIndices(line)
  }

  private[meta] def offsetToLine(offset: Int): Int = {
    val chars = this.chars
    val a = lineIndices
    // NOTE: We allow chars.length, because it's a valid value for an offset.
    if (!(0 <= offset && offset <= chars.length)) {
      val message = s"$offset is not a valid offset, allowed [0..${chars.length}]"
      throw new IllegalArgumentException(message)
    }
    // NOTE: chars.length requires a really ugly special case.
    // If the file doesn't end with \n, then it's simply last_line:last_col+1.
    // But if the file does end with \n, then it's last_line+1:0.
    if (offset == chars.length && (0 < chars.length && chars(offset - 1) == '\n')) {
      return a.length - 1
    }
    var lo = 0
    var hi = a.length - 1
    while (hi - lo > 1) {
      val mid = (hi + lo) / 2
      if (offset < a(mid)) hi = mid
      else if (a(mid) == offset) return mid
      else /* if (a(mid) < offset */ lo = mid
    }
    return lo
  }
}

object Content {
  // TODO: Deduplicate wrt the implicits in Input.
  // I tried moving them to a shared trait, but then scalac produces super weird error messages like:
  //  [error]                     ^
  //  [error] /Users/xeno_by/Projects/core/scalameta/tokenquasiquotes/src/main/scala/scala/meta/internal/tokenquasiquotes/ReificationMacros.scala:68: ambiguous implicit values:
  //  [error]  both method stringToInput in trait InputConverters of type [T >: scala.meta.syntactic.Content <: scala.meta.syntactic.Input]=> org.scalameta.convert.Convert[String,T]
  //  [error]  and method stringToInput in trait InputConverters of type [T >: scala.meta.syntactic.Content <: scala.meta.syntactic.Input]=> org.scalameta.convert.Convert[String,T]
  //  [error]  match expected type org.scalameta.convert.Convert[String,scala.meta.syntactic.Content]
  implicit val charsToInput: Convert[Array[Char], Content] = Convert(chars => Input.String(new scala.Predef.String(chars)))
  implicit val stringToInput: Convert[scala.Predef.String, Content] = Convert(Input.String(_))
  implicit def streamToInput[T <: java.io.InputStream]: Convert[T, Input] = Convert(is => Input.Stream(is, Charset.forName("UTF-8")))
  implicit val fileToInput: Convert[java.io.File, Content] = Convert(f => Input.File(f, Charset.forName("UTF-8")))
}
