package scala.meta
package internal
package prettyprinters

import scala.annotation.tailrec

sealed abstract class QuoteStyle(styleStr: String) {
  private final val styleEscaped: String = {
    val sb = new java.lang.StringBuilder(2 * styleStr.length)
    styleStr.foreach(ch => sb.append("\\").append(ch))
    sb.toString
  }

  override def toString: String = styleStr

  def apply(s: String): String = {
    val sb = new java.lang.StringBuilder(styleStr)
    val styleLen = styleStr.length
    if (styleLen > 1) {
      @tailrec
      def iter(off: Int): Unit = {
        val newoff = s.indexOf(styleStr, off)
        if (newoff < 0) sb.append(s, off, s.length)
        else {
          sb.append(s, off, newoff)
          sb.append(styleEscaped)
          iter(newoff + styleLen)
        }
      }
      iter(0)
    } else {
      val styleCh = styleStr(0)
      s.foreach {
        case '\t' => sb.append("\\t")
        case '\b' => sb.append("\\b")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\f' => sb.append("\\f")
        case '\\' => sb.append("\\\\")
        case `styleCh` => sb.append(styleEscaped)
        case c =>
          val isNonReadableAscii = c < ' ' || c > '~'
          if (isNonReadableAscii && !Character.isLetter(c)) sb.append("\\u%04x".format(c.toInt))
          else sb.append(c)
      }
    }
    sb.append(styleStr)
    sb.toString
  }
}

case object SingleQuotes extends QuoteStyle("'") {
  def apply(ch: Char): String = super.apply(ch.toString)
}
case object DoubleQuotes extends QuoteStyle("\"") {
  def orTriple(str: String): String = if (str.contains('\n')) TripleQuotes(str) else apply(str)
}
case object TripleQuotes extends QuoteStyle("\"\"\"")
