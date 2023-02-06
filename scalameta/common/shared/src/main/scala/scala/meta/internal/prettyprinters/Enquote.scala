package scala.meta
package internal
package prettyprinters

import scala.annotation.tailrec

sealed trait QuoteStyle
case object SingleQuotes extends QuoteStyle { override def toString = "'" }
case object DoubleQuotes extends QuoteStyle { override def toString = "\"" }
case object TripleQuotes extends QuoteStyle { override def toString = "\"\"\"" }

object enquote {
  def apply(s: String, style: QuoteStyle): String = {
    val styleStr = style.toString
    val sb = new java.lang.StringBuilder(styleStr)
    if (style == TripleQuotes) {
      val styleLen = styleStr.length
      @tailrec def iter(off: Int): Unit = {
        val newoff = s.indexOf(styleStr, off)
        if (newoff < 0) sb.append(s, off, s.length)
        else {
          sb.append(s, off, newoff)
          sb.append("\\\"\\\"\\\"")
          iter(newoff + styleLen)
        }
      }
      iter(0)
    } else {
      s.foreach {
        case '\t' => sb.append("\\t")
        case '\b' => sb.append("\\b")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\f' => sb.append("\\f")
        case '\\' => sb.append("\\\\")
        case '"' if style eq DoubleQuotes =>
          sb.append("\\\"")
        case '\'' if style eq SingleQuotes =>
          sb.append("\\\'")
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
