package scala.meta
package internal
package prettyprinters

sealed trait QuoteStyle
case object SingleQuotes extends QuoteStyle { override def toString = "'" }
case object DoubleQuotes extends QuoteStyle { override def toString = "\"" }
case object TripleQuotes extends QuoteStyle { override def toString = "\"\"\"" }

object enquote {
  def apply(s: String, style: QuoteStyle): String = {
    // TODO: comprehensive handling (e.g. escape triple quotes in triple quotes)
    val sb = new StringBuilder(style.toString)
    if(style == TripleQuotes)
      sb.append(s)
    else {
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
          sb.append(c)
      }
    }
    sb.append(style.toString)
    sb.toString
  }
}
