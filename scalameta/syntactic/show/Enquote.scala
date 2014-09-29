package scala.meta
package syntactic.show
package internal

sealed trait QuoteStyle
case object SingleQuotes extends QuoteStyle { override def toString = "'" }
case object DoubleQuotes extends QuoteStyle { override def toString = "\"" }
case object TripleQuotes extends QuoteStyle { override def toString = "\"\"\"" }

object enquote {
  def apply(s: String, style: QuoteStyle): String = {
    // TODO: comprehensive handling (e.g. escape triple quotes in triple quotes)
    val codepage = Map(
      "\t" -> "\\t",
      "\b" -> "\\b",
      "\n" -> "\\n",
      "\r" -> "\\r",
      "\f" -> "\\f",
      "\"" -> (if (style == DoubleQuotes) "\\\"" else "\""),
      "\'" -> (if (style == SingleQuotes) "\\\'" else "\'"),
      "\\" -> "\\\\"
    )
    val escaped = if (style != TripleQuotes) s.flatMap(c => codepage.getOrElse(c.toString, c.toString)) else s
    style.toString + escaped + style.toString
  }
}