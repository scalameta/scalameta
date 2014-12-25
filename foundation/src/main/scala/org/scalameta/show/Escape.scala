package org.scalameta.show

object escape {
  def apply(s: String): String = {
    // TODO: comprehensive handling
    val codepage = Map(
      "\t" -> "\\t",
      "\b" -> "\\b",
      "\n" -> "\\n",
      "\r" -> "\\r",
      "\f" -> "\\f",
      "\\" -> "\\\\"
    )
    s.flatMap(c => codepage.getOrElse(c.toString, c.toString))
  }
}