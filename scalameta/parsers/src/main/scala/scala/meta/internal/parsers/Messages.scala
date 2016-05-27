package scala.meta
package internal
package parsers

import scala.compat.Platform.EOL

// TODO: Would be nice to take all errors/warnings in tokenization and parsing,
// and then externalize them into dedicated objects.
object Messages {
  def QuasiquoteRankMismatch(found: Int, required: Int, hint: String = ""): String = {
    val s_found = "." * (found + 1) + "$"
    val s_required = 0.to(required + 1).filter(_ != 1).map(i => "." * i + "$").mkString(" or ")
    var message = s"rank mismatch when unquoting;$EOL found   : $s_found$EOL required: $s_required"
    if (hint.nonEmpty) message = message + EOL + hint
    message
  }

  def QuasiquoteAdjacentEllipsesInPattern(rank: Int): String = {
    val hint = {
      "Note that you can extract a sequence into an unquote when pattern matching," + EOL+
      "it just cannot follow another sequence either directly or indirectly."
    }
    QuasiquoteRankMismatch(rank, rank - 1, hint)
  }
}