package org.scalameta

import org.scalameta.internal.ScalaCompat.EOL

class UnreachableError(message: String) extends Error(message)

object UnreachableError {
  def raise(debuggees: Map[String, Any]): Nothing = raise(debuggees, null)

  def raise(debuggees: Map[String, Any], clue: String): Nothing = {
    val mandatory = "this code path should've been unreachable"
    throw new UnreachableError(
      if (debuggees.isEmpty && clue == null) mandatory
      else {
        val sb = new StringBuilder()
        sb.append(mandatory)
        if (clue ne null) sb.append(" (").append(clue).append(')')
        sb.append(EOL)
        ExceptionHelpers.formatDebuggees(sb, debuggees)
        sb.result()
      }
    )
  }
}
