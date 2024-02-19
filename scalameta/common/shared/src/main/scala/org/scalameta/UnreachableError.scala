package org.scalameta

import org.scalameta.internal.ScalaCompat.EOL

class UnreachableError(message: String) extends Error(message)

object UnreachableError {
  def raise(debuggees: Map[String, Any]): Nothing = {
    val mandatory = "this code path should've been unreachable"
    throw new UnreachableError(
      if (debuggees.isEmpty) mandatory
      else {
        val sb = new StringBuilder()
        sb.append(mandatory).append(EOL)
        ExceptionHelpers.formatDebuggees(sb, debuggees)
        sb.toString()
      }
    )
  }
}
