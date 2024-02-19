package org.scalameta

import org.scalameta.internal.ScalaCompat.EOL

class UnreachableError(message: String) extends Error(message)

object UnreachableError {
  def raise(debuggees: Map[String, Any]): Nothing = {
    def relevantValues =
      debuggees.toList.sortBy(_._1).map({ case (k, v) => s"where $k = $v" }).mkString(EOL)
    val mandatory = "this code path should've been unreachable"
    val optional = if (debuggees.nonEmpty) EOL + relevantValues else ""
    throw new UnreachableError(mandatory + optional)
  }
}
