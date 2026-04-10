package org.scalameta
package invariants

import org.scalameta.internal.ScalaCompat.EOL

class InvariantFailedException(message: String) extends Exception(message)

object InvariantFailedException {
  def raise(invariant: String, failures: List[String], debuggees: Map[String, Any]): Nothing =
    raise(invariant, null, failures, debuggees)
  def raise(
      invariant: String,
      clue: String,
      failures: List[String],
      debuggees: Map[String, Any]
  ): Nothing = {
    val clueStr = if (clue eq null) "" else s" ($clue)"
    val sb = new StringBuilder()
    sb.append(
      s"""|invariant failed$clueStr:
          |when verifying $invariant
          |found that ${failures.mkString(s"\nand also ")}
          |""".stripMargin.replace("\n", EOL)
    )
    ExceptionHelpers.formatDebuggees(sb, debuggees)
    throw new InvariantFailedException(sb.result())
  }
}
