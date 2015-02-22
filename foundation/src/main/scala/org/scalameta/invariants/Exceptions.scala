package org.scalameta.invariants

import scala.compat.Platform.EOL

class InvariantFailedException(message: String) extends Exception(message)
object InvariantFailedException {
  def raise(invariant: String, failures: List[String], debuggees: Map[String, Any]): Nothing = {
    val mandatory = s"""
      |invariant failed:
      |when verifying $invariant
      |found that ${failures.head}
    """.trim.stripMargin
    val optionalFailures = failures.tail.headOption.map(_ => EOL + failures.tail.map("and also " + _).mkString(EOL)).getOrElse("")
    val optionalLocals = if (debuggees.nonEmpty) EOL + debuggees.toList.sortBy(_._1).map({ case (k, v) => s"where $k = $v"}).mkString(EOL)
    throw new InvariantFailedException(mandatory + optionalFailures + optionalLocals)
  }
}
