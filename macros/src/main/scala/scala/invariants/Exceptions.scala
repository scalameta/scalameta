package scala.invariants

class InvariantFailedException(message: String) extends Exception(message)
object InvariantFailedException {
  def raise(invariant: String, failures: List[String], context: Option[Any]): InvariantFailedException = {
    val mandatory = s"""
      |invariant failed:
      |when verifying $invariant
      |found that ${failures.head}
    """.trim.stripMargin
    val optionalFailures = failures.tail.headOption.map(_ => "\n" + failures.tail.map("and also " + _).mkString("\n")).getOrElse("")
    val optionalContext = context.map(ctx => "\n" + "context is " + ctx).getOrElse("")
    throw new InvariantFailedException(mandatory + optionalFailures + optionalContext)
  }
}
