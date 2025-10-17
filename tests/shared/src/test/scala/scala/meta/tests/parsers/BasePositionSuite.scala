package scala.meta.tests.parsers

import scala.meta.Dialect
import scala.meta.parsers.Parse

import munit.{Location, TestOptions}

abstract class BasePositionSuite(defaultDialect: Dialect) extends ParseSuite {
  import scala.meta._
  import scala.meta.tests.parsers.MoreHelpers._

  def checkPositions[T <: Tree: Parse](code: TestOptions)(implicit loc: Location): Unit =
    checkPositions[T](code, "")

  /**
   * Position tests assert that the position of tree nodes enclose the expected source range.
   *
   * Position tests are declared like this
   * {{{
   *   checkPositions[Type](
   *     "[X] =>> (X, X)",
   *     """|Type.Bounds [X@@] =>> (X, X)
   *        |Type.Tuple (X, X)
   *        |""".stripMargin
   *   )
   * }}}
   *
   * Every line in the output format shows the AST tree node type and the tokens for that tree node.
   * Offset positions are rendered as "@@".
   *
   * Below is an example bug that is easy to catch with position tests.
   *
   * {{{
   *   checkPositions[Stat](
   *     "trait A { self: B => }",
   *     """|Ctor.Primary trait A @@{ self: B => }
   *        |Name.Anonymous {
   *        |Template { self: B => }
   *        |Self self: B
   *        |""".stripMargin
   *   )
   * }}}
   *
   * Observe that the line {{{Name.Anonymous {}}} indicates that the position of the anonymous name
   * encloses the `{` token. The correct output should be
   * {{{Name.Anonymous trait A @@{ self: B =>}}}.
   */
  def checkPositions[T <: Tree: Parse](
      code: TestOptions,
      expected: String,
      expectedTokens: String = "",
      showPosition: Boolean = false,
      showFieldName: Boolean = false
  )(implicit loc: Location): Unit = test(code) {
    implicit val D = defaultDialect
    if (expectedTokens.nonEmpty) assertTokenizedAsStructureLines(code.name, expectedTokens)
    val tree = code.name.asInput.parse[T]
      .fold(x => fail("parse failure", x.details), MoreHelpers.requireNonEmptyOrigin(_))
    assertPositions(tree, expected, showPosition = showPosition, showFieldName = showFieldName)
  }
}
