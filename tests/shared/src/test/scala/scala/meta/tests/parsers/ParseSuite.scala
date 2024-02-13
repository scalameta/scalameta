package scala.meta.tests
package parsers

import munit._
import scala.meta._
import scala.meta.internal.parsers._
import scala.meta.trees.Origin
import MoreHelpers._

import org.scalameta.logger

class ParseSuite extends TreeSuiteBase with CommonTrees {
  val EOL = org.scalameta.internal.ScalaCompat.EOL
  val escapedEOL = if (EOL == "\n") """\n""" else """\r\n"""

  implicit def parseStat(code: String, dialect: Dialect): Stat = templStat(code)(dialect)
  implicit def parseSource(code: String, dialect: Dialect): Source = source(code)(dialect)
  implicit def parseType(code: String, dialect: Dialect): Type = tpe(code)(dialect)
  implicit def parsePat(code: String, dialect: Dialect): Pat = pat(code)(dialect)
  implicit def parseCaseTree(code: String, dialect: Dialect): Case = parseCase(code)(dialect)

  // This should eventually be replaced by DiffAssertions.assertNoDiff
  def assertSameLines(actual: String, expected: String)(implicit loc: munit.Location) = {
    val actualLines = actual.linesIterator.toList
    val expectedLines = expected.linesIterator.toList
    assertEquals(actualLines, expectedLines)
  }

  def stat(code: String)(implicit dialect: Dialect) = code.applyRule(_.parseStat())

  def parseRule[T <: Tree](code: String, f: ScalametaParser => T)(implicit dialect: Dialect) =
    code.parseRule(f)
  def term(code: String)(implicit dialect: Dialect) = parseRule(code, _.expr())
  def pat(code: String)(implicit dialect: Dialect) = parseRule(code, _.pattern())
  def patternTyp(code: String)(implicit dialect: Dialect) = parseRule(code, _.patternTyp())
  def tpe(code: String)(implicit dialect: Dialect) = parseRule(code, _.typ())
  def topStat(code: String)(implicit dialect: Dialect) =
    parseRule(code, p => p.statSeq(p.topStat).head)
  def templStat(code: String)(implicit dialect: Dialect) =
    parseRule(code, p => p.statSeq(p.templateStat()).head)
  def blockStat(code: String)(implicit dialect: Dialect) = parseRule(code, _.blockStatSeq().head)
  def parseCase(code: String)(implicit dialect: Dialect) = code.applyRule(_.parseCase())
  def source(code: String)(implicit dialect: Dialect) = parseRule(code, _.source())

  def ammonite(code: String)(implicit dialect: Dialect) =
    code.asAmmoniteInput.parseRule(_.entryPointAmmonite())

  def interceptParseErrors(stats: String*)(implicit loc: munit.Location) = {
    stats.foreach(interceptParseError(_))
  }

  def interceptParseError(stat: String)(implicit loc: munit.Location): String =
    try {
      intercept[parsers.ParseException] { templStat(stat) }.getMessage().replace("\r", "")
    } catch {
      case scala.util.control.NonFatal(t) =>
        val msg = "no exception was thrown"
        val richFeedback = t.getMessage.replace(msg, s"$msg for '$stat'")
        fail(richFeedback)
    }

  def checkError(stat: String)(implicit dialect: Dialect) =
    test(logger.revealWhitespace(stat).take(50)) { interceptParseError(stat) }
  def checkOK(stat: String)(implicit dialect: Dialect) =
    test(logger.revealWhitespace(stat).take(50)) { templStat(stat) }

  protected def checkParsedTree[T <: Tree](
      code: String,
      f: ScalametaParser => T,
      syntax: String = null
  )(tree: Tree)(implicit loc: munit.Location, dialect: Dialect): Unit =
    checkTree(parseRule(code, f), syntax)(tree)

  protected def checkStat(
      code: String,
      syntax: String = null
  )(tree: Tree)(implicit loc: munit.Location, dialect: Dialect): Unit =
    checkParsedTree(code, _.entrypointStat(), syntax)(tree)

  protected def runTestError[T <: Tree](
      code: String,
      expected: String
  )(implicit parser: (String, Dialect) => T, dialect: Dialect, loc: munit.Location): Unit = {
    val error = intercept[inputs.InputException] {
      val result = parser(code, dialect)
      throw new ParseException(
        Position.None,
        s"Statement ${code} should not parse! Got result ${result.structure}"
      )
    }
    val obtained = error.getMessage().replace("\r", "")
    assert(
      obtained.contains(expected),
      s"Expected [$obtained] to contain [${expected}]."
    )
  }

  protected def matchSubStructure[T <: Tree](
      code: String,
      expected: PartialFunction[Tree, Unit]
  )(implicit parser: (String, Dialect) => T, dialect: Dialect, loc: munit.Location): Unit = {
    val obtained = parser(code, dialect)
    expected.lift(obtained).getOrElse(fail("Got unexpected tree: " + obtained))
  }

  protected def matchSubStructureWithDialect[T <: Tree](
      code: String,
      expected: PartialFunction[Tree, Unit],
      dialect: Dialect
  )(implicit parser: (String, Dialect) => T, loc: munit.Location): Unit =
    matchSubStructure(code, expected)(parser, dialect, loc)

  /**
   * Check if code can be parsed to expected syntax tree.
   *
   * @see
   *   runTestAssert(code, assertLayout)(expected)
   */
  protected def runTestAssert[T <: Tree](code: String, assertLayout: String = null)(
      expected: Tree
  )(implicit loc: munit.Location, parser: (String, Dialect) => T, dialect: Dialect): Unit = {
    val assertLayoutOpt = Some(if (assertLayout eq null) code else assertLayout)
    runTestAssert[T](code, assertLayoutOpt)(expected)
  }

  /**
   * General method used to assert a given 'code' parses to expected tree structure and back. We
   * cannot assert trees by equality(==) that's why we check if they are identical by asserting
   * their structure representation and optionally syntax. If expectedLayout is provided then we
   * print back generated tree structure and assert generated text is equal to expectedLayout (in
   * most cases it should be the same as 'code' param but sometimes formatting is a little different
   * or for safety () are added). If you are not interested in asserting layout just provide None.
   * After printing generated tree to text representation we parse it again. This ensures that
   * invariant holds: parse(code) = parse(print(parse(code))) Reprint cannot be handled by
   * `tree.syntax` because syntax is cached by default and would not be reprinted but only input
   * code would be returned.
   *
   * @param code
   *   valid scala code
   * @param assertLayout
   *   string representation of code to be printed
   * @param expected
   *   provided 'code' should parse to this tree structure
   * @param parser
   *   Function used to convert code into structured tree
   */
  protected def runTestAssert[T <: Tree](code: String, assertLayout: Option[String])(
      expected: Tree
  )(implicit loc: munit.Location, parser: (String, Dialect) => T, dialect: Dialect): Unit = {
    val expectedStructure = expected.structure
    val obtained: T = parser(code, dialect)
    MoreHelpers.requireNonEmptyOrigin(obtained)

    // check bijection
    val reprintedCode = obtained.reprint
    assertLayout.foreach(assertNoDiff(reprintedCode, _, s"Reprinted syntax:\n $expectedStructure"))

    assertNoDiff(obtained.structure, expectedStructure, "Generated stat")
    val obtainedAgain: T = parser(reprintedCode, dialect)
    assertNoDiff(obtainedAgain.structure, expectedStructure, s"Reprinted stat: \n${reprintedCode}")
  }

  protected def parseAndCheckTree[T <: Tree](code: String, syntax: String = null)(
      expected: Tree
  )(implicit loc: munit.Location, parser: (String, Dialect) => T, dialect: Dialect): Unit = {
    val obtained: T = parser(code, dialect)
    MoreHelpers.requireNonEmptyOrigin(obtained)
    checkTree(obtained, syntax)(expected)
  }

  protected def checkWithOriginalSyntax[T <: Tree](tree: T, originalOpt: String = null)(
      reprinted: String,
      reprintedFails: String = null
  )(implicit loc: munit.Location, parser: (String, Dialect) => T, dialect: Dialect): Unit = {
    val original = Option(originalOpt).getOrElse(reprinted)
    assertOriginalSyntax(tree, original)
    if (reprintedFails ne null) {
      runTestError[T](reprinted, reprintedFails)
      if (original != reprinted)
        checkTree(parser(original, dialect), reprinted)(tree)
    } else
      runTestAssert[T](original, reprinted)(tree)
  }

}

object MoreHelpers {
  def requireNonEmptyOrigin(tree: Tree)(implicit dialect: Dialect): tree.type = {
    val missingOrigin = tree.collect {
      case t if t.origin == Origin.None => t
    }
    Assertions.assertEquals(
      missingOrigin.map(_.structure),
      Nil,
      "Expected all trees to have non-empty `.origin`.\n" +
        "To fix this failure, update ScalametaParser to use `autoPos()` where the trees below got constructed.\n" +
        "Pro tip: you may also want to add a PositionSuite test for this tree node to verify that the position you set is correct."
    )
    tree
  }
  implicit class XtensionCode(private val code: String) extends AnyVal {
    def asInput: Input = Input.String(code)
    def asAmmoniteInput: Input = Input.Ammonite(asInput)
    def applyRule[T <: Tree](rule: ScalametaParser => T)(implicit dialect: Dialect): T = {
      asInput.applyRule(rule)
    }
    def parseRule[T <: Tree](rule: ScalametaParser => T)(implicit dialect: Dialect): T = {
      asInput.parseRule(rule)
    }
  }
  implicit class XtensionInput(private val input: Input) extends AnyVal {
    def applyRule[T <: Tree](rule: ScalametaParser => T)(implicit dialect: Dialect): T = {
      requireNonEmptyOrigin(rule(new ScalametaParser(input)))
    }
    def parseRule[T <: Tree](rule: ScalametaParser => T)(implicit dialect: Dialect): T = {
      applyRule(_.parseRule(rule))
    }
  }
}
