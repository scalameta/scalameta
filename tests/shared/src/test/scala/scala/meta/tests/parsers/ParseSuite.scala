package scala.meta.tests
package parsers

import munit._
import scala.meta._
import scala.meta.internal.parsers._
import MoreHelpers._

import org.scalameta.logger
import scala.meta.internal.trees.Origin

class ParseSuite extends TreeSuiteBase with CommonTrees {
  val EOL = scala.compat.Platform.EOL
  val escapedEOL = if (EOL == "\n") """\n""" else """\r\n"""

  // This should eventually be replaced by DiffAssertions.assertNoDiff
  def assertSameLines(actual: String, expected: String) = {
    val actualLines = actual.linesIterator.toList
    val expectedLines = expected.linesIterator.toList
    assert(actualLines == expectedLines)
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
    stats.foreach { stat =>
      try {
        intercept[parsers.ParseException] {
          templStat(stat)
        }
      } catch {
        case scala.util.control.NonFatal(t) =>
          val msg = "no exception was thrown"
          val richFeedback = t.getMessage.replace(msg, s"$msg for '$stat'")
          fail(richFeedback)
      }
    }
  }
  def checkError(stat: String)(implicit dialect: Dialect) =
    test(logger.revealWhitespace(stat).take(50)) { interceptParseErrors(stat) }
  def checkOK(stat: String)(implicit dialect: Dialect) =
    test(logger.revealWhitespace(stat).take(50)) { templStat(stat) }

  protected def checkParsedTree[T <: Tree](
      code: String,
      f: ScalametaParser => T,
      syntax: String = null
  )(tree: Tree)(implicit loc: munit.Location): Unit =
    checkTree(parseRule(code, f), syntax)(tree)

  protected def checkStat(code: String, syntax: String = null)(tree: Tree)(
      implicit loc: munit.Location
  ): Unit =
    checkParsedTree(code, _.entrypointStat(), syntax)(tree)

  protected def runTestError[T <: Tree](
      code: String,
      expected: String
  )(implicit parser: (String, Dialect) => T, dialect: Dialect): Unit = {
    val error = intercept[ParseException] {
      val result = parser(code, dialect)
      throw new ParseException(
        Position.None,
        s"Statement ${code} should not parse! Got result ${result.structure}"
      )
    }
    assert(
      error.getMessage().contains(expected),
      s"Expected [${error.getMessage}] to contain [${expected}]."
    )
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
