package scala.meta.tests
package parsers

import munit._
import scala.meta._
import scala.meta.internal.parsers._
import MoreHelpers._

import org.scalameta.logger
import scala.meta.internal.trees.Origin

class ParseSuite extends FunSuite with CommonTrees {
  val EOL = scala.compat.Platform.EOL
  val escapedEOL = if (EOL == "\n") """\n""" else """\r\n"""

  //This should eventually be replaced by DiffAssertions.assertNoDiff
  def assertSameLines(actual: String, expected: String) = {
    val actualLines = actual.linesIterator.toList
    val expectedLines = expected.linesIterator.toList
    assert(actualLines == expectedLines)
  }

  def stat(code: String)(implicit dialect: Dialect) = code.applyRule(_.parseStat())
  def term(code: String)(implicit dialect: Dialect) = code.parseRule(_.expr())
  def pat(code: String)(implicit dialect: Dialect) = code.parseRule(_.pattern())
  def tpe(code: String)(implicit dialect: Dialect) = code.parseRule(_.typ())
  def topStat(code: String)(implicit dialect: Dialect) = code.parseRule(_.topStatSeq().head)
  def templStat(code: String)(implicit dialect: Dialect) = code.parseRule(_.templateStats().head)
  def blockStat(code: String)(implicit dialect: Dialect) = code.parseRule(_.blockStatSeq().head)
  def caseClause(code: String)(implicit dialect: Dialect) = code.parseRule(_.caseClause())
  def source(code: String)(implicit dialect: Dialect) = code.parseRule(_.source())
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
  implicit class XtensionCode(code: String) {
    def applyRule[T <: Tree](rule: ScalametaParser => T)(implicit dialect: Dialect): T = {
      requireNonEmptyOrigin(rule(new ScalametaParser(Input.String(code))))
    }
    def parseRule[T <: Tree](rule: ScalametaParser => T)(implicit dialect: Dialect): T = {
      requireNonEmptyOrigin(new ScalametaParser(Input.String(code)).parseRule(rule))
    }
  }
}
