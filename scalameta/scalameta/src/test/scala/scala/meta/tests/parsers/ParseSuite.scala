package scala.meta.tests
package parsers

import org.scalatest._
import org.scalatest.exceptions.TestFailedException
import scala.meta._
import scala.meta.internal.parsers._
import MoreHelpers._

class ParseSuite extends FunSuite with CommonTrees {
  val EOL = scala.compat.Platform.EOL
  val escapedEOL = if (EOL == "\n") """\n""" else """\r\n"""
  def stat(code: String)(implicit dialect: Dialect) = code.applyRule(_.parseStat())
  def term(code: String)(implicit dialect: Dialect) = code.parseRule(_.expr())
  def pat(code: String)(implicit dialect: Dialect) = code.parseRule(_.pattern())
  def tpe(code: String)(implicit dialect: Dialect) = code.parseRule(_.typ())
  def topStat(code: String)(implicit dialect: Dialect) = code.parseRule(_.topStatSeq().head)
  def templStat(code: String)(implicit dialect: Dialect) = code.parseRule(_.templateStats().head)
  def blockStat(code: String)(implicit dialect: Dialect) = code.parseRule(_.blockStatSeq().head)
  def caseClause(code: String)(implicit dialect: Dialect) = code.parseRule(_.caseClause())
  def source(code: String)(implicit dialect: Dialect) = code.parseRule(_.source())
  def interceptParseErrors(stats: String*) = {
    stats.foreach { stat =>
      try {
        intercept[parsers.ParseException] {
          templStat(stat)
        }
      } catch {
        case t: TestFailedException =>
          val msg = "no exception was thrown"
          val richFeedback = t.message.map(_.replace(msg, s"$msg for '$stat'"))
          throw new TestFailedException(richFeedback.get,
            t.failedCodeStackDepth)
      }
    }
  }
}

object MoreHelpers {
  implicit class XtensionCode(code: String) {
    def applyRule[T <: Tree](rule: ScalametaParser => T)(implicit dialect: Dialect): T = {
      rule(new ScalametaParser(Input.String(code), dialect))
    }
    def parseRule[T <: Tree](rule: ScalametaParser => T)(implicit dialect: Dialect): T = {
      new ScalametaParser(Input.String(code), dialect).parseRule(rule)
    }
  }
}
