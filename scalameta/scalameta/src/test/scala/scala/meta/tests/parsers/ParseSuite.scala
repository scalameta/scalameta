package scala.meta.tests
package parsers

import org.scalatest._
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
