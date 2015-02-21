import org.scalatest._
import scala.meta._
import scala.meta.internal.parsers.Helpers._

class ParseSuite extends FunSuite with CommonTrees {
  val EOL = scala.compat.Platform.EOL
  val escapedEOL = if (EOL == "\n") """\n""" else """\r\n"""
  def term(code: String)(implicit dialect: Dialect) = code.parseRule(_.expr())
  def pat(code: String)(implicit dialect: Dialect) = code.parseRule(_.pattern())
  def tpe(code: String)(implicit dialect: Dialect) = code.parseRule(_.typ())
  def topStat(code: String)(implicit dialect: Dialect) = code.parseRule(_.topStatSeq().head)
  def templStat(code: String)(implicit dialect: Dialect) = code.parseRule(_.templateStats().head)
  def source(code: String)(implicit dialect: Dialect) = code.parseRule(_.compilationUnit())
  def tokenize(code: String)(implicit dialect: Dialect) = code.tokens
}

package scala.meta.internal.parsers {
  object Helpers {
    implicit class RichCode(code: String) {
      def parseRule[T](rule: Parser => T)(implicit dialect: Dialect): T = new Parser(code).parseRule(rule)
    }
  }
}
