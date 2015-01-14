import org.scalatest._
import scala.meta.syntactic._
import scala.meta.syntactic.parsers._
import scala.meta.Dialect

class ParseSuite extends FunSuite with CommonTrees {
  val EOL = scala.compat.Platform.EOL
  val escapedEOL = if (EOL == "\n") """\n""" else """\r\n"""
  private def parse[T](code: String, rule: Parser => T)(implicit dialect: Dialect): T = new Parser(code).parseRule(rule)
  def term(code: String)(implicit dialect: Dialect) = parse(code, _.expr())(dialect)
  def pat(code: String)(implicit dialect: Dialect) = parse(code, _.pattern())(dialect)
  def tpe(code: String)(implicit dialect: Dialect) = parse(code, _.typ())(dialect)
  def topStat(code: String)(implicit dialect: Dialect) = parse(code, p => p.topStatSeq().head)(dialect)
  def templStat(code: String)(implicit dialect: Dialect) = parse(code, p => p.templateStats().head)(dialect)
  def source(code: String)(implicit dialect: Dialect) = parse(code, _.compilationUnit())(dialect)
  def tokenize(code: String)(implicit dialect: Dialect): Seq[Token] = code.tokens
}
