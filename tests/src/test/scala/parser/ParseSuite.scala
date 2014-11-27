import org.scalatest._
import scala.meta.syntactic.parsers._

class ParseSuite extends FunSuite with CommonTrees {
  val EOL = scala.compat.Platform.EOL
  val escapedEOL = if (EOL == "\n") """\n""" else """\r\n"""
  def parse[T](rule: Parser => T): String => T =
    code => new Parser(code).parseRule(rule)
  def term = parse(_.expr())
  def pat = parse(_.pattern())
  def tpe = parse(_.typ())
  def topStat = parse(p => p.topStatSeq().head)
  def templStat = parse(p => p.templateStats().head)
  def source = parse(_.compilationUnit())
}
