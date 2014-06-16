import org.scalatest._
import scala.meta.syntactic.parsers._

class ParseSuite extends FunSuite with CommonTrees {
  def parse[T](rule: Parser => T): String => T =
    code => new Parser(code).parseRule(rule)
  def term = parse(_.expr())
  def pat = parse(_.pattern())
  def tpe = parse(_.typ())
  def topStat = parse(p => p.topStat(p.in.token))
  def templStat = parse(p => p.templateStat(p.in.token))
  def compUnit = parse(_.compilationUnit())
}
