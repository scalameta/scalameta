import org.scalatest._
import scala.reflect.syntactic._

class ParseSuite extends FunSuite {
  def parse[T](rule: SourceParser => T): String => T =
    code => new SourceParser(code).parseRule(rule)
  def term = parse(_.expr())
  def pat = parse(_.pattern())
  def tpe = parse(_.typ())
  def topStat = parse(p => p.topStat(p.in.token))
  def templStat = parse(p => p.templateStat(p.in.token))
  def compUnit = parse(_.compilationUnit())
}
