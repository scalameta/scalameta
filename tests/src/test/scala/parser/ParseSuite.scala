import org.scalatest._
import cbc._

class ParseSuite extends FunSuite {
  def parse[T](rule: SourceParser => T): String => T =
    code => (new SourceParser(code)).parseRule(rule)
  def term = parse(_.expr())
  def pat = parse(_.pattern())
  def tpe = parse(_.typ())
}
