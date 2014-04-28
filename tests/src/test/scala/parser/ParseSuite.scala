import org.scalatest.FunSuite
import cbc._

class ParseSuite extends FunSuite {
  def parse[T](rule: SourceParser => T): String => T = { (code: String) =>
    val parser = new SourceParser(StringSource(code))
    parser.parseRule(rule)
  }

  def parseTerm = parse(_.expr())
}
