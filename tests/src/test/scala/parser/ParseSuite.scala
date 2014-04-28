import org.scalatest.FunSuite
import cbc.util.BatchSourceFile
import cbc.Parsers.SourceFileParser

class ParseSuite extends FunSuite {
  def parse[T](rule: SourceFileParser => T): String => T = { (code: String) =>
    val source = new BatchSourceFile("", code)
    val parser = new SourceFileParser(source)
    parser.parseRule(rule)
  }

  def parseTerm = parse(_.expr())
}
