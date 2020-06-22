package scala.meta.tests.contrib

import scala.meta._
import scala.meta.internal.parsers.ScaladocParser
import scala.meta.testkit._
import scala.util.Try

import munit.FunSuite

class ScaladocParserPropsSuite extends FunSuite {

  test("parser does not crash") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      val failed = ast.tokens.toList.exists {
        case c: Token.Comment => Try(ScaladocParser.parse(c.syntax)).isFailure
        case _ => false
      }
      if (failed) List(ast) else Nil
    }
    assert(errors.isEmpty)
  }

}
