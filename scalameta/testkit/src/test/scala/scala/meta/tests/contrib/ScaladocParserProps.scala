package scala.meta.tests
package contrib

import org.scalatest.FunSuite
import scala.meta.testkit._
import scala.meta.tokens.Token.Comment
import scala.meta._
import scala.meta.contrib._
import scala.util.Try

class ScaladocParserProps extends FunSuite {

  test("parser does not crash") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      val commentTokens: List[Comment] = ast.tokens.toList.collect {
        case c: Comment => c
      }
      if (commentTokens.map(c => Try(ScaladocParser.parseScaladoc(c))).exists(_.isFailure)) {
        List(ast)
      } else {
        Nil
      }
    }
    assert(errors.isEmpty)
  }
}
