package scala.meta.contrib

import org.scalatest.FunSuite

import scala.meta.contrib.DocToken._
import scala.meta.testkit._
import scala.meta.tokens.Token.Comment
import scala.meta.{Defn, _}
import scala.util.Try

class ScaladocParserProps extends FunSuite {

  test("parser does not crash") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      val commentTokens: Seq[Comment] = ast.tokens.collect {
        case c: Comment => c
      }
      if (commentTokens.map(c => Try(ScaladocParser.parseScaladoc(c))).exists(_.isFailure)) {
        Seq(ast)
      } else {
        Nil
      }
    }
    assert(errors.isEmpty)
  }
}
