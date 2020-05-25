package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers.ParseSuite
import scala.meta.internal.tokenizers.LegacyScanner
import scala.meta.internal.tokenizers.ScalametaTokenizer
import scala.meta.inputs.Input

class TokenizeSignificantIndentSuite extends ParseSuite {

  val qwerty: String = "AA"

  test("trait-indent") {

    val code =
      """|trait A:
         |  def f: Int
         |""".stripMargin

    stat(code)
  }

  test("scanner-A".only) {
    val code =
      """|trait A : 
         |  def f: Int
         |""".stripMargin
    val t = ScalametaTokenizer.toTokenize(Input.String(code), scala.meta.dialects.Dotty).get
    println(t.tokens.toList.map(_.name))
  }
}
