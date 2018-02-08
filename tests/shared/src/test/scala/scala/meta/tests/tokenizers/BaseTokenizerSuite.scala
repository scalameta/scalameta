package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.testkit.DiffAssertions
import org.scalatest.FunSuite
import org.scalameta.logger._

abstract class BaseTokenizerSuite extends FunSuite with DiffAssertions {

  def tokenize(code: String): Tokens = {
    val convert = scala.meta.inputs.stringToInput
    val tokenize = scala.meta.tokenizers.Tokenize.scalametaTokenize
    val dialect = Scala211
    code.tokenize(convert, tokenize, dialect).get
  }

  def checkStructure(original: String, expected: String): Unit = {
    test(revealWhitespace(original)) {
      val tokens = tokenize(original)
      println(tokens.syntax)
      val obtained = tokens
        .map(t => f"${t.productPrefix}%-15s ${revealWhitespace(t.show[Structure])}")
        .mkString("\n")
       println(obtained)
      assertNoDiff(obtained, expected)
    }
  }

}
