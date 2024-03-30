package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.tests.TreeSuiteBase

import munit.Location

abstract class BaseTokenizerSuite extends TreeSuiteBase {

  def tokenize(code: String, dialect: Dialect = Scala211): Tokens = {
    val convert = scala.meta.inputs.Input.stringToInput
    val tokenize = scala.meta.tokenizers.Tokenize.scalametaTokenize

    code.tokenize(convert, tokenize, dialect).get
  }

  def assertTokens(code: String, dialect: Dialect = Scala211)(
      expected: PartialFunction[Tokens, Unit]
  ) = {
    val obtained = tokenize(code, dialect)
    expected.lift(obtained).getOrElse(fail("Got unexpected tokens: " + obtained))
  }

  def assertTokenizedAsStructureLines(code: String, expected: String, dialect: Dialect = Scala211)(
      implicit loc: Location
  ): Unit = assertNoDiff(tokenize(code, dialect).map(_.structure).mkString("\n"), expected)

  def assertTokenizedAsSyntax(code: String, expected: String, dialect: Dialect = Scala211)(implicit
      loc: Location
  ): Unit = assertNoDiff(tokenize(code, dialect).map(_.syntax).mkString, expected)

  implicit class ImplicitString(value: String) {
    def tq(repl: String): String = value.replace(repl, "\"\"\"")
  }

}
