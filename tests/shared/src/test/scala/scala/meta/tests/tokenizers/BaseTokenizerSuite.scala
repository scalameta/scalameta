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
  ): Unit = assertTokensAsStructureLines(tokenize(code, dialect), expected)

  def assertTokensAsStructureLines(tokens: Tokens, expected: String)(implicit loc: Location): Unit =
    assertNoDiff(tokens.map(_.structure).mkString("\n"), expected)

  def assertTokenizedAsSyntax(code: String, expected: String, dialect: Dialect = Scala211)(implicit
      loc: Location
  ): Unit = assertTokensAsSyntax(tokenize(code, dialect), expected)

  def assertTokensAsSyntax(tokens: Tokens, expected: String)(implicit loc: Location): Unit =
    assertNoDiff(tokens.map(_.syntax).mkString, expected)

}
