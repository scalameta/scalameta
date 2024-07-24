package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.tests.TreeSuiteBase

import munit.Location

abstract class BaseTokenizerSuite extends TreeSuiteBase {

  import BaseTokenizerSuite._

  def tokenize(code: String, dialect: Dialect = Scala211): Tokens = {
    val convert = scala.meta.inputs.Input.stringToInput
    val tokenize = scala.meta.tokenizers.Tokenize.scalametaTokenize

    code.tokenize(convert, tokenize, dialect).get
  }

  def assertTokens(code: String, dialect: Dialect = Scala211)(
      expected: PartialFunction[Tokens, Unit]
  )(implicit location: munit.Location) = {
    val obtained = tokenize(code, dialect)
    expected.lift(obtained).getOrElse(fail("Got unexpected tokens: " + obtained))
  }

  def assertTokenizedAsStructureLines(code: String, expected: String, dialect: Dialect = Scala211)(
      implicit loc: Location
  ): Unit = assertTokensAsStructureLines(tokenize(code, dialect), expected)

  def assertTokensAsStructureLines(tokens: Iterable[Token], expected: String)(implicit
      loc: Location
  ): Unit = assertNoDiff(tokensAsStructureLines(tokens.iterator), expected)

  def assertTokenizedAsSyntax(code: String, expected: String, dialect: Dialect = Scala211)(implicit
      loc: Location
  ): Unit = assertTokensAsSyntax(tokenize(code, dialect), expected)

  def assertTokensAsSyntax(tokens: Iterable[Token], expected: String)(implicit
      loc: Location
  ): Unit = assertNoDiff(tokensAsSyntax(tokens.iterator), expected)

  def assertLegacyScannedAsStringLines(code: String, expected: String, dialect: Dialect = Scala211)(
      implicit loc: Location
  ): Unit = {
    import scala.meta.internal.tokenizers._
    import scala.meta.tests.parsers.MoreHelpers._

    val input = code.asInput
    implicit val reporter: Reporter = Reporter(input)
    val scanner = new LegacyScanner(input = input, dialect = dialect)
    scanner.initialize()
    val sb = new StringBuilder
    while ({
      val ltd = scanner.nextTokenOrEof()
      sb.append(ltd).append('\n')
      ltd.ok
    }) {}
    assertEquals(sb.result(), expected)
  }

}

object BaseTokenizerSuite {

  def tokensAsStructureLines(tokens: Iterator[Token]) = tokens.map(_.structure).mkString("\n")

  def tokensAsSyntax(tokens: Iterator[Token]) = tokens.map(_.syntax).mkString

}
