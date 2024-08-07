package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.tests.TestHelpers
import scala.meta.tests.TreeSuiteBase

import munit.Location

abstract class BaseTokenizerSuite extends TreeSuiteBase {

  def tokenize(code: String, dialect: Dialect = Scala211): Tokens = {
    val convert = scala.meta.inputs.Input.stringToInput
    val tokenize = scala.meta.tokenizers.Tokenize.scalametaTokenize

    code.tokenize(convert, tokenize, dialect).get
  }

  implicit def implicitTokenize: TestHelpers.Tokenize = new TestHelpers.Tokenize {
    override def apply(code: String)(implicit dialect: Dialect): Iterable[Token] =
      tokenize(code, dialect)
  }

  def assertTokens(code: String, dialect: Dialect = Scala211)(
      expected: PartialFunction[Tokens, Unit]
  )(implicit location: munit.Location) = {
    val obtained = tokenize(code, dialect)
    expected.lift(obtained).getOrElse(fail("Got unexpected tokens: " + obtained))
  }

  def assertLegacyScannedAsStringLines(code: String, expected: String, dialect: Dialect = Scala211)(
      implicit loc: Location
  ): Unit = {
    import scala.meta.internal.tokenizers._
    import scala.meta.tests.parsers.MoreHelpers._

    val input = code.asInput
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
