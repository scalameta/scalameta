package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.tests.{TestHelpers, TreeSuiteBase}

import munit.Location

abstract class BaseTokenizerSuite extends TreeSuiteBase {

  protected val dialect: Dialect = dialects.Scala211

  implicit def implicitTokenize: TestHelpers.Tokenize = new TestHelpers.Tokenize {
    override def apply(code: String)(implicit dialect: Dialect): Iterable[Token] = tokenize(code)
  }

  def assertTokens(code: String, dialect: Dialect = this.dialect)(
      expected: PartialFunction[Tokens, Unit]
  )(implicit location: munit.Location) = {
    implicit val implicitDialect = dialect
    val obtained = tokenize(code)
    expected.lift(obtained).getOrElse(fail("Got unexpected tokens: " + obtained))
  }

  def assertLegacyScannedAsStringLines(
      code: String,
      expected: String,
      dialect: Dialect = this.dialect
  )(implicit loc: Location): Unit = {
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

  protected def testTokenizedStructLines(
      options: munit.TestOptions,
      testDialect: Dialect = dialect
  )(codeTemplate: String, expected: String)(implicit loc: munit.Location): Unit = test(options) {
    implicit val dialect: Dialect = testDialect
    assertTokenizedAsStructureLines(codeTemplate.replace("'''", "\"\"\""), expected.nl2lf)
  }

  protected def testTokenizedStructLinesEscaped(
      options: munit.TestOptions,
      testDialect: Dialect = dialect
  )(codeTemplate: String, expected: String)(implicit loc: munit.Location): Unit =
    testTokenizedStructLines(options, testDialect)(
      codeTemplate.replace("\\\\", "\\"),
      expected.replace("\\\\", "\\")
    )

}
