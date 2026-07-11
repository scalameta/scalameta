package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.tests.{TestHelpers, TreeSuiteBase}

abstract class BaseTokenizerSuite extends TreeSuiteBase {

  protected val dialect: Dialect = dialects.Scala211

  implicit def implicitTokenize: TestHelpers.Tokenize = new TestHelpers.Tokenize {
    override def apply(code: String)(implicit dialect: Dialect): Iterable[Token] = tokenize(code)
  }

  def assertTokens(code: String, dialect: Dialect = this.dialect)(
      expected: PartialFunction[Tokens, Unit],
  )(implicit location: munit.Location) = {
    implicit val implicitDialect = dialect
    val obtained = tokenize(code)
    expected.lift(obtained).getOrElse(fail("Got unexpected tokens: " + obtained))
  }

  protected def testTokenizedStructLines(
      options: munit.TestOptions,
      testDialect: Dialect = dialect,
  )(codeTemplate: String, expected: String)(implicit loc: munit.Location): Unit = test(options) {
    implicit val dialect: Dialect = testDialect
    assertTokenizedAsStructureLines(codeTemplate.replace("'''", "\"\"\""), expected.nl2lf)
  }

  protected def testTokenizedStructLinesEscaped(
      options: munit.TestOptions,
      testDialect: Dialect = dialect,
  )(codeTemplate: String, expected: String)(implicit loc: munit.Location): Unit =
    testTokenizedStructLines(options, testDialect)(
      codeTemplate.replace("\\\\", "\\"),
      expected.replace("\\\\", "\\"),
    )

}
