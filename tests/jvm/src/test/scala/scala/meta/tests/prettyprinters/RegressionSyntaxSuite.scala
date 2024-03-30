package scala.meta.tests.prettyprinters

import scala.meta._
import scala.meta.tests.parsers.ParseSuite

import munit.TestOptions

class RegressionSyntaxSuite extends ParseSuite {

  def check(
      testOpts: TestOptions,
      code: String,
      expected: String,
      newDialect: Dialect = dialects.Scala213
  )(implicit dialect: Dialect) = test(testOpts) {
    val obtained = code.parse[Stat].get
    val reprintedCode = obtained.reprint(newDialect)
    assertEquals(reprintedCode, expected)
  }

  check("question-mark 211->213", "type T = List[?]", "type T = List[`?`]")(dialects.Scala211)
  check("underscore", "type T = List[_]", "type T = List[_]")

  check("211->3x", "type T = List[_]", "type T = List[?]", dialects.Scala3Future)(dialects.Scala211)
  check("211->30", "type T = List[_]", "type T = List[_]", dialects.Scala30)(dialects.Scala211)
  check("211->213", "type T = List[_]", "type T = List[_]", dialects.Scala213)(dialects.Scala211)
  check("3->213", "type T = List[?]", "type T = List[?]", dialects.Scala213)(dialects.Scala3)
  check("3->211", "type T = List[?]", "type T = List[?]", dialects.Scala213)(dialects.Scala3)

  test("no-origin")(assertEquals(Type.AnonymousParam(None).toString(), "_"))
}
