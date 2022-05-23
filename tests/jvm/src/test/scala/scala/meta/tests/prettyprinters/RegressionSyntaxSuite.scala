package scala.meta.tests.prettyprinters

import scala.meta.tests.parsers.ParseSuite

import scala.meta._
import munit.TestOptions

class RegressionSyntaxSuite extends ParseSuite {

  def check(
      testOpts: TestOptions,
      code: String,
      expected: String,
      newDialect: Option[Dialect] = None
  )(implicit dialect: Dialect) = test(testOpts) {
    val obtained = code.parse[Stat].get
    val reprintedCode =
      scala.meta.internal.prettyprinters.TreeSyntax
        .reprint(obtained)(newDialect.getOrElse(dialects.Scala213))
        .toString
    assertEquals(reprintedCode, expected)
  }

  check("question-mark", "type T = List[?]", "type T = List[?]")
  check("underscore", "type T = List[_]", "type T = List[_]")
  check(
    "new-dialect-scala3",
    "type T = List[_]",
    "type T = List[?]",
    newDialect = Some(dialects.Scala3)
  )(
    dialects.Scala211
  )

  check(
    "new-dialect-scala213",
    "type T = List[_]",
    "type T = List[_]",
    newDialect = Some(dialects.Scala213)
  )(
    dialects.Scala211
  )
  check(
    "new-dialect-scala213-from-scala3",
    "type T = List[?]",
    "type T = List[?]",
    newDialect = Some(dialects.Scala213)
  )(
    dialects.Scala3
  )

  test("no-origin") {
    assertEquals(
      Type.Placeholder(Type.Bounds(None, None)).toString(),
      "_"
    )
  }
}
