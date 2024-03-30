package scala.meta.tests
package parsers

import scala.meta._

class QuasiquoteSuite extends ParseSuite {
  locally {
    implicit val dialect = dialects.Scala211.unquoteTerm(multiline = false)

    test("single-line disallow normal escaping") {
      assertTree(term("\\n"))(Term.Select(tname("\\"), tname("n")))
    }

    test("single-line allow unicode escaping") { assertTree(term("\\u0061"))(tname("a")) }

    test("single-line disallow line breaks") {
      intercept[TokenizeException] { term("foo + \n bar") }
    }

    test("single-line disallow double quotes") { intercept[TokenizeException] { term("\"a\"") } }
  }

  locally {
    implicit val dialect = dialects.Scala211.unquoteTerm(multiline = true)

    test("multi-line disallow do normal escaping") {
      assertTree(term("\\n"))(Term.Select(tname("\\"), tname("n")))
    }

    test("multi-line allow unicode escaping") { assertTree(term("\\u0061"))(tname("a")) }

    test("multi-line allow line breaks") {
      assertTree(term("foo + \n bar"))(
        Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar")))
      )
    }

    test("multi-line allow double quotes") { assertTree(term("\"a\""))(str("a")) }
  }
}
