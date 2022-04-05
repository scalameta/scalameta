package scala.meta.tests
package parsers

import scala.meta._

class QuasiquoteSuite extends ParseSuite {
  locally {
    implicit val dialect = dialects.QuasiquoteTerm(dialects.Scala211, multiline = false)

    test("single-line disallow normal escaping") {
      assertTree(term("\\n"))(Term.Select(Term.Name("\\"), Term.Name("n")))
    }

    test("single-line allow unicode escaping") {
      assertTree(term("\\u0061"))(Term.Name("a"))
    }

    test("single-line disallow line breaks") {
      intercept[TokenizeException] { term("foo + \n bar") }
    }

    test("single-line disallow double quotes") {
      intercept[TokenizeException] { term("\"a\"") }
    }
  }

  locally {
    implicit val dialect = dialects.QuasiquoteTerm(dialects.Scala211, multiline = true)

    test("multi-line disallow do normal escaping") {
      assertTree(term("\\n"))(Term.Select(Term.Name("\\"), Term.Name("n")))
    }

    test("multi-line allow unicode escaping") {
      assertTree(term("\\u0061"))(Term.Name("a"))
    }

    test("multi-line allow line breaks") {
      assertTree(term("foo + \n bar"))(
        Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))
      )
    }

    test("multi-line allow double quotes") {
      assertTree(term("\"a\""))(Lit.String("a"))
    }
  }
}
