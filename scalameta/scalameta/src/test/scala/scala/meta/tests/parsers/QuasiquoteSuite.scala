package scala.meta.tests
package parsers

import org.scalatest._
import scala.meta._

class QuasiquoteSuite extends ParseSuite {
  locally {
    implicit val dialect = dialects.QuasiquoteTerm(dialects.Scala211, multiline = false)

    test("single-line disallow normal escaping") {
      assert(term("\\n").structure === """Term.Select(Term.Name("\\"), Term.Name("n"))""")
    }

    test("single-line allow unicode escaping") {
      assert(term("\\u0061").structure === """Term.Name("a")""")
    }

    test("single-line disallow line breaks") {
      intercept[TokenizeException]{ term("foo + \n bar") }
    }

    test("single-line disallow double quotes") {
      intercept[TokenizeException]{ term("\"a\"") }
    }
  }

  locally {
    implicit val dialect = dialects.QuasiquoteTerm(dialects.Scala211, multiline = true)

    test("multi-line disallow do normal escaping") {
      assert(term("\\n").structure === """Term.Select(Term.Name("\\"), Term.Name("n"))""")
    }

    test("multi-line allow unicode escaping") {
      assert(term("\\u0061").structure === """Term.Name("a")""")
    }

    test("multi-line allow line breaks") {
      assert(term("foo + \n bar").structure === """Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, Seq(Term.Name("bar")))""")
    }

    test("multi-line allow double quotes") {
      assert(term("\"a\"").structure === """Lit("a")""")
    }
  }
}