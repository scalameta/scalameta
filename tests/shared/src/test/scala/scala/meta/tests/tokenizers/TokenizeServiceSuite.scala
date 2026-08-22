package scala.meta.tests
package tokenizers

import scala.meta._

import munit.FunSuite

class TokenizeServiceSuite extends FunSuite {

  test("a tree tokenizes without the parser having run") {
    val tree = Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, List(Term.Name("b")))
    assertEquals(tree.tokens.structure.isEmpty, false)
  }

}
