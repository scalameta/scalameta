package scala.meta.tests
package parsers

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Dotty

class DottySuite extends ParseSuite {
  test("case List(xs: _*)") {
    val tree = pat("List(xs: _*)")
    assert(tree.show[Structure] === "Pat.Extract(Term.Name(\"List\"), Nil, Seq(Pat.Bind(Pat.Var.Term(Term.Name(\"xs\")), Pat.Arg.SeqWildcard())))")
    assert(tree.show[Syntax] === "List(xs: _*)")
  }
  test("xml literals") {
    intercept[TokenizeException]{ term("<foo>{bar}</foo>") }
  }
}