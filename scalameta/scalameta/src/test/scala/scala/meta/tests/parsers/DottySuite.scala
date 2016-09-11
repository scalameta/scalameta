package scala.meta.tests
package parsers

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

  test("inline def x = 42") {
    val tree1@Defn.Def(Seq(Mod.Inline()), Term.Name("x"), Nil, Nil, None, Lit(42)) = templStat("inline def x = 42")
    val tree2@Defn.Def(Seq(Mod.Inline()), Term.Name("x"), Nil, Nil, None, Lit(42)) = blockStat("inline def x = 42")

    assert(tree1.show[Syntax] === "inline def x = 42")
    assert(tree2.show[Syntax] === "inline def x = 42")
  }

  test("inline tokens are allowed") {
    val tree = dialects.Dotty("{ inline def x = 42 }").parse[Term].get
    assert(tree.syntax === "{ inline def x = 42 }")
  }

  test("mod\"inline\"") {
    assert(mod"inline".show[Structure] === "Mod.Inline()")
  }

  test("a val cannot be called inline") {
    intercept[ParseException] {
      dialects.Dotty("{ val inline = 42 }").parse[Term].get
    }
  }
}