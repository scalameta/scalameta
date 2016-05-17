package scala.meta.tests
package ast

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class TokensSuite extends FunSuite {
  test("Tree.tokens: parsed, same dialect") {
    val tree = dialects.Scala211("foo + bar // baz").parse[Term].get
    assert(tree.syntax === "foo + bar // baz")
    assert(tree.tokens.syntax === "foo + bar // baz")
  }

  test("Tree.tokens: parsed, different dialect") {
    val tree = dialects.Scala210("foo + bar // baz").parse[Term].get
    assert(tree.syntax === "foo + bar")
    assert(tree.tokens.syntax === "foo + bar // baz")
  }

  test("Tree.tokens: manual") {
    val tree = Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))
    assert(tree.syntax === "foo + bar")
    assert(tree.tokens.syntax === "foo + bar")
    assert(tree.tokens.forall(_.input.isInstanceOf[scala.meta.internal.inputs.VirtualInput]))
  }

  test("wat") {
    // NOTE: if this test fails, then we'll get ScalametaTokenizer.megaCache corruptions
    assert(scala.meta.internal.inputs.VirtualInput("abcdefgh") != Input.String("abcdefgh"))
  }

  test("Tree.tokens: empty") {
    val emptySelf = "class C".parse[Stat].get.children(2)
    assert(emptySelf.structure === "Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), None)")
    assert(emptySelf.tokens.structure === "Tokens()")
  }
}
