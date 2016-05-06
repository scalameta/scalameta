package scala.meta.tests
package ast

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class TokensSuite extends FunSuite {
  test("Tokens.slice: ()") {
    val input = Input.String("")
    val tokens = input.tokenize.get.slice(0, 0)
    assert(tokens.structure === "Tokens()")
    intercept[IllegalArgumentException](tokens.slice(Position.Range(input, 0, 0, 0)))
  }

  test("Tokens.slice: (BOF, EOF)") {
    val input = Input.String("")
    val tokens = input.tokenize.get
    assert(tokens.structure === "Tokens(BOF [0..0), EOF [0..0))")
    intercept[IllegalArgumentException](tokens.slice(Position.Range(input, 0, 0, 0)))
  }

  test("Tokens.slice: (BOF, foo, EOF)") {
    val input = Input.String("foo")
    val tokens = input.tokenize.get
    assert(tokens.structure === "Tokens(BOF [0..0), foo [0..3), EOF [3..3))")
    intercept[IllegalArgumentException](tokens.slice(Position.Range(input, 0, 0, 0)))
    intercept[IllegalArgumentException](tokens.slice(Position.Range(input, 0, 0, 1)))
    assert(tokens.slice(Position.Range(input, 0, 0, 3)).structure === "Tokens(foo [0..3))")
  }

  test("Tokens.slice: wrong input") {
    val input = Input.String("foo")
    val tokens = input.tokenize.get
    assert(tokens.structure === "Tokens(BOF [0..0), foo [0..3), EOF [3..3))")
    val input2 = Input.String("foo")
    assert(tokens.slice(Position.Range(input2, 0, 0, 3)).structure === "Tokens(foo [0..3))")
    val input3 = Input.String("bar")
    intercept[IllegalArgumentException](tokens.slice(Position.Range(input3, 0, 0, 3)))
  }

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
