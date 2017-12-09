package scala.meta.tests
package trees

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
    assert(tree.tokens.forall(_.input.isInstanceOf[Input.VirtualFile]))
  }

  test("Tree.tokens: empty") {
    val emptyTemplate = "class C".parse[Stat].get.children(2)
    assert(emptyTemplate.structure === "Template(Nil, Nil, Self(Name(\"\"), None), Nil)")
    assert(emptyTemplate.tokens.structure === "Tokens()")
  }

  test("inline can be used as an identifier") {
    val tree = dialects.Scala211("{ val inline = 42 }").parse[Term].get
    assert(tree.syntax === "{ val inline = 42 }")
  }
}
