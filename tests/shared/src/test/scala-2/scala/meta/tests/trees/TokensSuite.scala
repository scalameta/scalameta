package scala.meta.tests
package trees

import scala.meta._

class TokensSuite extends TreeSuiteBase {

  implicit val dialect: Dialect = dialects.Scala211

  test("Tree.tokens: parsed, same dialect") {
    val tree = dialects.Scala211("foo + bar // baz").parse[Term].get
    assertEquals(tree.syntax, "foo + bar // baz")
    assertEquals(tree.tokens.syntax, "foo + bar // baz")
  }

  test("Tree.tokens: parsed, different dialect") {
    val tree = dialects.Scala210("foo + bar // baz").parse[Term].get
    assertEquals(tree.syntax, "foo + bar")
    assertEquals(tree.tokens.syntax, "foo + bar // baz")
  }

  test("Tree.tokens: manual") {
    val dialect = implicitly[Dialect]
    val tree = Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar")))
    assertEquals(tree.text, "foo + bar")
    assertEquals(tree.printSyntaxFor(dialect), "foo + bar")
    assertEquals(tree.syntax, "foo + bar")
    assert(tree.origin ne trees.Origin.None)
    assertEquals(tree.origin.dialectOpt, Some(dialect))
    val tokens = tree.tokenizeFor(dialect)
    assertEquals(tree.tokens.structure, tokens.structure)
    assertEquals(tokens.syntax, "foo + bar")
    assert(tokens.forall(_.input.isInstanceOf[Input.VirtualFile]))
    val dialectTree = tree.withDialectIfRootAndNotSet
    assertEquals(dialectTree, tree)
    assertEquals(dialectTree.text, "foo + bar")
    assertEquals(dialectTree.printSyntaxFor(dialect), "foo + bar")
    assert(dialectTree.origin ne trees.Origin.None)
    val dialectTokens = dialectTree.tokens
    assertEquals(dialectTokens.syntax, "foo + bar")
    assert(dialectTokens.forall(_.input.isInstanceOf[Input.VirtualFile]))

    val parsedTree = tree.maybeParseAs[Stat].get
    assert(parsedTree eq parsedTree.maybeParse.get)
    assertEquals(parsedTree.printSyntaxFor(dialect), "foo + bar")
    assertEquals(parsedTree.syntax, "foo + bar")
    assert(parsedTree.origin.isInstanceOf[trees.Origin.Parsed])
    val parsedTokens = parsedTree.tokens
    assertEquals(parsedTokens.syntax, "foo + bar")
    assert(parsedTokens.forall(_.input.isInstanceOf[Input.VirtualFile]))
  }

  test("Tree.tokens: empty") {
    val emptyTemplate = "class C".parse[Stat].get.children(3)
    assertTree(emptyTemplate)(tplNoBody())
    assertEquals(emptyTemplate.tokens.structure, "Tokens()")
  }

  test("inline can be used as an identifier") {
    val tree = dialects.Scala211("{ val inline = 42 }").parse[Term].get
    assertEquals(tree.syntax, "{ val inline = 42 }")
  }
}
