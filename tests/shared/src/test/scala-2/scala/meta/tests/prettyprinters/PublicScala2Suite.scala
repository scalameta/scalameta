package scala.meta.tests.prettyprinters

import scala.meta._
import scala.meta.tests.TreeSuiteBase

import java.io._
import java.nio.charset.Charset

class PublicScala2Suite extends TreeSuiteBase {

  test("scala.meta.Tree.toString (quasiquotes)") {
    val tree = q"foo + bar // baz"
    assertEquals(tree.toString, "foo + bar // baz")
  }

  test("scala.meta.Tree.structure (quasiquoted)") {
    val tree = q"foo + bar // baz"
    val expected = Term.ApplyInfix
      .createWithComments(tname("foo"), "+", Nil, List(tname("bar")), endComment = Seq("// baz"))
    assertTree(tree)(expected)
  }

  test("scala.meta.Tree.syntax (quasiquoted)") {
    val tree = q"foo + bar // baz"
    assertWithOriginalSyntax(tree, "foo + bar // baz", "foo + bar // baz")
  }
}
