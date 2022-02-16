package scala.meta.tests.transversers

import munit._

import scala.compat.Platform.EOL
import scala.meta._

class TransformerSuite extends FunSuite {

  test("Transformer Ok") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x + 1
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    object transformer extends Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case Term.Name("x") => Term.Name("y")
        case Type.Name("x") => Type.Name("y")
        case _ => super.apply(tree)
      }
    }
    val tree1 = transformer(tree0)
    assert(tree1.toString == """
      |{
      |  def foo(y: y)(y: Int) = y + y + 1
      |  class C(y: y) { def bar(y: y) = ??? }
      |}
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("Transformer Fail") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    object transformer extends Transformer {
      override def apply(tree: Tree): Tree = {
        if (tree.toString == "x") q"y"
        else super.apply(tree)
      }
    }
    intercept[UnsupportedOperationException] { transformer(tree0) }
  }

  test("Tree.transform") {
    val tree0 = q"x + y"
    val tree1 = tree0.transform { case Term.Name(s) => Term.Name(s + s) }
    assert(tree1.toString == "xx ++ yy")
  }

  test("dotty-derives-transform") {

    import scala.meta.dialects.Dotty

    val before = "case class Node(name: String) extends Tree derives a.b.OldName { def a = 1 }"
    val after = "case class Node(name: String) extends Tree derives a.b.NewName { def a = 1 }"

    val beforeTree =
      before
        .parse[Source]
        .get

    val afterTree = beforeTree
      .transform({ case Type.Name("OldName") =>
        Type.Name("NewName")
      })

    assertEquals(afterTree.toString, after)

  }
}
