package scala.meta.tests.transversers

import munit._

import scala.compat.Platform.EOL
import scala.meta._

class TransformerSuite extends FunSuite {

  test("Transformer Ok") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    val log = scala.collection.mutable.ListBuffer[String]()
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
      |  def foo(y: y)(y: Int) = y + y
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
    val log = scala.collection.mutable.ListBuffer[String]()
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
}
