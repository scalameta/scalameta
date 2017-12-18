package scala.meta.tests
package transversers

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._

class TransverserSuite extends FunSuite {
  test("Traverser Ok") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    val log = scala.collection.mutable.ListBuffer[String]()
    object traverser extends Traverser {
      override def apply(tree: Tree): Unit = {
        log += tree.toString.trim.replace("\n", " ")
        super.apply(tree)
      }
    }
    traverser(tree0)
    assert(log.mkString("\n").replace("\r", "") === """
      |{   def foo(x: x)(x: Int) = x + x   class C(x: x) { def bar(x: x) = ??? } }
      |def foo(x: x)(x: Int) = x + x
      |foo
      |x: x
      |x
      |x
      |x: Int
      |x
      |Int
      |x + x
      |x
      |+
      |x
      |class C(x: x) { def bar(x: x) = ??? }
      |C
      |def this(x: x)
      |_
      |x: x
      |x
      |x
      |{ def bar(x: x) = ??? }
      |_
      |_
      |def bar(x: x) = ???
      |bar
      |x: x
      |x
      |x
      |???
    """.trim.stripMargin)
  }

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
    assert(tree1.toString === """
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
    intercept[UnsupportedOperationException]{ transformer(tree0) }
  }

  test("Tree.transform") {
    val tree0 = q"x + y"
    val tree1 = tree0.transform { case Term.Name(s) => Term.Name(s + s) }
    assert(tree1.toString == "xx ++ yy")
  }

  test("Tree.traverse") {
    var cnt = 0
    val tree0 = q"x + y"
    tree0.traverse { case Term.Name(s) => cnt += 1 }
    assert(cnt == 3)
  }

  test("Tree.collect") {
    val tree0 = q"x + y"
    val result1 = tree0.collect { case Term.Name(s) => s }
    assert(result1.toString == "List(x, +, y)")
  }

  test("#1200") {
    var i = 0
    val fn: PartialFunction[Tree, Tree] = {
      case q"A" if { i += 1; i < 2 } =>  q"B"
    }
    q"A".collect(fn)
    i = 0
    q"A".traverse(fn.andThen(_ => Unit))
    i = 0
    q"A".transform(fn)
  }
}
