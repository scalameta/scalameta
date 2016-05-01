package scala.meta.tests
package transversers

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.internal.semantic._
import scala.meta.internal.prettyprinters._

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
    assert(log.mkString(EOL) === """
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
      |this
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
    """.trim.stripMargin)
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

  test("Transformed Attributes") {
    def attributeTypeName(name: Type.Name): Type.Name = name.withAttrs(Denotation.Single(Prefix.None, Symbol.RootPackage))
    val Foo = attributeTypeName(Type.Name("Foo"))
    def attributeTermName(name: Term.Name): Term.Name = name.withAttrs(Denotation.Single(Prefix.None, Symbol.RootPackage), Foo.setTypechecked)
    def attributeTerm(term: Term): Term = term.withAttrs(Foo.setTypechecked)
    val denot1 = Denotation.Single(Prefix.None, Symbol.RootPackage)
    val denot2 = Denotation.Single(Prefix.None, Symbol.EmptyPackage)
    val typing = Foo.setTypechecked
    val x = q"x".withAttrs(denot1, typing).setTypechecked
    val z = q"z".withAttrs(denot2, typing).setTypechecked
    val attr0 = q"$x + $z".withAttrs(typing)
    assert(attr0.show[Attributes] === """
      |Term.ApplyInfix(Term.Name("x")[1]{1}, Term.Name("+")*, Nil, Seq(Term.Name("z")[2]{1})){1}*
      |[1] {0}::_root_
      |[2] {0}::_empty_
      |{1} Type.Name("Foo")[1]
    """.trim.stripMargin)

    object transformer extends Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case Term.Name("x") => Term.Name("y")
        case Type.Name("x") => Type.Name("y")
        case _ => super.apply(tree)
      }
    }

    val attr1 = transformer(attr0)
    assert(attr1.show[Attributes] === """
      |Term.ApplyInfix(Term.Name("y")*, Term.Name("+")*, Nil, Seq(Term.Name("z")[1]{1}))*
      |[1] {0}::_empty_
      |[2] {0}::_root_
      |{1} Type.Name("Foo")[2]
    """.trim.stripMargin)
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
}