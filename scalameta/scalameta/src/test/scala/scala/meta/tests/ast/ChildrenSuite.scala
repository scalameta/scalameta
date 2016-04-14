package scala.meta.tests
package ast

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class ChildrenSuite extends FunSuite {
  test("Template.children") {
    val tree = q"""
      class Foo {
        import bar.baz.one
        import bar.baz.two
      }
    """
    assert(tree.children.length === 3)
    assert(tree.children(0).productPrefix == "Type.Name")
    assert(tree.children(1).productPrefix == "Ctor.Primary")
    assert(tree.children(2).productPrefix == "Template")
  }
}
