package scala.meta.tests
package ast

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.tests.parsers.ParseSuite

class ChildrenSuite extends ParseSuite {
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
