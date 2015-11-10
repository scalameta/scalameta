package scala.meta.tests
package ast

import scala.meta.tests.parsers.ParseSuite
import scala.meta.internal.{ast => impl}

class ChildrenSuite extends ParseSuite {
  test("Template.children") {
    val impl.Defn.Class(_, _, _, _, impl.Template(_, _, _, Some(imports))) = topStat("""class Foo {
      import bar.baz.one
      import bar.baz.two
    }""")
    val children = imports.head.parent.get.children
    assert(children.length === 3)
    assert(children(0).productPrefix == "Term.Param")
    assert(children(1).productPrefix == "Import")
    assert(children(2).productPrefix == "Import")
  }
}
