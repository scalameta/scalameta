package scala.meta.tests
package trees

import munit._
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
    assert(tree.children.length == 3)
    assert(tree.children(0).productPrefix == "Type.Name")
    assert(tree.children(1).productPrefix == "Ctor.Primary")
    assert(tree.children(2).productPrefix == "Template")
  }

  test("derives-in-children") {
    val source = """|
                    |class Foo derives A[T], B[T] {  }
                    |""".stripMargin
    val tree = dialects.Scala3(source).parse[Stat].get
    val containsBinaryCompatFields = tree.children.exists {
      case t: Template =>
        t.children.exists { c => c.is[Type.Apply] && c.toString() == "A[T]" }
      case _ => false
    }
    assert(
      containsBinaryCompatFields,
      "Binary compatible fields should be contained in the children method"
    )
  }
}
