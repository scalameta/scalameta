package scala.meta.tests
package trees

import scala.meta._
import scala.meta.dialects.Scala211

import munit._

class ChildrenSuite extends FunSuite {
  test("Template.children") {
    val tree = q"""
      class Foo {
        import bar.baz.one
        import bar.baz.two
      }
    """
    assertEquals(tree.children.length, 4)
    assertEquals(tree.children(0).productPrefix, "Type.Name")
    assertEquals(tree.children(1).productPrefix, "Type.ParamClause")
    assertEquals(tree.children(2).productPrefix, "Ctor.Primary")
    assertEquals(tree.children(3).productPrefix, "Template")
  }

  test("derives-in-children") {
    val source = """|
                    |class Foo derives A[T], B[T] {  }
                    |""".stripMargin
    val tree = dialects.Scala3(source).parse[Stat].get
    val containsBinaryCompatFields = tree.children.exists {
      case t: Template => t.children.exists(c => c.is[Type.Apply] && c.toString == "A[T]")
      case _ => false
    }
    assert(
      containsBinaryCompatFields,
      "Binary compatible fields should be contained in the children method"
    )
  }
}
