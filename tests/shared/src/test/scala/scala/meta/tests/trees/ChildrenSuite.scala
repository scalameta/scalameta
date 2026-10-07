package scala.meta.tests
package trees

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.tests.parsers.ParseSuite

import scala.reflect.ClassTag

import munit._

class ChildrenSuite extends ParseSuite {
  test("Template.children") {
    val tree = stat(
      """|
         |class Foo {
         |  import bar.baz.one
         |  import bar.baz.two
         |}
         |""".stripMargin,
    )
    assertEquals(tree.children.length, 4)
    assertEquals(tree.children(0).productPrefix, "Type.Name")
    assertEquals(tree.children(1).productPrefix, "Type.ParamClause")
    assertEquals(tree.children(2).productPrefix, "Ctor.Primary")
    assertEquals(tree.children(3).productPrefix, "Template")
  }

  test("derives-in-children") {
    val source =
      """|
         |class Foo derives A[T], B[T] {  }
         |""".stripMargin
    val tree = dialects.Scala3(source).parse[Stat].get
    val containsBinaryCompatFields = tree.children.exists {
      case t: Template => t.children.exists(c => c.is[Type.Apply] && c.toString == "A[T]")
      case _ => false
    }
    assert(
      containsBinaryCompatFields,
      "Binary compatible fields should be contained in the children method",
    )
  }

  test("lastChild: past an empty option, an empty list, and with no children") {
    val tree = stat(
      """|class A[T](x: Int = 1)(y: Int) extends B {
         |  def f(z: Int): Unit = new D
         |  x match { case 1 => 2 }
         |}
         |""".stripMargin,
    )
    tree.collect { case t => t }
      .foreach(t => assertEquals(t.lastChild, t.children.lastOption.orNull, t.structure))
  }

  test("allElems: the elements of each block, in order") {
    def elems[T <: Tree.Block: ClassTag](tree: Tree): List[String] = tree.collect { case t: T => t }
      .head.allElems.map(_.syntax)
    val scala3 = dialects.Scala3
    val obtained = List(
      elems[Term.Block](term("{ a; b }")),
      elems[Term.CasesBlock](term("x match { case 1 => a; case 2 => b }")),
      elems[Term.EnumeratorsBlock](term("for (x <- xs; if p) yield x")),
      elems[Type.Block](term("'[ type t = Int; List[t] ]")(scala3)),
      elems[Type.CasesBlock](
        stat(
          """|type T = X match {
             |  case Int => A
             |  case _ => B
             |}
             |""".stripMargin,
        )(scala3),
      ),
      elems[Stat.Block](stat("type T = A { def f: Int; def g: Int }")),
      elems[Pkg.Body](source("package a { class B; class C }")),
      elems[Ctor.Block](stat("class A { def this(x: Int) = { this(); f } }")),
      elems[Template.Body](stat("class A { self => f }")),
      elems[Template.Body](stat("class A { f; g }")),
      elems[Source](source("class A; class B")),
    )
    val expected = List(
      List("a", "b"),
      List("case 1 => a;", "case 2 => b"),
      List("x <- xs", "if p"),
      List("type t = Int", "List[t]"),
      List("case Int => A", "case _ => B"),
      List("def f: Int", "def g: Int"),
      List("class B", "class C"),
      List("this()", "f"),
      List("self =>", "f"),
      List("f", "g"),
      List("class A", "class B"),
    )
    assertEquals(obtained, expected)
  }
}
