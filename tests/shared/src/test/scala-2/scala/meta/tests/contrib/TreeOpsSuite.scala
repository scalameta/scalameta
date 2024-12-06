package scala.meta.tests
package contrib

import scala.meta._
import scala.meta.contrib._

import scala.collection.mutable.ListBuffer

import munit.FunSuite

class TreeOpsSuite extends TreeSuiteBase {
  val a: Defn.Val = q"val x = 2"

  test("testForeach") {
    val obtained = new ListBuffer[String]
    a.foreach(x => obtained.append(x.structure))
    assertEquals(
      obtained.result(),
      List(
        """|Defn.Val(
           |  Nil,
           |  List(
           |    Pat.Var(Term.Name("x"))
           |  ),
           |  None,
           |  Lit.Int(2)
           |)""".stripMargin.lf2nl,
        """Pat.Var(Term.Name("x"))""",
        """Term.Name("x")""",
        """Lit.Int(2)"""
      )
    )
  }

  test("ancestors") {
    val lit: Tree = q"val x = { 2 + 3 }".find(_.isEqual(q"3")).get
    assertEquals(lit.ancestors.length, 4)
    assert(q"val x = { 2 + 3 }".ancestors.isEmpty)
  }

  test("descendants") {
    assertEquals(a.descendants.length, 3)
    assertEquals(q"3".descendants.length, 0)
    val tree: Defn.Val = q"val x = { 2 + 3 }"
    assertEquals(tree.descendants.size, 9)
  }

  test("forall") {
    assert(a.forall(_.is[Tree]))
    assert(!a.forall(_.is[Term]))
    assert(!a.forall(_.is[Type]))
    assert(q"3".forall(_.syntax == "3"))
  }

  test("exists") {
    assert(a.exists(_.is[Defn.Val]))
    assert(a.exists(_.is[Lit]))
    assert(a.exists(_.is[Pat]))
    assert(!a.exists(_.is[Type.Name]))
    assert(q"val x = { 2 + 3 }".exists(_.syntax == "3"))
  }

  test("contains") {
    a.foreach(t => assert(a.contains(t)))
    val b: Defn.Object = q"object Foo { def bar: Any = ??? }"
    b.foreach(t => assert(!a.contains(t)))
  }

  test("collectFirst") {
    assert(a.collectFirst { case t: Tree => t } == Some(a))
    assert(a.collectFirst { case t: Defn.Val => t }.nonEmpty)
    assert(a.collectFirst { case t: Lit => t }.nonEmpty)
    assert(a.collectFirst { case t: Type => t }.nonEmpty)
    assert(a.collectFirst { case t: Defn.Trait => t }.isEmpty)
    assert(a.collectFirst { case t: Defn.Def => t }.isEmpty)
  }
}
