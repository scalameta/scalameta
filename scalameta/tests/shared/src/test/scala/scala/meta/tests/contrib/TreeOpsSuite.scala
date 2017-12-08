package scala.meta.tests
package contrib

import org.scalatest.FunSuite
import scala.meta._
import scala.meta.contrib._

class TreeOpsSuite extends FunSuite {
  val a: Defn.Val = q"val x = 2"

  test("testForeach") {
    var obtained = List.empty[String]
    a.foreach(x => obtained = x.structure :: obtained)
    assert(
      obtained ==
        List(
          "Lit.Int(2)",
          "Term.Name(\"x\")",
          "Pat.Var(Term.Name(\"x\"))",
          "Defn.Val(Nil, List(Pat.Var(Term.Name(\"x\"))), None, Lit.Int(2))"
        ))
  }

  test("ancestors") {
    val lit: Tree = q"val x = { 2 + 3 }".find(_.isEqual(q"3")).get
    assert(lit.ancestors.length == 3)
    assert(q"val x = { 2 + 3 }".ancestors.isEmpty)
  }

  test("descendants") {
    assert(a.descendants.length == 3)
    assert(q"3".descendants.isEmpty)
    val tree: Defn.Val = q"val x = { 2 + 3 }"
    assert(tree.descendants.size == 7)
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

}
