package scala.meta.tests
package contrib

import org.scalatest.FunSuite
import scala.meta._
import scala.meta.contrib._

class EqualSuite extends FunSuite {
  val a: Stat = "val x = 2 // foo".parse[Stat].get
  val b: Defn.Val = q"val x = 2"
  val c: Defn.Val = q"val x = 2"
  val d: Defn.Def = q"def x = 2"
  val e: Defn.Class = q"class Foo { val x = 2 }"

  test("syntactic") {
    assert(!a.isEqual[Syntactically](b))
    assert(b.isEqual[Syntactically](c))
    assert(!e.contains[Syntactically](a))
    assert(e.contains[Syntactically](b))
    assert(!e.contains[Structurally](d))
    assert(Set[Syntactically[Tree]](a, b, c, d).size == 3)
  }

  test("structural") {
    assertTypeError("""c.isEqual(d)""") // Defn.Val cannot be Defn.Def
    assert(a.isEqual(b))
    assert(b.isEqual(c))
    assert(e.contains(a))
    assert(e.contains(b))
    assert(!e.contains(d))
    assert(Set[Structurally[Tree]](a, b, c, d).size == 2)
  }

}
