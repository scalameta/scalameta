package scala.meta.tests
package contrib

import scala.meta._
import scala.meta.contrib._

import munit.FunSuite

class SetExtensionsTest extends FunSuite {
  val typeFoo = t"Foo"
  val termFoo = q"Foo"

  val set = Set(typeFoo, termFoo, q"Foo")

  test("Reference equality holds normally") {
    assertEquals(set.size, 3)
    assert(set.contains(typeFoo))
    assert(set.contains(termFoo))
    assert(!set.contains(q"Foo"))
  }

  test("Structurally") {
    val structuralSet = set.structurally
    assertEquals(structuralSet.size, 2)
    assert(structuralSet.contains(typeFoo))
    assert(structuralSet.contains(termFoo))
    assert(structuralSet.contains(q"Foo"))
  }

  test("Syntactically") {
    val syntacticSet = set.syntactically
    assertEquals(syntacticSet.size, 1)
    assert(syntacticSet.contains(typeFoo))
    assert(syntacticSet.contains(termFoo))
    assert(syntacticSet.contains(q"Foo"))
  }
}
