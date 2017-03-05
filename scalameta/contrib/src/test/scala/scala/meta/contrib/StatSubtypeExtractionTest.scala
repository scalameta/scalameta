package scala.meta.contrib

import org.scalatest.FunSuite

import scala.meta._
import scala.meta.contrib._

class StatSubtypeExtractionTest extends FunSuite {

  test("Test extract when none exist") {
    val stats = q"class Foo".extract[Defs]
    assert(stats == Nil)
  }

  test("Test extract Defs") {
    val stats = q"class Foo { def foo = 2 }".extract[Defs]
    assert(stats.exists(_ isEqual q"def foo = 2"))
    assert(stats.size == 1)
  }

  test("Test extract Vals") {
    val stats = q"class Foo { val foo = 2 }".extract[Vals]
    assert(stats.exists(_ isEqual q"val foo = 2"))
    assert(stats.size == 1)
  }

  test("Test extract Vars") {
    val stats = q"class Foo { var foo = 2 }".extract[Vars]
    assert(stats.exists(_ isEqual q"var foo = 2"))
    assert(stats.size == 1)
  }

  test("Test extract Types") {
    val stats = q"class Foo { type Foo = A with B }".extract[Types]
    assert(stats.exists(_ isEqual q"type Foo = A with B"))
    assert(stats.size == 1)
  }

  test("Test extract Objects") {
    val stats = q"class Foo { object Bar }".extract[Objects]
    assert(stats.exists(_ isEqual q"object Bar"))
    assert(stats.size == 1)
  }

  test("Test extract Classes") {
    val stats = q"class Foo { class Bar }".extract[Classes]
    assert(stats.exists(_ isEqual q"class Bar"))
    assert(stats.size == 1)
  }

  test("Test extract Traits") {
    val stats = q"class Foo { trait Bar }".extract[Traits]
    assert(stats.exists(_ isEqual q"trait Bar"))
    assert(stats.size == 1)
  }
}
