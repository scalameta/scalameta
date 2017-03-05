package scala.meta.contrib

import org.scalatest.FunSuite

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib._

class StatSubtypeExtractionTest extends FunSuite {

  test("Test extract when none exist") {
    val stats = q"class Foo".extract[Seq[Defn.Def]]
    assert(stats == Nil)
  }

  test("Test extract Defs") {
    val stats = q"class Foo { def foo = 2 }".extract[Seq[Defn.Def]]
    assert(stats.exists(_ isEqual q"def foo = 2"))
    assert(stats.size == 1)
  }

  test("Test extract Vals") {
    val stats = q"class Foo { val foo = 2 }".extract[Seq[Defn.Val]]
    assert(stats.exists(_ isEqual q"val foo = 2"))
    assert(stats.size == 1)
  }

  test("Test extract Vars") {
    val stats = q"class Foo { var foo = 2 }".extract[Seq[Defn.Var]]
    assert(stats.exists(_ isEqual q"var foo = 2"))
    assert(stats.size == 1)
  }

  test("Test extract Types") {
    val stats = q"class Foo { type Foo = A with B }".extract[Seq[Defn.Type]]
    assert(stats.exists(_ isEqual q"type Foo = A with B"))
    assert(stats.size == 1)
  }

  test("Test extract Objects") {
    val stats = q"class Foo { object Bar }".extract[Seq[Defn.Object]]
    assert(stats.exists(_ isEqual q"object Bar"))
    assert(stats.size == 1)
  }

  test("Test extract Classes") {
    val stats = q"class Foo { class Bar }".extract[Seq[Defn.Class]]
    assert(stats.exists(_ isEqual q"class Bar"))
    assert(stats.size == 1)
  }

  test("Test extract Traits") {
    val stats = q"class Foo { trait Bar }".extract[Seq[Defn.Trait]]
    assert(stats.exists(_ isEqual q"trait Bar"))
    assert(stats.size == 1)
  }
}
