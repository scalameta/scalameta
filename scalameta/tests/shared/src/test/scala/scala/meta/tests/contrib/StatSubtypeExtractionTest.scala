package scala.meta.tests
package contrib

import org.scalatest.FunSuite
import scala.meta._
import scala.meta.contrib._

class StatSubtypeExtractionTest extends FunSuite {

  test("Test extract when none exist") {
    val stats = q"class Foo".extract[Defn.Def]
    assert(stats == Nil)
  }

  test("Test extract Defs") {
    val stats = q"class Foo { def foo = 2 }".extract[Defn.Def]
    assert(stats.exists(_ isEqual q"def foo = 2"))
    assert(stats.size == 1)
  }

  test("Test extract Vals") {
    val stats = q"class Foo { val foo = 2 }".extract[Defn.Val]
    assert(stats.exists(_ isEqual q"val foo = 2"))
    assert(stats.size == 1)
  }

  test("Test extract Vars") {
    val stats = q"class Foo { var foo = 2 }".extract[Defn.Var]
    assert(stats.exists(_ isEqual q"var foo = 2"))
    assert(stats.size == 1)
  }

  test("Test extract Types") {
    val stats = q"class Foo { type Foo = A with B }".extract[Defn.Type]
    assert(stats.exists(_ isEqual q"type Foo = A with B"))
    assert(stats.size == 1)
  }

  test("Test extract Objects") {
    val stats = q"class Foo { object Bar }".extract[Defn.Object]
    assert(stats.exists(_ isEqual q"object Bar"))
    assert(stats.size == 1)
  }

  test("Test extract Classes") {
    val stats = q"class Foo { class Bar }".extract[Defn.Class]
    assert(stats.exists(_ isEqual q"class Bar"))
    assert(stats.size == 1)
  }

  test("Test extract Traits") {
    val stats = q"class Foo { trait Bar }".extract[Defn.Trait]
    assert(stats.exists(_ isEqual q"trait Bar"))
    assert(stats.size == 1)
  }

  test("Test extract Members") {
    val stats = q"class Foo { trait Bar }".extract[Member]
    assert(stats.exists(_ isEqual q"trait Bar"))
    assert(stats.size == 1)
  }

  test("Test extract Terms") {
    val stats = q"class Foo { 1 }".extract[Term]
    assert(stats.exists(_ isEqual q"1"))
    assert(stats.size == 1)
  }

  test("Test extract Decls") {
    val stats = q"class Foo { var foo: Int; val foo: Int }".extract[Decl]
    assert(stats.exists(_ isEqual q"var foo: Int"))
    assert(stats.exists(_ isEqual q"val foo: Int"))
    assert(stats.size == 2)
  }

  test("Test extract DeclDefs") {
    val stats = q"class Foo { def foo: Int }".extract[Decl.Def]
    assert(stats.exists(_ isEqual q"def foo: Int"))
    assert(stats.size == 1)
  }

  test("Test extract DeclVals") {
    val stats = q"class Foo { val foo: Int }".extract[Decl.Val]
    assert(stats.exists(_ isEqual q"val foo: Int"))
    assert(stats.size == 1)
  }

  test("Test extract DeclVars") {
    val stats = q"class Foo { var foo: Int }".extract[Decl.Var]
    assert(stats.exists(_ isEqual q"var foo: Int"))
    assert(stats.size == 1)
  }

  test("Test extract DeclTypes") {
    val stats = q"class Foo { type Foo }".extract[Decl.Type]
    assert(stats.exists(_ isEqual q"type Foo"))
    assert(stats.size == 1)
  }
}
