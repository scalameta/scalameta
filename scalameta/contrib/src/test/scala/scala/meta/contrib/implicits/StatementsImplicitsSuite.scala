package scala.meta.contrib.implicits

import org.scalatest.FunSuite

import scala.meta._
import scala.meta.contrib._

class StatementsImplicitsSuite extends FunSuite {

  private val classA = q"class a"

  private val valA = q"val a = 2"
  private val valB = q"val b = 2"

  private val classWithVal = q"class a { val a = 2 }"
  private val classWith2Vals = q"class a { val a = 2; val b = 2 }"
  private val classWith2ValsInverted = q"class a { val b = 2; val a = 2 }"

  test("getStats") {
    assert(classA.getStats.isEmpty)
    assert(classWithVal.getStats.size === 1)
  }

  test("withStats") {
    assert(classA.withStats(valA :: Nil).equal[Structurally](classWithVal))
  }

  test("updateStats mapper") {
    assert(classWithVal.updateStats(_ :+ valB).equal[Structurally](classWith2Vals))
  }

  test("classes") {
    assert(q"object foo { }".classes.isEmpty)
    assert(q"object foo { class a }".classes.size === 1)
    assert(q"object foo { class a; object b }".classes.size === 1)
  }

  test("objects") {
    assert(q"object foo { }".objects.isEmpty)
    assert(q"object foo { object a }".objects.size === 1)
    assert(q"object foo { class a; object b }".objects.size === 1)
  }

  test("traits") {
    assert(q"object foo { }".classes.isEmpty)
    assert(q"object foo { trait a }".traits.size === 1)
    assert(q"object foo { class a; trait b }".traits.size === 1)
  }

  test("vars") {
    assert(q"object foo { }".vars.isEmpty)
    assert(q"object foo { var a = 1 }".vars.size === 1)
    assert(q"object foo { class a; var b = 1 }".vars.size === 1)
  }

  test("vals") {
    assert(q"object foo { }".vals.isEmpty)
    assert(q"object foo { val a = 1 }".vals.size === 1)
    assert(q"object foo { class a; val b = 1 }".vals.size === 1)
  }

  test("defs") {
    assert(q"object foo { }".defs.isEmpty)
    assert(q"object foo { def a = 1 }".defs.size === 1)
    assert(q"object foo { class a; def b = 1 }".defs.size === 1)
  }

  test("types") {
    assert(q"object foo { }".types.isEmpty)
    assert(q"object foo { type A = Foo }".types.size === 1)
    assert(q"object foo { class a; type A = Foo }".types.size === 1)
  }
}
