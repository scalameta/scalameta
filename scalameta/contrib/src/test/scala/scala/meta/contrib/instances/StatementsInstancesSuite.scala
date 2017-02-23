package scala.meta.contrib.instances

import org.scalatest.FunSuite

import scala.meta._
import scala.meta.contrib._

// TODO: Add withStat tests
class StatementsInstancesSuite extends FunSuite {

  test("class instance") {
    assert(q"class foo".getStats.isEmpty)
    assert(q"class foo { }".getStats.isEmpty)
    assert(q"class foo { val foo = 2 }".getStats.size === 1)
  }

  test("trait instance") {
    assert(q"trait foo".getStats.isEmpty)
    assert(q"trait foo { }".getStats.isEmpty)
    assert(q"trait foo { val foo = 2 }".getStats.size === 1)
  }

  test("object instance") {
    assert(q"object foo { }".getStats.isEmpty)
    assert(q"object foo".getStats.isEmpty)
    assert(q"object foo { val foo = 2 }".getStats.size === 1)
  }

  test("val instance") {
    assert(q"val foo = 2".getStats.size === 1)
    assert(q"val foo = { println(1); 2 }".getStats.size === 2)
  }

  test("var instance") {
    assert(q"var foo = 2".getStats.size === 1)
    assert(q"var foo = { println(1); 2 }".getStats.size === 2)
  }

  test("def instance") {
    assert(q"def foo = 2".getStats.size === 1)
    assert(q"def foo = { println(1); 2 }".getStats.size === 2)
  }

  test("pkg instance") {
    assert(q"package foo { }".getStats.isEmpty)
    assert(q"package foo { class foo }".getStats.size === 1)
    assert(q"package foo { class foo; class bar }".getStats.size === 2)
    assert(q"package foo { package bar { class foo; class bar } }".getStats.size === 2)
  }
}
