package scala.meta.contrib

import org.scalatest.FunSuite

import scala.meta._

class StatExtractionTest extends FunSuite {

  test("Test extract class stats") {
    val stats = q"class Foo".extract[Stats]
    assert(stats == Nil)
  }

  test("Test extract trait stats") {
    val stats = q"trait Foo".extract[Stats]
    assert(stats == Nil)
  }

  test("Test extract object stats") {
    val stats = q"object Foo".extract[Stats]
    assert(stats == Nil)
  }

  test("Test extract def stats") {
    val stats = q"def foo = {}".extract[Stats]
    assert(stats == Nil)
  }

  test("Test extract def stats with single stat") {
    val stats = q"def foo = 2".extract[Stats]
    assert(stats.exists(_ isEqual q"2"))
  }

  test("Test extract val stats with single stat") {
    val stats = q"val foo = 2".extract[Stats]
    assert(stats.exists(_ isEqual q"2"))
    assert(stats.size == 1)
  }

  test("Test extract var stats with single stat") {
    val stats = q"var foo = 2".extract[Stats]
    assert(stats.exists(_ isEqual q"2"))
    assert(stats.size == 1)
  }
}
