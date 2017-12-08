package scala.meta.tests
package contrib

import org.scalatest.FunSuite
import scala.meta._
import scala.meta.contrib._

class StatExtractionTest extends FunSuite {

  test("Test extract class stats") {
    val stats = q"class Foo".extract[Stat]
    assert(stats == Nil)
  }

  test("Test extract trait stats") {
    val stats = q"trait Foo".extract[Stat]
    assert(stats == Nil)
  }

  test("Test extract object stats") {
    val stats = q"object Foo".extract[Stat]
    assert(stats == Nil)
  }

  test("Test extract def stats") {
    val stats = q"def foo = {}".extract[Stat]
    assert(stats == Nil)
  }

  test("Test extract def stats with single stat") {
    val stats = q"def foo = 2".extract[Stat]
    assert(stats.exists(_ isEqual q"2"))
  }

  test("Test extract def stats with single stat in a block") {
    val stats = q"def foo = { 2 }".extract[Stat]
    assert(stats.exists(_ isEqual q"2"))
  }

  test("Test extract val stats with single stat") {
    val stats = q"val foo = 2".extract[Stat]
    assert(stats.exists(_ isEqual q"2"))
    assert(stats.size == 1)
  }

  test("Test extract var stats with single stat") {
    val stats = q"var foo = 2".extract[Stat]
    assert(stats.exists(_ isEqual q"2"))
    assert(stats.size == 1)
  }
}
