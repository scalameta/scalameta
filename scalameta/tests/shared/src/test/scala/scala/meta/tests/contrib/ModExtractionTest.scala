package scala.meta.tests
package contrib

import org.scalatest.FunSuite
import scala.meta._
import scala.meta.contrib._

class ModExtractionTest extends FunSuite {
  test("Test extract class mods with no mods") {
    val mods = q"class Foo".extract[Mod]
    assert(mods == Nil)
  }

  test("Test extract class mods with mods") {
    val mods = q"final class Foo".extract[Mod]
    assert(mods.exists(_ isEqual mod"final"))
    assert(mods.size == 1)
  }

  test("Test extract trait mods with mods") {
    val mods = q"sealed trait Foo".extract[Mod]
    assert(mods.exists(_ isEqual mod"sealed"))
    assert(mods.size == 1)
  }

  test("Test extract object mods with mods") {
    val mods = q"private trait Foo".extract[Mod]
    assert(mods.exists(_ isEqual mod"private"))
    assert(mods.size == 1)
  }

  test("Test extract def mods with mods") {
    val mods = q"private def foo = 2".extract[Mod]
    assert(mods.exists(_ isEqual mod"private"))
    assert(mods.size == 1)
  }

  test("Test extract val mods with mods") {
    val mods = q"private val foo = 2".extract[Mod]
    assert(mods.exists(_ isEqual mod"private"))
    assert(mods.size == 1)
  }

  test("Test extract var mods with mods") {
    val mods = q"private var foo = 2".extract[Mod]
    assert(mods.exists(_ isEqual mod"private"))
    assert(mods.size == 1)
  }
}
