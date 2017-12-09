package scala.meta.tests
package contrib

import org.scalatest.FunSuite
import scala.meta._
import scala.meta.contrib._

class ModReplacementTest extends FunSuite {
  test("Test replace class mods with no existing mods") {
    val newClass = q"class Foo".withMods(mod"final" :: Nil)

    assert(newClass.hasMod(mod"final"))
  }

  test("Test replace class mods with existing mods") {
    val newClass = q"sealed class Foo".withMods(mod"final" :: Nil)

    assert(newClass.hasMod(mod"final"))
  }

  test("Test replace trait mods") {
    val newTrait = q"trait Foo".withMods(mod"final" :: Nil)

    assert(newTrait.hasMod(mod"final"))
  }

  test("Test replace object mods") {
    val newObject = q"object Foo".withMods(mod"final" :: Nil)

    assert(newObject.hasMod(mod"final"))
  }

  test("Test replace def mods") {
    val newDef = q"def foo = 1".withMods(mod"final" :: Nil)

    assert(newDef.hasMod(mod"final"))
  }

  test("Test replace var mods") {
    val newVar = q"var foo = 1".withMods(mod"final" :: Nil)

    assert(newVar.hasMod(mod"final"))
  }

  test("Test replace val mods") {
    val newVal = q"val foo = 1".withMods(mod"final" :: Nil)

    assert(newVal.hasMod(mod"final"))
  }
}
