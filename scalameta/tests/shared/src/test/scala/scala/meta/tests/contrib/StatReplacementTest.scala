package scala.meta.tests
package contrib

import org.scalatest.FunSuite
import scala.meta._
import scala.meta.contrib._

class StatReplacementTest extends FunSuite {
  val methodDef = q"def foo = 1"
  val lit = q"1"

  test("Test replace class stats") {
    val newClass = q"class Foo".withStats(methodDef :: Nil)
    assert(newClass.extract[Stat].head isEqual methodDef)
  }

  // This an edge case,
  // just ensure the outcome is as expected.
  test("Test replace empty class stats with Nil") {
    val newClass = q"class Foo".withStats(Nil)
    assert(newClass.extract[Stat] == Nil)
    assert(newClass isEqual q"class Foo {}")
  }

  test("Test replace trait stats") {
    val newTrait = q"trait Foo".withStats(methodDef :: Nil)
    assert(newTrait.extract[Stat].head isEqual methodDef)
  }

  test("Test replace object stats") {
    val newObject = q"class Foo".withStats(methodDef :: Nil)
    assert(newObject.extract[Stat].head isEqual methodDef)
  }

  test("Test replace def stats with single stat") {
    val newDef = q"def foo = 2".withStats(methodDef :: Nil)
    assert(newDef.extract[Stat].head isEqual methodDef)
  }

  test("Test replace def stats with single non-term in block") {
    val newDef = q"def foo = { 2 }".withStats(methodDef :: Nil)
    assert(newDef.extract[Stat].head isEqual methodDef)
    assert(newDef isEqual q"def foo = { $methodDef }")
  }

  test("Test replace def stats with single term in block") {
    val newDef = q"def foo = { 2 }".withStats(lit :: Nil)
    assert(newDef.extract[Stat].head isEqual lit)
    assert(newDef isEqual q"def foo = $lit")
  }

  test("Test replace def stats with Nil") {
    val newDef = q"def foo = 2".withStats(Nil)
    assert(newDef.extract[Stat] == Nil)
    assert(newDef isEqual q"def foo = {}")
  }

  test("Test replace val stats with single stat") {
    val newVal = q"val foo = 2".withStats(methodDef :: Nil)
    assert(newVal.extract[Stat].head isEqual methodDef)
  }

  test("Test replace var stats with single stat") {
    val newVar = q"def foo = 2".withStats(methodDef :: Nil)
    assert(newVar.extract[Stat].head isEqual methodDef)
  }
}
