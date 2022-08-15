package scala.meta.tests

import munit._

import scala.meta.dialects._

class DialectSuite extends FunSuite {

  private val scala3copy = Scala3.withAllowSignificantIndentation(true)

  test("scala3 == scala3") {
    assertEquals(Scala3, Scala3)
  }

  test("scala3 != scala3copy") {
    assertNotEquals(Scala3, scala3copy)
  }

  test("scala3 isEquivalentTo scala3copy") {
    assert(Scala3.isEquivalentTo(scala3copy))
  }

  test("scala3 isEquivalentTo scala3copy without deprecated") {
    assert(Scala3.isEquivalentTo(scala3copy.withAllowAndTypes(false)))
  }

  test("scala212 !isEquivalentTo scala213") {
    assert(!Scala212.isEquivalentTo(Scala213))
  }

  test("scala213 != scala213Source3") {
    assertNotEquals(Scala213, Scala213Source3)
  }

  test("scala213 !isEquivalentTo scala213Source3") {
    assert(!Scala213.isEquivalentTo(Scala213Source3))
  }

}
