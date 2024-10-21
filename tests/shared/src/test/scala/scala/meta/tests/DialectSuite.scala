package scala.meta.tests

import scala.meta.dialects._

import munit._

class DialectSuite extends FunSuite {

  private val scala3copy = Scala3.withAllowSignificantIndentation(true)

  test("scala3 == scala3")(assertEquals(Scala3, Scala3))

  test("scala3 isEquivalentTo scala3copy") {
    assertEquals(Scala3, scala3copy)
    assert(Scala3.isEquivalentTo(scala3copy))
  }

  test("scala3 isEquivalentTo scala3 without deprecated") {
    val that = Scala3.withAllowAndTypes(false)
    assertEquals(that, Scala3)
    assert(Scala3.isEquivalentTo(that))
  }

  test("scala3 isEquivalentTo scala3copy without deprecated") {
    val that = scala3copy.withAllowAndTypes(false)
    assertEquals(that, scala3copy)
    assertEquals(that, Scala3)
    assert(Scala3.isEquivalentTo(that))
  }

  test("scala212 !isEquivalentTo scala213") {
    assertNotEquals(Scala213, Scala212)
    assert(!Scala212.isEquivalentTo(Scala213))
  }

  test("scala213 !isEquivalentTo scala213Source3") {
    assertNotEquals(Scala213, Scala213Source3)
    assert(!Scala213.isEquivalentTo(Scala213Source3))
  }

  test("scala3 toString")(assertEquals(Scala3.toString, "Scala36"))

  test("scala3copy toString")(assertEquals(scala3copy.toString, "Scala36"))

  test("scala3 without indentation toString") {
    assertEquals(Scala3.withAllowSignificantIndentation(false).toString, "Dialect()")
  }

  test("scala3.unquoteTerm toString")(assertEquals(Scala3.unquoteTerm(true).toString, "Scala36"))

}
