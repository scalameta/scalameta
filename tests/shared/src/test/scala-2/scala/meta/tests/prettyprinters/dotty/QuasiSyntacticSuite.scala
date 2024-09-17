package scala.meta.tests.prettyprinters.dotty

import scala.meta._

class QuasiSyntacticSuite extends scala.meta.tests.parsers.ParseSuite {

  import dialects.Scala3

  test("trait parameters")(assertEquals(Scala3(q"trait T(a: Int)").syntax, "trait T(a: Int)"))

  test("literalTypes") {
    assertEquals(q"val a: 42 = 42".syntax, "val a: 42 = 42")
    assertEquals(q"val a: 42L = 42L".syntax, "val a: 42L = 42L")
    assertEquals(q"val a: 42d = 42d".syntax, "val a: 42d = 42d")
    assertEquals(q"val a: 42.0d = 42.0d".syntax, "val a: 42.0d = 42.0d")
    assertEquals(q"val a: 42f = 42f".syntax, "val a: 42f = 42f")
    assertEquals(q"val a: 42.0f = 42.0f".syntax, "val a: 42.0f = 42.0f")
    assertEquals(q"val a: true = true".syntax, "val a: true = true")
    assertEquals(q"val a: false = false".syntax, "val a: false = false")
  }

  test("type with a literal type param (#2725)") {
    assertEquals(t"Foo[42]".syntax, "Foo[42]")
    assertEquals(t"Foo @@ 42".syntax, "Foo @@ 42")
    assertEquals(t"Foo[true]".syntax, "Foo[true]")
    assertEquals(q"val x = new Foo[42]".syntax, "val x = new Foo[42]")
  }

}
