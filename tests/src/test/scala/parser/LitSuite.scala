import scala.reflect.core._

class LitSuite extends ParseSuite {
  test("parse `true`") {
    val Lit.True() = term("true")
  }

  test("parse `false`") {
    val Lit.False() = term("false")
  }

  test("parse `42`") {
    val Lit.Int(42) = term("42")
  }

  test("parse `42L`") {
    val Lit.Long(42L) = term("42L")
  }

  test("parse `42.0`") {
    val Lit.Double(42.0) = term("42.0")
  }

  test("parse `42.0f`") {
    val Lit.Float(42.0f) = term("42.0f")
  }

  test("parse `'c'`") {
    val Lit.Char('c') = term("'c'")
  }

  test("parse `\"foo\"`") {
    val Lit.String("foo") = term("\"foo\"")
  }

  test("parse `'foo'") {
    val Lit.Symbol('foo) = term("'foo")
  }

  test("parse `null`") {
    val Lit.Null() = term("null")
  }

  test("parse `()`") {
    val Lit.Unit() = term("()")
  }
}

