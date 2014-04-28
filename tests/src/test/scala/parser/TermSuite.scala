import scala.reflect.core._

class LitSuite extends ParseSuite {
  test("parse `true`") {
    val Lit.True() = parseTerm("true")
  }

  test("parse `false`") {
    val Lit.False() = parseTerm("false")
  }

  test("parse `42`") {
    val Lit.Int(42) = parseTerm("42")
  }

  test("parse `42L`") {
    val Lit.Long(42L) = parseTerm("42L")
  }

  test("parse `42.0`") {
    val Lit.Double(42.0) = parseTerm("42.0")
  }

  test("parse `42.0f`") {
    val Lit.Float(42.0f) = parseTerm("42.0f")
  }

  test("parse `'c'`") {
    val Lit.Char('c') = parseTerm("'c'")
  }

  test("parse `\"foo\"`") {
    val Lit.String("foo") = parseTerm("\"foo\"")
  }

  test("parse `'foo'") {
    val Lit.Symbol('foo) = parseTerm("'foo")
  }

  test("parse `null`") {
    val Lit.Null() = parseTerm("null")
  }

  test("parse `()`") {
    val Lit.Unit() = parseTerm("()")
  }
}

