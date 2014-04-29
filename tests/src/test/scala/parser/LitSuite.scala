import scala.reflect.core._

class LitSuite extends ParseSuite {
  test("true") {
    val Lit.True() = term("true")
  }

  test("false") {
    val Lit.False() = term("false")
  }

  test("42") {
    val Lit.Int(42) = term("42")
  }

  test("42L") {
    val Lit.Long(42L) = term("42L")
  }

  test("42.0") {
    val Lit.Double(42.0) = term("42.0")
  }

  test("42.0f") {
    val Lit.Float(42.0f) = term("42.0f")
  }

  test("'c'") {
    val Lit.Char('c') = term("'c'")
  }

  test("\"foo\"") {
    val Lit.String("foo") = term("\"foo\"")
  }

  test("'foo'") {
    val Lit.Symbol('foo) = term("'foo")
  }

  test("null") {
    val Lit.Null() = term("null")
  }

  test("()") {
    val Lit.Unit() = term("()")
  }
}

