package scala.meta.tests
package scalameta
package parsers

import scala.meta.ParseException
import scala.meta.internal.ast._
import scala.meta.dialects.Scala211

class LitSuite extends ParseSuite {
  test("true") {
    val Lit.Bool(true) = term("true")
  }

  test("false") {
    val Lit.Bool(false) = term("false")
  }

  test("42") {
    val Lit.Int(42) = term("42")
  }

  test("2147483648") {
    intercept[ParseException] { term("2147483648") }
  }

  test("2147483647") {
    val Lit.Int(2147483647) = term("2147483647")
  }

  test("-2147483648") {
    val Lit.Int(-2147483648) = term("-2147483648")
  }

  test("-2147483649") {
    intercept[ParseException] { term("-2147483649") }
  }

  test("42L") {
    val Lit.Long(42L) = term("42L")
  }

  test("2147483648L") {
    val Lit.Long(2147483648L) = term("2147483648L")
  }

  test("9223372036854775808L") {
    intercept[ParseException] { term("9223372036854775808L") }
  }

  test("9223372036854775807L") {
    val Lit.Long(9223372036854775807L) = term("9223372036854775807L")
  }

  test("-9223372036854775808L") {
    val Lit.Long(-9223372036854775808L) = term("-9223372036854775808L")
  }

  test("-9223372036854775809L") {
    intercept[ParseException] { term("-9223372036854775809L") }
  }

  test("42.42") {
    val Lit.Double(42.42) = term("42.42")
  }

  test("42.0f") {
    val Lit.Float(42.42f) = term("42.42f")
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

