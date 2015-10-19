package scala.meta.tests
package parsers

import scala.meta.parsers.common.ParseException
import scala.meta.internal.ast._
import scala.meta.dialects.Scala211

class LitSuite extends ParseSuite {
  test("true") {
    val Lit(true) = term("true")
  }

  test("false") {
    val Lit(false) = term("false")
  }

  test("42") {
    val Lit(42) = term("42")
  }

  test("2147483648") {
    intercept[ParseException] { term("2147483648") }
  }

  test("2147483647") {
    val Lit(2147483647) = term("2147483647")
  }

  test("-2147483648") {
    val Lit(-2147483648) = term("-2147483648")
  }

  test("-2147483649") {
    intercept[ParseException] { term("-2147483649") }
  }

  test("42L") {
    val Lit(42L) = term("42L")
  }

  test("2147483648L") {
    val Lit(2147483648L) = term("2147483648L")
  }

  test("9223372036854775808L") {
    intercept[ParseException] { term("9223372036854775808L") }
  }

  test("9223372036854775807L") {
    val Lit(9223372036854775807L) = term("9223372036854775807L")
  }

  test("-9223372036854775808L") {
    val Lit(-9223372036854775808L) = term("-9223372036854775808L")
  }

  test("-9223372036854775809L") {
    intercept[ParseException] { term("-9223372036854775809L") }
  }

  test("42.42") {
    val Lit(42.42) = term("42.42")
  }

  test("42.0f") {
    val Lit(42.42f) = term("42.42f")
  }

  test("'c'") {
    val Lit('c') = term("'c'")
  }

  test("\"foo\"") {
    val Lit("foo") = term("\"foo\"")
  }

  test("'foo'") {
    val Lit('foo) = term("'foo")
  }

  test("null") {
    val Lit(null) = term("null")
  }

  test("()") {
    val Lit(()) = term("()")
  }
}

