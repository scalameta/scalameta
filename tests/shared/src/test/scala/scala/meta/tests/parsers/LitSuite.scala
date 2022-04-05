package scala.meta.tests
package parsers

import scala.meta._
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

  test("0xCAFEBABE") {
    val Lit(-889275714) = term("0xCAFEBABE")
    val Lit(889275714) = term("-0xCAFEBABE")
  }

  test("#344") {
    val Term.ApplyInfix(_, _, _, List(minusOne)) = term("1 + -1")
    assert(minusOne.tokens.structure == "Tokens(- [4..5), 1 [5..6))")
  }

  test("#342") {
    assertTree(term("""( 50).toString"""))(Term.Select(Lit.Int(50), Term.Name("toString")))
  }

  test("#360") {
    val result = """ "sobaka """.parse[Stat]
    assert(result.isInstanceOf[Parsed.Error])
  }

  test("#1382") {
    val Lit("\"\"") = term("\"\"\"\"\"\"\"\"")
    val Lit("\"\"\"\"\"\"\"") = term("\"\"\"\"\"\"\"\"\"\"\"\"\"")

    val Term.Interpolate(Name("raw"), List(Lit("\"\"")), Nil) = term("raw\"\"\"\"\"\"\"\"")
    val Term.Interpolate(Name("raw"), List(Lit("\"\"\"\"\"\"\"")), Nil) =
      term("raw\"\"\"\"\"\"\"\"\"\"\"\"\"")
  }

  test("minus-sign") {
    val code = """|object X {
                  |  sealed trait Foo {
                  |    def negate1 : - = -
                  |    def negate2 : - = -.fn("d")
                  |  }
                  |  trait -
                  |  case object - extends -
                  |}
                  |""".stripMargin
    source(code)

    val code2 = "trait Foo { def negate: - = - }"
    checkStat(code2)(
      Defn.Trait(
        Nil,
        Type.Name("Foo"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Defn.Def(Nil, Term.Name("negate"), Nil, Nil, Some(Type.Name("-")), Term.Name("-"))),
          Nil
        )
      )
    )
  }

  test("simple-expression-parse-error") {
    intercept[parsers.ParseException] {
      templStat("def neg: Unit = 2 + throw")
    }
  }
}
