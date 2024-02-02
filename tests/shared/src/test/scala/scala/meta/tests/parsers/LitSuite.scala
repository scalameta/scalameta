package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala211

class LitSuite extends ParseSuite {

  test("true") {
    assertTree(term("true"))(bool(true))
  }

  test("false") {
    assertTree(term("false"))(bool(false))
  }

  test("42") {
    assertTree(term("42"))(int(42))
  }

  test("2147483648") {
    intercept[ParseException] { term("2147483648") }
  }

  test("2147483647") {
    assertTree(term("2147483647"))(int(2147483647))
  }

  test("-2147483648") {
    assertTree(term("-2147483648"))(int(-2147483648))
  }

  test("-2147483649") {
    intercept[ParseException] { term("-2147483649") }
  }

  test("42L") {
    assertTree(term("42L"))(Lit.Long(42L))
  }

  test("2147483648L") {
    assertTree(term("2147483648L"))(Lit.Long(2147483648L))
  }

  test("9223372036854775808L") {
    intercept[ParseException] { term("9223372036854775808L") }
  }

  test("9223372036854775807L") {
    assertTree(term("9223372036854775807L"))(Lit.Long(9223372036854775807L))
  }

  test("-9223372036854775808L") {
    assertTree(term("-9223372036854775808L"))(Lit.Long(-9223372036854775808L))
  }

  test("-9223372036854775809L") {
    intercept[ParseException] { term("-9223372036854775809L") }
  }

  test("42.42") {
    matchSubStructure[Stat](
      "42.42",
      { case Lit(42.42) => () }
    )
  }

  test("42.0f") {
    matchSubStructure[Stat](
      "42.42f",
      { case Lit(42.42f) => () }
    )
  }

  test("'c'") {
    assertTree(term("'c'"))(Lit.Char('c'))
  }

  test("\"foo\"") {
    assertTree(term("\"foo\""))(str("foo"))
  }

  test("'foo'") {
    assertTree(term("'foo"))(Lit.Symbol('foo))
  }

  test("null") {
    assertTree(term("null"))(Lit.Null())
  }

  test("()") {
    assertTree(term("()"))(Lit.Unit())
  }

  test("0xCAFEBABE") {
    matchSubStructure[Stat](
      "0xCAFEBABE",
      { case Lit(-889275714) => () }
    )
    matchSubStructure[Stat](
      "-0xCAFEBABE",
      { case Lit(889275714) => () }
    )
  }

  test("#344") {
    val minusOne = term("1 + -1").collect { case Term.ApplyInfix(_, _, _, List(minusOne)) =>
      minusOne
    }.head
    assert(minusOne.tokens.structure == "Tokens(- [4..5), 1 [5..6))")
  }

  test("#342") {
    assertTree(term("""( 50).toString"""))(Term.Select(int(50), tname("toString")))
  }

  test("#360") {
    val result = """ "sobaka """.parse[Stat]
    assert(result.isInstanceOf[Parsed.Error])
  }

  test("#1382") {
    matchSubStructure[Stat](
      "\"\"\"\"\"\"\"\"",
      { case Lit("\"\"") => () }
    )
    matchSubStructure[Stat](
      "\"\"\"\"\"\"\"\"\"\"\"\"\"",
      { case Lit("\"\"\"\"\"\"\"") => () }
    )

    assertTree(term("raw\"\"\"\"\"\"\"\""))(
      Term.Interpolate(tname("raw"), List(str("\"\"")), Nil)
    )
    assertTree(term("raw\"\"\"\"\"\"\"\"\"\"\"\"\""))(
      Term.Interpolate(tname("raw"), List(str("\"\"\"\"\"\"\"")), Nil)
    )
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
        pname("Foo"),
        Nil,
        EmptyCtor(),
        tpl(Defn.Def(Nil, tname("negate"), Nil, Nil, Some(pname("-")), tname("-")))
      )
    )
  }

  test("simple-expression-parse-error") {
    intercept[parsers.ParseException] {
      templStat("def neg: Unit = 2 + throw")
    }
  }
}
