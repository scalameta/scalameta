package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala213

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
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large
         |2147483648
         |^""".stripMargin.replace("\n", EOL)
    ) { term("2147483648") }
  }

  test("2147483647") {
    assertTree(term("2147483647"))(int(2147483647))
  }

  test("-2147483648") {
    assertTree(term("-2147483648"))(int(-2147483648))
  }

  test("-2147483649") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small
         |-2147483649
         | ^""".stripMargin.replace("\n", EOL)
    ) { term("-2147483649") }
  }

  test("42L") {
    assertTree(term("42L"))(Lit.Long(42L))
  }

  test("2147483648L") {
    assertTree(term("2147483648L"))(Lit.Long(2147483648L))
  }

  test("9223372036854775808L") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large
         |9223372036854775808L
         |^""".stripMargin.replace("\n", EOL)
    ) { term("9223372036854775808L") }
  }

  test("9223372036854775807L") {
    assertTree(term("9223372036854775807L"))(Lit.Long(9223372036854775807L))
  }

  test("-9223372036854775808L") {
    assertTree(term("-9223372036854775808L"))(Lit.Long(-9223372036854775808L))
  }

  test("-9223372036854775809L") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small
         |-9223372036854775809L
         | ^""".stripMargin.replace("\n", EOL)
    ) { term("-9223372036854775809L") }
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
    assertEquals(minusOne.tokens.structure, "Tokens(Ident(-) [4..5), Constant.Int(1) [5..6))")
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

  test("binary literals") {
    matchSubStructureWithDialect[Stat](
      "0b1",
      { case Lit(1) => () },
      dialects.Scala213
    )

    matchSubStructureWithDialect[Stat](
      "0b_0010_1010",
      { case Lit(42) => () },
      dialects.Scala213
    )

    matchSubStructureWithDialect[Stat](
      "0B00101010L",
      { case Lit(42) => () },
      dialects.Scala213
    )

  }

  test("numeric literals with separators") {
    runTestAssert[Stat]("1_000_000_000.0", "1000000000.0d")(dbl("1000000000"))
    runTestAssert[Stat]("1_000_000_000d", "1000000000d")(dbl("1000000000"))
    runTestAssert[Stat]("1_000_000_000D", "1000000000d")(dbl("1000000000"))
    runTestAssert[Stat]("1000000000d")(dbl("1000000000d"))
    runTestAssert[Stat]("1000000000D", "1000000000d")(dbl("1000000000d"))
    runTestAssert[Stat]("1_000_000_000l", "1000000000L")(lit(1000000000L))
    runTestAssert[Stat]("1_000_000_000L", "1000000000L")(lit(1000000000L))
  }

  // non-decimal, signed overflow within unsigned range

  test("0xffffffff") {
    assertTree(term("0xffffffff"))(Lit.Int(-1))
    assertTree(term("-0xffffffff"))(Lit.Int(1))
  }

  test("0xffffffff0") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large
         |0xffffffff0
         |^""".stripMargin.replace("\n", EOL)
    )(term("0xffffffff0"))
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small
         |-0xffffffff0
         | ^""".stripMargin.replace("\n", EOL)
    )(term("-0xffffffff0"))
  }

  test("0b11111111111111111111111111111111") { // 32
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large
         |0b11111111111111111111111111111111
         |^""".stripMargin.replace("\n", EOL)
    )(term("0b11111111111111111111111111111111"))
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small
         |-0b11111111111111111111111111111111
         | ^""".stripMargin.replace("\n", EOL)
    )(term("-0b11111111111111111111111111111111"))
  }

  test("0b111111111111111111111111111111110") { // 33
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large
         |0b111111111111111111111111111111110
         |^""".stripMargin.replace("\n", EOL)
    )(term("0b111111111111111111111111111111110"))
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small
         |-0b111111111111111111111111111111110
         | ^""".stripMargin.replace("\n", EOL)
    )(term("-0b111111111111111111111111111111110"))
  }

  test("0xffffffffffffffffL") {
    assertTree(term("0xffffffffffffffffL"))(Lit.Long(-1L))
    assertTree(term("-0xffffffffffffffffL"))(Lit.Long(1L))
  }

  test("0xffffffffffffffff0L") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large
         |0xffffffffffffffff0L
         |^""".stripMargin.replace("\n", EOL)
    )(term("0xffffffffffffffff0L"))
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small
         |-0xffffffffffffffff0L
         | ^""".stripMargin.replace("\n", EOL)
    )(term("-0xffffffffffffffff0L"))
  }

}
