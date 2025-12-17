package scala.meta.tests
package parsers

import scala.meta._

class LitSuite extends ParseSuite {

  implicit val dialect: Dialect = dialects.Scala213

  test("true")(assertTree(term("true"))(bool(true)))

  test("false")(assertTree(term("false"))(bool(false)))

  test("42")(assertTree(term("42"))(int(42)))

  test("2147483647")(assertTree(term("2147483647"))(int(2147483647)))

  test("-2147483648")(assertTree(term("-2147483648"))(int(-2147483648)))

  test("42L")(assertTree(term("42L"))(lit(42L)))

  test("2147483648L")(assertTree(term("2147483648L"))(lit(2147483648L)))

  test("9223372036854775807L")(assertTree(term("9223372036854775807L"))(lit(9223372036854775807L)))

  test("-9223372036854775808L")(assertTree(term("-9223372036854775808L"))(lit(-9223372036854775808L)))

  test("42.42")(matchSubStructure[Stat]("42.42", { case Lit(42.42) => () }))

  test("42.0f")(matchSubStructure[Stat]("42.42f", { case Lit(42.42f) => () }))

  test("'c'")(assertTree(term("'c'"))(lit('c')))

  test("\"foo\"")(assertTree(term("\"foo\""))(str("foo")))

  test("'foo'")(assertTree(term("'foo"))(Lit.Symbol(Symbol("foo"))))

  test("null")(assertTree(term("null"))(Lit.Null()))

  test("()")(assertTree(term("()"))(Lit.Unit()))

  test("0xCAFEBABE") {
    matchSubStructure[Stat]("0xCAFEBABE", { case Lit(-889275714) => () })
    matchSubStructure[Stat]("-0xCAFEBABE", { case Lit(889275714) => () })
  }

  test("#344") {
    val minusOne =
      term("1 + -1") match { case Term.ApplyInfix(_, _, _, List(minusOne)) => minusOne }
    assertEquals(minusOne.tokens.structure, "Tokens(Ident(-) [4..5), Constant.Int(1) [5..6))")
  }

  test("#342")(assertTree(term("""( 50).toString"""))(tselect(int(50), "toString")))

  test("#360") {
    val result = """ "sobaka """.parse[Stat]
    assert(result.isInstanceOf[Parsed.Error])
  }

  test("#1382") {
    matchSubStructure[Stat]("\"\"\"\"\"\"\"\"", { case Lit("\"\"") => () })
    matchSubStructure[Stat]("\"\"\"\"\"\"\"\"\"\"\"\"\"", { case Lit("\"\"\"\"\"\"\"") => () })

    assertTree(term("raw\"\"\"\"\"\"\"\""))(Term.Interpolate(tname("raw"), List(str("\"\"")), Nil))
    assertTree(
      term("raw\"\"\"\"\"\"\"\"\"\"\"\"\"")
    )(Term.Interpolate(tname("raw"), List(str("\"\"\"\"\"\"\"")), Nil))
  }

  test("minus-sign") {
    val code =
      """|object X {
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
    checkStat(code2)(Defn.Trait(
      Nil,
      pname("Foo"),
      Nil,
      EmptyCtor(),
      tpl(Defn.Def(Nil, tname("negate"), Nil, Nil, Some(pname("-")), tname("-")))
    ))
  }

  test("simple-expression-parse-error")(
    intercept[parsers.ParseException](templStat("def neg: Unit = 2 + throw"))
  )

  test("binary literals") {
    matchSubStructure[Stat]("0b1", { case Lit(1) => () })

    matchSubStructure[Stat]("0b_0010_1010", { case Lit(42) => () })

    matchSubStructure[Stat]("0B00101010L", { case Lit(42) => () })

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

  test("0b11111111111111111111111111111111") { // 32
    assertTree(term("0b11111111111111111111111111111111"))(Lit.Int(-1))
    assertTree(term("-0b11111111111111111111111111111111"))(Lit.Int(1))
  }

  test("0xffffffffffffffffL") {
    assertTree(term("0xffffffffffffffffL"))(lit(-1L))
    assertTree(term("-0xffffffffffffffffL"))(lit(1L))
  }

  test("unary: +1") {
    val number = lit(1)
    runTestAssert[Stat]("+1", "1")(number)
    val tree = tapply(number, lit(0))
    runTestAssert[Stat]("+1(0)", "1(0)")(tree)
  }

  test("unary: -1") {
    val number = lit(-1)
    runTestAssert[Stat]("-1")(number)
    val tree = tapply(number, lit(0))
    runTestAssert[Stat]("-1(0)")(tree)
  }

  test("unary: ~1") {
    val number = lit(-2)
    runTestAssert[Stat]("~1", "-2")(number)
    val tree = tapply(number, lit(0))
    runTestAssert[Stat]("~1(0)", "-2(0)")(tree)
  }

  test("unary: !1") {
    val number = lit(1)
    runTestAssert[Stat]("!1")(Term.ApplyUnary(tname("!"), number))
    val tree = Term.ApplyUnary(tname("!"), tapply(number, lit(0)))
    runTestAssert[Stat]("!1(0)")(tree)
  }

  test("unary: +1.0") {
    val number = lit(1d)
    runTestAssert[Stat]("+1.0", "1.0d")(number)
    val tree = tapply(number, lit(0))
    runTestAssert[Stat]("+1.0(0)", "1.0d(0)")(tree)
  }

  test("unary: -1.0") {
    val number = lit(-1d)
    runTestAssert[Stat]("-1.0", "-1.0d")(number)
    val tree = tapply(number, lit(0))
    runTestAssert[Stat]("-1.0(0)", "-1.0d(0)")(tree)
  }

  test("unary: ~1.0") {
    val tree = lit("~", lit(1d))
    runTestAssert[Stat]("~1.0", "~1.0d")(tree)
    runTestAssert[Stat]("~1.0(0)", "~1.0d(0)")(tapply(tree, lit(0)))
  }

  test("unary: !1.0") {
    val number = lit(1d)
    runTestAssert[Stat]("!1.0", "!1.0d")(Term.ApplyUnary(tname("!"), number))
    val tree = Term.ApplyUnary(tname("!"), tapply(number, lit(0)))
    runTestAssert[Stat]("!1.0(0)", "!1.0d(0)")(tree)
  }

  test("unary: !true") {
    val number = lit(false)
    runTestAssert[Stat]("!true", "false")(number)
    val tree = tapply(number, lit(0))
    runTestAssert[Stat]("!true(0)", "false(0)")(tree)
  }

  test("unary: !false") {
    val number = lit(true)
    runTestAssert[Stat]("!false", "true")(number)
    val tree = tapply(number, lit(0))
    runTestAssert[Stat]("!false(0)", "true(0)")(tree)
  }

  test("scalatest-like infix without literal") {
    val code =
      """|behavior of something {
         |  a shouldBe b
         |}
         |""".stripMargin
    val layout =
      """|behavior of something {
         |  a shouldBe b
         |}
         |""".stripMargin
    val tree = tinfix(
      tname("behavior"),
      "of",
      tapply(tname("something"), blk(tinfix(tname("a"), "shouldBe", tname("b"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalatest-like infix with literal") {
    val code =
      """|behavior of "..." {
         |  a shouldBe b
         |}
         |""".stripMargin
    val tree = tinfix(
      tname("behavior"),
      "of",
      tapply(str("..."), blk(tinfix(tname("a"), "shouldBe", tname("b"))))
    )
    runTestAssert[Stat](code)(tree)
  }

  Seq(
    ("0d", lit(0d), 0d, "0d"),
    ("0.0", lit(0d), 0d, "0.0d"),
    ("00e0", lit(0d), 0d, "0d"),
    ("00e0f", lit(0f), 0f, "0f"),
    ("0xe1", lit(225), 225, "225"),
    ("0xd", lit(13), 13, "13"),
    ("0f", lit(0f), 0f, "0f"),
    ("0.0f", lit(0f), 0f, "0.0f"),
    ("0xf", lit(15), 15, "15"),
    ("0", lit(0), 0, "0"),
    ("0l", lit(0L), 0L, "0L"),
    ("0L", lit(0L), 0L, "0L"),
    ("0x80000000", lit(-2147483648), -2147483648, "-2147483648"),
    (
      "0x8000000000000000L",
      lit(-9223372036854775808L),
      -9223372036854775808L,
      "-9223372036854775808L"
    ),
    ("3.4028235e38f", flt("3.4028235E+38"), Float.MaxValue, "3.4028235E+38f"),
    ("-3.4028235e38f", flt("-3.4028235E+38"), Float.MinValue, "-3.4028235E+38f"),
    (
      "1.7976931348623157e+308d",
      dbl("1.7976931348623157E+308"),
      Double.MaxValue,
      "1.7976931348623157E+308d"
    ),
    (
      "-1.7976931348623157e+308d",
      dbl("-1.7976931348623157E+308"),
      Double.MinValue,
      "-1.7976931348623157E+308d"
    ),
    ("1e-500d", dbl("1E-500"), 0d, "1E-500d"),
    ("-1E-500d", dbl("-1E-500"), 0d, "-1E-500d"),
    ("2.4E-324d", dbl("2.4E-324"), 0d, "2.4E-324d"),
    ("-2.4e-324d", dbl("-2.4E-324"), 0d, "-2.4E-324d"),
    ("2.5e-324d", dbl("2.5E-324"), Double.MinPositiveValue, "2.5E-324d"),
    ("-2.5E-324d", dbl("-2.5E-324"), -Double.MinPositiveValue, "-2.5E-324d"),
    ("4.8E-324d", dbl("4.8E-324"), Double.MinPositiveValue, "4.8E-324d"),
    ("-4.8e-324d", dbl("-4.8E-324"), -Double.MinPositiveValue, "-4.8E-324d"),
    ("1E-500", dbl("1E-500"), 0d, "1E-500d"),
    ("-1e-500", dbl("-1E-500"), 0d, "-1E-500d"),
    ("2.4e-324", dbl("2.4E-324"), 0d, "2.4E-324d"),
    ("-2.4E-324", dbl("-2.4E-324"), 0d, "-2.4E-324d"),
    ("2.5E-324", dbl("2.5E-324"), Double.MinPositiveValue, "2.5E-324d"),
    ("-2.5e-324", dbl("-2.5E-324"), -Double.MinPositiveValue, "-2.5E-324d"),
    ("4.8e-324", dbl("4.8E-324"), Double.MinPositiveValue, "4.8E-324d"),
    ("-4.8E-324", dbl("-4.8E-324"), -Double.MinPositiveValue, "-4.8E-324d"),
    ("0b00101010", lit(42), 42, "42"),
    ("0B_0010_1010", lit(42), 42, "42"),
    ("0b_0010_1010L", lit(42L), 42L, "42L")
  ).foreach { case (code, tree: Lit, number, syntax) =>
    test(s"numeric literal ok scala213: $code") {
      implicit val dialect: Dialect = dialects.Scala213
      runTestAssert[Stat](code, syntax)(tree)
      assertEquals(tree.value, number)
    }
  }

  Seq(
    ("0.f", tselect(lit(0), "f")),
    ("1.0 + 2.0f", tinfix(lit(1d), "+", lit(2f))),
    ("1d + 2f", tinfix(lit(1d), "+", lit(2f))),
    ("0b01.toString", tselect(lit(1), "toString"))
  ).foreach { case (code, tree: Tree) =>
    test(s"expr with numeric literal ok scala213: $code")(runTestAssert[Stat](code, None)(tree))
  }

  Seq(
    (
      "2147483648",
      """|<input>:1: error: integer number out of range for Int
         |2147483648
         |^""".stripMargin
    ),
    (
      "-2147483649",
      """|<input>:1: error: integer number out of range for Int
         |-2147483649
         | ^""".stripMargin
    ),
    (
      "9223372036854775808L",
      """|<input>:1: error: integer number out of range for Long
         |9223372036854775808L
         |^""".stripMargin
    ),
    (
      "-9223372036854775809L",
      """|<input>:1: error: integer number out of range for Long
         |-9223372036854775809L
         | ^""".stripMargin
    ),
    (
      "0xffffffff0",
      """|<input>:1: error: integer number out of range for Int
         |0xffffffff0
         |^""".stripMargin
    ),
    (
      "-0xffffffff0",
      """|<input>:1: error: integer number out of range for Int
         |-0xffffffff0
         | ^""".stripMargin
    ),
    (
      "0b111111111111111111111111111111110", // 33
      """|<input>:1: error: integer number out of range for Int
         |0b111111111111111111111111111111110
         |^""".stripMargin
    ),
    (
      "-0b111111111111111111111111111111110",
      """|<input>:1: error: integer number out of range for Int
         |-0b111111111111111111111111111111110
         | ^""".stripMargin
    ),
    (
      "0xffffffffffffffff0L",
      """|<input>:1: error: integer number out of range for Long
         |0xffffffffffffffff0L
         |^""".stripMargin
    ),
    (
      "-0xffffffffffffffff0L",
      """|<input>:1: error: integer number out of range for Long
         |-0xffffffffffffffff0L
         | ^""".stripMargin
    ),
    (
      "1.7976931348623158e+308",
      """|<input>:1: error: floating-point value out of range for Double
         |1.7976931348623158e+308
         |^""".stripMargin
    ),
    (
      "-1.7976931348623158e+308",
      """|<input>:1: error: floating-point value out of range for Double
         |-1.7976931348623158e+308
         | ^""".stripMargin
    ),
    (
      "1e10_0000_000_000",
      """|<input>:1: error: malformed floating-point Double number
         |1e10_0000_000_000
         |^""".stripMargin
    ),
    (
      "1_000_000_000_000",
      """|<input>:1: error: integer number out of range for Int
         |1_000_000_000_000
         |^""".stripMargin
    ),
    (
      "1_000_000_000_000_000_000_000l",
      """|<input>:1: error: integer number out of range for Long
         |1_000_000_000_000_000_000_000l
         |^""".stripMargin
    ),
    (
      "00",
      """|<input>:1: error: Non-zero integral values may not have a leading zero.
         |00
         |^""".stripMargin
    ),
    (
      "00l",
      """|<input>:1: error: Non-zero integral values may not have a leading zero.
         |00l
         |^""".stripMargin
    ),
    (
      "0b01d",
      """|<input>:1: error: Invalid literal number, followed by identifier character
         |0b01d
         |    ^""".stripMargin
    ),
    (
      "0b01f",
      """|<input>:1: error: Invalid literal number, followed by identifier character
         |0b01f
         |    ^""".stripMargin
    ),
    (
      "0b0123",
      """|<input>:1: error: Invalid literal number, followed by identifier character
         |0b0123
         |    ^""".stripMargin
    ),
    (
      "0b01.2",
      """|<input>:1: error: `;` expected but `double constant` found
         |0b01.2
         |    ^""".stripMargin
    ),
    (
      "1. + 2.",
      """|<input>:1: error: `;` expected but `integer constant` found
         |1. + 2.
         |     ^""".stripMargin
    ),
    (
      ".f",
      """|<input>:1: error: illegal start of definition `.`
         |.f
         |^""".stripMargin
    ),
    (
      "3.4028236e38f",
      """|<input>:1: error: floating-point value out of range for Float
         |3.4028236e38f
         |^""".stripMargin
    ),
    (
      "-3.4028236e38f",
      """|<input>:1: error: floating-point value out of range for Float
         |-3.4028236e38f
         | ^""".stripMargin
    ),
    (
      "1.7976931348623158e+308d",
      """|<input>:1: error: floating-point value out of range for Double
         |1.7976931348623158e+308d
         |^""".stripMargin
    ),
    (
      "-1.7976931348623158e+308d",
      """|<input>:1: error: floating-point value out of range for Double
         |-1.7976931348623158e+308d
         | ^""".stripMargin
    )
  ).foreach { case (code, error) =>
    test(s"numeric literal fail scala213: $code")(runTestError[Stat](code, error))
  }

}
