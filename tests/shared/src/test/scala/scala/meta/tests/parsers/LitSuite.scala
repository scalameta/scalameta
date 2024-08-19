package scala.meta.tests
package parsers

import scala.meta._

class LitSuite extends ParseSuite {

  implicit val dialect: Dialect = dialects.Scala213

  test("true")(assertTree(term("true"))(bool(true)))

  test("false")(assertTree(term("false"))(bool(false)))

  test("42")(assertTree(term("42"))(int(42)))

  test("2147483648") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large for Int
         |2147483648
         |^""".stripMargin.lf2nl
    )(term("2147483648"))
  }

  test("2147483647")(assertTree(term("2147483647"))(int(2147483647)))

  test("-2147483648")(assertTree(term("-2147483648"))(int(-2147483648)))

  test("-2147483649") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small for Int
         |-2147483649
         | ^""".stripMargin.lf2nl
    )(term("-2147483649"))
  }

  test("42L")(assertTree(term("42L"))(Lit.Long(42L)))

  test("2147483648L")(assertTree(term("2147483648L"))(Lit.Long(2147483648L)))

  test("9223372036854775808L") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large for Long
         |9223372036854775808L
         |^""".stripMargin.lf2nl
    )(term("9223372036854775808L"))
  }

  test("9223372036854775807L") {
    assertTree(term("9223372036854775807L"))(Lit.Long(9223372036854775807L))
  }

  test("-9223372036854775808L") {
    assertTree(term("-9223372036854775808L"))(Lit.Long(-9223372036854775808L))
  }

  test("-9223372036854775809L") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small for Long
         |-9223372036854775809L
         | ^""".stripMargin.lf2nl
    )(term("-9223372036854775809L"))
  }

  test("42.42")(matchSubStructure[Stat]("42.42", { case Lit(42.42) => () }))

  test("42.0f")(matchSubStructure[Stat]("42.42f", { case Lit(42.42f) => () }))

  test("'c'")(assertTree(term("'c'"))(Lit.Char('c')))

  test("\"foo\"")(assertTree(term("\"foo\""))(str("foo")))

  test("'foo'")(assertTree(term("'foo"))(Lit.Symbol('foo)))

  test("null")(assertTree(term("null"))(Lit.Null()))

  test("()")(assertTree(term("()"))(Lit.Unit()))

  test("0xCAFEBABE") {
    matchSubStructure[Stat]("0xCAFEBABE", { case Lit(-889275714) => () })
    matchSubStructure[Stat]("-0xCAFEBABE", { case Lit(889275714) => () })
  }

  test("#344") {
    val minusOne = term("1 + -1").collect { case Term.ApplyInfix(_, _, _, List(minusOne)) =>
      minusOne
    }.head
    assertEquals(minusOne.tokens.structure, "Tokens(Ident(-) [4..5), Constant.Int(1) [5..6))")
  }

  test("#342")(assertTree(term("""( 50).toString"""))(Term.Select(int(50), tname("toString"))))

  test("#360") {
    val result = """ "sobaka """.parse[Stat]
    assert(result.isInstanceOf[Parsed.Error])
  }

  test("#1382") {
    matchSubStructure[Stat]("\"\"\"\"\"\"\"\"", { case Lit("\"\"") => () })
    matchSubStructure[Stat]("\"\"\"\"\"\"\"\"\"\"\"\"\"", { case Lit("\"\"\"\"\"\"\"") => () })

    assertTree(term("raw\"\"\"\"\"\"\"\""))(Term.Interpolate(tname("raw"), List(str("\"\"")), Nil))
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
    checkStat(code2)(Defn.Trait(
      Nil,
      pname("Foo"),
      Nil,
      EmptyCtor(),
      tpl(Defn.Def(Nil, tname("negate"), Nil, Nil, Some(pname("-")), tname("-")))
    ))
  }

  test("simple-expression-parse-error") {
    intercept[parsers.ParseException](templStat("def neg: Unit = 2 + throw"))
  }

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

  test("0xffffffff0") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large for Int
         |0xffffffff0
         |^""".stripMargin.lf2nl
    )(term("0xffffffff0"))
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small for Int
         |-0xffffffff0
         | ^""".stripMargin.lf2nl
    )(term("-0xffffffff0"))
  }

  test("0b11111111111111111111111111111111") { // 32
    assertTree(term("0b11111111111111111111111111111111"))(Lit.Int(-1))
    assertTree(term("-0b11111111111111111111111111111111"))(Lit.Int(1))
  }

  test("0b111111111111111111111111111111110") { // 33
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large for Int
         |0b111111111111111111111111111111110
         |^""".stripMargin.lf2nl
    )(term("0b111111111111111111111111111111110"))
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small for Int
         |-0b111111111111111111111111111111110
         | ^""".stripMargin.lf2nl
    )(term("-0b111111111111111111111111111111110"))
  }

  test("0xffffffffffffffffL") {
    assertTree(term("0xffffffffffffffffL"))(Lit.Long(-1L))
    assertTree(term("-0xffffffffffffffffL"))(Lit.Long(1L))
  }

  test("0xffffffffffffffff0L") {
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too large for Long
         |0xffffffffffffffff0L
         |^""".stripMargin.lf2nl
    )(term("0xffffffffffffffff0L"))
    interceptMessage[ParseException](
      """|<input>:1: error: integer number too small for Long
         |-0xffffffffffffffff0L
         | ^""".stripMargin.lf2nl
    )(term("-0xffffffffffffffff0L"))
  }

  test("unary: +1") {
    runTestAssert[Stat]("+1", "1")(lit(1))
    val tree = Term.Apply(lit(1), List(lit(0)))
    runTestAssert[Stat]("+1(0)", "1(0)")(tree)
  }

  test("unary: -1") {
    runTestAssert[Stat]("-1")(lit(-1))
    val tree = Term.Apply(lit(-1), List(lit(0)))
    runTestAssert[Stat]("-1(0)")(tree)
  }

  test("unary: ~1") {
    runTestAssert[Stat]("~1", "-2")(lit(-2))
    val tree = Term.Apply(lit(-2), List(lit(0)))
    runTestAssert[Stat]("~1(0)", "-2(0)")(tree)
  }

  test("unary: !1") {
    runTestAssert[Stat]("!1")(Term.ApplyUnary(tname("!"), lit(1)))
    val tree = Term.ApplyUnary(tname("!"), Term.Apply(lit(1), List(lit(0))))
    runTestAssert[Stat]("!1(0)")(tree)
  }

  test("unary: +1.0") {
    runTestAssert[Stat]("+1.0", "1.0d")(lit(1d))
    val tree = Term.Apply(lit(1d), List(lit(0)))
    runTestAssert[Stat]("+1.0(0)", "1.0d(0)")(tree)
  }

  test("unary: -1.0") {
    runTestAssert[Stat]("-1.0", "-1.0d")(lit(-1d))
    val tree = Term.Apply(lit(-1d), List(lit(0)))
    runTestAssert[Stat]("-1.0(0)", "-1.0d(0)")(tree)
  }

  test("unary: ~1.0") {
    val tree = Term.ApplyUnary(tname("~"), lit(1d))
    runTestAssert[Stat]("~1.0", "~1.0d")(tree)
    runTestAssert[Stat]("~1.0(0)", "(~1.0d)(0)")(Term.Apply(tree, List(lit(0))))
  }

  test("unary: !1.0") {
    runTestAssert[Stat]("!1.0", "!1.0d")(Term.ApplyUnary(tname("!"), lit(1d)))
    val tree = Term.ApplyUnary(tname("!"), Term.Apply(lit(1d), List(lit(0))))
    runTestAssert[Stat]("!1.0(0)", "!1.0d(0)")(tree)
  }

  test("unary: !true") {
    runTestAssert[Stat]("!true", "false")(lit(false))
    val tree = Term.Apply(lit(false), List(lit(0)))
    runTestAssert[Stat]("!true(0)", "false(0)")(tree)
  }

  test("unary: !false") {
    runTestAssert[Stat]("!false", "true")(lit(true))
    val tree = Term.Apply(lit(true), List(lit(0)))
    runTestAssert[Stat]("!false(0)", "true(0)")(tree)
  }

  test("scalatest-like infix without literal") {
    val code = """|behavior of something {
                  |  a shouldBe b
                  |}
                  |""".stripMargin
    val layout = """|behavior of something {
                    |  a shouldBe b
                    |}
                    |""".stripMargin
    val tree = Term.ApplyInfix(
      tname("behavior"),
      tname("of"),
      Nil,
      Term.Apply(
        tname("something"),
        blk(Term.ApplyInfix(tname("a"), tname("shouldBe"), Nil, List(tname("b")))) :: Nil
      ) :: Nil
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scalatest-like infix with literal") {
    val code = """|behavior of "..." {
                  |  a shouldBe b
                  |}
                  |""".stripMargin
    val tree = Term.ApplyInfix(
      tname("behavior"),
      tname("of"),
      Nil,
      Term.Apply(
        str("..."),
        blk(Term.ApplyInfix(tname("a"), tname("shouldBe"), Nil, List(tname("b")))) :: Nil
      ) :: Nil
    )
    runTestAssert[Stat](code)(tree)
  }

  Seq(
    ("0d", 0d),
    ("0.0", 0d),
    ("00e0", 0d),
    ("00e0f", 0f),
    ("0xe1", 225),
    ("0xd", 13),
    ("0f", 0f),
    ("0.0f", 0f),
    ("0xf", 15),
    ("0", 0),
    ("0l", 0L),
    ("0L", 0L),
    ("0x80000000", -2147483648),
    ("0x8000000000000000L", -9223372036854775808L),
    ("3.4028235e38f", Float.MaxValue),
    ("-3.4028235e38f", Float.MinValue),
    ("1.7976931348623157e+308d", Double.MaxValue),
    ("-1.7976931348623157e+308d", Double.MinValue),
    ("0b00101010", 42),
    ("0B_0010_1010", 42),
    ("0b_0010_1010L", 42L)
  ).foreach { case (code, expected) =>
    test(s"numeric literal ok scala213: $code") {
      implicit val dialect: Dialect = dialects.Scala213
      parseStat(code) match { case lit: Lit => assertEquals(lit.value, expected) }
    }
  }

  Seq(
    ("0.f", Term.Select(lit(0), tname("f"))),
    ("1.0 + 2.0f", Term.ApplyInfix(lit(1d), tname("+"), Nil, List(lit(2f)))),
    ("1d + 2f", Term.ApplyInfix(lit(1d), tname("+"), Nil, List(lit(2f)))),
    ("0b01.toString", Term.Select(lit(1), tname("toString")))
  ).foreach { case (code, tree: Tree) =>
    test(s"expr with numeric literal ok scala213: $code")(runTestAssert[Stat](code, None)(tree))
  }

  Seq(
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
