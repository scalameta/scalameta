package scala.meta.tests.tokenizers

import scala.meta._

import munit.{Location, TestOptions}

class DedentedStringSuite extends BaseTokenizerSuite {

  override protected val dialect: Dialect = dialects.Scala3

  protected val dialectWithFlag: Dialect = dialects.Scala3.withAllowDedentedStringLiterals(true)

  /* Each case is tokenized without and with `allowDedentedStringLiterals`;
   * a single expectation covers both dialects until the flag changes the
   * outcome, then `expectedWithFlag` carries the difference. */
  private def checkStruct(
      name: TestOptions,
  )(code: String, expected: String, expectedWithFlag: String = null)(implicit loc: Location): Unit =
    test(name) {
      assertTokenizedAsStructureLines(code, expected.nl2lf, dialect)
      assertTokenizedAsStructureLines(
        code,
        Option(expectedWithFlag).getOrElse(expected).nl2lf,
        dialectWithFlag,
      )
    }

  checkStruct("basic")(
    "'''\n  i am cow\n  hear me moo\n  '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Space [4..5)
       |Space [5..6)
       |Ident(i) [6..7)
       |Space [7..8)
       |Ident(am) [8..10)
       |Space [10..11)
       |Ident(cow) [11..14)
       |LF [14..15)
       |Space [15..16)
       |Space [16..17)
       |Ident(hear) [17..21)
       |Space [21..22)
       |Ident(me) [22..24)
       |Space [24..25)
       |Ident(moo) [25..28)
       |LF [28..29)
       |Space [29..30)
       |Space [30..31)
       |Constant.Char(') [31..34)
       |EOF [34..34)
       |""".stripMargin,
  )

  checkStruct("no indentation")(
    "'''\nfoo\n'''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Ident(foo) [4..7)
       |LF [7..8)
       |Constant.Char(') [8..11)
       |EOF [11..11)
       |""".stripMargin,
  )

  checkStruct("content indented more than closing")(
    "'''\n    foo\n  '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Space [4..5)
       |Space [5..6)
       |Space [6..7)
       |Space [7..8)
       |Ident(foo) [8..11)
       |LF [11..12)
       |Space [12..13)
       |Space [13..14)
       |Constant.Char(') [14..17)
       |EOF [17..17)
       |""".stripMargin,
  )

  checkStruct("empty")(
    "'''\n  '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Space [4..5)
       |Space [5..6)
       |Constant.Char(') [6..9)
       |EOF [9..9)
       |""".stripMargin,
  )

  checkStruct("blank line")(
    "'''\n  a\n\n  b\n  '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Space [4..5)
       |Space [5..6)
       |Ident(a) [6..7)
       |LF [7..8)
       |LF [8..9)
       |Space [9..10)
       |Space [10..11)
       |Ident(b) [11..12)
       |LF [12..13)
       |Space [13..14)
       |Space [14..15)
       |Constant.Char(') [15..18)
       |EOF [18..18)
       |""".stripMargin,
  )

  checkStruct("whitespace-only line")(
    "'''\n  a\n \n  b\n  '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Space [4..5)
       |Space [5..6)
       |Ident(a) [6..7)
       |LF [7..8)
       |Space [8..9)
       |LF [9..10)
       |Space [10..11)
       |Space [11..12)
       |Ident(b) [12..13)
       |LF [13..14)
       |Space [14..15)
       |Space [15..16)
       |Constant.Char(') [16..19)
       |EOF [19..19)
       |""".stripMargin,
  )

  checkStruct("tab indentation")(
    "'''\n\t\tfoo\n\t'''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Tab [4..5)
       |Tab [5..6)
       |Ident(foo) [6..9)
       |LF [9..10)
       |Tab [10..11)
       |Constant.Char(') [11..14)
       |EOF [14..14)
       |""".stripMargin,
  )

  checkStruct("tab and space indentation")(
    "'''\n\t  foo\n    '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Tab [4..5)
       |Space [5..6)
       |Space [6..7)
       |Ident(foo) [7..10)
       |LF [10..11)
       |Space [11..12)
       |Space [12..13)
       |Space [13..14)
       |Space [14..15)
       |Constant.Char(') [15..18)
       |EOF [18..18)
       |""".stripMargin,
  )

  checkStruct("line under-indented vs closing")(
    "'''\n    a\n  b\n    '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Space [4..5)
       |Space [5..6)
       |Space [6..7)
       |Space [7..8)
       |Ident(a) [8..9)
       |LF [9..10)
       |Space [10..11)
       |Space [11..12)
       |Ident(b) [12..13)
       |LF [13..14)
       |Space [14..15)
       |Space [15..16)
       |Space [16..17)
       |Space [17..18)
       |Constant.Char(') [18..21)
       |EOF [21..21)
       |""".stripMargin,
  )

  checkStruct("content on opening line")(
    "'''foo\n  '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |Ident(foo) [3..6)
       |LF [6..7)
       |Space [7..8)
       |Space [8..9)
       |Constant.Char(') [9..12)
       |EOF [12..12)
       |""".stripMargin,
  )

  checkStruct("text before closing delimiter")(
    "'''\n  foo'''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Space [4..5)
       |Space [5..6)
       |Ident(foo) [6..9)
       |Constant.Char(') [9..12)
       |EOF [12..12)
       |""".stripMargin,
  )

  checkStruct("whitespace after opening quotes")(
    "'''  \n  foo\n  '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |Space [3..4)
       |Space [4..5)
       |LF [5..6)
       |Space [6..7)
       |Space [7..8)
       |Ident(foo) [8..11)
       |LF [11..12)
       |Space [12..13)
       |Space [13..14)
       |Constant.Char(') [14..17)
       |EOF [17..17)
       |""".stripMargin,
  )

  checkStruct("four-quote delimiter")(
    "''''\n  '''\n  foo\n  ''''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |Invalid(can't use unescaped LF in character literals) [4..4)
       |LF [4..5)
       |Space [5..6)
       |Space [6..7)
       |Constant.Char(') [7..10)
       |LF [10..11)
       |Space [11..12)
       |Space [12..13)
       |Ident(foo) [13..16)
       |LF [16..17)
       |Space [17..18)
       |Space [18..19)
       |Constant.Char(') [19..22)
       |MacroQuote [22..23)
       |EOF [23..23)
       |""".stripMargin,
  )

  checkStruct("five-quote delimiter")(
    "'''''\n  ''''\n  foo\n  '''''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |MacroQuote [3..4)
       |Invalid(can't use unescaped LF in character literals) [5..5)
       |LF [5..6)
       |Space [6..7)
       |Space [7..8)
       |Constant.Char(') [8..11)
       |Invalid(can't use unescaped LF in character literals) [12..12)
       |LF [12..13)
       |Space [13..14)
       |Space [14..15)
       |Ident(foo) [15..18)
       |LF [18..19)
       |Space [19..20)
       |Space [20..21)
       |Constant.Char(') [21..24)
       |MacroQuote [24..25)
       |MacroQuote [25..26)
       |EOF [26..26)
       |""".stripMargin,
  )

  checkStruct("two quotes in content")(
    "'''\n  ''\n  '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Space [4..5)
       |Space [5..6)
       |MacroQuote [6..7)
       |Invalid(can't use unescaped LF in character literals) [8..8)
       |LF [8..9)
       |Space [9..10)
       |Space [10..11)
       |Constant.Char(') [11..14)
       |EOF [14..14)
       |""".stripMargin,
  )

  checkStruct("longer quote run at closing")(
    "'''\n  foo\n  ''''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Space [4..5)
       |Space [5..6)
       |Ident(foo) [6..9)
       |LF [9..10)
       |Space [10..11)
       |Space [11..12)
       |Constant.Char(') [12..15)
       |MacroQuote [15..16)
       |EOF [16..16)
       |""".stripMargin,
  )

  checkStruct("unclosed")(
    "'''\n  foo",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |LF [3..4)
       |Space [4..5)
       |Space [5..6)
       |Ident(foo) [6..9)
       |EOF [9..9)
       |""".stripMargin,
  )

  checkStruct("CRLF line endings")(
    "'''\r\n  foo\r\n  '''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |CRLF [3..5)
       |Space [5..6)
       |Space [6..7)
       |Ident(foo) [7..10)
       |CRLF [10..12)
       |Space [12..13)
       |Space [13..14)
       |Constant.Char(') [14..17)
       |EOF [17..17)
       |""".stripMargin,
  )

  checkStruct("in val definition")(
    "val x = '''\n  foo\n  '''",
    """|BOF [0..0)
       |KwVal [0..3)
       |Space [3..4)
       |Ident(x) [4..5)
       |Space [5..6)
       |Equals [6..7)
       |Space [7..8)
       |Constant.Char(') [8..11)
       |LF [11..12)
       |Space [12..13)
       |Space [13..14)
       |Ident(foo) [14..17)
       |LF [17..18)
       |Space [18..19)
       |Space [19..20)
       |Constant.Char(') [20..23)
       |EOF [23..23)
       |""".stripMargin,
  )

  checkStruct("in concatenation")(
    "\"a\" + '''\n  b\n  ''' + \"c\"",
    """|BOF [0..0)
       |Constant.String(a) [0..3)
       |Space [3..4)
       |Ident(+) [4..5)
       |Space [5..6)
       |Constant.Char(') [6..9)
       |LF [9..10)
       |Space [10..11)
       |Space [11..12)
       |Ident(b) [12..13)
       |LF [13..14)
       |Space [14..15)
       |Space [15..16)
       |Constant.Char(') [16..19)
       |Space [19..20)
       |Ident(+) [20..21)
       |Space [21..22)
       |Constant.String(c) [22..25)
       |EOF [25..25)
       |""".stripMargin,
  )

  checkStruct("two quotes alone")(
    "''",
    """|BOF [0..0)
       |MacroQuote [0..1)
       |MacroQuote [1..2)
       |EOF [2..2)
       |""".stripMargin,
  )

  checkStruct("four quotes alone")(
    "''''",
    """|BOF [0..0)
       |Constant.Char(') [0..3)
       |MacroQuote [3..4)
       |EOF [4..4)
       |""".stripMargin,
  )

  checkStruct("char literal")(
    "'a'",
    """|BOF [0..0)
       |Constant.Char(a) [0..3)
       |EOF [3..3)
       |""".stripMargin,
  )

  checkStruct("escaped quote char literal")(
    "'\\''",
    """|BOF [0..0)
       |Constant.Char(') [0..4)
       |EOF [4..4)
       |""".stripMargin,
  )

  checkStruct("macro quote block")(
    "'{ foo }",
    """|BOF [0..0)
       |MacroQuote [0..1)
       |LeftBrace [1..2)
       |Space [2..3)
       |Ident(foo) [3..6)
       |Space [6..7)
       |RightBrace [7..8)
       |EOF [8..8)
       |""".stripMargin,
  )

  checkStruct("interpolation - basic")(
    "s'''\n  a $x b\n  '''",
    """|BOF [0..0)
       |Ident(s) [0..1)
       |Constant.Char(') [1..4)
       |LF [4..5)
       |Space [5..6)
       |Space [6..7)
       |Ident(a) [7..8)
       |Space [8..9)
       |Ident($x) [9..11)
       |Space [11..12)
       |Ident(b) [12..13)
       |LF [13..14)
       |Space [14..15)
       |Space [15..16)
       |Constant.Char(') [16..19)
       |EOF [19..19)
       |""".stripMargin,
  )

  checkStruct("interpolation - block splice")(
    "s'''\n  a ${x + 1} b\n  '''",
    """|BOF [0..0)
       |Ident(s) [0..1)
       |Constant.Char(') [1..4)
       |LF [4..5)
       |Space [5..6)
       |Space [6..7)
       |Ident(a) [7..8)
       |Space [8..9)
       |Ident($) [9..10)
       |LeftBrace [10..11)
       |Ident(x) [11..12)
       |Space [12..13)
       |Ident(+) [13..14)
       |Space [14..15)
       |Constant.Int(1) [15..16)
       |RightBrace [16..17)
       |Space [17..18)
       |Ident(b) [18..19)
       |LF [19..20)
       |Space [20..21)
       |Space [21..22)
       |Constant.Char(') [22..25)
       |EOF [25..25)
       |""".stripMargin,
  )

  checkStruct("interpolation - dollar escape")(
    "s'''\n  a $$ b\n  '''",
    """|BOF [0..0)
       |Ident(s) [0..1)
       |Constant.Char(') [1..4)
       |LF [4..5)
       |Space [5..6)
       |Space [6..7)
       |Ident(a) [7..8)
       |Space [8..9)
       |Ident($$) [9..11)
       |Space [11..12)
       |Ident(b) [12..13)
       |LF [13..14)
       |Space [14..15)
       |Space [15..16)
       |Constant.Char(') [16..19)
       |EOF [19..19)
       |""".stripMargin,
  )

  checkStruct("interpolation - dollar-quote escape")(
    "s'''\n  a $' b\n  '''",
    """|BOF [0..0)
       |Ident(s) [0..1)
       |Constant.Char(') [1..4)
       |LF [4..5)
       |Space [5..6)
       |Space [6..7)
       |Ident(a) [7..8)
       |Space [8..9)
       |Ident($) [9..10)
       |MacroQuote [10..11)
       |Space [11..12)
       |Ident(b) [12..13)
       |LF [13..14)
       |Space [14..15)
       |Space [15..16)
       |Constant.Char(') [16..19)
       |EOF [19..19)
       |""".stripMargin,
  )

  checkStruct("interpolation - four-quote delimiter")(
    "s''''\n  ''' $x\n  ''''",
    """|BOF [0..0)
       |Ident(s) [0..1)
       |Constant.Char(') [1..4)
       |Invalid(can't use unescaped LF in character literals) [5..5)
       |LF [5..6)
       |Space [6..7)
       |Space [7..8)
       |Constant.Char(') [8..11)
       |Space [11..12)
       |Ident($x) [12..14)
       |LF [14..15)
       |Space [15..16)
       |Space [16..17)
       |Constant.Char(') [17..20)
       |MacroQuote [20..21)
       |EOF [21..21)
       |""".stripMargin,
  )

  checkStruct("interpolation - nested dedented string")(
    "s'''\n  a ${'''\n  b\n  '''} c\n  '''",
    """|BOF [0..0)
       |Ident(s) [0..1)
       |Constant.Char(') [1..4)
       |LF [4..5)
       |Space [5..6)
       |Space [6..7)
       |Ident(a) [7..8)
       |Space [8..9)
       |Ident($) [9..10)
       |LeftBrace [10..11)
       |Constant.Char(') [11..14)
       |LF [14..15)
       |Space [15..16)
       |Space [16..17)
       |Ident(b) [17..18)
       |LF [18..19)
       |Space [19..20)
       |Space [20..21)
       |Constant.Char(') [21..24)
       |RightBrace [24..25)
       |Space [25..26)
       |Ident(c) [26..27)
       |LF [27..28)
       |Space [28..29)
       |Space [29..30)
       |Constant.Char(') [30..33)
       |EOF [33..33)
       |""".stripMargin,
  )

  checkStruct("interpolation - f id")(
    "f'''\n  v: $x%d\n  '''",
    """|BOF [0..0)
       |Ident(f) [0..1)
       |Constant.Char(') [1..4)
       |LF [4..5)
       |Space [5..6)
       |Space [6..7)
       |Ident(v) [7..8)
       |Colon [8..9)
       |Space [9..10)
       |Ident($x) [10..12)
       |Ident(%) [12..13)
       |Ident(d) [13..14)
       |LF [14..15)
       |Space [15..16)
       |Space [16..17)
       |Constant.Char(') [17..20)
       |EOF [20..20)
       |""".stripMargin,
  )

  checkStruct("interpolation - two quotes alone")(
    "s''",
    """|BOF [0..0)
       |Ident(s) [0..1)
       |MacroQuote [1..2)
       |MacroQuote [2..3)
       |EOF [3..3)
       |""".stripMargin,
  )

  checkStruct("interpolation - char-like")(
    "s'a'",
    """|BOF [0..0)
       |Ident(s) [0..1)
       |Constant.Char(a) [1..4)
       |EOF [4..4)
       |""".stripMargin,
  )

  checkStruct("interpolation - unclosed")(
    "s'''\n  foo",
    """|BOF [0..0)
       |Ident(s) [0..1)
       |Constant.Char(') [1..4)
       |LF [4..5)
       |Space [5..6)
       |Space [6..7)
       |Ident(foo) [7..10)
       |EOF [10..10)
       |""".stripMargin,
  )

  checkStruct("match case")(
    "x match { case '''\n  a\n  ''' => 1 }",
    """|BOF [0..0)
       |Ident(x) [0..1)
       |Space [1..2)
       |KwMatch [2..7)
       |Space [7..8)
       |LeftBrace [8..9)
       |Space [9..10)
       |KwCase [10..14)
       |Space [14..15)
       |Constant.Char(') [15..18)
       |LF [18..19)
       |Space [19..20)
       |Space [20..21)
       |Ident(a) [21..22)
       |LF [22..23)
       |Space [23..24)
       |Space [24..25)
       |Constant.Char(') [25..28)
       |Space [28..29)
       |RightArrow [29..31)
       |Space [31..32)
       |Constant.Int(1) [32..33)
       |Space [33..34)
       |RightBrace [34..35)
       |EOF [35..35)
       |""".stripMargin,
  )

  checkStruct("match case interpolation")(
    "x match { case s'''\n  a $y b\n  ''' => 1 }",
    """|BOF [0..0)
       |Ident(x) [0..1)
       |Space [1..2)
       |KwMatch [2..7)
       |Space [7..8)
       |LeftBrace [8..9)
       |Space [9..10)
       |KwCase [10..14)
       |Space [14..15)
       |Ident(s) [15..16)
       |Constant.Char(') [16..19)
       |LF [19..20)
       |Space [20..21)
       |Space [21..22)
       |Ident(a) [22..23)
       |Space [23..24)
       |Ident($y) [24..26)
       |Space [26..27)
       |Ident(b) [27..28)
       |LF [28..29)
       |Space [29..30)
       |Space [30..31)
       |Constant.Char(') [31..34)
       |Space [34..35)
       |RightArrow [35..37)
       |Space [37..38)
       |Constant.Int(1) [38..39)
       |Space [39..40)
       |RightBrace [40..41)
       |EOF [41..41)
       |""".stripMargin,
  )

  checkStruct("literal type")(
    "val x: '''\n  a\n  ''' = y",
    """|BOF [0..0)
       |KwVal [0..3)
       |Space [3..4)
       |Ident(x) [4..5)
       |Colon [5..6)
       |Space [6..7)
       |Constant.Char(') [7..10)
       |LF [10..11)
       |Space [11..12)
       |Space [12..13)
       |Ident(a) [13..14)
       |LF [14..15)
       |Space [15..16)
       |Space [16..17)
       |Constant.Char(') [17..20)
       |Space [20..21)
       |Equals [21..22)
       |Space [22..23)
       |Ident(y) [23..24)
       |EOF [24..24)
       |""".stripMargin,
  )

}
