package scala.meta.tests
package tokenizers

import scala.meta._
import scala.meta.tests.parsers.MoreHelpers._
import scala.meta.tokenizers.TokenizerOptions
import scala.meta.tokens.Token._

class TokenizerSuite extends BaseTokenizerSuite {

  private implicit val dialect: Dialect = dialects.Scala211
  override protected implicit def tokenizerOptions: TokenizerOptions =
    new TokenizerOptions(groupWhitespace = true)

  test("showCode without comments - simple") {
    assertTokenizedAsSyntax("class C  {\t val x = 2}\n\n", "class C  {\t val x = 2}\n\n")
  }

  test("showcode without comments - hard") {
    assertTokenizedAsSyntax(
      """
        |class C {
        |  val x1a = 2
        |  val x1b = 0x002
        |  val x1c = 0x002a
        |  val x2a = 2l
        |  val x2b = 2L
        |  val x2c = 0x002l
        |  val x2d = 0x002L
        |  val x2e = 0x002al
        |  val x2f = 0x002aL
        |  val x3a = 2f
        |  val x3b = 2.0F
        |  val x4a = 2d
        |  val x4b = 2.0D
        |  val x4c = 2.0
        |  val x5a = 'a'
        |  val x5b = '\b'
        |  val x5c = '"'
        |  val x5d = '\"'
        |  val x6 = 'a
        |  val x7a = ""
        |  val x7b = "\b"
        |  val x7c = "c"
        |  val x7d = "\""
        |  val x7e = QQQQQQ
        |  val x7f = QQQf\nQQQ
        |  val hello = 42
        |  val `world` = 42
        |}
        |""".stripMargin.tq(),
      """
        |class C {
        |  val x1a = 2
        |  val x1b = 0x002
        |  val x1c = 0x002a
        |  val x2a = 2l
        |  val x2b = 2L
        |  val x2c = 0x002l
        |  val x2d = 0x002L
        |  val x2e = 0x002al
        |  val x2f = 0x002aL
        |  val x3a = 2f
        |  val x3b = 2.0F
        |  val x4a = 2d
        |  val x4b = 2.0D
        |  val x4c = 2.0
        |  val x5a = 'a'
        |  val x5b = '\b'
        |  val x5c = '"'
        |  val x5d = '\"'
        |  val x6 = 'a
        |  val x7a = ""
        |  val x7b = "\b"
        |  val x7c = "c"
        |  val x7d = "\""
        |  val x7e = QQQQQQ
        |  val x7f = QQQf\nQQQ
        |  val hello = 42
        |  val `world` = 42
        |}
        |""".stripMargin.tq()
    )
  }

  test("showCode without comments - insane") {
    assertTokenizedAsSyntax(
      """
        |class C {
        |  q""
        |  q"$b + 2"
        |  q"${b} + 2"
        |  q"class $X"
        |  q"class ${X}"
        |  qQQQQQQ
        |  qQQQ$d + 2QQQ
        |  qQQQ${d} + 2QQQ
        |  qQQQclass $YQQQ
        |  qQQQclass ${Y}QQQ
        |}
        |""".stripMargin.tq(),
      """
        |class C {
        |  q""
        |  q"$b + 2"
        |  q"${b} + 2"
        |  q"class $X"
        |  q"class ${X}"
        |  qQQQQQQ
        |  qQQQ$d + 2QQQ
        |  qQQQ${d} + 2QQQ
        |  qQQQclass $YQQQ
        |  qQQQclass ${Y}QQQ
        |}
        |""".stripMargin.tq()
    )
  }

  test("showCode with comments - easy") {
    assertTokenizedAsSyntax(
      "class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n",
      "class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n"
    )
  }

  test("showCode with comments - tricky") {
    val code = "x ~/**/y"
    val syntax = "x ~/**/y"
    assertTokenizedAsSyntax(code, syntax)
    val struct = """|BOF [0..0)
                    |Ident(x) [0..1)
                    |Space [1..2)
                    |Ident(~) [2..3)
                    |Comment() [3..7)
                    |Ident(y) [7..8)
                    |EOF [8..8)
                    |""".stripMargin
    assertTokenizedAsStructureLines(code, struct)
  }

  test("showRaw without comments - easy") {
    assertTokenizedAsStructureLines(
      "class C  {\t val x = 2}\n\n",
      """
        |BOF [0..0)
        |KwClass [0..5)
        |Space [5..6)
        |Ident(C) [6..7)
        |MultiHS(2) [7..9)
        |LeftBrace [9..10)
        |MultiHS(2) [10..12)
        |KwVal [12..15)
        |Space [15..16)
        |Ident(x) [16..17)
        |Space [17..18)
        |Equals [18..19)
        |Space [19..20)
        |Constant.Int(2) [20..21)
        |RightBrace [21..22)
        |MultiNL(2) [22..24)
        |EOF [24..24)
        |""".stripMargin
    )
  }

  test("tokenized-wrong-number-braces1") {
    assertTokenizedAsStructureLines(
      "class C  {\t val x = 2}}\n",
      """|BOF [0..0)
         |KwClass [0..5)
         |Space [5..6)
         |Ident(C) [6..7)
         |MultiHS(2) [7..9)
         |LeftBrace [9..10)
         |MultiHS(2) [10..12)
         |KwVal [12..15)
         |Space [15..16)
         |Ident(x) [16..17)
         |Space [17..18)
         |Equals [18..19)
         |Space [19..20)
         |Constant.Int(2) [20..21)
         |RightBrace [21..22)
         |RightBrace [22..23)
         |LF [23..24)
         |EOF [24..24)
         |""".stripMargin
    )
  }

  test("tokenized-wrong-number-braces2") {
    assertTokenizedAsStructureLines(
      "class C  {{\t val x = 2}\n{}",
      """|BOF [0..0)
         |KwClass [0..5)
         |Space [5..6)
         |Ident(C) [6..7)
         |MultiHS(2) [7..9)
         |LeftBrace [9..10)
         |LeftBrace [10..11)
         |MultiHS(2) [11..13)
         |KwVal [13..16)
         |Space [16..17)
         |Ident(x) [17..18)
         |Space [18..19)
         |Equals [19..20)
         |Space [20..21)
         |Constant.Int(2) [21..22)
         |RightBrace [22..23)
         |LF [23..24)
         |LeftBrace [24..25)
         |RightBrace [25..26)
         |EOF [26..26)
         |""".stripMargin
    )
  }

  test("showRaw without comments - hard") {
    assertTokenizedAsStructureLines(
      """|class C {
         |  val x1a = 2
         |  val x1b = 0x002
         |  val x1c = 0x002a
         |  val x2a = 2l
         |  val x2b = 2L
         |  val x2c = 0x002l
         |  val x2d = 0x002L
         |  val x2e = 0x002al
         |  val x2f = 0x002aL
         |  val x3a = 2f
         |  val x3b = 2.0F
         |  val x4a = 2d
         |  val x4b = 2.0D
         |  val x4c = 2.0
         |  val x5a = 'a'
         |  val x5b = '\b'
         |  val x5c = '"'
         |  val x5d = '\"'
         |  val x6 = 'a
         |  val x7a = ""
         |  val x7b = "\b"
         |  val x7c = "c"
         |  val x7d = "\""
         |  val x7e = QQQQQQ
         |  val x7f = QQQf\nQQQ
         |  val hello = 42
         |  val `world` = 42
         |}""".stripMargin.tq(),
      """
        |BOF [0..0)
        |KwClass [0..5)
        |Space [5..6)
        |Ident(C) [6..7)
        |Space [7..8)
        |LeftBrace [8..9)
        |LF [9..10)
        |MultiHS(2) [10..12)
        |KwVal [12..15)
        |Space [15..16)
        |Ident(x1a) [16..19)
        |Space [19..20)
        |Equals [20..21)
        |Space [21..22)
        |Constant.Int(2) [22..23)
        |LF [23..24)
        |MultiHS(2) [24..26)
        |KwVal [26..29)
        |Space [29..30)
        |Ident(x1b) [30..33)
        |Space [33..34)
        |Equals [34..35)
        |Space [35..36)
        |Constant.Int(2) [36..41)
        |LF [41..42)
        |MultiHS(2) [42..44)
        |KwVal [44..47)
        |Space [47..48)
        |Ident(x1c) [48..51)
        |Space [51..52)
        |Equals [52..53)
        |Space [53..54)
        |Constant.Int(42) [54..60)
        |LF [60..61)
        |MultiHS(2) [61..63)
        |KwVal [63..66)
        |Space [66..67)
        |Ident(x2a) [67..70)
        |Space [70..71)
        |Equals [71..72)
        |Space [72..73)
        |Constant.Long(2) [73..75)
        |LF [75..76)
        |MultiHS(2) [76..78)
        |KwVal [78..81)
        |Space [81..82)
        |Ident(x2b) [82..85)
        |Space [85..86)
        |Equals [86..87)
        |Space [87..88)
        |Constant.Long(2) [88..90)
        |LF [90..91)
        |MultiHS(2) [91..93)
        |KwVal [93..96)
        |Space [96..97)
        |Ident(x2c) [97..100)
        |Space [100..101)
        |Equals [101..102)
        |Space [102..103)
        |Constant.Long(2) [103..109)
        |LF [109..110)
        |MultiHS(2) [110..112)
        |KwVal [112..115)
        |Space [115..116)
        |Ident(x2d) [116..119)
        |Space [119..120)
        |Equals [120..121)
        |Space [121..122)
        |Constant.Long(2) [122..128)
        |LF [128..129)
        |MultiHS(2) [129..131)
        |KwVal [131..134)
        |Space [134..135)
        |Ident(x2e) [135..138)
        |Space [138..139)
        |Equals [139..140)
        |Space [140..141)
        |Constant.Long(42) [141..148)
        |LF [148..149)
        |MultiHS(2) [149..151)
        |KwVal [151..154)
        |Space [154..155)
        |Ident(x2f) [155..158)
        |Space [158..159)
        |Equals [159..160)
        |Space [160..161)
        |Constant.Long(42) [161..168)
        |LF [168..169)
        |MultiHS(2) [169..171)
        |KwVal [171..174)
        |Space [174..175)
        |Ident(x3a) [175..178)
        |Space [178..179)
        |Equals [179..180)
        |Space [180..181)
        |Constant.Float(2) [181..183)
        |LF [183..184)
        |MultiHS(2) [184..186)
        |KwVal [186..189)
        |Space [189..190)
        |Ident(x3b) [190..193)
        |Space [193..194)
        |Equals [194..195)
        |Space [195..196)
        |Constant.Float(2.0) [196..200)
        |LF [200..201)
        |MultiHS(2) [201..203)
        |KwVal [203..206)
        |Space [206..207)
        |Ident(x4a) [207..210)
        |Space [210..211)
        |Equals [211..212)
        |Space [212..213)
        |Constant.Double(2) [213..215)
        |LF [215..216)
        |MultiHS(2) [216..218)
        |KwVal [218..221)
        |Space [221..222)
        |Ident(x4b) [222..225)
        |Space [225..226)
        |Equals [226..227)
        |Space [227..228)
        |Constant.Double(2.0) [228..232)
        |LF [232..233)
        |MultiHS(2) [233..235)
        |KwVal [235..238)
        |Space [238..239)
        |Ident(x4c) [239..242)
        |Space [242..243)
        |Equals [243..244)
        |Space [244..245)
        |Constant.Double(2.0) [245..248)
        |LF [248..249)
        |MultiHS(2) [249..251)
        |KwVal [251..254)
        |Space [254..255)
        |Ident(x5a) [255..258)
        |Space [258..259)
        |Equals [259..260)
        |Space [260..261)
        |Constant.Char(a) [261..264)
        |LF [264..265)
        |MultiHS(2) [265..267)
        |KwVal [267..270)
        |Space [270..271)
        |Ident(x5b) [271..274)
        |Space [274..275)
        |Equals [275..276)
        |Space [276..277)
        |Constant.Char(\b) [277..281)
        |LF [281..282)
        |MultiHS(2) [282..284)
        |KwVal [284..287)
        |Space [287..288)
        |Ident(x5c) [288..291)
        |Space [291..292)
        |Equals [292..293)
        |Space [293..294)
        |Constant.Char(") [294..297)
        |LF [297..298)
        |MultiHS(2) [298..300)
        |KwVal [300..303)
        |Space [303..304)
        |Ident(x5d) [304..307)
        |Space [307..308)
        |Equals [308..309)
        |Space [309..310)
        |Constant.Char(") [310..314)
        |LF [314..315)
        |MultiHS(2) [315..317)
        |KwVal [317..320)
        |Space [320..321)
        |Ident(x6) [321..323)
        |Space [323..324)
        |Equals [324..325)
        |Space [325..326)
        |Constant.Symbol(a) [326..328)
        |LF [328..329)
        |MultiHS(2) [329..331)
        |KwVal [331..334)
        |Space [334..335)
        |Ident(x7a) [335..338)
        |Space [338..339)
        |Equals [339..340)
        |Space [340..341)
        |Constant.String() [341..343)
        |LF [343..344)
        |MultiHS(2) [344..346)
        |KwVal [346..349)
        |Space [349..350)
        |Ident(x7b) [350..353)
        |Space [353..354)
        |Equals [354..355)
        |Space [355..356)
        |Constant.String(\b) [356..360)
        |LF [360..361)
        |MultiHS(2) [361..363)
        |KwVal [363..366)
        |Space [366..367)
        |Ident(x7c) [367..370)
        |Space [370..371)
        |Equals [371..372)
        |Space [372..373)
        |Constant.String(c) [373..376)
        |LF [376..377)
        |MultiHS(2) [377..379)
        |KwVal [379..382)
        |Space [382..383)
        |Ident(x7d) [383..386)
        |Space [386..387)
        |Equals [387..388)
        |Space [388..389)
        |Constant.String(") [389..393)
        |LF [393..394)
        |MultiHS(2) [394..396)
        |KwVal [396..399)
        |Space [399..400)
        |Ident(x7e) [400..403)
        |Space [403..404)
        |Equals [404..405)
        |Space [405..406)
        |Constant.String() [406..412)
        |LF [412..413)
        |MultiHS(2) [413..415)
        |KwVal [415..418)
        |Space [418..419)
        |Ident(x7f) [419..422)
        |Space [422..423)
        |Equals [423..424)
        |Space [424..425)
        |Constant.String(f\\n) [425..434)
        |LF [434..435)
        |MultiHS(2) [435..437)
        |KwVal [437..440)
        |Space [440..441)
        |Ident(hello) [441..446)
        |Space [446..447)
        |Equals [447..448)
        |Space [448..449)
        |Constant.Int(42) [449..451)
        |LF [451..452)
        |MultiHS(2) [452..454)
        |KwVal [454..457)
        |Space [457..458)
        |Ident(world) [458..465)
        |Space [465..466)
        |Equals [466..467)
        |Space [467..468)
        |Constant.Int(42) [468..470)
        |LF [470..471)
        |RightBrace [471..472)
        |EOF [472..472)
        |""".stripMargin.tq()
    )
  }

  test("showRaw without comments - insane") {
    assertTokenizedAsStructureLines(
      """|class C {
         |  q""
         |  q"$b + 2"
         |  q"${b} + 2"
         |  q"class $X"
         |  q"class ${X}"
         |  qQQQQQQ
         |  qQQQ$d + 2QQQ
         |  qQQQ${d} + 2QQQ
         |  qQQQclass $YQQQ
         |  qQQQclass ${Y}QQQ
         |}""".stripMargin.tq(),
      """
        |BOF [0..0)
        |KwClass [0..5)
        |Space [5..6)
        |Ident(C) [6..7)
        |Space [7..8)
        |LeftBrace [8..9)
        |LF [9..10)
        |MultiHS(2) [10..12)
        |Interpolation.Id(q) [12..13)
        |Interpolation.Start(") [13..14)
        |Interpolation.Part() [14..14)
        |Interpolation.End(") [14..15)
        |LF [15..16)
        |MultiHS(2) [16..18)
        |Interpolation.Id(q) [18..19)
        |Interpolation.Start(") [19..20)
        |Interpolation.Part() [20..20)
        |Interpolation.SpliceStart [20..21)
        |Ident(b) [21..22)
        |Interpolation.SpliceEnd [22..22)
        |Interpolation.Part( + 2) [22..26)
        |Interpolation.End(") [26..27)
        |LF [27..28)
        |MultiHS(2) [28..30)
        |Interpolation.Id(q) [30..31)
        |Interpolation.Start(") [31..32)
        |Interpolation.Part() [32..32)
        |Interpolation.SpliceStart [32..33)
        |LeftBrace [33..34)
        |Ident(b) [34..35)
        |RightBrace [35..36)
        |Interpolation.SpliceEnd [36..36)
        |Interpolation.Part( + 2) [36..40)
        |Interpolation.End(") [40..41)
        |LF [41..42)
        |MultiHS(2) [42..44)
        |Interpolation.Id(q) [44..45)
        |Interpolation.Start(") [45..46)
        |Interpolation.Part(class ) [46..52)
        |Interpolation.SpliceStart [52..53)
        |Ident(X) [53..54)
        |Interpolation.SpliceEnd [54..54)
        |Interpolation.Part() [54..54)
        |Interpolation.End(") [54..55)
        |LF [55..56)
        |MultiHS(2) [56..58)
        |Interpolation.Id(q) [58..59)
        |Interpolation.Start(") [59..60)
        |Interpolation.Part(class ) [60..66)
        |Interpolation.SpliceStart [66..67)
        |LeftBrace [67..68)
        |Ident(X) [68..69)
        |RightBrace [69..70)
        |Interpolation.SpliceEnd [70..70)
        |Interpolation.Part() [70..70)
        |Interpolation.End(") [70..71)
        |LF [71..72)
        |MultiHS(2) [72..74)
        |Interpolation.Id(q) [74..75)
        |Interpolation.Start(QQQ) [75..78)
        |Interpolation.Part() [78..78)
        |Interpolation.End(QQQ) [78..81)
        |LF [81..82)
        |MultiHS(2) [82..84)
        |Interpolation.Id(q) [84..85)
        |Interpolation.Start(QQQ) [85..88)
        |Interpolation.Part() [88..88)
        |Interpolation.SpliceStart [88..89)
        |Ident(d) [89..90)
        |Interpolation.SpliceEnd [90..90)
        |Interpolation.Part( + 2) [90..94)
        |Interpolation.End(QQQ) [94..97)
        |LF [97..98)
        |MultiHS(2) [98..100)
        |Interpolation.Id(q) [100..101)
        |Interpolation.Start(QQQ) [101..104)
        |Interpolation.Part() [104..104)
        |Interpolation.SpliceStart [104..105)
        |LeftBrace [105..106)
        |Ident(d) [106..107)
        |RightBrace [107..108)
        |Interpolation.SpliceEnd [108..108)
        |Interpolation.Part( + 2) [108..112)
        |Interpolation.End(QQQ) [112..115)
        |LF [115..116)
        |MultiHS(2) [116..118)
        |Interpolation.Id(q) [118..119)
        |Interpolation.Start(QQQ) [119..122)
        |Interpolation.Part(class ) [122..128)
        |Interpolation.SpliceStart [128..129)
        |Ident(Y) [129..130)
        |Interpolation.SpliceEnd [130..130)
        |Interpolation.Part() [130..130)
        |Interpolation.End(QQQ) [130..133)
        |LF [133..134)
        |MultiHS(2) [134..136)
        |Interpolation.Id(q) [136..137)
        |Interpolation.Start(QQQ) [137..140)
        |Interpolation.Part(class ) [140..146)
        |Interpolation.SpliceStart [146..147)
        |LeftBrace [147..148)
        |Ident(Y) [148..149)
        |RightBrace [149..150)
        |Interpolation.SpliceEnd [150..150)
        |Interpolation.Part() [150..150)
        |Interpolation.End(QQQ) [150..153)
        |LF [153..154)
        |RightBrace [154..155)
        |EOF [155..155)
        |""".stripMargin.tq()
    )
  }

  test("showRaw with comments - easy") {
    assertTokenizedAsStructureLines(
      "class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n",
      """
        |BOF [0..0)
        |KwClass [0..5)
        |Space [5..6)
        |Ident(C) [6..7)
        |MultiHS(2) [7..9)
        |Comment(hello world) [9..24)
        |LeftBrace [24..25)
        |MultiHS(2) [25..27)
        |KwVal [27..30)
        |Space [30..31)
        |Ident(x) [31..32)
        |Space [32..33)
        |Equals [33..34)
        |Space [34..35)
        |Constant.Int(2) [35..36)
        |RightBrace [36..37)
        |LF [37..38)
        |Comment(bye-bye world) [38..53)
        |LF [53..54)
        |EOF [54..54)
        |""".stripMargin
    )
  }

  test("showRaw with comments - tricky") {
    assertTokenizedAsStructureLines(
      "x ~/**/y",
      """
        |BOF [0..0)
        |Ident(x) [0..1)
        |Space [1..2)
        |Ident(~) [2..3)
        |Comment() [3..7)
        |Ident(y) [7..8)
        |EOF [8..8)
        |""".stripMargin
    )
  }

  test("showRaw with comments - skip unicode escape 1") {
    assertTokenizedAsStructureLines(
      "// Note: '\\u000A' = '\\n'",
      """|BOF [0..0)
         |Comment( Note: '\\u000A' = '\\n') [0..24)
         |EOF [24..24)
         |""".stripMargin
    )
  }

  test("showRaw with comments - skip unicode escape 2") {
    assertTokenizedAsStructureLines(
      "/* Note: '\\u000A' = '\\n' */",
      """|BOF [0..0)
         |Comment( Note: '\\u000A' = '\\n' ) [0..27)
         |EOF [27..27)
         |""".stripMargin
    )
  }

  test("interpolation start & end - episode 01") {
    assertTokenizedAsStructureLines(
      "q\"\"",
      """
        |BOF [0..0)
        |Interpolation.Id(q) [0..1)
        |Interpolation.Start(") [1..2)
        |Interpolation.Part() [2..2)
        |Interpolation.End(") [2..3)
        |EOF [3..3)
        |""".stripMargin
    )
  }

  test("interpolation start & end - episode 02") {
    assertTokenizedAsStructureLines(
      "q\"\";",
      """
        |BOF [0..0)
        |Interpolation.Id(q) [0..1)
        |Interpolation.Start(") [1..2)
        |Interpolation.Part() [2..2)
        |Interpolation.End(") [2..3)
        |Semicolon [3..4)
        |EOF [4..4)
        |""".stripMargin
    )
  }

  test("interpolation start & end - episode 03") {
    assertTokenizedAsStructureLines(
      "q\"a\"",
      """
        |BOF [0..0)
        |Interpolation.Id(q) [0..1)
        |Interpolation.Start(") [1..2)
        |Interpolation.Part(a) [2..3)
        |Interpolation.End(") [3..4)
        |EOF [4..4)
        |""".stripMargin
    )
  }

  test("interpolation start & end - episode 04") {
    assertTokenizedAsStructureLines(
      "q\"a\";",
      """
        |BOF [0..0)
        |Interpolation.Id(q) [0..1)
        |Interpolation.Start(") [1..2)
        |Interpolation.Part(a) [2..3)
        |Interpolation.End(") [3..4)
        |Semicolon [4..5)
        |EOF [5..5)
        |""".stripMargin
    )
  }

  test("interpolation start & end - episode 05") {
    assertTokenizedAsStructureLines(
      "q\"\"\"\"\"\"",
      """
        |BOF [0..0)
        |Interpolation.Id(q) [0..1)
        |Interpolation.Start(QQQ) [1..4)
        |Interpolation.Part() [4..4)
        |Interpolation.End(QQQ) [4..7)
        |EOF [7..7)
        |""".stripMargin.tq()
    )
  }

  test("interpolation start & end - episode 06") {
    assertTokenizedAsStructureLines(
      "q\"\"\"\"\"\";",
      """
        |BOF [0..0)
        |Interpolation.Id(q) [0..1)
        |Interpolation.Start(QQQ) [1..4)
        |Interpolation.Part() [4..4)
        |Interpolation.End(QQQ) [4..7)
        |Semicolon [7..8)
        |EOF [8..8)
        |""".stripMargin.tq()
    )
  }

  test("interpolation start & end - episode 07") {
    assertTokenizedAsStructureLines(
      "q\"\"\"a\"\"\"",
      """
        |BOF [0..0)
        |Interpolation.Id(q) [0..1)
        |Interpolation.Start(QQQ) [1..4)
        |Interpolation.Part(a) [4..5)
        |Interpolation.End(QQQ) [5..8)
        |EOF [8..8)
        |""".stripMargin.tq()
    )
  }

  test("interpolation start & end - episode 08") {
    assertTokenizedAsStructureLines(
      "q\"\"\"a\"\"\";",
      """
        |BOF [0..0)
        |Interpolation.Id(q) [0..1)
        |Interpolation.Start(QQQ) [1..4)
        |Interpolation.Part(a) [4..5)
        |Interpolation.End(QQQ) [5..8)
        |Semicolon [8..9)
        |EOF [9..9)
        |""".stripMargin.tq()
    )
  }

  test("interpolation start & end - episode 09") {
    assertTokenizedAsStructureLines(
      "q\"a\"\r\n",
      """
        |BOF [0..0)
        |Interpolation.Id(q) [0..1)
        |Interpolation.Start(") [1..2)
        |Interpolation.Part(a) [2..3)
        |Interpolation.End(") [3..4)
        |CRLF [4..6)
        |EOF [6..6)
        |""".stripMargin
    )
  }

  test("interpolation-underscore") {
    assertTokenizedAsStructureLines(
      """s"checking redundancy in $_match"""",
      """|BOF [0..0)
         |Interpolation.Id(s) [0..1)
         |Interpolation.Start(") [1..2)
         |Interpolation.Part(checking redundancy in ) [2..25)
         |Interpolation.SpliceStart [25..26)
         |Ident(_match) [26..32)
         |Interpolation.SpliceEnd [32..32)
         |Interpolation.Part() [32..32)
         |Interpolation.End(") [32..33)
         |EOF [33..33)
         |""".stripMargin
    )
  }

  test("$this") {
    assertTokenizedAsStructureLines(
      "q\"$this\"",
      """
        |BOF [0..0)
        |Interpolation.Id(q) [0..1)
        |Interpolation.Start(") [1..2)
        |Interpolation.Part() [2..2)
        |Interpolation.SpliceStart [2..3)
        |KwThis [3..7)
        |Interpolation.SpliceEnd [7..7)
        |Interpolation.Part() [7..7)
        |Interpolation.End(") [7..8)
        |EOF [8..8)
        |""".stripMargin
    )
  }

  test("monocle 1") {
    assertTokenizedAsStructureLines(
      "x => x",
      """
        |BOF [0..0)
        |Ident(x) [0..1)
        |Space [1..2)
        |RightArrow [2..4)
        |Space [4..5)
        |Ident(x) [5..6)
        |EOF [6..6)
        |""".stripMargin
    )
  }

  test("monocle 2") {
    assertTokenizedAsStructureLines(
      "x ⇒ x",
      """
        |BOF [0..0)
        |Ident(x) [0..1)
        |Space [1..2)
        |RightArrow [2..3)
        |Space [3..4)
        |Ident(x) [4..5)
        |EOF [5..5)
        |""".stripMargin
    )
  }

  test("monocle 3") {
    assertTokenizedAsStructureLines(
      "for (x <- xs) println(x)",
      """
        |BOF [0..0)
        |KwFor [0..3)
        |Space [3..4)
        |LeftParen [4..5)
        |Ident(x) [5..6)
        |Space [6..7)
        |LeftArrow [7..9)
        |Space [9..10)
        |Ident(xs) [10..12)
        |RightParen [12..13)
        |Space [13..14)
        |Ident(println) [14..21)
        |LeftParen [21..22)
        |Ident(x) [22..23)
        |RightParen [23..24)
        |EOF [24..24)
        |""".stripMargin
    )
  }

  test("monocle 4") {
    assertTokenizedAsStructureLines(
      "for (x ← xs) println(x)",
      """
        |BOF [0..0)
        |KwFor [0..3)
        |Space [3..4)
        |LeftParen [4..5)
        |Ident(x) [5..6)
        |Space [6..7)
        |LeftArrow [7..8)
        |Space [8..9)
        |Ident(xs) [9..11)
        |RightParen [11..12)
        |Space [12..13)
        |Ident(println) [13..20)
        |LeftParen [20..21)
        |Ident(x) [21..22)
        |RightParen [22..23)
        |EOF [23..23)
        |""".stripMargin
    )
  }

  test("-2147483648") {
    assertTokenizedAsStructureLines(
      "-2147483648",
      """
        |BOF [0..0)
        |Ident(-) [0..1)
        |Constant.Int(2147483648) [1..11)
        |EOF [11..11)
        |""".stripMargin.tq()
    )
  }

  test("simple xml literal - 1") {
    assertTokenizedAsStructureLines(
      "<foo>bar</foo>",
      """
        |BOF [0..0)
        |Xml.Start [0..0)
        |Xml.Part(<foo>bar</foo>) [0..14)
        |Xml.End [14..14)
        |EOF [14..14)
        |""".stripMargin.tq()
    )
  }

  test("simple xml literal - 2") {
    assertTokenizedAsStructureLines(
      "<foo>bar</foo> ",
      """
        |BOF [0..0)
        |Xml.Start [0..0)
        |Xml.Part(<foo>bar</foo>) [0..14)
        |Xml.End [14..14)
        |Space [14..15)
        |EOF [15..15)
        |""".stripMargin.tq()
    )
  }

  test("incomplete xml literal - 1") {
    assertTokenizedAsStructureLines(
      """|val a = 
         |  <foo>bar</fo>""".stripMargin,
      """|BOF [0..0)
         |KwVal [0..3)
         |Space [3..4)
         |Ident(a) [4..5)
         |Space [5..6)
         |Equals [6..7)
         |LF [8..9)
         |MultiHS(2) [9..11)
         |Xml.Start [11..11)
         |Xml.Part(<foo>bar</fo>) [11..24)
         |Xml.End [24..24)
         |EOF [24..24)
         |""".stripMargin.tq()
    )
  }

  test("incomplete xml literal - 2") {
    val code = """|object A {
                  |  val a = <foo>bar
                  |  val b = <baz qux="...">cde<?
                  |""".stripMargin
    interceptMessage[ParseException](
      """|<input>:3: error: malformed xml literal, expected:
         |Expected ("{{" | "}}" | "&" | "&#" | "&#x" | "{" | "<xml:unparsed" | "<![CDATA[" | "<!--" | "</"):3:29, found "<?\n"
         |  val b = <baz qux="...">cde<?
         |                            ^""".stripMargin.lf2nl
    )(code.asInput.parseRule(_.entrypointStat()))
  }

  test("unsupported xml literal - 1 BOF") {
    implicit val dialect = dialects.Scala213.withAllowXmlLiterals(false)
    val code = "<foo>bar</foo>"
    interceptMessage[ParseException](
      """|<input>:1: error: xml literals are not supported
         |<foo>bar</foo>
         |^""".stripMargin.lf2nl
    )(code.asInput.parseRule(_.entrypointStat()))
  }

  test("unsupported xml literal - 2 after space") {
    implicit val dialect = dialects.Scala213.withAllowXmlLiterals(false)
    val code = "val a = <foo>bar</foo>"
    interceptMessage[ParseException](
      """|<input>:1: error: xml literals are not supported
         |val a = <foo>bar</foo>
         |        ^""".stripMargin.lf2nl
    )(code.asInput.parseRule(_.entrypointStat()))
  }

  test("unsupported xml literal - 3 plus/no space") {
    assertTokenizedAsStructureLines(
      """|a+<foo>bar</foo>
         |""".stripMargin,
      """|BOF [0..0)
         |Ident(a) [0..1)
         |Ident(+<) [1..3)
         |Ident(foo) [3..6)
         |Ident(>) [6..7)
         |Ident(bar) [7..10)
         |Ident(</) [10..12)
         |Ident(foo) [12..15)
         |Ident(>) [15..16)
         |LF [16..17)
         |EOF [17..17)
         |""".stripMargin.tq(),
      dialects.Scala213.withAllowXmlLiterals(false)
    )
  }

  test("parsed trees don't have BOF/EOF in their tokens") {
    val tree = "foo + bar".asInput.parse[Term].get
    assert(tree.pos != Position.None)
    assertEquals(
      tree.tokens.structure,
      "Tokens(BOF [0..0), Ident(foo) [0..3), Space [3..4), Ident(+) [4..5), Space [5..6), Ident(bar) [6..9), EOF [9..9))"
    )
  }

  test("synthetic trees don't have BOF/EOF in their tokens") {
    val tree = Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar")))
    assertEquals(tree.pos, Position.None)
    val tokens = tree.retokenize
    val tokensStructure = tokens.structure
    assertEquals(tree.tokens.structure, tokensStructure)
    assertEquals(
      tokensStructure,
      "Tokens(BOF [0..0), Ident(foo) [0..3), Space [3..4), Ident(+) [4..5), Space [5..6), Ident(bar) [6..9), EOF [9..9))"
    )
  }

  test("Ident.value for normal") {
    "foo".asInput.parse[Term].get.tokens match {
      case Tokens(bof, foo: Ident, eof) => assertEquals(foo.value, "foo")
    }
  }

  test("Ident.value for backquoted") {
    "`foo`".asInput.parse[Term].get.tokens match {
      case Tokens(bof, foo: Ident, eof) =>
        assertEquals(foo.value, "foo")
        assertEquals(foo.syntax, "`foo`")
    }
  }

  test("Interpolation.Id.value") {
    assertTokens(""" q"" """) { case Tokens(bof, _, id: Interpolation.Id, _, _, _, _, eof) =>
      assertEquals(id.value, "q")
    }
  }

  test("Interpolation.Part.value") {
    assertTokens(""" q"foo" """) { case Tokens(bof, _, _, _, part: Interpolation.Part, _, _, eof) =>
      assertEquals(part.value, "foo")
    }
  }

  test("Interpolated tree parsed succesfully with windows newline") {
    assertTokens(""" q"foo"""" + "\r\n") {
      case Tokens(bof, _, _, _, part: Interpolation.Part, _, crlf: CRLF, eof) =>
        assertEquals(part.value, "foo")
        assertEquals(crlf.text, "\r\n")
        assertEquals(part.len, 3)
        assertEquals(crlf.len, 2)
    }
  }

  test("Interpolated tree parsed succesfully with windows newline, with LF escaped") {
    assertTokenizedAsStructureLines(
      """ q"foo"""" + "\r\\u000A",
      """
        |BOF [0..0)
        |Space [0..1)
        |Interpolation.Id(q) [1..2)
        |Interpolation.Start(") [2..3)
        |Interpolation.Part(foo) [3..6)
        |Interpolation.End(") [6..7)
        |LF [7..8)
        |EOF [14..14)
        |""".stripMargin
    )
  }

  test("Interpolated tree parsed succesfully with unix newline") {
    assertTokens(""" q"foo"""" + "\n") {
      case Tokens(bof, _, _, _, part: Interpolation.Part, _, lf: LF, eof) =>
        assertEquals(part.value, "foo")
        assertEquals(lf.syntax, "\n")
    }
  }

  test("Interpolated with quote escape 1") {
    val stringInterpolation = """s"$"$name$" in quotes""""

    assertTokens(stringInterpolation, dialects.Scala3) {
      case Tokens(
            BOF(),
            _: Interpolation.Id,
            Interpolation.Start(),
            Interpolation.Part("\""),
            Interpolation.SpliceStart(),
            Ident("name"),
            Interpolation.SpliceEnd(),
            Interpolation.Part("\" in quotes"),
            Interpolation.End(),
            EOF()
          ) =>
    }

    assertTokenizedAsStructureLines(
      stringInterpolation,
      """|BOF [0..0)
         |Interpolation.Id(s) [0..1)
         |Interpolation.Start(") [1..2)
         |Interpolation.Part() [2..2)
         |Interpolation.SpliceStart [2..3)
         |Invalid(Not one of: `$'_, `$$', `$'ident, `$'this, `$'BlockExpr) [3..3)
         |Constant.String() [3..3)
         |Interpolation.SpliceEnd [4..4)
         |Interpolation.Part($name$) [4..10)
         |Interpolation.End(") [10..11)
         |Constant.String( in quotes) [11..21)
         |EOF [22..22)
         |""".stripMargin,
      dialects.Scala212
    )
  }

  test("Interpolated with quote escape 2") {
    val stringInterpolationWithUnicode = s"""check_success(s"${'\\' + "u0024"}")"""

    assertTokens(stringInterpolationWithUnicode, dialects.Scala3) {
      case Tokens(
            BOF(),
            Ident("check_success"),
            LeftParen(),
            _: Interpolation.Id,
            Interpolation.Start(),
            Interpolation.Part("$"),
            Interpolation.End(),
            RightParen(),
            EOF()
          ) =>
    }

  }

  test("Comment.value") {
    assertTokens("//foo") { case Tokens(bof, comment: Comment, eof) =>
      assertEquals(comment.value, "foo")
    }
  }

  test("enum") {
    assertTokens("enum", dialects.Scala3) { case Tokens(bof @ BOF(), te: KwEnum, eof @ EOF()) =>
      assertEquals(bof.len, 0)
      assertEquals(bof.isEmpty, true)
      assertEquals(te.len, 4)
      assertEquals(te.isEmpty, false)
      assertEquals(eof.len, 0)
      assertEquals(eof.isEmpty, true)
    }
    assertTokens("enum", dialects.Scala212) { case Tokens(BOF(), te @ Ident("enum"), EOF()) =>
      assertEquals(te.len, 4)
      assertEquals(te.isEmpty, false)
    }
    assertTokens("s\"$enum\"", dialects.Scala212) {
      case Tokens(
            BOF(),
            Interpolation.Id("s"),
            Interpolation.Start(),
            Interpolation.Part(""),
            Interpolation.SpliceStart(),
            Ident("enum"),
            Interpolation.SpliceEnd(),
            Interpolation.Part(""),
            Interpolation.End(),
            EOF()
          ) =>
    }
    assertTokens("s\"$enum\"", dialects.Scala213) {
      case Tokens(
            BOF(),
            Interpolation.Id("s"),
            Interpolation.Start(),
            Interpolation.Part(""),
            Interpolation.SpliceStart(),
            Ident("enum"),
            Interpolation.SpliceEnd(),
            Interpolation.Part(""),
            Interpolation.End(),
            EOF()
          ) =>
    }
    assertTokenizedAsStructureLines(
      "s\"$enum\"",
      """|BOF [0..0)
         |Interpolation.Id(s) [0..1)
         |Interpolation.Start(") [1..2)
         |Interpolation.Part() [2..2)
         |Interpolation.SpliceStart [2..3)
         |KwEnum [3..7)
         |Invalid(invalid unquote: `$'ident, `$'BlockExpr, `$'this or `$'_ expected) [3..3)
         |Interpolation.SpliceEnd [7..7)
         |Interpolation.Part() [7..7)
         |Interpolation.End(") [7..8)
         |EOF [8..8)
         |""".stripMargin,
      dialects.Scala3
    )
  }

  test("macro") {
    assertTokens("' { a }", dialects.Scala3) {
      case Tokens(
            BOF(),
            MacroQuote(),
            Space(),
            LeftBrace(),
            Space(),
            Ident("a"),
            Space(),
            RightBrace(),
            EOF()
          ) =>
    }

    assertTokens("$ { a }", dialects.Scala3) {
      case Tokens(
            BOF(),
            MacroSplice(),
            Space(),
            LeftBrace(),
            Space(),
            Ident("a"),
            Space(),
            RightBrace(),
            EOF()
          ) =>
    }
  }

  Seq(
    "1_024",
    "1_024L",
    "3_14e-2",
    "3_14E-2_1",
    "3_14E-1__2",
    "0x_1234",
    "0b_0101",
    "123_456__789",
    "0x__123_456__789"
  ).foreach { value =>
    test(s"numeric literal separator ok scala213: $value") {
      implicit val dialect: Dialect = dialects.Scala213
      tokenize(value) // no exception
    }
  }

  Seq(
    (
      "123_456_",
      """|<input>:1: error: trailing number separator
         |123_456_
         |       ^""".stripMargin
    ),
    (
      "123_456_L",
      """|<input>:1: error: trailing number separator
         |123_456_L
         |       ^""".stripMargin
    ),
    (
      "3_14_E-2",
      """|<input>:1: error: trailing number separator
         |3_14_E-2
         |    ^""".stripMargin
    ),
    (
      "3_14E-_2",
      """|<input>:1: error: leading number separator
         |3_14E-_2
         |      ^""".stripMargin
    ),
    (
      "3_14E",
      """|<input>:1: error: Invalid literal floating-point number, exponent not followed by integer
         |3_14E
         |     ^""".stripMargin
    ),
    (
      "3_14E_2",
      """|<input>:1: error: leading number separator
         |3_14E_2
         |     ^""".stripMargin
    ),
    (
      "3_14E_F",
      """|<input>:1: error: Invalid literal floating-point number, exponent not followed by integer
         |3_14E_F
         |     ^""".stripMargin
    ),
    (
      "3_14ef",
      """|<input>:1: error: Invalid literal floating-point number, exponent not followed by integer
         |3_14ef
         |     ^""".stripMargin
    ),
    (
      "3_14E-2_",
      """|<input>:1: error: trailing number separator
         |3_14E-2_
         |       ^""".stripMargin
    ),
    (
      "3.1_4_",
      """|<input>:1: error: trailing number separator
         |3.1_4_
         |     ^""".stripMargin
    ),
    (
      "3.1_4_d",
      """|<input>:1: error: trailing number separator
         |3.1_4_d
         |     ^""".stripMargin
    ),
    (
      "3.1_4_dd",
      """|<input>:1: error: trailing number separator
         |3.1_4_dd
         |     ^""".stripMargin
    )
  ).foreach { case (value, error) =>
    test(s"numeric literal separator fail scala213: $value") {
      implicit val dialect: Dialect = dialects.Scala213
      interceptMessage[ParseException](error.lf2nl)(value.parseRule(_.entrypointExpr()))
    }
  }

  Seq((
    "1_024",
    """|<input>:1: error: numeric separators are not allowed
       |1_024
       | ^""".stripMargin
  )).foreach { case (value, error) =>
    test(s"numeric literal separator fail scala212: $value") {
      implicit val dialect: Dialect = dialects.Scala212
      interceptMessage[ParseException](error.lf2nl)(value.parseRule(_.entrypointExpr()))
    }
  }

  test("numeric literal separator scala213: check positions") {
    implicit val dialect: Dialect = dialects.Scala213
    val tokens = tokenize(" 1_000_000 ")
    val intConstant = tokens(2).asInstanceOf[Token.Constant.Int]
    assertEquals(intConstant.pos.text, "1_000_000") // assert token position includes underscores
    assertEquals(intConstant.value, BigInt(1000000))
  }

  test("Interpolated string - escape") {
    assertTokens("""s"\"Hello\", $person"""") {
      case Tokens(
            BOF(),
            Interpolation.Id("s"),
            Interpolation.Start(),
            Interpolation.Part("\\\"Hello\\\", "),
            Interpolation.SpliceStart(),
            Ident("person"),
            Interpolation.SpliceEnd(),
            Interpolation.Part(""),
            Interpolation.End(),
            EOF()
          ) =>
    }
  }

  test("Interpolated string - escape, unclosed") {
    val struct = """|BOF [0..0)
                    |Interpolation.Id(s) [0..1)
                    |Interpolation.Start(") [1..2)
                    |Interpolation.Part(\\\\) [2..4)
                    |Interpolation.End(") [4..5)
                    |Interpolation.Id(Hello) [5..10)
                    |Interpolation.Start(") [10..11)
                    |Interpolation.Part() [11..11)
                    |Invalid(unclosed single-line string interpolation) [11..11)
                    |Interpolation.End() [11..11)
                    |EOF [11..11)""".stripMargin
    assertTokenizedAsStructureLines("""s"\\"Hello"""", struct)
  }

  test("Multiline interpolated string - ignore escape") {
    assertTokens("raw\"\"\"\\$host\\$share\\\"\"\"") {
      case Tokens(
            BOF(),
            Interpolation.Id("raw"),
            Interpolation.Start(),
            Interpolation.Part("\\"),
            Interpolation.SpliceStart(),
            Ident("host"),
            Interpolation.SpliceEnd(),
            Interpolation.Part("\\"),
            Interpolation.SpliceStart(),
            Ident("share"),
            Interpolation.SpliceEnd(),
            Interpolation.Part("\\"),
            Interpolation.End(),
            EOF()
          ) =>
    }
  }

  test("#3328") {
    val code = "val \\uD835\\uDF11: Double"
    implicit val dialect: Dialect = dialects.Scala212
    assertEquals(tokenize(code).toString, code)
  }

  test("#3328 2") {
    assertTokenizedAsStructureLines(
      "val \uD835\uDF11: Double",
      """
        |BOF [0..0)
        |KwVal [0..3)
        |Space [3..4)
        |Ident(\uD835\uDF11) [4..6)
        |Colon [6..7)
        |Space [7..8)
        |Ident(Double) [8..14)
        |EOF [14..14)
        |""".stripMargin
    )
  }

  test("#3402") {
    val code = "val MIN_HIGH_SURROGATE = '\\uD800'"
    implicit val dialect: Dialect = dialects.Scala212
    assertEquals(tokenize(code).toString, code)
  }

  test("binary literals") {
    val code = """|val v1 = 0b00101010
                  |val v2 = 0B_0010_1010
                  |val v3 = 0b_0010_1010L
                  |""".stripMargin
    implicit val dialect: Dialect = dialects.Scala213
    assertEquals(tokenize(code).toString, code)
  }

  Seq(
    (
      "0d",
      """|BOF [0..0)
         |Constant.Double(0) [0..2)
         |EOF [2..2)
         |""".stripMargin
    ),
    (
      "0.0",
      """|BOF [0..0)
         |Constant.Double(0.0) [0..3)
         |EOF [3..3)
         |""".stripMargin
    ),
    (
      "00e0",
      """|BOF [0..0)
         |Constant.Double(0) [0..4)
         |EOF [4..4)
         |""".stripMargin
    ),
    (
      "00e0f",
      """|BOF [0..0)
         |Constant.Float(0) [0..5)
         |EOF [5..5)
         |""".stripMargin
    ),
    (
      "0xe1",
      """|BOF [0..0)
         |Constant.Int(225) [0..4)
         |EOF [4..4)
         |""".stripMargin
    ),
    (
      "0xd",
      """|BOF [0..0)
         |Constant.Int(13) [0..3)
         |EOF [3..3)
         |""".stripMargin
    ),
    (
      "0f",
      """|BOF [0..0)
         |Constant.Float(0) [0..2)
         |EOF [2..2)
         |""".stripMargin
    ),
    (
      "0.0f",
      """|BOF [0..0)
         |Constant.Float(0.0) [0..4)
         |EOF [4..4)
         |""".stripMargin
    ),
    (
      "0.f",
      """|BOF [0..0)
         |Constant.Int(0) [0..1)
         |Dot [1..2)
         |Ident(f) [2..3)
         |EOF [3..3)
         |""".stripMargin
    ),
    (
      ".f",
      """|BOF [0..0)
         |Dot [0..1)
         |Ident(f) [1..2)
         |EOF [2..2)
         |""".stripMargin
    ),
    (
      "1. + 2.",
      """|BOF [0..0)
         |Constant.Int(1) [0..1)
         |Dot [1..2)
         |Space [2..3)
         |Ident(+) [3..4)
         |Space [4..5)
         |Constant.Int(2) [5..6)
         |Dot [6..7)
         |EOF [7..7)
         |""".stripMargin
    ),
    (
      "1.0 + 2.0f",
      """|BOF [0..0)
         |Constant.Double(1.0) [0..3)
         |Space [3..4)
         |Ident(+) [4..5)
         |Space [5..6)
         |Constant.Float(2.0) [6..10)
         |EOF [10..10)
         |""".stripMargin
    ),
    (
      "1d + 2f",
      """|BOF [0..0)
         |Constant.Double(1) [0..2)
         |Space [2..3)
         |Ident(+) [3..4)
         |Space [4..5)
         |Constant.Float(2) [5..7)
         |EOF [7..7)
         |""".stripMargin
    ),
    (
      "0xf",
      """|BOF [0..0)
         |Constant.Int(15) [0..3)
         |EOF [3..3)
         |""".stripMargin
    ),
    (
      "0",
      """|BOF [0..0)
         |Constant.Int(0) [0..1)
         |EOF [1..1)
         |""".stripMargin
    ),
    (
      "0l",
      """|BOF [0..0)
         |Constant.Long(0) [0..2)
         |EOF [2..2)
         |""".stripMargin
    ),
    (
      "0L",
      """|BOF [0..0)
         |Constant.Long(0) [0..2)
         |EOF [2..2)
         |""".stripMargin
    ),
    (
      "0x80000000",
      """|BOF [0..0)
         |Constant.Int(2147483648) [0..10)
         |EOF [10..10)
         |""".stripMargin
    ),
    (
      "0x8000000000000000L",
      """|BOF [0..0)
         |Constant.Long(9223372036854775808) [0..19)
         |EOF [19..19)
         |""".stripMargin
    ),
    (
      "3.4028235e38f",
      """|BOF [0..0)
         |Constant.Float(3.4028235E+38) [0..13)
         |EOF [13..13)
         |""".stripMargin
    ),
    (
      "-3.4028235e38f",
      """|BOF [0..0)
         |Ident(-) [0..1)
         |Constant.Float(3.4028235E+38) [1..14)
         |EOF [14..14)
         |""".stripMargin
    ),
    (
      "1.7976931348623157e+308d",
      """|BOF [0..0)
         |Constant.Double(1.7976931348623157E+308) [0..24)
         |EOF [24..24)
         |""".stripMargin
    ),
    (
      "-1.7976931348623157e+308d",
      """|BOF [0..0)
         |Ident(-) [0..1)
         |Constant.Double(1.7976931348623157E+308) [1..25)
         |EOF [25..25)
         |""".stripMargin
    ),
    (
      "0b00101010",
      """|BOF [0..0)
         |Constant.Int(42) [0..10)
         |EOF [10..10)
         |""".stripMargin
    ),
    (
      "0B_0010_1010",
      """|BOF [0..0)
         |Constant.Int(42) [0..12)
         |EOF [12..12)
         |""".stripMargin
    ),
    (
      "0b_0010_1010L",
      """|BOF [0..0)
         |Constant.Long(42) [0..13)
         |EOF [13..13)
         |""".stripMargin
    ),
    (
      "0b01.2",
      """|BOF [0..0)
         |Constant.Int(1) [0..4)
         |Constant.Double(0.2) [4..6)
         |EOF [6..6)
         |""".stripMargin
    ),
    (
      "0b01.toString",
      """|BOF [0..0)
         |Constant.Int(1) [0..4)
         |Dot [4..5)
         |Ident(toString) [5..13)
         |EOF [13..13)
         |""".stripMargin
    )
  ).foreach { case (code, value) =>
    test(s"numeric literal ok scala213: $code") {
      assertTokenizedAsStructureLines(code, value, dialects.Scala213)
    }
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
    test(s"numeric literal fail scala213: $code") {
      implicit val dialect: Dialect = dialects.Scala213
      interceptMessage[ParseException](error.lf2nl)(code.parseRule(_.entrypointExpr()))
    }
  }

  Seq(
    // regular or half-surrogate char codes
    55297,
    56375
  ).foreach { ch =>
    test(s"#3690 any character with char code $ch") {
      implicit val dialect = dialects.Scala213
      tokenize(
        s"""|"${ch.toChar}"
            |""".stripMargin
      ) match {
        case Tokens(_: BOF, Constant.String(str), _: LF, _: EOF) =>
          assertEquals(str.length, 1)
          assertEquals(str, s"${ch.toChar}")
        case tokens => fail(s"unexpected tokens: ${tokens.structure}")
      }
    }
  }

  Seq(
    // full-surrogate char code pairs
    (55297, 56375),
    (0xdbff, 0xdfff)
  ).foreach { case (hi, lo) =>
    test(s"#3690 full-surrogate with char codes $hi and $lo") {
      implicit val dialect = dialects.Scala213
      tokenize(
        s"""|"${hi.toChar}${lo.toChar}"
            |""".stripMargin
      ) match {
        case Tokens(_: BOF, Constant.String(str), _: LF, _: EOF) =>
          assertEquals(str, s"${hi.toChar}${lo.toChar}")
        case tokens => fail(s"unexpected tokens: ${tokens.structure}")
      }
    }
  }

  test("scala3 code using CRLF") {
    val code = """|object A:
                  |  foo.map:
                  |    bar
                  |    + baz
                  |  for
                  |    a <- foo
                  |    b <- bar
                  |  yield
                  |    a + b
                  |""".stripMargin
    val struct = """|BOF [0..0)
                    |KwObject [0..6)
                    |Space [6..7)
                    |Ident(A) [7..8)
                    |Colon [8..9)
                    |CRLF [9..11)
                    |MultiHS(2) [11..13)
                    |Ident(foo) [13..16)
                    |Dot [16..17)
                    |Ident(map) [17..20)
                    |Colon [20..21)
                    |CRLF [21..23)
                    |MultiHS(4) [23..27)
                    |Ident(bar) [27..30)
                    |CRLF [30..32)
                    |MultiHS(4) [32..36)
                    |Ident(+) [36..37)
                    |Space [37..38)
                    |Ident(baz) [38..41)
                    |CRLF [41..43)
                    |MultiHS(2) [43..45)
                    |KwFor [45..48)
                    |CRLF [48..50)
                    |MultiHS(4) [50..54)
                    |Ident(a) [54..55)
                    |Space [55..56)
                    |LeftArrow [56..58)
                    |Space [58..59)
                    |Ident(foo) [59..62)
                    |CRLF [62..64)
                    |MultiHS(4) [64..68)
                    |Ident(b) [68..69)
                    |Space [69..70)
                    |LeftArrow [70..72)
                    |Space [72..73)
                    |Ident(bar) [73..76)
                    |CRLF [76..78)
                    |MultiHS(2) [78..80)
                    |KwYield [80..85)
                    |CRLF [85..87)
                    |MultiHS(4) [87..91)
                    |Ident(a) [91..92)
                    |Space [92..93)
                    |Ident(+) [93..94)
                    |Space [94..95)
                    |Ident(b) [95..96)
                    |CRLF [96..98)
                    |EOF [98..98)
                    |""".stripMargin
    assertTokenizedAsStructureLines(code.replace("\n", "\r\n"), struct, dialects.Scala3)
  }

  test("code with Shebang line") {
    val code = """|#!/usr/bin/env foo bar && qux >/dev/null
                  |package foo
                  |
                  |import bar
                  |
                  |class Baz()
                  |""".stripMargin
    val struct = """|BOF [0..0)
                    |Shebang [0..40)
                    |CRLF [40..42)
                    |KwPackage [42..49)
                    |Space [49..50)
                    |Ident(foo) [50..53)
                    |MultiNL(2) [53..57)
                    |KwImport [57..63)
                    |Space [63..64)
                    |Ident(bar) [64..67)
                    |MultiNL(2) [67..71)
                    |KwClass [71..76)
                    |Space [76..77)
                    |Ident(Baz) [77..80)
                    |LeftParen [80..81)
                    |RightParen [81..82)
                    |CRLF [82..84)
                    |EOF [84..84)
                    |""".stripMargin
    assertTokenizedAsStructureLines(code.replace("\n", "\r\n"), struct, dialects.Scala3)
  }

  test("code with non-matching braces 1") {
    val code = """|object a {
                  |  def foo = {
                  |    val a = q"bar${qux}"
                  |""".stripMargin
    val struct = """|BOF [0..0)
                    |KwObject [0..6)
                    |Space [6..7)
                    |Ident(a) [7..8)
                    |Space [8..9)
                    |LeftBrace [9..10)
                    |LF [10..11)
                    |MultiHS(2) [11..13)
                    |KwDef [13..16)
                    |Space [16..17)
                    |Ident(foo) [17..20)
                    |Space [20..21)
                    |Equals [21..22)
                    |Space [22..23)
                    |LeftBrace [23..24)
                    |LF [24..25)
                    |MultiHS(4) [25..29)
                    |KwVal [29..32)
                    |Space [32..33)
                    |Ident(a) [33..34)
                    |Space [34..35)
                    |Equals [35..36)
                    |Space [36..37)
                    |Interpolation.Id(q) [37..38)
                    |Interpolation.Start(") [38..39)
                    |Interpolation.Part(bar) [39..42)
                    |Interpolation.SpliceStart [42..43)
                    |LeftBrace [43..44)
                    |Ident(qux) [44..47)
                    |RightBrace [47..48)
                    |Interpolation.SpliceEnd [48..48)
                    |Interpolation.Part() [48..48)
                    |Interpolation.End(") [48..49)
                    |LF [49..50)
                    |EOF [50..50)
                    |""".stripMargin
    assertTokenizedAsStructureLines(code, struct, dialects.Scala3)
  }

  test("code with non-matching braces 2") {
    val code = """|object a {
                  |  def foo = {
                  |    val a = "b"
                  |  }
                  |  }
                  |}
                  |""".stripMargin
    val struct = """|BOF [0..0)
                    |KwObject [0..6)
                    |Space [6..7)
                    |Ident(a) [7..8)
                    |Space [8..9)
                    |LeftBrace [9..10)
                    |LF [10..11)
                    |MultiHS(2) [11..13)
                    |KwDef [13..16)
                    |Space [16..17)
                    |Ident(foo) [17..20)
                    |Space [20..21)
                    |Equals [21..22)
                    |Space [22..23)
                    |LeftBrace [23..24)
                    |LF [24..25)
                    |MultiHS(4) [25..29)
                    |KwVal [29..32)
                    |Space [32..33)
                    |Ident(a) [33..34)
                    |Space [34..35)
                    |Equals [35..36)
                    |Space [36..37)
                    |Constant.String(b) [37..40)
                    |LF [40..41)
                    |MultiHS(2) [41..43)
                    |RightBrace [43..44)
                    |LF [44..45)
                    |MultiHS(2) [45..47)
                    |RightBrace [47..48)
                    |LF [48..49)
                    |RightBrace [49..50)
                    |LF [50..51)
                    |EOF [51..51)
                    |""".stripMargin
    assertTokenizedAsStructureLines(code, struct, dialects.Scala3)
  }

  test("code with incomplete interpolation") {
    val code = """|object a {
                  |  def foo = s"b$"
                  |}
                  |""".stripMargin

    // scala213
    val struct213 = """|BOF [0..0)
                       |KwObject [0..6)
                       |Space [6..7)
                       |Ident(a) [7..8)
                       |Space [8..9)
                       |LeftBrace [9..10)
                       |LF [10..11)
                       |MultiHS(2) [11..13)
                       |KwDef [13..16)
                       |Space [16..17)
                       |Ident(foo) [17..20)
                       |Space [20..21)
                       |Equals [21..22)
                       |Space [22..23)
                       |Interpolation.Id(s) [23..24)
                       |Interpolation.Start(") [24..25)
                       |Interpolation.Part(b) [25..26)
                       |Interpolation.SpliceStart [26..27)
                       |Invalid(Not one of: `$'_, `$$', `$'ident, `$'this, `$'BlockExpr) [27..27)
                       |Constant.String() [27..27)
                       |Interpolation.SpliceEnd [28..28)
                       |Interpolation.Part(\n) [28..29)
                       |Interpolation.End() [29..29)
                       |RightBrace [29..30)
                       |LF [30..31)
                       |EOF [31..31)
                       |""".stripMargin
    assertTokenizedAsStructureLines(code, struct213, dialects.Scala213)

    // scala3
    val struct3 = """|BOF [0..0)
                     |KwObject [0..6)
                     |Space [6..7)
                     |Ident(a) [7..8)
                     |Space [8..9)
                     |LeftBrace [9..10)
                     |LF [10..11)
                     |MultiHS(2) [11..13)
                     |KwDef [13..16)
                     |Space [16..17)
                     |Ident(foo) [17..20)
                     |Space [20..21)
                     |Equals [21..22)
                     |Space [22..23)
                     |Interpolation.Id(s) [23..24)
                     |Interpolation.Start(") [24..25)
                     |Interpolation.Part(b") [25..28)
                     |Invalid(unclosed single-line string interpolation) [28..28)
                     |Interpolation.End() [28..28)
                     |LF [28..29)
                     |RightBrace [29..30)
                     |LF [30..31)
                     |EOF [31..31)
                     |""".stripMargin
    assertTokenizedAsStructureLines(code, struct3, dialects.Scala3)
  }

  test("code with double-quote string within single-line quasiquotes") {
    val code = " \"...\" "
    val struct = """|BOF [0..0)
                    |Space [0..1)
                    |Constant.String(...) [1..6)
                    |Invalid(double quotes are not allowed in single-line quasiquotes) [1..1)
                    |Space [6..7)
                    |EOF [7..7)
                    |""".stripMargin
    assertTokenizedAsStructureLines(code, struct, dialects.Scala213.unquoteTerm(multiline = false))
  }

  test("code with interpolation within single-line quasiquotes") {
    val code = " s\"...\" "
    val struct = """|BOF [0..0)
                    |Space [0..1)
                    |Interpolation.Id(s) [1..2)
                    |Invalid(double quotes are not allowed in single-line quasiquotes) [2..2)
                    |Interpolation.Start(") [2..3)
                    |Interpolation.Part(...) [3..6)
                    |Interpolation.End(") [6..7)
                    |Space [7..8)
                    |EOF [8..8)
                    |""".stripMargin
    assertTokenizedAsStructureLines(code, struct, dialects.Scala213.unquoteTerm(multiline = false))
  }

  test("interpolator with $ character") {
    val code = """s"$$$foo$$""""
    val struct = """|BOF [0..0)
                    |Interpolation.Id(s) [0..1)
                    |Interpolation.Start(") [1..2)
                    |Interpolation.Part($) [2..4)
                    |Interpolation.SpliceStart [4..5)
                    |Ident(foo) [5..8)
                    |Interpolation.SpliceEnd [8..8)
                    |Interpolation.Part($) [8..10)
                    |Interpolation.End(") [10..11)
                    |EOF [11..11) 
                    |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct)
  }
  test("broken-interpolator") {
    val code = """|
                  |s'''|Available languages (default = $):
                  |verilog - Verilog or SystemVerilog dialects
                  |vhdl    - VHDL dialects
                  |
                  |Available Verilog/SystemVerilog dialects (default = $defaultDialect):
                  |v2001   - Verilog 2001
                  |sv2005  - 
                  |'''.stripMargin
                  |""".stripMargin.replace("'''", "\"\"\"")

    val struct =
      """|BOF [0..0)
         |LF [0..1)
         |Interpolation.Id(s) [1..2)
         |Interpolation.Start(''') [2..5)
         |Interpolation.Part(|Available languages (default = ) [5..37)
         |Interpolation.SpliceStart [37..38)
         |Invalid(Not one of: `$'_, `$$', `$'ident, `$'this, `$'BlockExpr) [38..38)
         |Interpolation.SpliceEnd [38..38)
         |Interpolation.Part():\nverilog - Verilog or SystemVerilog dialects\nvhdl    - VHDL dialects\n\nAvailable Verilog/SystemVerilog dialects (default = ) [38..162)
         |Interpolation.SpliceStart [162..163)
         |Ident(defaultDialect) [163..177)
         |Interpolation.SpliceEnd [177..177)
         |Interpolation.Part():\nv2001   - Verilog 2001\nsv2005  - \n) [177..214)
         |Interpolation.End(''') [214..217)
         |Dot [217..218)
         |Ident(stripMargin) [218..229)
         |LF [229..230)
         |EOF [230..230)
         |""".stripMargin.replace("'''", "\"\"\"").nl2lf
    assertTokenizedAsStructureLines(code, struct)
  }
  test("unexpected-character") {
    val code = """|
                  |val 。 = 123
                  |
                  |""".stripMargin.replace("'''", "\"\"\"")

    val struct = s"""|BOF [0..0)
                     |LF [0..1)
                     |KwVal [1..4)
                     |Invalid(illegal character '\\u3002') [5..6)
                     |MultiHS(3) [4..7)
                     |Equals [7..8)
                     |Space [8..9)
                     |Constant.Int(123) [9..12)
                     |MultiNL(2) [12..14)
                     |EOF [14..14)
                     |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct)
  }

}
