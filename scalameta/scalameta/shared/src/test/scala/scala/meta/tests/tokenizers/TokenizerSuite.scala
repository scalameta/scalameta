package scala.meta.tests
package tokenizers

import org.scalatest._
import scala.meta._
import scala.meta.tokens.Token._
import scala.meta.dialects.Scala211

class TokenizerSuite extends FunSuite {
  def tokenize(code: String): Tokens = {
    val convert = scala.meta.inputs.stringToInput
    val tokenize = scala.meta.tokenizers.Tokenize.scalametaTokenize
    val dialect = Scala211
    code.tokenize(convert, tokenize, dialect).get
  }

  test("showCode without comments - simple") {
    assert(tokenize("class C  {\t val x = 2}\n\n").map(_.syntax).mkString === "class C  {\t val x = 2}\n\n")
  }

  test("showcode without comments - hard") {
    assert(tokenize("""
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
    """.trim.stripMargin.replace("QQQ", "\"\"\"")).map(_.syntax).mkString === """
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
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("showCode without comments - insane") {
    assert(tokenize("""
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
    """.trim.stripMargin.replace("QQQ", "\"\"\"")).map(_.syntax).mkString === """
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
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("showCode with comments - easy") {
    assert(tokenize("class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n").map(_.syntax).mkString === "class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n")
  }

  test("showCode with comments - tricky") {
    assert(tokenize("x ~/**/y").map(_.syntax).mkString === "x ~/**/y")
  }

  test("showRaw without comments - easy") {
    assert(tokenize("class C  {\t val x = 2}\n\n").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |class [0..5)
      |  [5..6)
      |C [6..7)
      |  [7..8)
      |  [8..9)
      |{ [9..10)
      |\t [10..11)
      |  [11..12)
      |val [12..15)
      |  [15..16)
      |x [16..17)
      |  [17..18)
      |= [18..19)
      |  [19..20)
      |2 [20..21)
      |} [21..22)
      |\n [22..23)
      |\n [23..24)
      |EOF [24..24)
    """.trim.stripMargin)
  }

  test("showRaw without comments - hard") {
    assert(tokenize("""
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
    """.trim.stripMargin.replace("QQQ", "\"\"\"")).map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |class [0..5)
      |  [5..6)
      |C [6..7)
      |  [7..8)
      |{ [8..9)
      |\n [9..10)
      |  [10..11)
      |  [11..12)
      |val [12..15)
      |  [15..16)
      |x1a [16..19)
      |  [19..20)
      |= [20..21)
      |  [21..22)
      |2 [22..23)
      |\n [23..24)
      |  [24..25)
      |  [25..26)
      |val [26..29)
      |  [29..30)
      |x1b [30..33)
      |  [33..34)
      |= [34..35)
      |  [35..36)
      |0x002 [36..41)
      |\n [41..42)
      |  [42..43)
      |  [43..44)
      |val [44..47)
      |  [47..48)
      |x1c [48..51)
      |  [51..52)
      |= [52..53)
      |  [53..54)
      |0x002a [54..60)
      |\n [60..61)
      |  [61..62)
      |  [62..63)
      |val [63..66)
      |  [66..67)
      |x2a [67..70)
      |  [70..71)
      |= [71..72)
      |  [72..73)
      |2l [73..75)
      |\n [75..76)
      |  [76..77)
      |  [77..78)
      |val [78..81)
      |  [81..82)
      |x2b [82..85)
      |  [85..86)
      |= [86..87)
      |  [87..88)
      |2L [88..90)
      |\n [90..91)
      |  [91..92)
      |  [92..93)
      |val [93..96)
      |  [96..97)
      |x2c [97..100)
      |  [100..101)
      |= [101..102)
      |  [102..103)
      |0x002l [103..109)
      |\n [109..110)
      |  [110..111)
      |  [111..112)
      |val [112..115)
      |  [115..116)
      |x2d [116..119)
      |  [119..120)
      |= [120..121)
      |  [121..122)
      |0x002L [122..128)
      |\n [128..129)
      |  [129..130)
      |  [130..131)
      |val [131..134)
      |  [134..135)
      |x2e [135..138)
      |  [138..139)
      |= [139..140)
      |  [140..141)
      |0x002al [141..148)
      |\n [148..149)
      |  [149..150)
      |  [150..151)
      |val [151..154)
      |  [154..155)
      |x2f [155..158)
      |  [158..159)
      |= [159..160)
      |  [160..161)
      |0x002aL [161..168)
      |\n [168..169)
      |  [169..170)
      |  [170..171)
      |val [171..174)
      |  [174..175)
      |x3a [175..178)
      |  [178..179)
      |= [179..180)
      |  [180..181)
      |2f [181..183)
      |\n [183..184)
      |  [184..185)
      |  [185..186)
      |val [186..189)
      |  [189..190)
      |x3b [190..193)
      |  [193..194)
      |= [194..195)
      |  [195..196)
      |2.0F [196..200)
      |\n [200..201)
      |  [201..202)
      |  [202..203)
      |val [203..206)
      |  [206..207)
      |x4a [207..210)
      |  [210..211)
      |= [211..212)
      |  [212..213)
      |2d [213..215)
      |\n [215..216)
      |  [216..217)
      |  [217..218)
      |val [218..221)
      |  [221..222)
      |x4b [222..225)
      |  [225..226)
      |= [226..227)
      |  [227..228)
      |2.0D [228..232)
      |\n [232..233)
      |  [233..234)
      |  [234..235)
      |val [235..238)
      |  [238..239)
      |x4c [239..242)
      |  [242..243)
      |= [243..244)
      |  [244..245)
      |2.0 [245..248)
      |\n [248..249)
      |  [249..250)
      |  [250..251)
      |val [251..254)
      |  [254..255)
      |x5a [255..258)
      |  [258..259)
      |= [259..260)
      |  [260..261)
      |'a' [261..264)
      |\n [264..265)
      |  [265..266)
      |  [266..267)
      |val [267..270)
      |  [270..271)
      |x5b [271..274)
      |  [274..275)
      |= [275..276)
      |  [276..277)
      |'\b' [277..281)
      |\n [281..282)
      |  [282..283)
      |  [283..284)
      |val [284..287)
      |  [287..288)
      |x5c [288..291)
      |  [291..292)
      |= [292..293)
      |  [293..294)
      |'"' [294..297)
      |\n [297..298)
      |  [298..299)
      |  [299..300)
      |val [300..303)
      |  [303..304)
      |x5d [304..307)
      |  [307..308)
      |= [308..309)
      |  [309..310)
      |'\"' [310..314)
      |\n [314..315)
      |  [315..316)
      |  [316..317)
      |val [317..320)
      |  [320..321)
      |x6 [321..323)
      |  [323..324)
      |= [324..325)
      |  [325..326)
      |'a [326..328)
      |\n [328..329)
      |  [329..330)
      |  [330..331)
      |val [331..334)
      |  [334..335)
      |x7a [335..338)
      |  [338..339)
      |= [339..340)
      |  [340..341)
      |"" [341..343)
      |\n [343..344)
      |  [344..345)
      |  [345..346)
      |val [346..349)
      |  [349..350)
      |x7b [350..353)
      |  [353..354)
      |= [354..355)
      |  [355..356)
      |"\b" [356..360)
      |\n [360..361)
      |  [361..362)
      |  [362..363)
      |val [363..366)
      |  [366..367)
      |x7c [367..370)
      |  [370..371)
      |= [371..372)
      |  [372..373)
      |"c" [373..376)
      |\n [376..377)
      |  [377..378)
      |  [378..379)
      |val [379..382)
      |  [382..383)
      |x7d [383..386)
      |  [386..387)
      |= [387..388)
      |  [388..389)
      |"\"" [389..393)
      |\n [393..394)
      |  [394..395)
      |  [395..396)
      |val [396..399)
      |  [399..400)
      |x7e [400..403)
      |  [403..404)
      |= [404..405)
      |  [405..406)
      |QQQQQQ [406..412)
      |\n [412..413)
      |  [413..414)
      |  [414..415)
      |val [415..418)
      |  [418..419)
      |x7f [419..422)
      |  [422..423)
      |= [423..424)
      |  [424..425)
      |QQQf\nQQQ [425..434)
      |\n [434..435)
      |  [435..436)
      |  [436..437)
      |val [437..440)
      |  [440..441)
      |hello [441..446)
      |  [446..447)
      |= [447..448)
      |  [448..449)
      |42 [449..451)
      |\n [451..452)
      |  [452..453)
      |  [453..454)
      |val [454..457)
      |  [457..458)
      |`world` [458..465)
      |  [465..466)
      |= [466..467)
      |  [467..468)
      |42 [468..470)
      |\n [470..471)
      |} [471..472)
      |EOF [472..472)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("showRaw without comments - insane") {
    assert(tokenize("""
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
    """.trim.stripMargin.replace("QQQ", "\"\"\"")).map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |class [0..5)
      |  [5..6)
      |C [6..7)
      |  [7..8)
      |{ [8..9)
      |\n [9..10)
      |  [10..11)
      |  [11..12)
      |q [12..13)
      |" [13..14)
      | [14..14)
      |" [14..15)
      |\n [15..16)
      |  [16..17)
      |  [17..18)
      |q [18..19)
      |" [19..20)
      | [20..20)
      |$ [20..21)
      |b [21..22)
      | [22..22)
      | + 2 [22..26)
      |" [26..27)
      |\n [27..28)
      |  [28..29)
      |  [29..30)
      |q [30..31)
      |" [31..32)
      | [32..32)
      |$ [32..33)
      |{ [33..34)
      |b [34..35)
      |} [35..36)
      | [36..36)
      | + 2 [36..40)
      |" [40..41)
      |\n [41..42)
      |  [42..43)
      |  [43..44)
      |q [44..45)
      |" [45..46)
      |class  [46..52)
      |$ [52..53)
      |X [53..54)
      | [54..54)
      | [54..54)
      |" [54..55)
      |\n [55..56)
      |  [56..57)
      |  [57..58)
      |q [58..59)
      |" [59..60)
      |class  [60..66)
      |$ [66..67)
      |{ [67..68)
      |X [68..69)
      |} [69..70)
      | [70..70)
      | [70..70)
      |" [70..71)
      |\n [71..72)
      |  [72..73)
      |  [73..74)
      |q [74..75)
      |QQQ [75..78)
      | [78..78)
      |QQQ [78..81)
      |\n [81..82)
      |  [82..83)
      |  [83..84)
      |q [84..85)
      |QQQ [85..88)
      | [88..88)
      |$ [88..89)
      |d [89..90)
      | [90..90)
      | + 2 [90..94)
      |QQQ [94..97)
      |\n [97..98)
      |  [98..99)
      |  [99..100)
      |q [100..101)
      |QQQ [101..104)
      | [104..104)
      |$ [104..105)
      |{ [105..106)
      |d [106..107)
      |} [107..108)
      | [108..108)
      | + 2 [108..112)
      |QQQ [112..115)
      |\n [115..116)
      |  [116..117)
      |  [117..118)
      |q [118..119)
      |QQQ [119..122)
      |class  [122..128)
      |$ [128..129)
      |Y [129..130)
      | [130..130)
      | [130..130)
      |QQQ [130..133)
      |\n [133..134)
      |  [134..135)
      |  [135..136)
      |q [136..137)
      |QQQ [137..140)
      |class  [140..146)
      |$ [146..147)
      |{ [147..148)
      |Y [148..149)
      |} [149..150)
      | [150..150)
      | [150..150)
      |QQQ [150..153)
      |\n [153..154)
      |} [154..155)
      |EOF [155..155)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("showRaw with comments - easy") {
    assert(tokenize("class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |class [0..5)
      |  [5..6)
      |C [6..7)
      |  [7..8)
      |  [8..9)
      |/*hello world*/ [9..24)
      |{ [24..25)
      |\t [25..26)
      |  [26..27)
      |val [27..30)
      |  [30..31)
      |x [31..32)
      |  [32..33)
      |= [33..34)
      |  [34..35)
      |2 [35..36)
      |} [36..37)
      |\n [37..38)
      |//bye-bye world [38..53)
      |\n [53..54)
      |EOF [54..54)
    """.trim.stripMargin)
  }

  test("showRaw with comments - tricky") {
    assert(tokenize("x ~/**/y").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |x [0..1)
      |  [1..2)
      |~ [2..3)
      |/**/ [3..7)
      |y [7..8)
      |EOF [8..8)
    """.trim.stripMargin)
  }

  test("interpolation start & end - episode 01") {
    assert(tokenize("q\"\"").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |q [0..1)
      |" [1..2)
      | [2..2)
      |" [2..3)
      |EOF [3..3)
    """.trim.stripMargin)
  }

  test("interpolation start & end - episode 02") {
    assert(tokenize("q\"\";").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |q [0..1)
      |" [1..2)
      | [2..2)
      |" [2..3)
      |; [3..4)
      |EOF [4..4)
    """.trim.stripMargin)
  }

  test("interpolation start & end - episode 03") {
    assert(tokenize("q\"a\"").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |q [0..1)
      |" [1..2)
      |a [2..3)
      |" [3..4)
      |EOF [4..4)
    """.trim.stripMargin)
  }

  test("interpolation start & end - episode 04") {
    assert(tokenize("q\"a\";").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |q [0..1)
      |" [1..2)
      |a [2..3)
      |" [3..4)
      |; [4..5)
      |EOF [5..5)
    """.trim.stripMargin)
  }

  test("interpolation start & end - episode 05") {
    assert(tokenize("q\"\"\"\"\"\"").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |q [0..1)
      |QQQ [1..4)
      | [4..4)
      |QQQ [4..7)
      |EOF [7..7)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("interpolation start & end - episode 06") {
    assert(tokenize("q\"\"\"\"\"\";").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |q [0..1)
      |QQQ [1..4)
      | [4..4)
      |QQQ [4..7)
      |; [7..8)
      |EOF [8..8)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("interpolation start & end - episode 07") {
    assert(tokenize("q\"\"\"a\"\"\"").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |q [0..1)
      |QQQ [1..4)
      |a [4..5)
      |QQQ [5..8)
      |EOF [8..8)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("interpolation start & end - episode 08") {
    assert(tokenize("q\"\"\"a\"\"\";").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |q [0..1)
      |QQQ [1..4)
      |a [4..5)
      |QQQ [5..8)
      |; [8..9)
      |EOF [9..9)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("$this") {
    assert(tokenize("q\"$this\"").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |q [0..1)
      |" [1..2)
      | [2..2)
      |$ [2..3)
      |this [3..7)
      | [7..7)
      | [7..7)
      |" [7..8)
      |EOF [8..8)
    """.trim.stripMargin)
  }

  test("monocle") {
    assert(tokenize("x => x").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |x [0..1)
      |  [1..2)
      |=> [2..4)
      |  [4..5)
      |x [5..6)
      |EOF [6..6)
    """.trim.stripMargin)
    assert(tokenize("x ⇒ x").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |x [0..1)
      |  [1..2)
      |⇒ [2..3)
      |  [3..4)
      |x [4..5)
      |EOF [5..5)
    """.trim.stripMargin)
    assert(tokenize("for (x <- xs) println(x)").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |for [0..3)
      |  [3..4)
      |( [4..5)
      |x [5..6)
      |  [6..7)
      |<- [7..9)
      |  [9..10)
      |xs [10..12)
      |) [12..13)
      |  [13..14)
      |println [14..21)
      |( [21..22)
      |x [22..23)
      |) [23..24)
      |EOF [24..24)
    """.trim.stripMargin)
    assert(tokenize("for (x ← xs) println(x)").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |for [0..3)
      |  [3..4)
      |( [4..5)
      |x [5..6)
      |  [6..7)
      |← [7..8)
      |  [8..9)
      |xs [9..11)
      |) [11..12)
      |  [12..13)
      |println [13..20)
      |( [20..21)
      |x [21..22)
      |) [22..23)
      |EOF [23..23)
    """.trim.stripMargin)
  }

  test("-2147483648") {
    assert(tokenize("-2147483648").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      |- [0..1)
      |2147483648 [1..11)
      |EOF [11..11)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("simple xml literal - 1") {
    assert(tokenize("<foo>bar</foo>").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      | [0..0)
      |<foo>bar</foo> [0..14)
      | [14..14)
      |EOF [14..14)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("simple xml literal - 2") {
    // TODO: the whitespace shouldn't be included here - looks like a bug in scalac's MarkupParser
    assert(tokenize("<foo>bar</foo> ").map(_.structure).mkString("\n") === """
      |BOF [0..0)
      | [0..0)
      |<foo>bar</foo> [0..14)
      | [14..14)
      |  [14..15)
      |EOF [15..15)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("parsed trees don't have BOF/EOF in their tokens") {
    val tree = "foo + bar".parse[Term].get
    assert(tree.pos != Position.None)
    assert(tree.tokens.structure === "Tokens(BOF [0..0), foo [0..3),   [3..4), + [4..5),   [5..6), bar [6..9), EOF [9..9))")
  }

  test("synthetic trees don't have BOF/EOF in their tokens") {
    val tree = Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))
    assert(tree.pos == Position.None)
    assert(tree.tokens.structure === "Tokens(BOF [0..0), foo [0..3),   [3..4), + [4..5),   [5..6), bar [6..9), EOF [9..9))")
  }

  test("Ident.value for normal") {
    val Tokens(bof, foo: Ident, eof) = "foo".parse[Term].get.tokens
    assert(foo.value === "foo")
  }

  test("Ident.value for backquoted") {
    val Tokens(bof, foo: Ident, eof) = "`foo`".parse[Term].get.tokens
    assert(foo.value === "foo")
    assert(foo.syntax === "`foo`")
  }

  test("Interpolation.Id.value") {
    val Tokens(bof, _, id: Interpolation.Id, _, _, _, _, eof) = """ q"" """.tokenize.get
    assert(id.value === "q")
  }

  test("Interpolation.Part.value") {
    val Tokens(bof, _, _, _, part: Interpolation.Part, _, _, eof) = """ q"foo" """.tokenize.get
    assert(part.value === "foo")
  }

  test("Xml.Part.value") {
    // TODO: not implemented yet
  }

  test("Comment.value") {
    val Tokens(bof, comment: Comment, eof) = "//foo".tokenize.get
    assert(comment.value === "foo")
  }

  test("enum") {
    val Tokens(BOF(), _: KwEnum, EOF()) = dialects.Dotty("enum").tokenize.get
    val Tokens(BOF(), Ident("enum"), EOF()) = dialects.Scala212("enum").tokenize.get

    val Tokens(
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
    ) = dialects.Scala212("s\"$enum\"").tokenize.get

    assert(dialects.Dotty("s\"$enum\"").tokenize.isInstanceOf[Tokenized.Error])
  }
}
