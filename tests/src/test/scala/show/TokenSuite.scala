import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class ShowTokenSuite extends ParseSuite {
  test("showCode without comments - simple") {
    assert(tokenize("class C  {\t val x = 2}\n\n").map(_.show[Code]).mkString === "class C  {\t val x = 2}\n\n")
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
    """.trim.stripMargin.replace("QQQ", "\"\"\"")).map(_.show[Code]).mkString === """
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
    """.trim.stripMargin.replace("QQQ", "\"\"\"")).map(_.show[Code]).mkString === """
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
    assert(tokenize("class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n").map(_.show[Code]).mkString === "class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n")
  }

  test("showCode with comments - tricky") {
    assert(tokenize("x ~/**/y").map(_.show[Code]).mkString === "x ~/**/y")
  }

  test("showRaw without comments - easy") {
    assert(tokenize("class C  {\t val x = 2}\n\n").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |class (0..4)
      |  (5..5)
      |C (6..6)
      |  (7..7)
      |  (8..8)
      |{ (9..9)
      |\t (10..10)
      |  (11..11)
      |val (12..14)
      |  (15..15)
      |x (16..16)
      |  (17..17)
      |= (18..18)
      |  (19..19)
      |2 (20..20)
      |} (21..21)
      |\n (22..22)
      |\n (23..23)
      |EOF (24..23)
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
    """.trim.stripMargin.replace("QQQ", "\"\"\"")).map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |class (0..4)
      |  (5..5)
      |C (6..6)
      |  (7..7)
      |{ (8..8)
      |\n (9..9)
      |  (10..10)
      |  (11..11)
      |val (12..14)
      |  (15..15)
      |x1a (16..18)
      |  (19..19)
      |= (20..20)
      |  (21..21)
      |2 (22..22)
      |\n (23..23)
      |  (24..24)
      |  (25..25)
      |val (26..28)
      |  (29..29)
      |x1b (30..32)
      |  (33..33)
      |= (34..34)
      |  (35..35)
      |0x002 (36..40)
      |\n (41..41)
      |  (42..42)
      |  (43..43)
      |val (44..46)
      |  (47..47)
      |x1c (48..50)
      |  (51..51)
      |= (52..52)
      |  (53..53)
      |0x002a (54..59)
      |\n (60..60)
      |  (61..61)
      |  (62..62)
      |val (63..65)
      |  (66..66)
      |x2a (67..69)
      |  (70..70)
      |= (71..71)
      |  (72..72)
      |2l (73..74)
      |\n (75..75)
      |  (76..76)
      |  (77..77)
      |val (78..80)
      |  (81..81)
      |x2b (82..84)
      |  (85..85)
      |= (86..86)
      |  (87..87)
      |2L (88..89)
      |\n (90..90)
      |  (91..91)
      |  (92..92)
      |val (93..95)
      |  (96..96)
      |x2c (97..99)
      |  (100..100)
      |= (101..101)
      |  (102..102)
      |0x002l (103..108)
      |\n (109..109)
      |  (110..110)
      |  (111..111)
      |val (112..114)
      |  (115..115)
      |x2d (116..118)
      |  (119..119)
      |= (120..120)
      |  (121..121)
      |0x002L (122..127)
      |\n (128..128)
      |  (129..129)
      |  (130..130)
      |val (131..133)
      |  (134..134)
      |x2e (135..137)
      |  (138..138)
      |= (139..139)
      |  (140..140)
      |0x002al (141..147)
      |\n (148..148)
      |  (149..149)
      |  (150..150)
      |val (151..153)
      |  (154..154)
      |x2f (155..157)
      |  (158..158)
      |= (159..159)
      |  (160..160)
      |0x002aL (161..167)
      |\n (168..168)
      |  (169..169)
      |  (170..170)
      |val (171..173)
      |  (174..174)
      |x3a (175..177)
      |  (178..178)
      |= (179..179)
      |  (180..180)
      |2f (181..182)
      |\n (183..183)
      |  (184..184)
      |  (185..185)
      |val (186..188)
      |  (189..189)
      |x3b (190..192)
      |  (193..193)
      |= (194..194)
      |  (195..195)
      |2.0F (196..199)
      |\n (200..200)
      |  (201..201)
      |  (202..202)
      |val (203..205)
      |  (206..206)
      |x4a (207..209)
      |  (210..210)
      |= (211..211)
      |  (212..212)
      |2d (213..214)
      |\n (215..215)
      |  (216..216)
      |  (217..217)
      |val (218..220)
      |  (221..221)
      |x4b (222..224)
      |  (225..225)
      |= (226..226)
      |  (227..227)
      |2.0D (228..231)
      |\n (232..232)
      |  (233..233)
      |  (234..234)
      |val (235..237)
      |  (238..238)
      |x4c (239..241)
      |  (242..242)
      |= (243..243)
      |  (244..244)
      |2.0 (245..247)
      |\n (248..248)
      |  (249..249)
      |  (250..250)
      |val (251..253)
      |  (254..254)
      |x5a (255..257)
      |  (258..258)
      |= (259..259)
      |  (260..260)
      |'a' (261..263)
      |\n (264..264)
      |  (265..265)
      |  (266..266)
      |val (267..269)
      |  (270..270)
      |x5b (271..273)
      |  (274..274)
      |= (275..275)
      |  (276..276)
      |'\b' (277..280)
      |\n (281..281)
      |  (282..282)
      |  (283..283)
      |val (284..286)
      |  (287..287)
      |x5c (288..290)
      |  (291..291)
      |= (292..292)
      |  (293..293)
      |'"' (294..296)
      |\n (297..297)
      |  (298..298)
      |  (299..299)
      |val (300..302)
      |  (303..303)
      |x5d (304..306)
      |  (307..307)
      |= (308..308)
      |  (309..309)
      |'\"' (310..313)
      |\n (314..314)
      |  (315..315)
      |  (316..316)
      |val (317..319)
      |  (320..320)
      |x6 (321..322)
      |  (323..323)
      |= (324..324)
      |  (325..325)
      |'a (326..327)
      |\n (328..328)
      |  (329..329)
      |  (330..330)
      |val (331..333)
      |  (334..334)
      |x7a (335..337)
      |  (338..338)
      |= (339..339)
      |  (340..340)
      |"" (341..342)
      |\n (343..343)
      |  (344..344)
      |  (345..345)
      |val (346..348)
      |  (349..349)
      |x7b (350..352)
      |  (353..353)
      |= (354..354)
      |  (355..355)
      |"\b" (356..359)
      |\n (360..360)
      |  (361..361)
      |  (362..362)
      |val (363..365)
      |  (366..366)
      |x7c (367..369)
      |  (370..370)
      |= (371..371)
      |  (372..372)
      |"c" (373..375)
      |\n (376..376)
      |  (377..377)
      |  (378..378)
      |val (379..381)
      |  (382..382)
      |x7d (383..385)
      |  (386..386)
      |= (387..387)
      |  (388..388)
      |"\"" (389..392)
      |\n (393..393)
      |  (394..394)
      |  (395..395)
      |val (396..398)
      |  (399..399)
      |x7e (400..402)
      |  (403..403)
      |= (404..404)
      |  (405..405)
      |QQQQQQ (406..411)
      |\n (412..412)
      |  (413..413)
      |  (414..414)
      |val (415..417)
      |  (418..418)
      |x7f (419..421)
      |  (422..422)
      |= (423..423)
      |  (424..424)
      |QQQf\nQQQ (425..433)
      |\n (434..434)
      |  (435..435)
      |  (436..436)
      |val (437..439)
      |  (440..440)
      |hello (441..445)
      |  (446..446)
      |= (447..447)
      |  (448..448)
      |42 (449..450)
      |\n (451..451)
      |  (452..452)
      |  (453..453)
      |val (454..456)
      |  (457..457)
      |`world` (458..464)
      |  (465..465)
      |= (466..466)
      |  (467..467)
      |42 (468..469)
      |\n (470..470)
      |} (471..471)
      |EOF (472..471)
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
    """.trim.stripMargin.replace("QQQ", "\"\"\"")).map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |class (0..4)
      |  (5..5)
      |C (6..6)
      |  (7..7)
      |{ (8..8)
      |\n (9..9)
      |  (10..10)
      |  (11..11)
      |q (12..12)
      |" (13..13)
      | (14..13)
      |" (14..14)
      |\n (15..15)
      |  (16..16)
      |  (17..17)
      |q (18..18)
      |" (19..19)
      | (20..19)
      |splice start (20..20)
      |b (21..21)
      |splice end (21..20)
      | + 2 (22..25)
      |" (26..26)
      |\n (27..27)
      |  (28..28)
      |  (29..29)
      |q (30..30)
      |" (31..31)
      | (32..31)
      |splice start (32..32)
      |{ (33..33)
      |b (34..34)
      |} (35..35)
      |splice end (35..34)
      | + 2 (36..39)
      |" (40..40)
      |\n (41..41)
      |  (42..42)
      |  (43..43)
      |q (44..44)
      |" (45..45)
      |class  (46..51)
      |splice start (52..52)
      |X (53..53)
      |splice end (53..52)
      | (54..53)
      |" (54..54)
      |\n (55..55)
      |  (56..56)
      |  (57..57)
      |q (58..58)
      |" (59..59)
      |class  (60..65)
      |splice start (66..66)
      |{ (67..67)
      |X (68..68)
      |} (69..69)
      |splice end (69..68)
      | (70..69)
      |" (70..70)
      |\n (71..71)
      |  (72..72)
      |  (73..73)
      |q (74..74)
      |QQQ (75..77)
      | (78..77)
      |QQQ (78..80)
      |\n (81..81)
      |  (82..82)
      |  (83..83)
      |q (84..84)
      |QQQ (85..87)
      | (88..87)
      |splice start (88..88)
      |d (89..89)
      |splice end (89..88)
      | + 2 (90..93)
      |QQQ (94..96)
      |\n (97..97)
      |  (98..98)
      |  (99..99)
      |q (100..100)
      |QQQ (101..103)
      | (104..103)
      |splice start (104..104)
      |{ (105..105)
      |d (106..106)
      |} (107..107)
      |splice end (107..106)
      | + 2 (108..111)
      |QQQ (112..114)
      |\n (115..115)
      |  (116..116)
      |  (117..117)
      |q (118..118)
      |QQQ (119..121)
      |class  (122..127)
      |splice start (128..128)
      |Y (129..129)
      |splice end (129..128)
      | (130..129)
      |QQQ (130..132)
      |\n (133..133)
      |  (134..134)
      |  (135..135)
      |q (136..136)
      |QQQ (137..139)
      |class  (140..145)
      |splice start (146..146)
      |{ (147..147)
      |Y (148..148)
      |} (149..149)
      |splice end (149..148)
      | (150..149)
      |QQQ (150..152)
      |\n (153..153)
      |} (154..154)
      |EOF (155..154)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("showRaw with comments - easy") {
    assert(tokenize("class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |class (0..4)
      |  (5..5)
      |C (6..6)
      |  (7..7)
      |  (8..8)
      |/*hello world*/ (9..23)
      |{ (24..24)
      |\t (25..25)
      |  (26..26)
      |val (27..29)
      |  (30..30)
      |x (31..31)
      |  (32..32)
      |= (33..33)
      |  (34..34)
      |2 (35..35)
      |} (36..36)
      |\n (37..37)
      |//bye-bye world (38..52)
      |\n (53..53)
      |EOF (54..53)
    """.trim.stripMargin)
  }

  test("showRaw with comments - tricky") {
    assert(tokenize("x ~/**/y").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |x (0..0)
      |  (1..1)
      |~ (2..2)
      |/**/ (3..6)
      |y (7..7)
      |EOF (8..7)
    """.trim.stripMargin)
  }

  test("interpolation start & end - episode 01") {
    assert(tokenize("q\"\"").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |q (0..0)
      |" (1..1)
      | (2..1)
      |" (2..2)
      |EOF (3..2)
    """.trim.stripMargin)
  }

  test("interpolation start & end - episode 02") {
    assert(tokenize("q\"\";").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |q (0..0)
      |" (1..1)
      | (2..1)
      |" (2..2)
      |; (3..3)
      |EOF (4..3)
    """.trim.stripMargin)
  }

  test("interpolation start & end - episode 03") {
    assert(tokenize("q\"a\"").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |q (0..0)
      |" (1..1)
      |a (2..2)
      |" (3..3)
      |EOF (4..3)
    """.trim.stripMargin)
  }

  test("interpolation start & end - episode 04") {
    assert(tokenize("q\"a\";").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |q (0..0)
      |" (1..1)
      |a (2..2)
      |" (3..3)
      |; (4..4)
      |EOF (5..4)
    """.trim.stripMargin)
  }

  test("interpolation start & end - episode 05") {
    assert(tokenize("q\"\"\"\"\"\"").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |q (0..0)
      |QQQ (1..3)
      | (4..3)
      |QQQ (4..6)
      |EOF (7..6)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("interpolation start & end - episode 06") {
    assert(tokenize("q\"\"\"\"\"\";").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |q (0..0)
      |QQQ (1..3)
      | (4..3)
      |QQQ (4..6)
      |; (7..7)
      |EOF (8..7)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("interpolation start & end - episode 07") {
    assert(tokenize("q\"\"\"a\"\"\"").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |q (0..0)
      |QQQ (1..3)
      |a (4..4)
      |QQQ (5..7)
      |EOF (8..7)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("interpolation start & end - episode 08") {
    assert(tokenize("q\"\"\"a\"\"\";").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |q (0..0)
      |QQQ (1..3)
      |a (4..4)
      |QQQ (5..7)
      |; (8..8)
      |EOF (9..8)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("$this") {
    assert(tokenize("q\"$this\"").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |q (0..0)
      |" (1..1)
      | (2..1)
      |splice start (2..2)
      |this (3..6)
      |splice end (6..5)
      | (7..6)
      |" (7..7)
      |EOF (8..7)
    """.trim.stripMargin)
  }

  test("monocle") {
    assert(tokenize("x => x").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |x (0..0)
      |  (1..1)
      |=> (2..3)
      |  (4..4)
      |x (5..5)
      |EOF (6..5)
    """.trim.stripMargin)
    assert(tokenize("x ⇒ x").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |x (0..0)
      |  (1..1)
      |⇒ (2..2)
      |  (3..3)
      |x (4..4)
      |EOF (5..4)
    """.trim.stripMargin)
    assert(tokenize("for (x <- xs) println(x)").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |for (0..2)
      |  (3..3)
      |( (4..4)
      |x (5..5)
      |  (6..6)
      |<- (7..8)
      |  (9..9)
      |xs (10..11)
      |) (12..12)
      |  (13..13)
      |println (14..20)
      |( (21..21)
      |x (22..22)
      |) (23..23)
      |EOF (24..23)
    """.trim.stripMargin)
    assert(tokenize("for (x ← xs) println(x)").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |for (0..2)
      |  (3..3)
      |( (4..4)
      |x (5..5)
      |  (6..6)
      |← (7..7)
      |  (8..8)
      |xs (9..10)
      |) (11..11)
      |  (12..12)
      |println (13..19)
      |( (20..20)
      |x (21..21)
      |) (22..22)
      |EOF (23..22)
    """.trim.stripMargin)
  }

  test("-2147483648") {
    assert(tokenize("-2147483648").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |- (0..0)
      |2147483648 (1..10)
      |EOF (11..10)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("simple xml literal - 1") {
    assert(tokenize("<foo>bar</foo>").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |xml start (0..-1)
      |<foo>bar</foo> (0..13)
      |xml end (13..12)
      |EOF (14..13)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("simple xml literal - 2") {
    // TODO: the whitespace shouldn't be included here - looks like a bug in scalac's MarkupParser
    assert(tokenize("<foo>bar</foo> ").map(_.show[Raw]).mkString("\n") === """
      |BOF (0..-1)
      |xml start (0..-1)
      |<foo>bar</foo>  (0..14)
      |xml end (14..13)
      |EOF (15..14)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }
}