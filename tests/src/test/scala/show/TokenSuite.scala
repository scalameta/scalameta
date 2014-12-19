import org.scalatest._
import scala.meta.syntactic.show._

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

  test("showCode with comments") {
    // TODO: uncomment me
    // assert(tokenize("class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n").map(_.show[Code]).mkString === "class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n")
  }

  test("showRaw without comments - easy") {
    assert(tokenize("class C  {\t val x = 2}\n\n").map(_.show[Raw]).mkString("\n") === """
      |class (0)
      |  (5)
      |C (6)
      |  (7)
      |  (8)
      |{ (9)
      |\t (10)
      |  (11)
      |val (12)
      |  (15)
      |x (16)
      |  (17)
      |= (18)
      |  (19)
      |2 (20)
      |} (21)
      |\n (22)
      |\n (23)
      |EOF (24)
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
      |class (0)
      |  (5)
      |C (6)
      |  (7)
      |{ (8)
      |\n (9)
      |  (10)
      |  (11)
      |val (12)
      |  (15)
      |x1a (16)
      |  (19)
      |= (20)
      |  (21)
      |2 (22)
      |\n (23)
      |  (24)
      |  (25)
      |val (26)
      |  (29)
      |x1b (30)
      |  (33)
      |= (34)
      |  (35)
      |0x002 (36)
      |\n (41)
      |  (42)
      |  (43)
      |val (44)
      |  (47)
      |x1c (48)
      |  (51)
      |= (52)
      |  (53)
      |0x002a (54)
      |\n (60)
      |  (61)
      |  (62)
      |val (63)
      |  (66)
      |x2a (67)
      |  (70)
      |= (71)
      |  (72)
      |2l (73)
      |\n (75)
      |  (76)
      |  (77)
      |val (78)
      |  (81)
      |x2b (82)
      |  (85)
      |= (86)
      |  (87)
      |2L (88)
      |\n (90)
      |  (91)
      |  (92)
      |val (93)
      |  (96)
      |x2c (97)
      |  (100)
      |= (101)
      |  (102)
      |0x002l (103)
      |\n (109)
      |  (110)
      |  (111)
      |val (112)
      |  (115)
      |x2d (116)
      |  (119)
      |= (120)
      |  (121)
      |0x002L (122)
      |\n (128)
      |  (129)
      |  (130)
      |val (131)
      |  (134)
      |x2e (135)
      |  (138)
      |= (139)
      |  (140)
      |0x002al (141)
      |\n (148)
      |  (149)
      |  (150)
      |val (151)
      |  (154)
      |x2f (155)
      |  (158)
      |= (159)
      |  (160)
      |0x002aL (161)
      |\n (168)
      |  (169)
      |  (170)
      |val (171)
      |  (174)
      |x3a (175)
      |  (178)
      |= (179)
      |  (180)
      |2f (181)
      |\n (183)
      |  (184)
      |  (185)
      |val (186)
      |  (189)
      |x3b (190)
      |  (193)
      |= (194)
      |  (195)
      |2.0F (196)
      |\n (200)
      |  (201)
      |  (202)
      |val (203)
      |  (206)
      |x4a (207)
      |  (210)
      |= (211)
      |  (212)
      |2d (213)
      |\n (215)
      |  (216)
      |  (217)
      |val (218)
      |  (221)
      |x4b (222)
      |  (225)
      |= (226)
      |  (227)
      |2.0D (228)
      |\n (232)
      |  (233)
      |  (234)
      |val (235)
      |  (238)
      |x4c (239)
      |  (242)
      |= (243)
      |  (244)
      |2.0 (245)
      |\n (248)
      |  (249)
      |  (250)
      |val (251)
      |  (254)
      |x5a (255)
      |  (258)
      |= (259)
      |  (260)
      |'a' (261)
      |\n (264)
      |  (265)
      |  (266)
      |val (267)
      |  (270)
      |x5b (271)
      |  (274)
      |= (275)
      |  (276)
      |'\b' (277)
      |\n (281)
      |  (282)
      |  (283)
      |val (284)
      |  (287)
      |x5c (288)
      |  (291)
      |= (292)
      |  (293)
      |'"' (294)
      |\n (297)
      |  (298)
      |  (299)
      |val (300)
      |  (303)
      |x5d (304)
      |  (307)
      |= (308)
      |  (309)
      |'\"' (310)
      |\n (314)
      |  (315)
      |  (316)
      |val (317)
      |  (320)
      |x6 (321)
      |  (323)
      |= (324)
      |  (325)
      |'a (326)
      |\n (328)
      |  (329)
      |  (330)
      |val (331)
      |  (334)
      |x7a (335)
      |  (338)
      |= (339)
      |  (340)
      |"" (341)
      |\n (343)
      |  (344)
      |  (345)
      |val (346)
      |  (349)
      |x7b (350)
      |  (353)
      |= (354)
      |  (355)
      |"\b" (356)
      |\n (360)
      |  (361)
      |  (362)
      |val (363)
      |  (366)
      |x7c (367)
      |  (370)
      |= (371)
      |  (372)
      |"c" (373)
      |\n (376)
      |  (377)
      |  (378)
      |val (379)
      |  (382)
      |x7d (383)
      |  (386)
      |= (387)
      |  (388)
      |"\"" (389)
      |\n (393)
      |  (394)
      |  (395)
      |val (396)
      |  (399)
      |x7e (400)
      |  (403)
      |= (404)
      |  (405)
      |QQQQQQ (406)
      |\n (412)
      |  (413)
      |  (414)
      |val (415)
      |  (418)
      |x7f (419)
      |  (422)
      |= (423)
      |  (424)
      |QQQf\nQQQ (425)
      |\n (434)
      |  (435)
      |  (436)
      |val (437)
      |  (440)
      |hello (441)
      |  (446)
      |= (447)
      |  (448)
      |42 (449)
      |\n (451)
      |  (452)
      |  (453)
      |val (454)
      |  (457)
      |`world` (458)
      |  (465)
      |= (466)
      |  (467)
      |42 (468)
      |\n (470)
      |} (471)
      |EOF (472)
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
      |class (0)
      |  (5)
      |C (6)
      |  (7)
      |{ (8)
      |\n (9)
      |  (10)
      |  (11)
      |q (12)
      |" (13)
      | (13)
      |" (16)
      |\n (15)
      |  (16)
      |  (17)
      |q (18)
      |" (19)
      | (20)
      |$ (20)
      |b (21)
      | (21)
      | + 2 (22)
      |" (28)
      |\n (27)
      |  (28)
      |  (29)
      |q (30)
      |" (31)
      | (32)
      |${ (32)
      |b (34)
      |} (35)
      | + 2 (36)
      |" (42)
      |\n (41)
      |  (42)
      |  (43)
      |q (44)
      |" (45)
      |class  (46)
      |$ (52)
      |X (53)
      | (53)
      | (54)
      |" (56)
      |\n (55)
      |  (56)
      |  (57)
      |q (58)
      |" (59)
      |class  (60)
      |${ (66)
      |X (68)
      |} (69)
      | (70)
      |" (72)
      |\n (71)
      |  (72)
      |  (73)
      |q (74)
      |QQQ (75)
      | (78)
      |QQQ (82)
      |\n (81)
      |  (82)
      |  (83)
      |q (84)
      |QQQ (85)
      | (88)
      |$ (88)
      |d (89)
      | (89)
      | + 2 (90)
      |QQQ (98)
      |\n (97)
      |  (98)
      |  (99)
      |q (100)
      |QQQ (101)
      | (104)
      |${ (104)
      |d (106)
      |} (107)
      | + 2 (108)
      |QQQ (116)
      |\n (115)
      |  (116)
      |  (117)
      |q (118)
      |QQQ (119)
      |class  (122)
      |$ (128)
      |Y (129)
      | (129)
      | (130)
      |QQQ (134)
      |\n (133)
      |  (134)
      |  (135)
      |q (136)
      |QQQ (137)
      |class  (140)
      |${ (146)
      |Y (148)
      |} (149)
      | (150)
      |QQQ (154)
      |\n (153)
      |} (154)
      |EOF (155)
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }

  test("showRaw with comments") {
    // TODO: uncomment me
    // assert(tokenize("class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n").map(_.show[Raw]).mkString("\n") === """
    //   |class (0)
    //   |  (5)
    //   |C (6)
    //   |  (7)
    //   |  (8)
    //   |/*hello world*/ (9)
    //   |{ (24)
    //   |\t (25)
    //   |  (26)
    //   |val (27)
    //   |  (30)
    //   |x (31)
    //   |  (32)
    //   |= (33)
    //   |  (34)
    //   |2 (35)
    //   |} (36)
    //   |\n (37)
    //   |//bye-bye world (38)
    //   |\n (53)
    //   |EOF (54)
    // """.trim.stripMargin)
  }
}