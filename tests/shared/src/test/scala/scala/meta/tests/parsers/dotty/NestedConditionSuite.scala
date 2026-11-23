package scala.meta.tests.parsers.dotty

import scala.meta._

class NestedConditionSuite extends BaseDottySuite {

  test("nested condition: an `if` with parentheses and `else` inside an `if` inside an indented condition, middle and outer `then` and `else` on the next lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if
         |        if (a) b else c
         |      then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |Indentation.Indent [35..35)
         |KwIf [44..46)
         |LeftParen [47..48)
         |Ident(a) [48..49)
         |RightParen [49..50)
         |Ident(b) [51..52)
         |KwElse [53..57)
         |Ident(c) [58..59)
         |Indentation.Outdent [59..59)
         |KwThen [66..70)
         |Ident(d) [71..72)
         |KwElse [73..77)
         |Ident(e) [78..79)
         |Indentation.Outdent [79..79)
         |KwThen [84..88)
         |Ident(g) [89..90)
         |KwElse [91..95)
         |Ident(h) [96..97)
         |LF [97..98)
         |Ident(k) [102..103)
         |Indentation.Outdent [103..103)
         |Indentation.Outdent [103..103)
         |EOF [104..104)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (if (a) b else c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tif(tname("a"), tname("b"), tname("c")), tname("d"), tname("e")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a `while` with parentheses, `;` and an identifier inside an `if` inside an indented condition, `then` and `else` on the same line, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if
         |        while (a) b; c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |Indentation.Indent [35..35)
         |KwWhile [44..49)
         |LeftParen [50..51)
         |Ident(a) [51..52)
         |RightParen [52..53)
         |Ident(b) [54..55)
         |Semicolon [55..56)
         |Ident(c) [57..58)
         |Indentation.Outdent [58..58)
         |KwThen [59..63)
         |Ident(d) [64..65)
         |KwElse [66..70)
         |Ident(e) [71..72)
         |Indentation.Outdent [72..72)
         |KwThen [77..81)
         |Ident(g) [82..83)
         |KwElse [84..88)
         |Ident(h) [89..90)
         |LF [90..91)
         |Ident(k) [95..96)
         |Indentation.Outdent [96..96)
         |Indentation.Outdent [96..96)
         |EOF [97..97)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if ({
         |      while (a) b
         |      c
         |    }) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: an `if` with parentheses, `;` and an identifier inside an indented `if` condition inside an indented `while` condition, `then` and `else` on the same line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if
         |        if (a) b; c then d else e
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |Indentation.Indent [38..38)
         |KwIf [47..49)
         |LeftParen [50..51)
         |Ident(a) [51..52)
         |RightParen [52..53)
         |Ident(b) [54..55)
         |Semicolon [55..56)
         |Ident(c) [57..58)
         |Indentation.Outdent [58..58)
         |KwThen [59..63)
         |Ident(d) [64..65)
         |KwElse [66..70)
         |Ident(e) [71..72)
         |Indentation.Outdent [72..72)
         |KwDo [77..79)
         |Ident(g) [80..81)
         |LF [81..82)
         |Ident(k) [86..87)
         |Indentation.Outdent [87..87)
         |Indentation.Outdent [87..87)
         |EOF [88..88)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if ({
         |      if (a) b
         |      c
         |    }) d else e) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tif(blk(tif(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses inside an indented `while` condition, `do` on the next line, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      for x <- xs
         |      do d
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwFor [36..39)
         |Ident(x) [40..41)
         |LeftArrow [42..44)
         |Ident(xs) [45..47)
         |LF [47..48)
         |KwDo [54..56)
         |Ident(d) [57..58)
         |Indentation.Outdent [58..58)
         |KwDo [63..65)
         |Ident(k) [66..67)
         |LF [67..68)
         |Ident(g) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with parentheses and `yield`, `;` and an identifier inside an indented condition, `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for (x <- xs) yield d; c then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |LeftParen [37..38)
         |Ident(x) [38..39)
         |LeftArrow [40..42)
         |Ident(xs) [43..45)
         |RightParen [45..46)
         |KwYield [47..52)
         |Ident(d) [53..54)
         |Semicolon [54..55)
         |Ident(c) [56..57)
         |Indentation.Outdent [57..57)
         |KwThen [58..62)
         |Ident(g) [63..64)
         |KwElse [65..69)
         |Ident(h) [70..71)
         |LF [71..72)
         |Ident(k) [76..77)
         |Indentation.Outdent [77..77)
         |Indentation.Outdent [77..77)
         |EOF [78..78)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      for (x <- xs) yield d
         |      c
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(
              Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
              tname("c"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with parentheses and a body, `;` and an identifier inside an indented condition, `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for (x <- xs) d; c then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |LeftParen [37..38)
         |Ident(x) [38..39)
         |LeftArrow [40..42)
         |Ident(xs) [43..45)
         |RightParen [45..46)
         |Ident(d) [47..48)
         |Semicolon [48..49)
         |Ident(c) [50..51)
         |Indentation.Outdent [51..51)
         |KwThen [52..56)
         |Ident(g) [57..58)
         |KwElse [59..63)
         |Ident(h) [64..65)
         |LF [65..66)
         |Ident(k) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      for (x <- xs) d
         |      c
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(
              Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
              tname("c"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with parentheses and `do`, `;` and an identifier inside an indented `while` condition, `do` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      for (x <- xs) do d; c do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwFor [36..39)
         |LeftParen [40..41)
         |Ident(x) [41..42)
         |LeftArrow [43..45)
         |Ident(xs) [46..48)
         |RightParen [48..49)
         |KwDo [50..52)
         |Ident(d) [53..54)
         |Semicolon [54..55)
         |Ident(c) [56..57)
         |Indentation.Outdent [57..57)
         |KwDo [58..60)
         |Ident(k) [61..62)
         |LF [62..63)
         |Ident(g) [67..68)
         |Indentation.Outdent [68..68)
         |Indentation.Outdent [68..68)
         |EOF [69..69)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      for (x <- xs) d
         |      c
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
              tname("c"),
            ),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses with `yield` inside an indented condition, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for x <- xs yield d
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |Ident(x) [37..38)
         |LeftArrow [39..41)
         |Ident(xs) [42..44)
         |KwYield [45..50)
         |Ident(d) [51..52)
         |Indentation.Outdent [52..52)
         |KwThen [57..61)
         |Ident(g) [62..63)
         |KwElse [64..68)
         |Ident(h) [69..70)
         |LF [70..71)
         |Ident(k) [75..76)
         |Indentation.Outdent [76..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (for (x <- xs) yield d) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with indented enumerators inside an indented condition, `yield` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for
         |        x <- xs
         |      yield d
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |Indentation.Indent [36..36)
         |Ident(x) [45..46)
         |LeftArrow [47..49)
         |Ident(xs) [50..52)
         |Indentation.Outdent [52..52)
         |KwYield [59..64)
         |Ident(d) [65..66)
         |Indentation.Outdent [66..66)
         |KwThen [71..75)
         |Ident(g) [76..77)
         |KwElse [78..82)
         |Ident(h) [83..84)
         |LF [84..85)
         |Ident(k) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (for (x <- xs) yield d) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with parentheses and `yield` inside an indented condition, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for (x <- xs) yield d
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |LeftParen [37..38)
         |Ident(x) [38..39)
         |LeftArrow [40..42)
         |Ident(xs) [43..45)
         |RightParen [45..46)
         |KwYield [47..52)
         |Ident(d) [53..54)
         |Indentation.Outdent [54..54)
         |KwThen [59..63)
         |Ident(g) [64..65)
         |KwElse [66..70)
         |Ident(h) [71..72)
         |LF [72..73)
         |Ident(k) [77..78)
         |Indentation.Outdent [78..78)
         |Indentation.Outdent [78..78)
         |EOF [79..79)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (for (x <- xs) yield d) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses with `do` inside an indented `while` condition, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      for x <- xs do d
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwFor [36..39)
         |Ident(x) [40..41)
         |LeftArrow [42..44)
         |Ident(xs) [45..47)
         |KwDo [48..50)
         |Ident(d) [51..52)
         |Indentation.Outdent [52..52)
         |KwDo [57..59)
         |Ident(k) [60..61)
         |LF [61..62)
         |Ident(g) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses inside an indented condition, `yield` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for x <- xs
         |      yield d
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |Ident(x) [37..38)
         |LeftArrow [39..41)
         |Ident(xs) [42..44)
         |KwYield [51..56)
         |Ident(d) [57..58)
         |Indentation.Outdent [58..58)
         |KwThen [63..67)
         |Ident(g) [68..69)
         |KwElse [70..74)
         |Ident(h) [75..76)
         |LF [76..77)
         |Ident(k) [81..82)
         |Indentation.Outdent [82..82)
         |Indentation.Outdent [82..82)
         |EOF [83..83)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (for (x <- xs) yield d) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with indented enumerators inside an indented `while` condition, `do` on the next line, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      for
         |        x <- xs
         |      do d
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwFor [36..39)
         |Indentation.Indent [39..39)
         |Ident(x) [48..49)
         |LeftArrow [50..52)
         |Ident(xs) [53..55)
         |Indentation.Outdent [55..55)
         |LF [55..56)
         |KwDo [62..64)
         |Ident(d) [65..66)
         |Indentation.Outdent [66..66)
         |KwDo [71..73)
         |Ident(k) [74..75)
         |LF [75..76)
         |Ident(g) [80..81)
         |Indentation.Outdent [81..81)
         |Indentation.Outdent [81..81)
         |EOF [82..82)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses, `;` and an identifier inside an indented `while` condition, `do` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) b; c do d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(b) [46..47)
         |Semicolon [47..48)
         |Ident(c) [49..50)
         |Indentation.Outdent [50..50)
         |KwDo [51..53)
         |Ident(d) [54..55)
         |LF [55..56)
         |Ident(g) [60..61)
         |Indentation.Outdent [61..61)
         |Indentation.Outdent [61..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a) b
         |      c
         |    }) d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d")), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses in a value of indented enumerators, leading `||` on the next line with `then` and `else`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      y = if (a) && b
         |        || c then d else e
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |Ident(y) [48..49)
         |Equals [50..51)
         |KwIf [52..54)
         |LeftParen [55..56)
         |Ident(a) [56..57)
         |RightParen [57..58)
         |Ident(&&) [59..61)
         |Ident(b) [62..63)
         |InfixLF [63..64)
         |Ident(||) [72..74)
         |Ident(c) [75..76)
         |KwThen [77..81)
         |Ident(d) [82..83)
         |KwElse [84..88)
         |Ident(e) [89..90)
         |Indentation.Outdent [90..90)
         |LF [90..91)
         |KwDo [95..97)
         |Ident(g) [98..99)
         |LF [99..100)
         |Ident(k) [104..105)
         |Indentation.Outdent [105..105)
         |Indentation.Outdent [105..105)
         |EOF [106..106)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; y = if (a && b || c) d else e) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.For(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(
                patvar("y"),
                tif(
                  tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
                  tname("d"),
                  tname("e"),
                ),
              ),
            ),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and `else` in a value of indented enumerators, `yield` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      y = if (a) b else c
         |    yield y
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |Ident(y) [48..49)
         |Equals [50..51)
         |KwIf [52..54)
         |LeftParen [55..56)
         |Ident(a) [56..57)
         |RightParen [57..58)
         |Ident(b) [59..60)
         |KwElse [61..65)
         |Ident(c) [66..67)
         |Indentation.Outdent [67..67)
         |KwYield [72..77)
         |Ident(y) [78..79)
         |LF [79..80)
         |Ident(k) [84..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; y = if (a) b else c) yield y
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.ForYield(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(patvar("y"), tif(tname("a"), tname("b"), tname("c"))),
            ),
            tname("y"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with parentheses and `yield` in a value of indented enumerators, `yield` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      y = for (z <- zs) yield z
         |    yield y
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |Ident(y) [48..49)
         |Equals [50..51)
         |KwFor [52..55)
         |LeftParen [56..57)
         |Ident(z) [57..58)
         |LeftArrow [59..61)
         |Ident(zs) [62..64)
         |RightParen [64..65)
         |KwYield [66..71)
         |Ident(z) [72..73)
         |Indentation.Outdent [73..73)
         |KwYield [78..83)
         |Ident(y) [84..85)
         |LF [85..86)
         |Ident(k) [90..91)
         |Indentation.Outdent [91..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; y = for (z <- zs) yield z) yield y
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.ForYield(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(
                patvar("y"),
                Term.ForYield(List(Enumerator.Generator(patvar("z"), tname("zs"))), tname("z")),
              ),
            ),
            tname("y"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses with `do` in a value of indented enumerators, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      y = for z <- zs do z
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |Ident(y) [48..49)
         |Equals [50..51)
         |KwFor [52..55)
         |Ident(z) [56..57)
         |LeftArrow [58..60)
         |Ident(zs) [61..63)
         |KwDo [64..66)
         |Ident(z) [67..68)
         |Indentation.Outdent [68..68)
         |LF [68..69)
         |KwDo [73..75)
         |Ident(g) [76..77)
         |LF [77..78)
         |Ident(k) [82..83)
         |Indentation.Outdent [83..83)
         |Indentation.Outdent [83..83)
         |EOF [84..84)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; y = for (z <- zs) z) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.For(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(
                patvar("y"),
                Term.For(List(Enumerator.Generator(patvar("z"), tname("zs"))), tname("z")),
              ),
            ),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses in a guard of indented enumerators, leading `||` on the next line, `yield` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      if (a) && b
         |        || c
         |    yield x
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |KwIf [48..50)
         |LeftParen [51..52)
         |Ident(a) [52..53)
         |RightParen [53..54)
         |Ident(&&) [55..57)
         |Ident(b) [58..59)
         |InfixLF [59..60)
         |Ident(||) [68..70)
         |Ident(c) [71..72)
         |Indentation.Outdent [72..72)
         |KwYield [77..82)
         |Ident(x) [83..84)
         |LF [84..85)
         |Ident(k) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; if a && b || c) yield x
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.ForYield(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Guard(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c"))),
            ),
            tname("x"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses in braces inside an indented condition, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      { if (a) && b
         |        || c then d else e }
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |LeftBrace [33..34)
         |KwIf [35..37)
         |LeftParen [38..39)
         |Ident(a) [39..40)
         |RightParen [40..41)
         |Ident(&&) [42..44)
         |Ident(b) [45..46)
         |InfixLF [46..47)
         |Ident(||) [55..57)
         |Ident(c) [58..59)
         |KwThen [60..64)
         |Ident(d) [65..66)
         |KwElse [67..71)
         |Ident(e) [72..73)
         |RightBrace [74..75)
         |Indentation.Outdent [75..75)
         |KwThen [80..84)
         |Ident(g) [85..86)
         |KwElse [87..91)
         |Ident(h) [92..93)
         |LF [93..94)
         |Ident(k) [98..99)
         |Indentation.Outdent [99..99)
         |Indentation.Outdent [99..99)
         |EOF [100..100)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a && b || c) d else e
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            )),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses in parentheses inside an indented condition, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      (if (a) && b
         |        || c then d else e)
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |LeftParen [33..34)
         |KwIf [34..36)
         |LeftParen [37..38)
         |Ident(a) [38..39)
         |RightParen [39..40)
         |Ident(&&) [41..43)
         |Ident(b) [44..45)
         |InfixLF [45..46)
         |Ident(||) [54..56)
         |Ident(c) [57..58)
         |KwThen [59..63)
         |Ident(d) [64..65)
         |KwElse [66..70)
         |Ident(e) [71..72)
         |RightParen [72..73)
         |Indentation.Outdent [73..73)
         |KwThen [78..82)
         |Ident(g) [83..84)
         |KwElse [85..89)
         |Ident(h) [90..91)
         |LF [91..92)
         |Ident(k) [96..97)
         |Indentation.Outdent [97..97)
         |Indentation.Outdent [97..97)
         |EOF [98..98)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses in braces inside an indented `while` condition, leading `||` on the next line with `do`, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      { while (a) && b
         |        || c do d }
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |LeftBrace [36..37)
         |KwWhile [38..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(&&) [48..50)
         |Ident(b) [51..52)
         |InfixLF [52..53)
         |Ident(||) [61..63)
         |Ident(c) [64..65)
         |KwDo [66..68)
         |Ident(d) [69..70)
         |RightBrace [71..72)
         |Indentation.Outdent [72..72)
         |KwDo [77..79)
         |Ident(k) [80..81)
         |LF [81..82)
         |Ident(g) [86..87)
         |Indentation.Outdent [87..87)
         |Indentation.Outdent [87..87)
         |EOF [88..88)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a && b || c) d
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d"))),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses with `do` in parentheses inside an indented `while` condition, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      (for x <- xs do d)
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |LeftParen [36..37)
         |KwFor [37..40)
         |Ident(x) [41..42)
         |LeftArrow [43..45)
         |Ident(xs) [46..48)
         |KwDo [49..51)
         |Ident(d) [52..53)
         |RightParen [53..54)
         |Indentation.Outdent [54..54)
         |KwDo [59..61)
         |Ident(k) [62..63)
         |LF [63..64)
         |Ident(g) [68..69)
         |Indentation.Outdent [69..69)
         |Indentation.Outdent [69..69)
         |EOF [70..70)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses, `;` and an identifier in braces on separate lines inside an indented condition, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      {
         |        if (a) b; c
         |      }
         |    then d else e
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |LeftBrace [33..34)
         |KwIf [43..45)
         |LeftParen [46..47)
         |Ident(a) [47..48)
         |RightParen [48..49)
         |Ident(b) [50..51)
         |Semicolon [51..52)
         |Ident(c) [53..54)
         |LF [54..55)
         |RightBrace [61..62)
         |Indentation.Outdent [62..62)
         |KwThen [67..71)
         |Ident(d) [72..73)
         |KwElse [74..78)
         |Ident(e) [79..80)
         |LF [80..81)
         |Ident(k) [85..86)
         |Indentation.Outdent [86..86)
         |Indentation.Outdent [86..86)
         |EOF [87..87)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      c
         |    }) d else e
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(blk(tif(tname("a"), tname("b")), tname("c")), tname("d"), tname("e")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with a colon argument after the parentheses in parentheses inside an indented condition, a deeper `if` on the next line, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      (if (a) f:
         |          if b then c else d
         |      )
         |    then y else z
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |LeftParen [33..34)
         |KwIf [34..36)
         |LeftParen [37..38)
         |Ident(a) [38..39)
         |RightParen [39..40)
         |Ident(f) [41..42)
         |Colon [42..43)
         |Indentation.Indent [43..43)
         |KwIf [54..56)
         |Ident(b) [57..58)
         |KwThen [59..63)
         |Ident(c) [64..65)
         |KwElse [66..70)
         |Ident(d) [71..72)
         |Indentation.Outdent [72..72)
         |RightParen [79..80)
         |Indentation.Outdent [80..80)
         |KwThen [85..89)
         |Ident(y) [90..91)
         |KwElse [92..96)
         |Ident(z) [97..98)
         |LF [98..99)
         |Ident(g) [103..104)
         |Indentation.Outdent [104..104)
         |Indentation.Outdent [104..104)
         |EOF [105..105)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) f {
         |      if (b) c else d
         |    }) y else z
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tname("a"), tapply(tname("f"), blk(tif(tname("b"), tname("c"), tname("d"))))),
            tname("y"),
            tname("z"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |InfixLF [47..48)
         |Ident(&&) [54..56)
         |Ident(w) [57..58)
         |Indentation.Outdent [58..58)
         |KwThen [63..67)
         |Ident(c) [68..69)
         |KwElse [70..74)
         |Ident(d) [75..76)
         |LF [76..77)
         |KwDef [79..82)
         |Ident(g) [83..84)
         |Equals [85..86)
         |Constant.Int(1) [87..88)
         |Indentation.Outdent [88..88)
         |EOF [89..89)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z && w
         |  }) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses, `else`, `;` and an identifier inside an indented condition, `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) b else c; d then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(b) [40..41)
         |KwElse [42..46)
         |Ident(c) [47..48)
         |Semicolon [48..49)
         |Ident(d) [50..51)
         |Indentation.Outdent [51..51)
         |KwThen [52..56)
         |Ident(g) [57..58)
         |KwElse [59..63)
         |Ident(h) [64..65)
         |LF [65..66)
         |Ident(k) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b else c
         |      d
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(blk(tif(tname("a"), tname("b"), tname("c")), tname("d")), tname("g"), tname("h")),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses with `do`, `;` and an identifier inside an indented condition, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      for x <- xs do d; e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwFor [33..36)
         |Ident(x) [37..38)
         |LeftArrow [39..41)
         |Ident(xs) [42..44)
         |KwDo [45..47)
         |Ident(d) [48..49)
         |Semicolon [49..50)
         |Ident(e) [51..52)
         |Indentation.Outdent [52..52)
         |KwThen [57..61)
         |Ident(g) [62..63)
         |KwElse [64..68)
         |Ident(h) [69..70)
         |LF [70..71)
         |Ident(k) [75..76)
         |Indentation.Outdent [76..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      for (x <- xs) d
         |      e
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(
              Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `then` and `else`, `;` and an identifier inside an indented condition, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if a then b else c; d
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |Ident(a) [36..37)
         |KwThen [38..42)
         |Ident(b) [43..44)
         |KwElse [45..49)
         |Ident(c) [50..51)
         |Semicolon [51..52)
         |Ident(d) [53..54)
         |Indentation.Outdent [54..54)
         |KwThen [59..63)
         |Ident(g) [64..65)
         |KwElse [66..70)
         |Ident(h) [71..72)
         |LF [72..73)
         |Ident(k) [77..78)
         |Indentation.Outdent [78..78)
         |Indentation.Outdent [78..78)
         |EOF [79..79)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b else c
         |      d
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(blk(tif(tname("a"), tname("b"), tname("c")), tname("d")), tname("g"), tname("h")),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses inside an indented `while` condition, `do` on the next line, a blank line, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      for x <- xs
         |      do d
         |
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwFor [36..39)
         |Ident(x) [40..41)
         |LeftArrow [42..44)
         |Ident(xs) [45..47)
         |LF [47..48)
         |KwDo [54..56)
         |Ident(d) [57..58)
         |Indentation.Outdent [59..59)
         |KwDo [64..66)
         |Ident(k) [67..68)
         |LF [68..69)
         |Ident(g) [73..74)
         |Indentation.Outdent [74..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a select after the parentheses, `;` and an identifier inside an indented `while` condition after a comment line, `do` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      // x
         |      if (a) b.g; c do d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [40..40)
         |KwIf [47..49)
         |LeftParen [50..51)
         |Ident(a) [51..52)
         |RightParen [52..53)
         |Ident(b) [54..55)
         |Dot [55..56)
         |Ident(g) [56..57)
         |Semicolon [57..58)
         |Ident(c) [59..60)
         |Indentation.Outdent [60..60)
         |KwDo [61..63)
         |Ident(d) [64..65)
         |LF [65..66)
         |Ident(g) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      // x
         |      if (a) b.g
         |      c
         |    }) d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              tif(tname("a"), tselect(tname("b"), "g")).toBuilder.begComment(Seq("// x")).result(),
              tname("c"),
            ),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses, `;` and an identifier inside an indented condition, a comment between `;` and the identifier, `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) b; /* x */ c then d else e
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(b) [40..41)
         |Semicolon [41..42)
         |Ident(c) [51..52)
         |Indentation.Outdent [52..52)
         |KwThen [53..57)
         |Ident(d) [58..59)
         |KwElse [60..64)
         |Ident(e) [65..66)
         |LF [66..67)
         |Ident(k) [71..72)
         |Indentation.Outdent [72..72)
         |Indentation.Outdent [72..72)
         |EOF [73..73)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      /* x */ c
         |    }) d else e
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(tif(tname("a"), tname("b")), tnameComments("c")("/* x */")()),
            tname("d"),
            tname("e"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses on the line of the outer `if`, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) && b
         |      || c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(&&) [34..36)
         |Ident(b) [37..38)
         |InfixLF [38..39)
         |Ident(||) [45..47)
         |Ident(c) [48..49)
         |KwThen [50..54)
         |Ident(d) [55..56)
         |KwElse [57..61)
         |Ident(e) [62..63)
         |KwThen [68..72)
         |Ident(g) [73..74)
         |KwElse [75..79)
         |Ident(h) [80..81)
         |LF [81..82)
         |Ident(k) [86..87)
         |Indentation.Outdent [87..87)
         |Indentation.Outdent [87..87)
         |EOF [88..88)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and `else` on the line of the outer `if`, outer `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) b else c then d else e
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(b) [34..35)
         |KwElse [36..40)
         |Ident(c) [41..42)
         |KwThen [43..47)
         |Ident(d) [48..49)
         |KwElse [50..54)
         |Ident(e) [55..56)
         |LF [56..57)
         |Ident(k) [61..62)
         |Indentation.Outdent [62..62)
         |Indentation.Outdent [62..62)
         |EOF [63..63)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) b else c) d else e
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tif(tname("a"), tname("b"), tname("c")), tname("d"), tname("e")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` without parentheses on the line of the outer `while`, `do` on the next line, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while for x <- xs
         |    do d
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwFor [30..33)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |KwDo [46..48)
         |Ident(d) [49..50)
         |KwDo [55..57)
         |Ident(k) [58..59)
         |LF [59..60)
         |Ident(g) [64..65)
         |Indentation.Outdent [65..65)
         |Indentation.Outdent [65..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (for (x <- xs) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.For(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and `else` on the line of the outer `if`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) b else c
         |    then d else e
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(b) [34..35)
         |KwElse [36..40)
         |Ident(c) [41..42)
         |KwThen [47..51)
         |Ident(d) [52..53)
         |KwElse [54..58)
         |Ident(e) [59..60)
         |LF [60..61)
         |Ident(k) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) b else c) d else e
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tif(tname("a"), tname("b"), tname("c")), tname("d"), tname("e")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` without parentheses with `then` at the line end inside an indented `while` condition, the body and outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if a then
         |        b do c
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |Ident(a) [39..40)
         |KwThen [41..45)
         |Indentation.Indent [45..45)
         |Ident(b) [54..55)
         |Indentation.Outdent [55..55)
         |Indentation.Outdent [55..55)
         |KwDo [56..58)
         |Ident(c) [59..60)
         |LF [60..61)
         |Ident(k) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if (a) b) c
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(tif(tname("a"), tname("b")), tname("c")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` without parentheses with `then` at the line end inside an indented `while` condition, the body on the next line, outer `do` on the line after it at the column of the body, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if a then
         |        b
         |        do c
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |Ident(a) [39..40)
         |KwThen [41..45)
         |Indentation.Indent [45..45)
         |Ident(b) [54..55)
         |LF [55..56)
         |Indentation.Outdent [55..55)
         |Indentation.Outdent [55..55)
         |KwDo [64..66)
         |Ident(c) [67..68)
         |LF [68..69)
         |Ident(k) [73..74)
         |Indentation.Outdent [74..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if (a) b) c
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(tif(tname("a"), tname("b")), tname("c")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` without parentheses with `then` at the line end inside an indented `while` condition, the body, a comment and outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if a then
         |        b /* x */ do c
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |Ident(a) [39..40)
         |KwThen [41..45)
         |Indentation.Indent [45..45)
         |Ident(b) [54..55)
         |Indentation.Outdent [63..63)
         |Indentation.Outdent [63..63)
         |KwDo [64..66)
         |Ident(c) [67..68)
         |LF [68..69)
         |Ident(k) [73..74)
         |Indentation.Outdent [74..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if (a) b /* x */ ) c
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tif(tname("a"), tname("b")).toBuilder.endComment(Seq("/* x */")).result(),
            tname("c"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses and `then` at the line end inside an indented condition, the body with `else` and outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b then
         |        c else d then e else g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |KwThen [45..49)
         |Indentation.Indent [49..49)
         |Ident(c) [58..59)
         |Indentation.Outdent [59..59)
         |KwElse [60..64)
         |Ident(d) [65..66)
         |Indentation.Outdent [66..66)
         |KwThen [67..71)
         |Ident(e) [72..73)
         |KwElse [74..78)
         |Ident(g) [79..80)
         |LF [80..81)
         |Ident(k) [85..86)
         |Indentation.Outdent [86..86)
         |Indentation.Outdent [86..86)
         |EOF [87..87)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b) c else d) e else g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")),
            tname("e"),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` without parentheses with `do` at the line end inside an indented condition, the body and outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      while a do
         |        b then c else d
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwWhile [33..38)
         |Ident(a) [39..40)
         |KwDo [41..43)
         |Indentation.Indent [43..43)
         |Ident(b) [52..53)
         |Indentation.Outdent [53..53)
         |Indentation.Outdent [53..53)
         |KwThen [54..58)
         |Ident(c) [59..60)
         |KwElse [61..65)
         |Ident(d) [66..67)
         |LF [67..68)
         |Ident(k) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (while (a) b) c else d
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(Term.While(tname("a"), tname("b")), tname("c"), tname("d")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and an interpolated string with an `if` in its splice inside an indented condition, `then` and `else` on the same line, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) f(s"${if b then c else d}") then g else h
         |    then k else m
         |    n
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(f) [40..41)
         |LeftParen [41..42)
         |Interpolation.Id(s) [42..43)
         |Interpolation.Start(") [43..44)
         |Interpolation.Part() [44..44)
         |Interpolation.SpliceStart [44..45)
         |LeftBrace [45..46)
         |KwIf [46..48)
         |Ident(b) [49..50)
         |KwThen [51..55)
         |Ident(c) [56..57)
         |KwElse [58..62)
         |Ident(d) [63..64)
         |RightBrace [64..65)
         |Interpolation.SpliceEnd [65..65)
         |Interpolation.Part() [65..65)
         |Interpolation.End(") [65..66)
         |RightParen [66..67)
         |KwThen [68..72)
         |Ident(g) [73..74)
         |KwElse [75..79)
         |Ident(h) [80..81)
         |Indentation.Outdent [81..81)
         |KwThen [86..90)
         |Ident(k) [91..92)
         |KwElse [93..97)
         |Ident(m) [98..99)
         |LF [99..100)
         |Ident(n) [104..105)
         |Indentation.Outdent [105..105)
         |Indentation.Outdent [105..105)
         |EOF [106..106)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a f (s"${if (b) c else d}")) g else h) k else m
         |    n
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(
                tname("a"),
                tname("f"),
                Term.Interpolate(
                  tname("s"),
                  List(lit(""), lit("")),
                  List(blk(tif(tname("b"), tname("c"), tname("d")))),
                ),
              ),
              tname("g"),
              tname("h"),
            ),
            tname("k"),
            tname("m"),
          ),
          tname("n"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `try` with `catch case`, an `if` with parentheses, `;` and an identifier inside an indented condition, outer `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      try a catch case _ => if (b) c; d then e else g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwTry [33..36)
         |Ident(a) [37..38)
         |KwCatch [39..44)
         |Indentation.Indent [45..45)
         |KwCase [45..49)
         |Underscore [50..51)
         |RightArrow [52..54)
         |KwIf [55..57)
         |LeftParen [58..59)
         |Ident(b) [59..60)
         |RightParen [60..61)
         |Ident(c) [62..63)
         |Semicolon [63..64)
         |Ident(d) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |KwThen [67..71)
         |Ident(e) [72..73)
         |KwElse [74..78)
         |Ident(g) [79..80)
         |LF [80..81)
         |Ident(k) [85..86)
         |Indentation.Outdent [86..86)
         |Indentation.Outdent [86..86)
         |EOF [87..87)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (try a catch {
         |      case _ =>
         |        if (b) c
         |        d
         |    }) e else g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Try(
              tname("a"),
              List(Case(patwildcard, None, blk(tif(tname("b"), tname("c")), tname("d")))),
              None,
            ),
            tname("e"),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `try` with `catch case`, `;` and an identifier inside an indented condition, outer `then` and `else` on the same line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      try a catch case _ => c; d then e else g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwTry [33..36)
         |Ident(a) [37..38)
         |KwCatch [39..44)
         |Indentation.Indent [45..45)
         |KwCase [45..49)
         |Underscore [50..51)
         |RightArrow [52..54)
         |Ident(c) [55..56)
         |Semicolon [56..57)
         |Ident(d) [58..59)
         |Indentation.Outdent [59..59)
         |Indentation.Outdent [59..59)
         |KwThen [60..64)
         |Ident(e) [65..66)
         |KwElse [67..71)
         |Ident(g) [72..73)
         |LF [73..74)
         |Ident(k) [78..79)
         |Indentation.Outdent [79..79)
         |Indentation.Outdent [79..79)
         |EOF [80..80)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (try a catch {
         |      case _ =>
         |        c
         |        d
         |    }) e else g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Try(tname("a"), List(Case(patwildcard, None, blk(tname("c"), tname("d")))), None),
            tname("e"),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `==` after the parentheses in an indented `val` body, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      if (a) == b
         |    then 1 else 2
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwIf [38..40)
         |LeftParen [41..42)
         |Ident(a) [42..43)
         |RightParen [43..44)
         |Ident(==) [45..47)
         |Ident(b) [48..49)
         |Indentation.Outdent [49..49)
         |KwThen [54..58)
         |Constant.Int(1) [59..60)
         |KwElse [61..65)
         |Constant.Int(2) [66..67)
         |LF [67..68)
         |Ident(r) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `match` in an indented `then` body, `else` on the line of the last case, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a then
         |      x match
         |        case 1 => b
         |        case _ => c else d
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |KwThen [29..33)
         |Indentation.Indent [33..33)
         |Ident(x) [40..41)
         |KwMatch [42..47)
         |Indentation.Indent [47..47)
         |KwCase [56..60)
         |Constant.Int(1) [61..62)
         |RightArrow [63..65)
         |Ident(b) [66..67)
         |LF [67..68)
         |KwCase [76..80)
         |Underscore [81..82)
         |RightArrow [83..85)
         |Ident(c) [86..87)
         |Indentation.Outdent [87..87)
         |Indentation.Outdent [87..87)
         |KwElse [88..92)
         |Ident(d) [93..94)
         |LF [94..95)
         |Ident(k) [99..100)
         |Indentation.Outdent [100..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a) x match {
         |      case 1 => b
         |      case _ => c
         |    } else d
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tname("a"),
            tmatch(tname("x"), Case(lit(1), None, tname("b")), Case(Pat.Wildcard(), None, tname("c"))),
            tname("d"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: two statements in an indented `then` body, `else` on the line of the second one, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a then
         |      b
         |      c else d
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |KwThen [29..33)
         |Indentation.Indent [33..33)
         |Ident(b) [40..41)
         |LF [41..42)
         |Ident(c) [48..49)
         |Indentation.Outdent [49..49)
         |KwElse [50..54)
         |Ident(d) [55..56)
         |LF [56..57)
         |Ident(k) [61..62)
         |Indentation.Outdent [62..62)
         |Indentation.Outdent [62..62)
         |EOF [63..63)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a) {
         |      b
         |      c
         |    } else d
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tname("a"), blk(tname("b"), tname("c")), tname("d")), tname("k")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `new` on the next line, `then` and `else` on the same line, a single-line comment at the column of the condition, next definition") {
    val code =
      """|def f =
         |  if
         |    new B(a).ok then c else d
         |    // comment
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwNew [17..20)
         |Ident(B) [21..22)
         |LeftParen [22..23)
         |Ident(a) [23..24)
         |RightParen [24..25)
         |Dot [25..26)
         |Ident(ok) [26..28)
         |Indentation.Outdent [28..28)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |Indentation.Outdent [57..57)
         |KwDef [58..61)
         |Ident(g) [62..63)
         |Equals [64..65)
         |Constant.Int(1) [66..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c else d
         |  // comment
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"), tname("d"))
            .toBuilder.endComment(Seq("// comment")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `new` on the next line, `then` and `else` on the same line, a blank line, a multi-line comment at the column of the condition, end of file") {
    val code =
      """|def f =
         |  if
         |    new B(a).ok then c else d
         |
         |    /* comment */
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwNew [17..20)
         |Ident(B) [21..22)
         |LeftParen [22..23)
         |Ident(a) [23..24)
         |RightParen [24..25)
         |Dot [25..26)
         |Ident(ok) [26..28)
         |Indentation.Outdent [28..28)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |Indentation.Outdent [61..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c else d
         |
         |  /* comment */
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"), tname("d"))
          .toBuilder.endComment(Seq("/* comment */")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `new` on the next line, `then` and `else` on the same line, a multi-line comment at the column of the `if`, next definition") {
    val code =
      """|def f =
         |  if
         |    new B(a).ok then c else d
         |  /* comment */
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwNew [17..20)
         |Ident(B) [21..22)
         |LeftParen [22..23)
         |Ident(a) [23..24)
         |RightParen [24..25)
         |Dot [25..26)
         |Ident(ok) [26..28)
         |Indentation.Outdent [28..28)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |Indentation.Outdent [58..58)
         |KwDef [59..62)
         |Ident(g) [63..64)
         |Equals [65..66)
         |Constant.Int(1) [67..68)
         |EOF [69..69)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c else d
         |  /* comment */
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"), tname("d"))
            .toBuilder.endComment(Seq("/* comment */")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `new` on the next line, `then` and `else` on the same line, a blank line, a single-line comment at the column of the `if`, end of file") {
    val code =
      """|def f =
         |  if
         |    new B(a).ok then c else d
         |
         |  // comment
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |KwNew [17..20)
         |Ident(B) [21..22)
         |LeftParen [22..23)
         |Ident(a) [23..24)
         |RightParen [24..25)
         |Dot [25..26)
         |Ident(ok) [26..28)
         |Indentation.Outdent [28..28)
         |KwThen [29..33)
         |Ident(c) [34..35)
         |KwElse [36..40)
         |Ident(d) [41..42)
         |Indentation.Outdent [56..56)
         |EOF [57..57)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (new B(a).ok) c else d
         |
         |  // comment
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"), tname("d"))
          .toBuilder.endComment(Seq("// comment")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `new` on the next line, `do` on the same line, a single-line comment at the column of the condition, next definition") {
    val code =
      """|def f =
         |  while
         |    new B(a).ok do c
         |    // comment
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |LeftParen [25..26)
         |Ident(a) [26..27)
         |RightParen [27..28)
         |Dot [28..29)
         |Ident(ok) [29..31)
         |Indentation.Outdent [31..31)
         |KwDo [32..34)
         |Ident(c) [35..36)
         |Indentation.Outdent [51..51)
         |KwDef [52..55)
         |Ident(g) [56..57)
         |Equals [58..59)
         |Constant.Int(1) [60..61)
         |EOF [62..62)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (new B(a).ok) c
         |  // comment
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"))
            .toBuilder.endComment(Seq("// comment")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `new` on the next line, `do` on the same line, a blank line, a multi-line comment at the column of the condition, end of file") {
    val code =
      """|def f =
         |  while
         |    new B(a).ok do c
         |
         |    /* comment */
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |LeftParen [25..26)
         |Ident(a) [26..27)
         |RightParen [27..28)
         |Dot [28..29)
         |Ident(ok) [29..31)
         |Indentation.Outdent [31..31)
         |KwDo [32..34)
         |Ident(c) [35..36)
         |Indentation.Outdent [55..55)
         |EOF [56..56)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (new B(a).ok) c
         |
         |  /* comment */
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"))
          .toBuilder.endComment(Seq("/* comment */")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `new` on the next line, `do` on the same line, a multi-line comment at the column of the `while`, next definition") {
    val code =
      """|def f =
         |  while
         |    new B(a).ok do c
         |  /* comment */
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |LeftParen [25..26)
         |Ident(a) [26..27)
         |RightParen [27..28)
         |Dot [28..29)
         |Ident(ok) [29..31)
         |Indentation.Outdent [31..31)
         |KwDo [32..34)
         |Ident(c) [35..36)
         |Indentation.Outdent [52..52)
         |KwDef [53..56)
         |Ident(g) [57..58)
         |Equals [59..60)
         |Constant.Int(1) [61..62)
         |EOF [63..63)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (new B(a).ok) c
         |  /* comment */
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"))
            .toBuilder.endComment(Seq("/* comment */")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `new` on the next line, `do` on the same line, a blank line, a single-line comment at the column of the `while`, end of file") {
    val code =
      """|def f =
         |  while
         |    new B(a).ok do c
         |
         |  // comment
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |KwNew [20..23)
         |Ident(B) [24..25)
         |LeftParen [25..26)
         |Ident(a) [26..27)
         |RightParen [27..28)
         |Dot [28..29)
         |Ident(ok) [29..31)
         |Indentation.Outdent [31..31)
         |KwDo [32..34)
         |Ident(c) [35..36)
         |Indentation.Outdent [50..50)
         |EOF [51..51)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (new B(a).ok) c
         |
         |  // comment
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(tselect(Term.New(init(pname("B"), List(tname("a")))), "ok"), tname("c"))
          .toBuilder.endComment(Seq("// comment")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses on the next line, `then` and `else` on the same line, a single-line comment at the column of the condition, next definition") {
    val code =
      """|def f =
         |  if
         |    (a) && b then c else d
         |    // comment
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |Ident(&&) [21..23)
         |Ident(b) [24..25)
         |Indentation.Outdent [25..25)
         |KwThen [26..30)
         |Ident(c) [31..32)
         |KwElse [33..37)
         |Ident(d) [38..39)
         |Indentation.Outdent [54..54)
         |KwDef [55..58)
         |Ident(g) [59..60)
         |Equals [61..62)
         |Constant.Int(1) [63..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (a && b) c else d
         |  // comment
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")).toBuilder
            .endComment(Seq("// comment")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses on the next line, `then` and `else` on the same line, a blank line, a multi-line comment at the column of the condition, end of file") {
    val code =
      """|def f =
         |  if
         |    (a) && b then c else d
         |
         |    /* comment */
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |Ident(&&) [21..23)
         |Ident(b) [24..25)
         |Indentation.Outdent [25..25)
         |KwThen [26..30)
         |Ident(c) [31..32)
         |KwElse [33..37)
         |Ident(d) [38..39)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (a && b) c else d
         |
         |  /* comment */
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")).toBuilder
          .endComment(Seq("/* comment */")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses on the next line, `then` and `else` on the same line, a multi-line comment at the column of the `if`, next definition") {
    val code =
      """|def f =
         |  if
         |    (a) && b then c else d
         |  /* comment */
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |Ident(&&) [21..23)
         |Ident(b) [24..25)
         |Indentation.Outdent [25..25)
         |KwThen [26..30)
         |Ident(c) [31..32)
         |KwElse [33..37)
         |Ident(d) [38..39)
         |Indentation.Outdent [55..55)
         |KwDef [56..59)
         |Ident(g) [60..61)
         |Equals [62..63)
         |Constant.Int(1) [64..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (a && b) c else d
         |  /* comment */
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")).toBuilder
            .endComment(Seq("/* comment */")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses on the next line, `then` and `else` on the same line, a blank line, a single-line comment at the column of the `if`, end of file") {
    val code =
      """|def f =
         |  if
         |    (a) && b then c else d
         |
         |  // comment
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwIf [10..12)
         |Indentation.Indent [12..12)
         |LeftParen [17..18)
         |Ident(a) [18..19)
         |RightParen [19..20)
         |Ident(&&) [21..23)
         |Ident(b) [24..25)
         |Indentation.Outdent [25..25)
         |KwThen [26..30)
         |Ident(c) [31..32)
         |KwElse [33..37)
         |Ident(d) [38..39)
         |Indentation.Outdent [53..53)
         |EOF [54..54)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  if (a && b) c else d
         |
         |  // comment
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        tif(tinfix(tname("a"), "&&", tname("b")), tname("c"), tname("d")).toBuilder
          .endComment(Seq("// comment")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses on the next line, `do` on the same line, a single-line comment at the column of the condition, next definition") {
    val code =
      """|def f =
         |  while
         |    (a) && b do c
         |    // comment
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |Ident(&&) [24..26)
         |Ident(b) [27..28)
         |Indentation.Outdent [28..28)
         |KwDo [29..31)
         |Ident(c) [32..33)
         |Indentation.Outdent [48..48)
         |KwDef [49..52)
         |Ident(g) [53..54)
         |Equals [55..56)
         |Constant.Int(1) [57..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (a && b) c
         |  // comment
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")).toBuilder
            .endComment(Seq("// comment")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses on the next line, `do` on the same line, a blank line, a multi-line comment at the column of the condition, end of file") {
    val code =
      """|def f =
         |  while
         |    (a) && b do c
         |
         |    /* comment */
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |Ident(&&) [24..26)
         |Ident(b) [27..28)
         |Indentation.Outdent [28..28)
         |KwDo [29..31)
         |Ident(c) [32..33)
         |Indentation.Outdent [52..52)
         |EOF [53..53)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (a && b) c
         |
         |  /* comment */
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")).toBuilder
          .endComment(Seq("/* comment */")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses on the next line, `do` on the same line, a multi-line comment at the column of the `while`, next definition") {
    val code =
      """|def f =
         |  while
         |    (a) && b do c
         |  /* comment */
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |Ident(&&) [24..26)
         |Ident(b) [27..28)
         |Indentation.Outdent [28..28)
         |KwDo [29..31)
         |Ident(c) [32..33)
         |Indentation.Outdent [49..49)
         |KwDef [50..53)
         |Ident(g) [54..55)
         |Equals [56..57)
         |Constant.Int(1) [58..59)
         |EOF [60..60)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (a && b) c
         |  /* comment */
         |}
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")).toBuilder
            .endComment(Seq("/* comment */")).result(),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses on the next line, `do` on the same line, a blank line, a single-line comment at the column of the `while`, end of file") {
    val code =
      """|def f =
         |  while
         |    (a) && b do c
         |
         |  // comment
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |KwWhile [10..15)
         |Indentation.Indent [15..15)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |Ident(&&) [24..26)
         |Ident(b) [27..28)
         |Indentation.Outdent [28..28)
         |KwDo [29..31)
         |Ident(c) [32..33)
         |Indentation.Outdent [47..47)
         |EOF [48..48)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = {
         |  while (a && b) c
         |
         |  // comment
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      blk(
        Term.While(tinfix(tname("a"), "&&", tname("b")), tname("c")).toBuilder
          .endComment(Seq("// comment")).result(),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and a `match` with braces inside an indented condition, outer `then` and `else` on the next line at the column of the inner `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) x match { case _ => true }
         |      then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(x) [40..41)
         |KwMatch [42..47)
         |LeftBrace [48..49)
         |KwCase [50..54)
         |Underscore [55..56)
         |RightArrow [57..59)
         |KwTrue [60..64)
         |RightBrace [65..66)
         |Indentation.Outdent [66..66)
         |KwThen [73..77)
         |Constant.Int(1) [78..79)
         |KwElse [80..84)
         |Constant.Int(2) [85..86)
         |LF [86..87)
         |Ident(g) [91..92)
         |Indentation.Outdent [92..92)
         |Indentation.Outdent [92..92)
         |EOF [93..93)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) x match {
         |      case _ => true
         |    }) 1 else 2
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tname("a"), tmatch(tname("x"), Case(patwildcard, None, bool(true)))),
            int(1),
            int(2),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses, `;` and an identifier inside an indented condition, outer `then` and `else` on the next line at the column of the inner `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) b; c
         |      then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(b) [40..41)
         |Semicolon [41..42)
         |Ident(c) [43..44)
         |Indentation.Outdent [44..44)
         |KwThen [51..55)
         |Constant.Int(1) [56..57)
         |KwElse [58..62)
         |Constant.Int(2) [63..64)
         |LF [64..65)
         |Ident(g) [69..70)
         |Indentation.Outdent [70..70)
         |Indentation.Outdent [70..70)
         |EOF [71..71)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      c
         |    }) 1 else 2
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(blk(tif(tname("a"), tname("b")), tname("c")), int(1), int(2)), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses, `;` and an identifier inside an indented `while` condition, outer `do` on the next line at the column of the inner `while`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) b; c
         |      do d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(b) [46..47)
         |Semicolon [47..48)
         |Ident(c) [49..50)
         |LF [50..51)
         |Indentation.Outdent [50..50)
         |KwDo [57..59)
         |Ident(d) [60..61)
         |LF [61..62)
         |Ident(g) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a) b
         |      c
         |    }) d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(blk(Term.While(tname("a"), tname("b")), tname("c")), tname("d")), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `then` and an identifier on the next line inside an indented condition, outer `then` and `else` on the next line at the column of the inner `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if a then b
         |      c
         |      then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |Ident(a) [36..37)
         |KwThen [38..42)
         |Ident(b) [43..44)
         |LF [44..45)
         |Ident(c) [51..52)
         |Indentation.Outdent [52..52)
         |KwThen [59..63)
         |Constant.Int(1) [64..65)
         |KwElse [66..70)
         |Constant.Int(2) [71..72)
         |LF [72..73)
         |Ident(g) [77..78)
         |Indentation.Outdent [78..78)
         |Indentation.Outdent [78..78)
         |EOF [79..79)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a) b
         |      c
         |    }) 1 else 2
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(blk(tif(tname("a"), tname("b")), tname("c")), int(1), int(2)), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses at the line end, an argument in parentheses with `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (a)
         |      (x) then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Indentation.Indent [30..30)
         |LeftParen [37..38)
         |Ident(x) [38..39)
         |RightParen [39..40)
         |KwThen [41..45)
         |Constant.Int(1) [46..47)
         |KwElse [48..52)
         |Constant.Int(2) [53..54)
         |Indentation.Outdent [54..54)
         |LF [54..55)
         |Ident(g) [59..60)
         |Indentation.Outdent [60..60)
         |Indentation.Outdent [60..60)
         |EOF [61..61)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `then` found
         |      (x) then 1 else 2
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end inside an indented condition, an argument in parentheses with `then` and `else` on the next line, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a)
         |        (x) then 1 else 2
         |    then 3 else 4
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Indentation.Indent [39..39)
         |LeftParen [48..49)
         |Ident(x) [49..50)
         |RightParen [50..51)
         |Indentation.Outdent [51..51)
         |Indentation.Outdent [51..51)
         |KwThen [52..56)
         |Constant.Int(1) [57..58)
         |KwElse [59..63)
         |Constant.Int(2) [64..65)
         |KwThen [70..74)
         |Constant.Int(3) [75..76)
         |KwElse [77..81)
         |Constant.Int(4) [82..83)
         |LF [83..84)
         |Ident(g) [88..89)
         |Indentation.Outdent [89..89)
         |Indentation.Outdent [89..89)
         |EOF [90..90)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `outdent` expected but `then` found
         |    then 3 else 4
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses and an argument with `new` after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) g(new B)
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [34..35)
         |LeftParen [35..36)
         |KwNew [36..39)
         |Ident(B) [40..41)
         |RightParen [41..42)
         |KwThen [47..51)
         |Constant.Int(1) [52..53)
         |KwElse [58..62)
         |Constant.Int(2) [63..64)
         |Indentation.Outdent [64..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) g(new B)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tapply(tname("g"), Term.New(init(pname("B"))))), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses in an `if` with parentheses and `else` in an `if` with parentheses, outer `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if (x) if (c) while (a) b else f
         |    else g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(x) [28..29)
         |RightParen [29..30)
         |KwIf [31..33)
         |LeftParen [34..35)
         |Ident(c) [35..36)
         |RightParen [36..37)
         |KwWhile [38..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(b) [48..49)
         |KwElse [50..54)
         |Ident(f) [55..56)
         |KwElse [61..65)
         |Ident(g) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A { def f = if (x) if (c) while (a) b else f else g }
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tname("x"), tif(tname("c"), Term.While(tname("a"), tname("b")), tname("f")), tname("g")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and an argument with `new` after them on the line of the outer `if`, outer `then` and `else` on the same line") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) g(new B) then 1 else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [34..35)
         |LeftParen [35..36)
         |KwNew [36..39)
         |Ident(B) [40..41)
         |RightParen [41..42)
         |KwThen [43..47)
         |Constant.Int(1) [48..49)
         |KwElse [50..54)
         |Constant.Int(2) [55..56)
         |Indentation.Outdent [56..56)
         |Indentation.Outdent [56..56)
         |EOF [57..57)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) g(new B)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tapply(tname("g"), Term.New(init(pname("B"))))), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses and an argument with `new` after them on the line of the outer `while`, outer `do` on the same line") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) g(new B) do h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(g) [40..41)
         |LeftParen [41..42)
         |KwNew [42..45)
         |Ident(B) [46..47)
         |RightParen [47..48)
         |KwDo [49..51)
         |Ident(h) [52..53)
         |Indentation.Outdent [53..53)
         |Indentation.Outdent [53..53)
         |EOF [54..54)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = while (while (a) g(new B)) h }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(Term.While(tname("a"), tapply(tname("g"), Term.New(init(pname("B"))))), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and an argument with an `if` without parentheses after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) g(if b then c else d)
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [34..35)
         |LeftParen [35..36)
         |KwIf [36..38)
         |Ident(b) [39..40)
         |KwThen [41..45)
         |Ident(c) [46..47)
         |KwElse [48..52)
         |Ident(d) [53..54)
         |RightParen [54..55)
         |KwThen [60..64)
         |Constant.Int(1) [65..66)
         |KwElse [71..75)
         |Constant.Int(2) [76..77)
         |Indentation.Outdent [77..77)
         |Indentation.Outdent [77..77)
         |EOF [78..78)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) g(if (b) c else d)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tif(tname("a"), tapply(tname("g"), tif(tname("b"), tname("c"), tname("d")))),
          int(1),
          int(2),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses, a multi-line comment and an argument with `new` after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) /* c */ g(new B)
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [42..43)
         |LeftParen [43..44)
         |KwNew [44..47)
         |Ident(B) [48..49)
         |RightParen [49..50)
         |KwThen [55..59)
         |Constant.Int(1) [60..61)
         |KwElse [66..70)
         |Constant.Int(2) [71..72)
         |Indentation.Outdent [72..72)
         |Indentation.Outdent [72..72)
         |EOF [73..73)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) /* c */ g(new B)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tif(
            tname("a"),
            tapply(tname("g"), Term.New(init(pname("B")))).toBuilder.begComment(Seq("/* c */"))
              .result(),
          ),
          int(1),
          int(2),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and `new` in parentheses with a selection after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) (new B).ok
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |LeftParen [34..35)
         |KwNew [35..38)
         |Ident(B) [39..40)
         |RightParen [40..41)
         |Dot [41..42)
         |Ident(ok) [42..44)
         |KwThen [49..53)
         |Constant.Int(1) [54..55)
         |KwElse [60..64)
         |Constant.Int(2) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) (new B).ok) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tselect(Term.New(init(pname("B"))), "ok")), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and a `match` with braces after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) x match { case _ => true }
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(x) [34..35)
         |KwMatch [36..41)
         |LeftBrace [42..43)
         |KwCase [44..48)
         |Underscore [49..50)
         |RightArrow [51..53)
         |KwTrue [54..58)
         |RightBrace [59..60)
         |KwThen [65..69)
         |Constant.Int(1) [70..71)
         |KwElse [76..80)
         |Constant.Int(2) [81..82)
         |Indentation.Outdent [82..82)
         |Indentation.Outdent [82..82)
         |EOF [83..83)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (if (a) x match {
         |    case _ => true
         |  }) 1 else 2
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tmatch(tname("x"), Case(patwildcard, None, lit(true)))), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and an argument with `new` after them on the line of the outer `if`, outer `then` indented on the next line, outer `else` on the line after it") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) g(new B)
         |      then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [34..35)
         |LeftParen [35..36)
         |KwNew [36..39)
         |Ident(B) [40..41)
         |RightParen [41..42)
         |KwThen [49..53)
         |Constant.Int(1) [54..55)
         |KwElse [60..64)
         |Constant.Int(2) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) g(new B)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tapply(tname("g"), Term.New(init(pname("B"))))), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with parentheses and an argument with `throw` after them on the line of the outer `if`, outer `then` and `else` on the next lines") {
    val code =
      """|object A:
         |  def f =
         |    if if (a) g(throw e)
         |    then 1
         |    else 2
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Ident(g) [34..35)
         |LeftParen [35..36)
         |KwThrow [36..41)
         |Ident(e) [42..43)
         |RightParen [43..44)
         |KwThen [49..53)
         |Constant.Int(1) [54..55)
         |KwElse [60..64)
         |Constant.Int(2) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = if (if (a) g(throw e)) 1 else 2 }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(tif(tname("a"), tapply(tname("g"), Term.Throw(tname("e")))), int(1), int(2)),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: an `if` inside an `if` inside an indented condition, `&&` after the parentheses, leading `||` on the next line with `then` and `else`, middle and outer `then` and `else` on the next lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if
         |        if (a) && b
         |          || c then d else e
         |      then f else g
         |    then h else k
         |    m
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |Indentation.Indent [35..35)
         |KwIf [44..46)
         |LeftParen [47..48)
         |Ident(a) [48..49)
         |RightParen [49..50)
         |Ident(&&) [51..53)
         |Ident(b) [54..55)
         |InfixLF [55..56)
         |Ident(||) [66..68)
         |Ident(c) [69..70)
         |KwThen [71..75)
         |Ident(d) [76..77)
         |KwElse [78..82)
         |Ident(e) [83..84)
         |Indentation.Outdent [84..84)
         |KwThen [91..95)
         |Ident(f) [96..97)
         |KwElse [98..102)
         |Ident(g) [103..104)
         |Indentation.Outdent [104..104)
         |KwThen [109..113)
         |Ident(h) [114..115)
         |KwElse [116..120)
         |Ident(k) [121..122)
         |LF [122..123)
         |Ident(m) [127..128)
         |Indentation.Outdent [128..128)
         |Indentation.Outdent [128..128)
         |EOF [129..129)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (if (a && b || c) d else e) f else g) h else k
         |    m
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tif(
                tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
                tname("d"),
                tname("e"),
              ),
              tname("f"),
              tname("g"),
            ),
            tname("h"),
            tname("k"),
          ),
          tname("m"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a `while` inside a `while` inside an indented `while` condition, `&&` after the parentheses, leading `||` on the next line with `do`, middle and outer `do` on the next lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while
         |        while (a) && b
         |          || c do d
         |        e
         |      do f
         |      g
         |    do k
         |    m
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |Indentation.Indent [41..41)
         |KwWhile [50..55)
         |LeftParen [56..57)
         |Ident(a) [57..58)
         |RightParen [58..59)
         |Ident(&&) [60..62)
         |Ident(b) [63..64)
         |InfixLF [64..65)
         |Ident(||) [75..77)
         |Ident(c) [78..79)
         |KwDo [80..82)
         |Ident(d) [83..84)
         |LF [84..85)
         |Ident(e) [93..94)
         |Indentation.Outdent [94..94)
         |KwDo [101..103)
         |Ident(f) [104..105)
         |LF [105..106)
         |Ident(g) [112..113)
         |Indentation.Outdent [113..113)
         |KwDo [118..120)
         |Ident(k) [121..122)
         |LF [122..123)
         |Ident(m) [127..128)
         |Indentation.Outdent [128..128)
         |Indentation.Outdent [128..128)
         |EOF [129..129)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while ({
         |        while (a && b || c) d
         |        e
         |      }) f
         |      g
         |    }) k
         |    m
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              Term.While(
                blk(
                  Term
                    .While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
                  tname("e"),
                ),
                tname("f"),
              ),
              tname("g"),
            ),
            tname("k"),
          ),
          tname("m"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and an identifier after the parentheses inside an indented `while` condition, an argument in parentheses on the next line with `then` and `else`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      if (a) && f
         |        (c) then d else e
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwIf [36..38)
         |LeftParen [39..40)
         |Ident(a) [40..41)
         |RightParen [41..42)
         |Ident(&&) [43..45)
         |Ident(f) [46..47)
         |LeftParen [56..57)
         |Ident(c) [57..58)
         |RightParen [58..59)
         |KwThen [60..64)
         |Ident(d) [65..66)
         |KwElse [67..71)
         |Ident(e) [72..73)
         |Indentation.Outdent [73..73)
         |KwDo [78..80)
         |Ident(g) [81..82)
         |LF [82..83)
         |Ident(k) [87..88)
         |Indentation.Outdent [88..88)
         |Indentation.Outdent [88..88)
         |EOF [89..89)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (if (a && f(c)) d else e) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tif(tinfix(tname("a"), "&&", tapply(tname("f"), tname("c"))), tname("d"), tname("e")),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses in a value of indented enumerators, leading `||` on the next line with `do`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    for
         |      x <- xs
         |      y = while (a) && b
         |        || c do d
         |    do g
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwFor [24..27)
         |Indentation.Indent [27..27)
         |Ident(x) [34..35)
         |LeftArrow [36..38)
         |Ident(xs) [39..41)
         |LF [41..42)
         |Ident(y) [48..49)
         |Equals [50..51)
         |KwWhile [52..57)
         |LeftParen [58..59)
         |Ident(a) [59..60)
         |RightParen [60..61)
         |Ident(&&) [62..64)
         |Ident(b) [65..66)
         |InfixLF [66..67)
         |Ident(||) [75..77)
         |Ident(c) [78..79)
         |KwDo [80..82)
         |Ident(d) [83..84)
         |Indentation.Outdent [84..84)
         |LF [84..85)
         |KwDo [89..91)
         |Ident(g) [92..93)
         |LF [93..94)
         |Ident(k) [98..99)
         |Indentation.Outdent [99..99)
         |Indentation.Outdent [99..99)
         |EOF [100..100)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    for (x <- xs; y = while (a && b || c) d) g
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.For(
            List(
              Enumerator.Generator(patvar("x"), tname("xs")),
              Enumerator.Val(
                patvar("y"),
                Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
              ),
            ),
            tname("g"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses inside an indented condition, leading `||` on the next line with `then`, `else`, `;` and an identifier, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |        || c then d else e; g
         |    then h else k
         |    m
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [44..45)
         |Ident(||) [53..55)
         |Ident(c) [56..57)
         |KwThen [58..62)
         |Ident(d) [63..64)
         |KwElse [65..69)
         |Ident(e) [70..71)
         |Semicolon [71..72)
         |Ident(g) [73..74)
         |Indentation.Outdent [74..74)
         |KwThen [79..83)
         |Ident(h) [84..85)
         |KwElse [86..90)
         |Ident(k) [91..92)
         |LF [92..93)
         |Ident(m) [97..98)
         |Indentation.Outdent [98..98)
         |Indentation.Outdent [98..98)
         |EOF [99..99)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      if (a && b || c) d else e
         |      g
         |    }) h else k
         |    m
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(
              tif(
                tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
                tname("d"),
                tname("e"),
              ),
              tname("g"),
            ),
            tname("h"),
            tname("k"),
          ),
          tname("m"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses inside an indented `while` condition, leading `||` on the next line with `do`, `;` and an identifier, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) && b
         |        || c do d; e
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(&&) [46..48)
         |Ident(b) [49..50)
         |InfixLF [50..51)
         |Ident(||) [59..61)
         |Ident(c) [62..63)
         |KwDo [64..66)
         |Ident(d) [67..68)
         |Semicolon [68..69)
         |Ident(e) [70..71)
         |Indentation.Outdent [71..71)
         |KwDo [76..78)
         |Ident(k) [79..80)
         |LF [80..81)
         |Ident(g) [85..86)
         |Indentation.Outdent [86..86)
         |Indentation.Outdent [86..86)
         |EOF [87..87)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a && b || c) d
         |      e
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
              tname("e"),
            ),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses inside an indented condition, a blank line, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |
         |        || c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |Ident(||) [54..56)
         |Ident(c) [57..58)
         |KwThen [59..63)
         |Ident(d) [64..65)
         |KwElse [66..70)
         |Ident(e) [71..72)
         |Indentation.Outdent [72..72)
         |KwThen [77..81)
         |Ident(g) [82..83)
         |KwElse [84..88)
         |Ident(h) [89..90)
         |LF [90..91)
         |Ident(k) [95..96)
         |Indentation.Outdent [96..96)
         |Indentation.Outdent [96..96)
         |EOF [97..97)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses inside an indented condition, leading `||` on the next line with `then` and `else`, a blank line, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |        || c then d else e
         |
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [44..45)
         |Ident(||) [53..55)
         |Ident(c) [56..57)
         |KwThen [58..62)
         |Ident(d) [63..64)
         |KwElse [65..69)
         |Ident(e) [70..71)
         |Indentation.Outdent [72..72)
         |KwThen [77..81)
         |Ident(g) [82..83)
         |KwElse [84..88)
         |Ident(h) [89..90)
         |LF [90..91)
         |Ident(k) [95..96)
         |Indentation.Outdent [96..96)
         |Indentation.Outdent [96..96)
         |EOF [97..97)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses inside an indented condition, a comment line, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |        // x
         |        || c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [57..58)
         |Ident(||) [66..68)
         |Ident(c) [69..70)
         |KwThen [71..75)
         |Ident(d) [76..77)
         |KwElse [78..82)
         |Ident(e) [83..84)
         |Indentation.Outdent [84..84)
         |KwThen [89..93)
         |Ident(g) [94..95)
         |KwElse [96..100)
         |Ident(h) [101..102)
         |LF [102..103)
         |Ident(k) [107..108)
         |Indentation.Outdent [108..108)
         |Indentation.Outdent [108..108)
         |EOF [109..109)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b
         |      // x
         |      || c) d else e) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(
                tinfix(tname("a"), "&&", tname("b")),
                Term.Name.newBuilder("||").begComment(detachedComments("// x")).result(),
                tname("c"),
              ),
              tname("d"),
              tname("e"),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `&&` after the parentheses inside an indented condition, leading `||` on the next line with `then`, `else` and a comment, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (a) && b
         |        || c then d else e // x
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [44..45)
         |Ident(||) [53..55)
         |Ident(c) [56..57)
         |KwThen [58..62)
         |Ident(d) [63..64)
         |KwElse [65..69)
         |Ident(e) [70..71)
         |Indentation.Outdent [76..76)
         |KwThen [81..85)
         |Ident(g) [86..87)
         |KwElse [88..92)
         |Ident(h) [93..94)
         |LF [94..95)
         |Ident(k) [99..100)
         |Indentation.Outdent [100..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a && b || c) d else e // x
         |    ) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
              tname("d"),
              tname("e"),
            ).toBuilder.endComment(Seq("// x")).result(),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses inside an indented `while` condition, leading `||` on the next line with `do`, another statement, a blank line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      while (a) && b
         |        || c do d
         |      e
         |
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |KwWhile [36..41)
         |LeftParen [42..43)
         |Ident(a) [43..44)
         |RightParen [44..45)
         |Ident(&&) [46..48)
         |Ident(b) [49..50)
         |InfixLF [50..51)
         |Ident(||) [59..61)
         |Ident(c) [62..63)
         |KwDo [64..66)
         |Ident(d) [67..68)
         |LF [68..69)
         |Ident(e) [75..76)
         |Indentation.Outdent [77..77)
         |KwDo [82..84)
         |Ident(k) [85..86)
         |LF [86..87)
         |Ident(g) [91..92)
         |Indentation.Outdent [92..92)
         |Indentation.Outdent [92..92)
         |EOF [93..93)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      while (a && b || c) d
         |      e
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
              tname("e"),
            ),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` after the parentheses on the line of the outer `while`, leading `||` on the next line with `do`, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) && b
         |      || c do d
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(&&) [40..42)
         |Ident(b) [43..44)
         |InfixLF [44..45)
         |Ident(||) [51..53)
         |Ident(c) [54..55)
         |KwDo [56..58)
         |Ident(d) [59..60)
         |KwDo [65..67)
         |Ident(k) [68..69)
         |LF [69..70)
         |Ident(g) [74..75)
         |Indentation.Outdent [75..75)
         |Indentation.Outdent [75..75)
         |EOF [76..76)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (while (a && b || c) d) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: an identifier and an `if` with `&&` after the parentheses inside an indented condition, leading `||` on the next line with `then` and `else`, outer `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      c
         |      if (a) && b
         |        || c then d else e
         |    then g else h
         |    k
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |Ident(c) [33..34)
         |LF [34..35)
         |KwIf [41..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(&&) [48..50)
         |Ident(b) [51..52)
         |InfixLF [52..53)
         |Ident(||) [61..63)
         |Ident(c) [64..65)
         |KwThen [66..70)
         |Ident(d) [71..72)
         |KwElse [73..77)
         |Ident(e) [78..79)
         |Indentation.Outdent [79..79)
         |KwThen [84..88)
         |Ident(g) [89..90)
         |KwElse [91..95)
         |Ident(h) [96..97)
         |LF [97..98)
         |Ident(k) [102..103)
         |Indentation.Outdent [103..103)
         |Indentation.Outdent [103..103)
         |EOF [104..104)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ({
         |      c
         |      if (a && b || c) d else e
         |    }) g else h
         |    k
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            blk(
              tname("c"),
              tif(
                tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")),
                tname("d"),
                tname("e"),
              ),
            ),
            tname("g"),
            tname("h"),
          ),
          tname("k"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: an identifier and a `while` with `&&` after the parentheses inside an indented `while` condition, leading `||` on the next line with `do`, an identifier, outer `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while
         |      c
         |      while (a) && b
         |        || c do d
         |      e
         |    do k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Indentation.Indent [29..29)
         |Ident(c) [36..37)
         |LF [37..38)
         |KwWhile [44..49)
         |LeftParen [50..51)
         |Ident(a) [51..52)
         |RightParen [52..53)
         |Ident(&&) [54..56)
         |Ident(b) [57..58)
         |InfixLF [58..59)
         |Ident(||) [67..69)
         |Ident(c) [70..71)
         |KwDo [72..74)
         |Ident(d) [75..76)
         |LF [76..77)
         |Ident(e) [83..84)
         |Indentation.Outdent [84..84)
         |KwDo [89..91)
         |Ident(k) [92..93)
         |LF [93..94)
         |Ident(g) [98..99)
         |Indentation.Outdent [99..99)
         |Indentation.Outdent [99..99)
         |EOF [100..100)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while ({
         |      c
         |      while (a && b || c) d
         |      e
         |    }) k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            blk(
              tname("c"),
              Term.While(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), tname("d")),
              tname("e"),
            ),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses and an argument with `new` after them on the line of the outer `while`, outer `do` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) g(new B)
         |    do h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(g) [40..41)
         |LeftParen [41..42)
         |KwNew [42..45)
         |Ident(B) [46..47)
         |RightParen [47..48)
         |KwDo [53..55)
         |Ident(h) [56..57)
         |Indentation.Outdent [57..57)
         |Indentation.Outdent [57..57)
         |EOF [58..58)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = while (while (a) g(new B)) h }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(Term.While(tname("a"), tapply(tname("g"), Term.New(init(pname("B"))))), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses and an argument with `try` and `catch` in braces after them on the line of the outer `while`, outer `do` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) g(try x catch { case _ => y })
         |    do h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(g) [40..41)
         |LeftParen [41..42)
         |KwTry [42..45)
         |Ident(x) [46..47)
         |KwCatch [48..53)
         |LeftBrace [54..55)
         |KwCase [56..60)
         |Underscore [61..62)
         |RightArrow [63..65)
         |Ident(y) [66..67)
         |RightBrace [68..69)
         |RightParen [69..70)
         |KwDo [75..77)
         |Ident(h) [78..79)
         |Indentation.Outdent [79..79)
         |Indentation.Outdent [79..79)
         |EOF [80..80)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = while (while (a) g(try x catch {
         |    case _ => y
         |  })) h
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(
          Term.While(
            tname("a"),
            tapply(tname("g"), Term.Try(tname("x"), List(Case(patwildcard, None, tname("y"))), None)),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses and an assignment with `new` after them on the line of the outer `while`, outer `do` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) x = new B
         |    do h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(x) [40..41)
         |Equals [42..43)
         |KwNew [44..47)
         |Ident(B) [48..49)
         |KwDo [54..56)
         |Ident(h) [57..58)
         |Indentation.Outdent [58..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = while (while (a) x = new B) h }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(
          Term.While(tname("a"), Term.Assign(tname("x"), Term.New(init(pname("B"))))),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses and an argument with `for` and `yield` without parentheses after them on the line of the outer `while`, outer `do` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    while while (a) g(for x <- xs yield x)
         |    do h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwWhile [30..35)
         |LeftParen [36..37)
         |Ident(a) [37..38)
         |RightParen [38..39)
         |Ident(g) [40..41)
         |LeftParen [41..42)
         |KwFor [42..45)
         |Ident(x) [46..47)
         |LeftArrow [48..50)
         |Ident(xs) [51..53)
         |KwYield [54..59)
         |Ident(x) [60..61)
         |RightParen [61..62)
         |KwDo [67..69)
         |Ident(h) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout = "object A { def f = while (while (a) g(for (x <- xs) yield x)) h }"
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        Term.While(
          Term.While(
            tname("a"),
            tapply(
              tname("g"),
              Term.ForYield(List(Enumerator.Generator(patvar("x"), tname("xs"))), tname("x")),
            ),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` and a colon argument after the parentheses, the argument on the next line, shallower leading `&&` on the next line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = while (x)(y) && foo:
         |        z
         |      && w
         |    do c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while (x(y) && foo {
         |    z && w
         |  }) c
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          Term.While(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` without parentheses with `&&` and a colon argument, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if x && foo:
         |        z
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (x && foo {
         |    z && w
         |  }) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` without parentheses with `&&` and a colon argument, the argument on the next line, shallower leading `&&` on the next line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = while x && foo:
         |        z
         |      && w
         |    do c
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = while (x && foo {
         |    z && w
         |  }) c
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          Term.While(
            tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
            tname("c"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument outside a condition, the argument on the next line, shallower leading `&&` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = a && foo:
         |        z
         |      && w
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = a && foo {
         |    z && w
         |  }
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w"))))),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |Indentation.Outdent [48..48)
         |InfixLF [48..49)
         |Ident(&&) [55..57)
         |Ident(w) [58..59)
         |KwThen [64..68)
         |Ident(c) [69..70)
         |KwElse [71..75)
         |Ident(d) [76..77)
         |LF [77..78)
         |KwDef [80..83)
         |Ident(g) [84..85)
         |Equals [86..87)
         |Constant.Int(1) [88..89)
         |Indentation.Outdent [89..89)
         |EOF [90..90)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with an `if` whose `then` body holds a colon argument, the argument on the next line, a blank line, leading `&&` on the next line, `else` and the outer `then` on later lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if a then foo:
         |          z
         |
         |        && w
         |      else d
         |    then c else e
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |KwThen [32..36)
         |Ident(foo) [37..40)
         |Colon [40..41)
         |Indentation.Indent [41..41)
         |Ident(z) [52..53)
         |Indentation.Outdent [54..54)
         |InfixLF [54..55)
         |Ident(&&) [63..65)
         |Ident(w) [66..67)
         |KwElse [74..78)
         |Ident(d) [79..80)
         |KwThen [85..89)
         |Ident(c) [90..91)
         |KwElse [92..96)
         |Ident(e) [97..98)
         |LF [98..99)
         |Ident(g) [103..104)
         |Indentation.Outdent [104..104)
         |Indentation.Outdent [104..104)
         |EOF [105..105)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) foo {
         |      z
         |    } && w else d) c else e
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tname("a"),
              tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
              tname("d"),
            ),
            tname("c"),
            tname("e"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with an `if` whose `then` body holds a colon argument, the argument on the next line, two blank lines, leading `&&` on the next line, `else` and the outer `then` on later lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if a then foo:
         |          z
         |
         |
         |        && w
         |      else d
         |    then c else e
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |KwThen [32..36)
         |Ident(foo) [37..40)
         |Colon [40..41)
         |Indentation.Indent [41..41)
         |Ident(z) [52..53)
         |Indentation.Outdent [55..55)
         |Ident(&&) [64..66)
         |Ident(w) [67..68)
         |KwElse [75..79)
         |Ident(d) [80..81)
         |KwThen [86..90)
         |Ident(c) [91..92)
         |KwElse [93..97)
         |Ident(e) [98..99)
         |LF [99..100)
         |Ident(g) [104..105)
         |Indentation.Outdent [105..105)
         |Indentation.Outdent [105..105)
         |EOF [106..106)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) foo {
         |      z
         |    } && w else d) c else e
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tname("a"),
              tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
              tname("d"),
            ),
            tname("c"),
            tname("e"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `for` with a colon argument after `do`, the argument on the next line, a blank line, leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if for (x <- xs) do foo:
         |          z
         |
         |        && w
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwFor [27..30)
         |LeftParen [31..32)
         |Ident(x) [32..33)
         |LeftArrow [34..36)
         |Ident(xs) [37..39)
         |RightParen [39..40)
         |KwDo [41..43)
         |Ident(foo) [44..47)
         |Colon [47..48)
         |Indentation.Indent [48..48)
         |Ident(z) [59..60)
         |Indentation.Outdent [61..61)
         |InfixLF [61..62)
         |Ident(&&) [70..72)
         |Ident(w) [73..74)
         |KwThen [79..83)
         |Ident(c) [84..85)
         |KwElse [86..90)
         |Ident(d) [91..92)
         |LF [92..93)
         |Ident(h) [97..98)
         |Indentation.Outdent [98..98)
         |Indentation.Outdent [98..98)
         |EOF [99..99)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (for (x <- xs) foo {
         |      z
         |    } && w) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.For(
              Term.EnumeratorsBlock(List(Enumerator.Generator(patvar("x"), tname("xs")))),
              tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a colon lambda, its body on the next line, a blank line, leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists: x =>
         |        x > 0
         |
         |      && w
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(xs) [27..29)
         |Dot [29..30)
         |Ident(exists) [30..36)
         |Colon [36..37)
         |Ident(x) [38..39)
         |RightArrow [40..42)
         |Indentation.Indent [42..42)
         |Ident(x) [51..52)
         |Ident(>) [53..54)
         |Constant.Int(0) [55..56)
         |Indentation.Outdent [57..57)
         |InfixLF [57..58)
         |Ident(&&) [64..66)
         |Ident(w) [67..68)
         |KwThen [73..77)
         |Ident(c) [78..79)
         |KwElse [80..84)
         |Ident(d) [85..86)
         |LF [86..87)
         |Ident(h) [91..92)
         |Indentation.Outdent [92..92)
         |Indentation.Outdent [92..92)
         |EOF [93..93)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (xs.exists {
         |      x => x > 0
         |    } && w) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tapply(
                tselect(tname("xs"), "exists"),
                blk(tfunc(tparam("x"))(tinfix(tname("x"), ">", int(0)))),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and nested colon arguments, the arguments on the next lines, a comment line, a blank line, leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |        bar:
         |          z
         |        // c
         |
         |      && w
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(bar) [45..48)
         |Colon [48..49)
         |Indentation.Indent [49..49)
         |Ident(z) [60..61)
         |Indentation.Outdent [61..61)
         |Indentation.Outdent [75..75)
         |Ident(&&) [82..84)
         |Ident(w) [85..86)
         |KwThen [91..95)
         |Ident(c) [96..97)
         |KwElse [98..102)
         |Ident(d) [103..104)
         |LF [104..105)
         |Ident(h) [109..110)
         |Indentation.Outdent [110..110)
         |Indentation.Outdent [110..110)
         |EOF [111..111)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a && foo {
         |      bar {
         |        z
         |      }
         |      // c
         |
         |    } && w) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tinfix(
                tname("a"),
                "&&",
                tapply(
                  tname("foo"),
                  blk(tapply(tname("bar"), blk(tname("z"))).toBuilder.endComment(Seq("// c")).result()),
                ),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with the condition in parentheses, a deeper `then` body in parentheses on the next line, `else` on that line, the outer `then` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if (a)
         |         (x) else y
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |LeftParen [43..44)
         |Ident(x) [44..45)
         |RightParen [45..46)
         |Indentation.Outdent [46..46)
         |KwElse [47..51)
         |Ident(y) [52..53)
         |KwThen [58..62)
         |Ident(c) [63..64)
         |KwElse [65..69)
         |Ident(d) [70..71)
         |LF [71..72)
         |Ident(h) [76..77)
         |Indentation.Outdent [77..77)
         |Indentation.Outdent [77..77)
         |EOF [78..78)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) x else y) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tif(tname("a"), tname("x"), tname("y")), tname("c"), tname("d")), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with the condition in parentheses, a deeper `then` body on the next line, `else` on that line, the outer `then` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if (a)
         |         x else y
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |Ident(x) [43..44)
         |Indentation.Outdent [44..44)
         |KwElse [45..49)
         |Ident(y) [50..51)
         |KwThen [56..60)
         |Ident(c) [61..62)
         |KwElse [63..67)
         |Ident(d) [68..69)
         |LF [69..70)
         |Ident(h) [74..75)
         |Indentation.Outdent [75..75)
         |Indentation.Outdent [75..75)
         |EOF [76..76)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) x else y) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tif(tname("a"), tname("x"), tname("y")), tname("c"), tname("d")), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a colon argument, the argument on the next line, a blank line, `match` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if foo:
         |        z
         |
         |      match { case _ => true }
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(foo) [27..30)
         |Colon [30..31)
         |Indentation.Indent [31..31)
         |Ident(z) [40..41)
         |Indentation.Outdent [42..42)
         |KwMatch [49..54)
         |LeftBrace [55..56)
         |KwCase [57..61)
         |Underscore [62..63)
         |RightArrow [64..66)
         |KwTrue [67..71)
         |RightBrace [72..73)
         |KwThen [78..82)
         |Ident(c) [83..84)
         |KwElse [85..89)
         |Ident(d) [90..91)
         |LF [91..92)
         |Ident(h) [96..97)
         |Indentation.Outdent [97..97)
         |Indentation.Outdent [97..97)
         |EOF [98..98)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (foo {
         |      z
         |    } match {
         |      case _ => true
         |    }) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tmatch(tapply(tname("foo"), blk(tname("z"))), Case(patwildcard, None, bool(true))),
            tname("c"),
            tname("d"),
          ),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with a colon argument, the argument on the next line, a blank line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while foo:
         |        z
         |
         |    do b
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Ident(foo) [30..33)
         |Colon [33..34)
         |Indentation.Indent [34..34)
         |Ident(z) [43..44)
         |Indentation.Outdent [45..45)
         |KwDo [50..52)
         |Ident(b) [53..54)
         |LF [54..55)
         |Ident(h) [59..60)
         |Indentation.Outdent [60..60)
         |Indentation.Outdent [60..60)
         |EOF [61..61)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (foo {
         |      z
         |    }) b
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(Term.While(tapply(tname("foo"), blk(tname("z"))), tname("b")), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a colon argument, the argument on the next line, a blank line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if foo:
         |        z
         |
         |    then c else d
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(foo) [27..30)
         |Colon [30..31)
         |Indentation.Indent [31..31)
         |Ident(z) [40..41)
         |Indentation.Outdent [42..42)
         |KwThen [47..51)
         |Ident(c) [52..53)
         |KwElse [54..58)
         |Ident(d) [59..60)
         |LF [60..61)
         |Ident(h) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (foo {
         |      z
         |    }) c else d
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(tif(tapply(tname("foo"), blk(tname("z"))), tname("c"), tname("d")), tname("h")),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a colon argument after `finally`, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if try a finally foo:
         |        z
         |
         |    && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |Ident(a) [31..32)
         |KwFinally [33..40)
         |Ident(foo) [41..44)
         |Colon [44..45)
         |Indentation.Indent [45..45)
         |Ident(z) [54..55)
         |Indentation.Outdent [56..56)
         |InfixLF [56..57)
         |Ident(&&) [61..63)
         |Ident(w) [64..65)
         |KwThen [70..74)
         |Ident(c) [75..76)
         |KwElse [77..81)
         |Ident(d) [82..83)
         |Indentation.Outdent [83..83)
         |Indentation.Outdent [83..83)
         |EOF [84..84)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (try a finally foo {
         |    z
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          Term.Try(
            tname("a"),
            None,
            Some(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w"))),
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and nested colon arguments, the arguments on the next lines, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |          bar:
         |            z
         |
         |    && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(bar) [47..50)
         |Colon [50..51)
         |Indentation.Indent [51..51)
         |Ident(z) [64..65)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |InfixLF [66..67)
         |Ident(&&) [71..73)
         |Ident(w) [74..75)
         |KwThen [80..84)
         |Ident(c) [85..86)
         |KwElse [87..91)
         |Ident(d) [92..93)
         |Indentation.Outdent [93..93)
         |Indentation.Outdent [93..93)
         |EOF [94..94)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (a && foo {
         |    bar {
         |      z
         |    }
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tinfix(
            tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tapply(tname("bar"), blk(tname("z")))))),
            "&&",
            tname("w"),
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and nested colon arguments, the arguments on the next lines, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |          bar:
         |            z
         |
         |      && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(bar) [47..50)
         |Colon [50..51)
         |Indentation.Indent [51..51)
         |Ident(z) [64..65)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |InfixLF [66..67)
         |Ident(&&) [73..75)
         |Ident(w) [76..77)
         |KwThen [82..86)
         |Ident(c) [87..88)
         |KwElse [89..93)
         |Ident(d) [94..95)
         |Indentation.Outdent [95..95)
         |Indentation.Outdent [95..95)
         |EOF [96..96)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (a && foo {
         |    bar {
         |      z
         |    }
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tinfix(
            tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tapply(tname("bar"), blk(tname("z")))))),
            "&&",
            tname("w"),
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and three nested colon arguments, the arguments on the next lines, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |          bar:
         |            baz:
         |              z
         |
         |    && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(bar) [47..50)
         |Colon [50..51)
         |Indentation.Indent [51..51)
         |Ident(baz) [64..67)
         |Colon [67..68)
         |Indentation.Indent [68..68)
         |Ident(z) [83..84)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |InfixLF [85..86)
         |Ident(&&) [90..92)
         |Ident(w) [93..94)
         |KwThen [99..103)
         |Ident(c) [104..105)
         |KwElse [106..110)
         |Ident(d) [111..112)
         |Indentation.Outdent [112..112)
         |LF [112..113)
         |KwDef [115..118)
         |Ident(g) [119..120)
         |Equals [121..122)
         |Constant.Int(1) [123..124)
         |Indentation.Outdent [124..124)
         |EOF [125..125)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (a && foo {
         |    bar {
         |      baz {
         |        z
         |      }
         |    }
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(
                tname("a"),
                "&&",
                tapply(tname("foo"), blk(tapply(tname("bar"), blk(tapply(tname("baz"), blk(tname("z"))))))),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with two `try` and a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if try try foo:
         |        z
         |
         |    && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |KwTry [31..34)
         |Ident(foo) [35..38)
         |Colon [38..39)
         |Indentation.Indent [39..39)
         |Ident(z) [48..49)
         |Indentation.Outdent [50..50)
         |InfixLF [50..51)
         |Ident(&&) [55..57)
         |Ident(w) [58..59)
         |KwThen [64..68)
         |Ident(c) [69..70)
         |KwElse [71..75)
         |Ident(d) [76..77)
         |Indentation.Outdent [77..77)
         |Indentation.Outdent [77..77)
         |EOF [78..78)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (try try foo {
         |    z
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          Term.Try(
            Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            None,
            None,
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the condition in parentheses, the argument on the next line, a blank line, leading `||` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if (a) && foo:
         |        z
         |
         |    || w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Ident(&&) [31..33)
         |Ident(foo) [34..37)
         |Colon [37..38)
         |Indentation.Indent [38..38)
         |Ident(z) [47..48)
         |Indentation.Outdent [49..49)
         |InfixLF [49..50)
         |Ident(||) [54..56)
         |Ident(w) [57..58)
         |KwThen [63..67)
         |Ident(c) [68..69)
         |KwElse [70..74)
         |Ident(d) [75..76)
         |Indentation.Outdent [76..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (a && foo {
         |    z
         |  } || w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "||", tname("w")),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `while`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while foo:
         |        z
         |
         |    && w
         |    do b
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Ident(foo) [30..33)
         |Colon [33..34)
         |Indentation.Indent [34..34)
         |Ident(z) [43..44)
         |Indentation.Outdent [45..45)
         |InfixLF [45..46)
         |Ident(&&) [50..52)
         |Ident(w) [53..54)
         |KwDo [59..61)
         |Ident(b) [62..63)
         |LF [63..64)
         |Ident(g) [68..69)
         |Indentation.Outdent [69..69)
         |Indentation.Outdent [69..69)
         |EOF [70..70)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (foo {
         |      z
         |    } && w) b
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), tname("b")),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` in a case body with `&&` and a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line, next case") {
    val code =
      """|object A:
         |  def f = x match
         |    case 1 =>
         |      if a && foo:
         |          z
         |
         |      && w
         |      then c else d
         |    case _ => e
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(x) [20..21)
         |KwMatch [22..27)
         |Indentation.Indent [27..27)
         |KwCase [32..36)
         |Constant.Int(1) [37..38)
         |RightArrow [39..41)
         |Indentation.Indent [41..41)
         |KwIf [48..50)
         |Ident(a) [51..52)
         |Ident(&&) [53..55)
         |Ident(foo) [56..59)
         |Colon [59..60)
         |Indentation.Indent [60..60)
         |Ident(z) [71..72)
         |Indentation.Outdent [73..73)
         |InfixLF [73..74)
         |Ident(&&) [80..82)
         |Ident(w) [83..84)
         |KwThen [91..95)
         |Ident(c) [96..97)
         |KwElse [98..102)
         |Ident(d) [103..104)
         |Indentation.Outdent [104..104)
         |LF [104..105)
         |KwCase [109..113)
         |Underscore [114..115)
         |RightArrow [116..118)
         |Ident(e) [119..120)
         |Indentation.Outdent [120..120)
         |Indentation.Outdent [120..120)
         |EOF [121..121)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = x match {
         |    case 1 =>
         |      if (a && foo {
         |        z
         |      } && w) c else d
         |    case _ =>
         |      e
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tmatch(
          tname("x"),
          Case(
            int(1),
            None,
            tif(
              tinfix(
                tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))),
                "&&",
                tname("w"),
              ),
              tname("c"),
              tname("d"),
            ),
          ),
          Case(patwildcard, None, tname("e")),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` in a case body within braces with `&&` and a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f = xs.map {
         |    case 1 =>
         |      if a && foo:
         |          z
         |
         |      && w
         |      then c else d
         |  }
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Ident(xs) [20..22)
         |Dot [22..23)
         |Ident(map) [23..26)
         |LeftBrace [27..28)
         |KwCase [33..37)
         |Constant.Int(1) [38..39)
         |RightArrow [40..42)
         |KwIf [49..51)
         |Ident(a) [52..53)
         |Ident(&&) [54..56)
         |Ident(foo) [57..60)
         |Colon [60..61)
         |Indentation.Indent [61..61)
         |Ident(z) [72..73)
         |Indentation.Outdent [74..74)
         |InfixLF [74..75)
         |Ident(&&) [81..83)
         |Ident(w) [84..85)
         |KwThen [92..96)
         |Ident(c) [97..98)
         |KwElse [99..103)
         |Ident(d) [104..105)
         |LF [105..106)
         |RightBrace [108..109)
         |Indentation.Outdent [109..109)
         |EOF [110..110)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = xs.map {
         |    case 1 =>
         |      if (a && foo {
         |        z
         |      } && w) c else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect(tname("xs"), "map"),
          Term.PartialFunction(List(Case(
            int(1),
            None,
            tif(
              tinfix(
                tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))),
                "&&",
                tname("w"),
              ),
              tname("c"),
              tname("d"),
            ),
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument, the argument on the next line, a blank line, a comment line at the column of the argument, leading `&&` at the column of the `if`, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |        z
         |
         |        // c
         |    && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(z) [45..46)
         |Indentation.Outdent [60..60)
         |InfixLF [60..61)
         |Ident(&&) [65..67)
         |Ident(w) [68..69)
         |KwThen [74..78)
         |Ident(c) [79..80)
         |KwElse [81..85)
         |Ident(d) [86..87)
         |Indentation.Outdent [87..87)
         |Indentation.Outdent [87..87)
         |EOF [88..88)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (a && foo {
         |    z
         |
         |    // c
         |  } && w) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tinfix(
            tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tnameComments("z")()("// c")))),
            "&&",
            tname("w"),
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `match` with a colon argument in a case body, the argument on the next line, a blank line, next case, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    if x match
         |        case 1 => foo:
         |            z
         |
         |        case _ => false
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(x) [27..28)
         |KwMatch [29..34)
         |Indentation.Indent [34..34)
         |KwCase [43..47)
         |Constant.Int(1) [48..49)
         |RightArrow [50..52)
         |Ident(foo) [53..56)
         |Colon [56..57)
         |Indentation.Indent [57..57)
         |Ident(z) [70..71)
         |Indentation.Outdent [72..72)
         |LFLF [72..73)
         |KwCase [81..85)
         |Underscore [86..87)
         |RightArrow [88..90)
         |KwFalse [91..96)
         |Indentation.Outdent [96..96)
         |KwThen [101..105)
         |Ident(c) [106..107)
         |KwElse [108..112)
         |Ident(d) [113..114)
         |Indentation.Outdent [114..114)
         |Indentation.Outdent [114..114)
         |EOF [115..115)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x match {
         |    case 1 =>
         |      foo {
         |        z
         |      }
         |    case _ =>
         |      false
         |  }) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          tmatch(
            tname("x"),
            Case(int(1), None, tapply(tname("foo"), blk(tname("z")))),
            Case(patwildcard, None, bool(false)),
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a colon argument, the argument on the next line, a blank line, `catch` on the next line, `then` and `else` on the line after") {
    val code =
      """|object A:
         |  def f =
         |    if try foo:
         |        z
         |
         |    catch case _ => false
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |Ident(foo) [31..34)
         |Colon [34..35)
         |Indentation.Indent [35..35)
         |Ident(z) [44..45)
         |Indentation.Outdent [46..46)
         |KwCatch [51..56)
         |Indentation.Indent [57..57)
         |KwCase [57..61)
         |Underscore [62..63)
         |RightArrow [64..66)
         |KwFalse [67..72)
         |Indentation.Outdent [72..72)
         |KwThen [77..81)
         |Ident(c) [82..83)
         |KwElse [84..88)
         |Ident(d) [89..90)
         |Indentation.Outdent [90..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (try foo {
         |    z
         |  } catch {
         |    case _ => false
         |  }) c else d
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tif(
          Term.Try(
            tapply(tname("foo"), blk(tname("z"))),
            Some(Term.CasesBlock(List(Case(patwildcard, None, bool(false))))),
            None,
          ),
          tname("c"),
          tname("d"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument, the argument on the next line, a blank line, `then` and `else` at the column of the `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |        z
         |
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(z) [45..46)
         |Indentation.Outdent [47..47)
         |KwThen [52..56)
         |Ident(c) [57..58)
         |KwElse [59..63)
         |Ident(d) [64..65)
         |LF [65..66)
         |Ident(g) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a && foo {
         |      z
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), tname("c"), tname("d")),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `try` and a colon argument, the argument on the next line, a blank line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while try foo:
         |        z
         |
         |    do b
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwTry [30..33)
         |Ident(foo) [34..37)
         |Colon [37..38)
         |Indentation.Indent [38..38)
         |Ident(z) [47..48)
         |Indentation.Outdent [49..49)
         |KwDo [54..56)
         |Ident(b) [57..58)
         |LF [58..59)
         |Ident(g) [63..64)
         |Indentation.Outdent [64..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (try foo {
         |      z
         |    }) b
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(Term.Try(tapply(tname("foo"), blk(tname("z"))), None, None), tname("b")),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` and a colon argument, the argument on the next line, a blank line, shallower leading `&&` on the next line, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while x && foo:
         |        z
         |
         |      && w
         |    do c
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |Ident(x) [30..31)
         |Ident(&&) [32..34)
         |Ident(foo) [35..38)
         |Colon [38..39)
         |Indentation.Indent [39..39)
         |Ident(z) [48..49)
         |Indentation.Outdent [50..50)
         |InfixLF [50..51)
         |Ident(&&) [57..59)
         |Ident(w) [60..61)
         |KwDo [66..68)
         |Ident(c) [69..70)
         |LF [70..71)
         |Ident(g) [75..76)
         |Indentation.Outdent [76..76)
         |Indentation.Outdent [76..76)
         |EOF [77..77)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (x && foo {
         |      z
         |    } && w) c
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tinfix(tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            tname("c"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (x)(y) && foo:
         |        z
         |
         |    && w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(x) [28..29)
         |RightParen [29..30)
         |LeftParen [30..31)
         |Ident(y) [31..32)
         |RightParen [32..33)
         |Ident(&&) [34..36)
         |Ident(foo) [37..40)
         |Colon [40..41)
         |Indentation.Indent [41..41)
         |Ident(z) [50..51)
         |Indentation.Outdent [52..52)
         |InfixLF [52..53)
         |Ident(&&) [57..59)
         |Ident(w) [60..61)
         |KwThen [66..70)
         |Ident(c) [71..72)
         |KwElse [73..77)
         |Ident(d) [78..79)
         |LF [79..80)
         |Ident(g) [84..85)
         |Indentation.Outdent [85..85)
         |Indentation.Outdent [85..85)
         |EOF [86..86)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (x(y) && foo {
         |      z
         |    } && w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon lambda after the parentheses, its body on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f = if (x)(y) && xs.exists: x =>
         |        z
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(xs) [33..35)
         |Dot [35..36)
         |Ident(exists) [36..42)
         |Colon [42..43)
         |Ident(x) [44..45)
         |RightArrow [46..48)
         |Indentation.Indent [48..48)
         |Ident(z) [57..58)
         |Indentation.Outdent [59..59)
         |InfixLF [59..60)
         |Ident(&&) [66..68)
         |Ident(w) [69..70)
         |KwThen [75..79)
         |Ident(c) [80..81)
         |KwElse [82..86)
         |Ident(d) [87..88)
         |LF [88..89)
         |KwDef [91..94)
         |Ident(g) [95..96)
         |Equals [97..98)
         |Constant.Int(1) [99..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && xs.exists {
         |    x => z
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(
                tapply(tname("x"), tname("y")),
                "&&",
                tapply(tselect(tname("xs"), "exists"), blk(tfunc(tparam("x"))(tname("z")))),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and nested colon arguments after the parentheses, the arguments on the next lines, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        bar:
         |            z
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(bar) [46..49)
         |Colon [49..50)
         |Indentation.Indent [50..50)
         |Ident(z) [63..64)
         |Indentation.Outdent [65..65)
         |Indentation.Outdent [65..65)
         |InfixLF [65..66)
         |Ident(&&) [72..74)
         |Ident(w) [75..76)
         |KwThen [81..85)
         |Ident(c) [86..87)
         |KwElse [88..92)
         |Ident(d) [93..94)
         |LF [94..95)
         |KwDef [97..100)
         |Ident(g) [101..102)
         |Equals [103..104)
         |Constant.Int(1) [105..106)
         |Indentation.Outdent [106..106)
         |EOF [107..107)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    bar {
         |      z
         |    }
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(
                tapply(tname("x"), tname("y")),
                "&&",
                tapply(tname("foo"), blk(tapply(tname("bar"), blk(tname("z"))))),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, a comment line, shallower leading `&&` on the next line, `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |
         |      // c
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |Indentation.Outdent [48..48)
         |InfixLF [59..60)
         |Ident(&&) [66..68)
         |Ident(w) [69..70)
         |KwThen [75..79)
         |Ident(c) [80..81)
         |KwElse [82..86)
         |Ident(d) [87..88)
         |LF [88..89)
         |KwDef [91..94)
         |Ident(g) [95..96)
         |Equals [97..98)
         |Constant.Int(1) [99..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z
         |  }
         |    // c
         |    && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              Term.Name.newBuilder("&&").begComment(detachedComments("// c")).result(),
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` on the line after `if`, `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, shallower leading `&&` on the next line, both `then` and `else` on the next lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if
         |      if (x)(y) && foo:
         |          z
         |
         |        && w
         |      then c else d
         |    then e else h
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Indentation.Indent [26..26)
         |KwIf [33..35)
         |LeftParen [36..37)
         |Ident(x) [37..38)
         |RightParen [38..39)
         |LeftParen [39..40)
         |Ident(y) [40..41)
         |RightParen [41..42)
         |Ident(&&) [43..45)
         |Ident(foo) [46..49)
         |Colon [49..50)
         |Indentation.Indent [50..50)
         |Ident(z) [61..62)
         |Indentation.Outdent [63..63)
         |InfixLF [63..64)
         |Ident(&&) [72..74)
         |Ident(w) [75..76)
         |KwThen [83..87)
         |Ident(c) [88..89)
         |KwElse [90..94)
         |Ident(d) [95..96)
         |Indentation.Outdent [96..96)
         |KwThen [101..105)
         |Ident(e) [106..107)
         |KwElse [108..112)
         |Ident(h) [113..114)
         |LF [114..115)
         |Ident(g) [119..120)
         |Indentation.Outdent [120..120)
         |Indentation.Outdent [120..120)
         |EOF [121..121)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (x(y) && foo {
         |      z
         |    } && w) c else d) e else h
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tinfix(
                tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
                "&&",
                tname("w"),
              ),
              tname("c"),
              tname("d"),
            ),
            tname("e"),
            tname("h"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, a shallower expression in parentheses on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (x)(y) && foo:
         |        z
         |
         |      (w)
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(x) [28..29)
         |RightParen [29..30)
         |LeftParen [30..31)
         |Ident(y) [31..32)
         |RightParen [32..33)
         |Ident(&&) [34..36)
         |Ident(foo) [37..40)
         |Colon [40..41)
         |Indentation.Indent [41..41)
         |Ident(z) [50..51)
         |Indentation.Outdent [52..52)
         |LeftParen [59..60)
         |Ident(w) [60..61)
         |RightParen [61..62)
         |KwThen [67..71)
         |Ident(c) [72..73)
         |KwElse [74..78)
         |Ident(d) [79..80)
         |LF [80..81)
         |Ident(g) [85..86)
         |Indentation.Outdent [86..86)
         |Indentation.Outdent [86..86)
         |EOF [87..87)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (x(y) && foo {
         |      z
         |    }(w)) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tapply(tname("foo"), blk(tname("z"))), tname("w")),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: a colon lambda, its body on the next line, a blank line, shallower leading `==` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if xs.exists: x =>
         |        x
         |
         |      == w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(xs) [27..29)
         |Dot [29..30)
         |Ident(exists) [30..36)
         |Colon [36..37)
         |Ident(x) [38..39)
         |RightArrow [40..42)
         |Indentation.Indent [42..42)
         |Ident(x) [51..52)
         |Indentation.Outdent [53..53)
         |InfixLF [53..54)
         |Ident(==) [60..62)
         |Ident(w) [63..64)
         |KwThen [69..73)
         |Ident(c) [74..75)
         |KwElse [76..80)
         |Ident(d) [81..82)
         |LF [82..83)
         |Ident(g) [87..88)
         |Indentation.Outdent [88..88)
         |Indentation.Outdent [88..88)
         |EOF [89..89)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (xs.exists {
         |      x => x
         |    } == w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tapply(tselect(tname("xs"), "exists"), blk(tfunc(tparam("x"))(tname("x")))),
              "==",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: after a statement with a leading `+` line, `&&` and a colon argument, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line") {
    val code =
      """|object A:
         |  def f =
         |    val v = a
         |      + b
         |    if x && foo:
         |        z
         |
         |      && w
         |    then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(v) [28..29)
         |Equals [30..31)
         |Ident(a) [32..33)
         |InfixLF [33..34)
         |Ident(+) [40..41)
         |Ident(b) [42..43)
         |LF [43..44)
         |KwIf [48..50)
         |Ident(x) [51..52)
         |Ident(&&) [53..55)
         |Ident(foo) [56..59)
         |Colon [59..60)
         |Indentation.Indent [60..60)
         |Ident(z) [69..70)
         |Indentation.Outdent [71..71)
         |InfixLF [71..72)
         |Ident(&&) [78..80)
         |Ident(w) [81..82)
         |KwThen [87..91)
         |Ident(c) [92..93)
         |KwElse [94..98)
         |Ident(d) [99..100)
         |Indentation.Outdent [100..100)
         |Indentation.Outdent [100..100)
         |EOF [101..101)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    val v = a + b
         |    if (x && foo {
         |      z
         |    } && w) c else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
          tif(
            tinfix(tinfix(tname("x"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
            tname("c"),
            tname("d"),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: top-level `def` with `if`, `&&` and a colon argument, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line") {
    val code =
      """|def f = if a && foo:
         |    z
         |
         |  && w
         |  then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |KwIf [8..10)
         |Ident(a) [11..12)
         |Ident(&&) [13..15)
         |Ident(foo) [16..19)
         |Colon [19..20)
         |Indentation.Indent [20..20)
         |Ident(z) [25..26)
         |Indentation.Outdent [27..27)
         |InfixLF [27..28)
         |Ident(&&) [30..32)
         |Ident(w) [33..34)
         |KwThen [37..41)
         |Ident(c) [42..43)
         |KwElse [44..48)
         |Ident(d) [49..50)
         |EOF [51..51)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = if (a && foo {
         |  z
         |} && w) c else d
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z")))), "&&", tname("w")),
        tname("c"),
        tname("d"),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: top-level `def` with `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line") {
    val code =
      """|def f = if (x)(y) && foo:
         |    z
         |
         |  && w
         |  then c else d
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |KwIf [8..10)
         |LeftParen [11..12)
         |Ident(x) [12..13)
         |RightParen [13..14)
         |LeftParen [14..15)
         |Ident(y) [15..16)
         |RightParen [16..17)
         |Ident(&&) [18..20)
         |Ident(foo) [21..24)
         |Colon [24..25)
         |Indentation.Indent [25..25)
         |Ident(z) [30..31)
         |Indentation.Outdent [32..32)
         |InfixLF [32..33)
         |Ident(&&) [35..37)
         |Ident(w) [38..39)
         |KwThen [42..46)
         |Ident(c) [47..48)
         |KwElse [49..53)
         |Ident(d) [54..55)
         |EOF [56..56)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|def f = if (x(y) && foo {
         |  z
         |} && w) c else d
         |""".stripMargin
    val tree = Source(List(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      tif(
        tinfix(
          tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
          "&&",
          tname("w"),
        ),
        tname("c"),
        tname("d"),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: top-level `val` with `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next definition") {
    val code =
      """|val r = if (x)(y) && foo:
         |    z
         |
         |  && w
         |  then c else d
         |def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwVal [0..3)
         |Ident(r) [4..5)
         |Equals [6..7)
         |KwIf [8..10)
         |LeftParen [11..12)
         |Ident(x) [12..13)
         |RightParen [13..14)
         |LeftParen [14..15)
         |Ident(y) [15..16)
         |RightParen [16..17)
         |Ident(&&) [18..20)
         |Ident(foo) [21..24)
         |Colon [24..25)
         |Indentation.Indent [25..25)
         |Ident(z) [30..31)
         |Indentation.Outdent [32..32)
         |InfixLF [32..33)
         |Ident(&&) [35..37)
         |Ident(w) [38..39)
         |KwThen [42..46)
         |Ident(c) [47..48)
         |KwElse [49..53)
         |Ident(d) [54..55)
         |LF [55..56)
         |KwDef [56..59)
         |Ident(g) [60..61)
         |Equals [62..63)
         |Constant.Int(1) [64..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|val r = if (x(y) && foo {
         |  z
         |} && w) c else d
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Val(
        Nil,
        List(patvar("r")),
        None,
        tif(
          tinfix(
            tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
            "&&",
            tname("w"),
          ),
          tname("c"),
          tname("d"),
        ),
      ),
      Defn.Def(Nil, tname("g"), Nil, None, int(1)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `if`, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if try foo:
         |        z
         |
         |    && w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |Ident(foo) [31..34)
         |Colon [34..35)
         |Indentation.Indent [35..35)
         |Ident(z) [44..45)
         |Indentation.Outdent [46..46)
         |InfixLF [46..47)
         |Ident(&&) [51..53)
         |Ident(w) [54..55)
         |KwThen [60..64)
         |Ident(c) [65..66)
         |KwElse [67..71)
         |Ident(d) [72..73)
         |LF [73..74)
         |Ident(g) [78..79)
         |Indentation.Outdent [79..79)
         |Indentation.Outdent [79..79)
         |EOF [80..80)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (try foo {
         |      z
         |    } && w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a colon argument, the argument on the next line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next definition") {
    val code =
      """|object A:
         |  def f = if try foo:
         |        z
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |KwTry [23..26)
         |Ident(foo) [27..30)
         |Colon [30..31)
         |Indentation.Indent [31..31)
         |Ident(z) [40..41)
         |Indentation.Outdent [42..42)
         |InfixLF [42..43)
         |Ident(&&) [49..51)
         |Ident(w) [52..53)
         |KwThen [58..62)
         |Ident(c) [63..64)
         |KwElse [65..69)
         |Ident(d) [70..71)
         |LF [71..72)
         |KwDef [74..77)
         |Ident(g) [78..79)
         |Equals [80..81)
         |Constant.Int(1) [82..83)
         |Indentation.Outdent [83..83)
         |EOF [84..84)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (try foo {
         |    z
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `try` and a colon argument, the argument on the next line, a blank line, leading `&&` at the column of the `while`, `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while try foo:
         |        z
         |
         |    && w
         |    do c
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |KwTry [30..33)
         |Ident(foo) [34..37)
         |Colon [37..38)
         |Indentation.Indent [38..38)
         |Ident(z) [47..48)
         |Indentation.Outdent [49..49)
         |InfixLF [49..50)
         |Ident(&&) [54..56)
         |Ident(w) [57..58)
         |KwDo [63..65)
         |Ident(c) [66..67)
         |LF [67..68)
         |Ident(g) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    while (try foo {
         |      z
         |    } && w) c
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            tname("c"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a comment line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |        // c
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z
         |      // c
         |      && w
         |  }) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tnameComments("z")()("// c"), "&&", tname("w")))),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    parseAndCheckTree[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses in braces, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line at the column of the `if`, next statement") {
    val code =
      """|object A {
         |  def f = {
         |    if (x)(y) && foo:
         |        z
         |      && w
         |    then c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    if (x(y) && foo {
         |      z && w
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses in an indented block, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line at the column of the `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (x)(y) && foo:
         |        z
         |      && w
         |    then c else d
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    if (x(y) && foo {
         |      z && w
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses after a statement in an indented block, the argument on the next line, shallower leading `&&` on the next line, `then` and `else` on the next line at the column of the `if`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    e
         |    if (x)(y) && foo:
         |        z
         |      && w
         |    then c else d
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    e
         |    if (x(y) && foo {
         |      z && w
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tname("e"),
          tif(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with `&&` and a colon argument after the parentheses in an indented block, the argument on the next line, shallower leading `&&` on the next line, `do` on the next line at the column of the `while`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while (x)(y) && foo:
         |        z
         |      && w
         |    do c
         |    g
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    while (x(y) && foo {
         |      z && w
         |    }) c
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          Term.While(
            tinfix(
              tapply(tname("x"), tname("y")),
              "&&",
              tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
            ),
            tname("c"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `while` with parentheses at the line end, an argument in parentheses with `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while (a)
         |      (x) do g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |LeftParen [40..41)
         |Ident(x) [41..42)
         |RightParen [42..43)
         |KwDo [44..46)
         |Ident(g) [47..48)
         |Indentation.Outdent [48..48)
         |LF [48..49)
         |Ident(h) [53..54)
         |Indentation.Outdent [54..54)
         |Indentation.Outdent [54..54)
         |EOF [55..55)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `do` found
         |      (x) do g
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end, an argument in braces with `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (a)
         |      { x } then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Indentation.Indent [30..30)
         |LeftBrace [37..38)
         |Ident(x) [39..40)
         |RightBrace [41..42)
         |KwThen [43..47)
         |Constant.Int(1) [48..49)
         |KwElse [50..54)
         |Constant.Int(2) [55..56)
         |Indentation.Outdent [56..56)
         |LF [56..57)
         |Ident(g) [61..62)
         |Indentation.Outdent [62..62)
         |Indentation.Outdent [62..62)
         |EOF [63..63)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `then` found
         |      { x } then 1 else 2
         |            ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `while` with parentheses at the line end, an argument in braces with `do` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while (a)
         |      { x } do g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |LeftBrace [40..41)
         |Ident(x) [42..43)
         |RightBrace [44..45)
         |KwDo [46..48)
         |Ident(g) [49..50)
         |Indentation.Outdent [50..50)
         |LF [50..51)
         |Ident(h) [55..56)
         |Indentation.Outdent [56..56)
         |Indentation.Outdent [56..56)
         |EOF [57..57)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `do` found
         |      { x } do g
         |            ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end, an argument in parentheses on the next line, `then` and `else` on the line after it, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (a)
         |      (x)
         |    then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Indentation.Indent [30..30)
         |LeftParen [37..38)
         |Ident(x) [38..39)
         |RightParen [39..40)
         |Indentation.Outdent [40..40)
         |KwThen [45..49)
         |Constant.Int(1) [50..51)
         |KwElse [52..56)
         |Constant.Int(2) [57..58)
         |LF [58..59)
         |Ident(g) [63..64)
         |Indentation.Outdent [64..64)
         |Indentation.Outdent [64..64)
         |EOF [65..65)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `while` with parentheses at the line end, an argument in parentheses on the next line, `do` on the line after it, next statement") {
    val code =
      """|object A:
         |  def f =
         |    while (a)
         |      (x)
         |    do g
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwWhile [24..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |LeftParen [40..41)
         |Ident(x) [41..42)
         |RightParen [42..43)
         |Indentation.Outdent [43..43)
         |LF [43..44)
         |KwDo [48..50)
         |Ident(g) [51..52)
         |LF [52..53)
         |Ident(h) [57..58)
         |Indentation.Outdent [58..58)
         |Indentation.Outdent [58..58)
         |EOF [59..59)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `while` expected but `identifier` found
         |    h
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end, a comment line, an argument in parentheses with `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if (a)
         |      // c
         |      (x) then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |LeftParen [27..28)
         |Ident(a) [28..29)
         |RightParen [29..30)
         |Indentation.Indent [41..41)
         |LeftParen [48..49)
         |Ident(x) [49..50)
         |RightParen [50..51)
         |KwThen [52..56)
         |Constant.Int(1) [57..58)
         |KwElse [59..63)
         |Constant.Int(2) [64..65)
         |Indentation.Outdent [65..65)
         |LF [65..66)
         |Ident(g) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |      (x) then 1 else 2
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end in braces, an argument in parentheses with `then` and `else` on the next line, next statement") {
    val code =
      """|object A {
         |  def f = {
         |    if (a)
         |      (x) then 1 else 2
         |    g
         |  }
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |LeftBrace [9..10)
         |KwDef [13..16)
         |Ident(f) [17..18)
         |Equals [19..20)
         |LeftBrace [21..22)
         |KwIf [27..29)
         |LeftParen [30..31)
         |Ident(a) [31..32)
         |RightParen [32..33)
         |Indentation.Indent [33..33)
         |LeftParen [40..41)
         |Ident(x) [41..42)
         |RightParen [42..43)
         |KwThen [44..48)
         |Constant.Int(1) [49..50)
         |KwElse [51..55)
         |Constant.Int(2) [56..57)
         |Indentation.Outdent [57..57)
         |LF [57..58)
         |Ident(g) [62..63)
         |LF [63..64)
         |RightBrace [66..67)
         |LF [67..68)
         |RightBrace [68..69)
         |EOF [70..70)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `outdent` expected but `then` found
         |      (x) then 1 else 2
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end after a statement in an indented block, an argument in parentheses with `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    e
         |    if (a)
         |      (x) then 1 else 2
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(e) [24..25)
         |LF [25..26)
         |KwIf [30..32)
         |LeftParen [33..34)
         |Ident(a) [34..35)
         |RightParen [35..36)
         |Indentation.Indent [36..36)
         |LeftParen [43..44)
         |Ident(x) [44..45)
         |RightParen [45..46)
         |KwThen [47..51)
         |Constant.Int(1) [52..53)
         |KwElse [54..58)
         |Constant.Int(2) [59..60)
         |Indentation.Outdent [60..60)
         |LF [60..61)
         |Ident(g) [65..66)
         |Indentation.Outdent [66..66)
         |Indentation.Outdent [66..66)
         |EOF [67..67)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |      (x) then 1 else 2
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with parentheses at the line end on the line of the `def`, an argument in parentheses with `then` and `else` on the next line at a column before the `if`, next definition") {
    val code =
      """|object A:
         |  def f = if (a)
         |    (x) then 1 else 2
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(a) [24..25)
         |RightParen [25..26)
         |Indentation.Indent [26..26)
         |LeftParen [31..32)
         |Ident(x) [32..33)
         |RightParen [33..34)
         |KwThen [35..39)
         |Constant.Int(1) [40..41)
         |KwElse [42..46)
         |Constant.Int(2) [47..48)
         |Indentation.Outdent [48..48)
         |LF [48..49)
         |KwDef [51..54)
         |Ident(g) [55..56)
         |Equals [57..58)
         |Constant.Int(1) [59..60)
         |Indentation.Outdent [60..60)
         |EOF [61..61)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:3: error: `outdent` expected but `then` found
         |    (x) then 1 else 2
         |        ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `while` with `==` after the parentheses in an indented `val` body, `do` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      while (a) == b
         |    do g
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwWhile [38..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(==) [48..50)
         |Ident(b) [51..52)
         |Indentation.Outdent [52..52)
         |LF [52..53)
         |KwDo [57..59)
         |Ident(g) [60..61)
         |LF [61..62)
         |Ident(r) [66..67)
         |Indentation.Outdent [67..67)
         |Indentation.Outdent [67..67)
         |EOF [68..68)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `while` expected but `identifier` found
         |    r
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` without parentheses with `==` in an indented `val` body, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      if a == b
         |    then 1 else 2
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwIf [38..40)
         |Ident(a) [41..42)
         |Ident(==) [43..45)
         |Ident(b) [46..47)
         |Indentation.Outdent [47..47)
         |KwThen [52..56)
         |Constant.Int(1) [57..58)
         |KwElse [59..63)
         |Constant.Int(2) [64..65)
         |LF [65..66)
         |Ident(r) [70..71)
         |Indentation.Outdent [71..71)
         |Indentation.Outdent [71..71)
         |EOF [72..72)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `then` expected but `outdent` found
         |      if a == b
         |               ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `while` without parentheses with `==` in an indented `val` body, `do` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      while a == b
         |    do g
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwWhile [38..43)
         |Ident(a) [44..45)
         |Ident(==) [46..48)
         |Ident(b) [49..50)
         |Indentation.Outdent [50..50)
         |LF [50..51)
         |KwDo [55..57)
         |Ident(g) [58..59)
         |LF [59..60)
         |Ident(r) [64..65)
         |Indentation.Outdent [65..65)
         |Indentation.Outdent [65..65)
         |EOF [66..66)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: `do` expected but `outdent` found
         |      while a == b
         |                  ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with `==` after the parentheses in an indented `val` body, a blank line, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      if (a) == b
         |
         |    then 1 else 2
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwIf [38..40)
         |LeftParen [41..42)
         |Ident(a) [42..43)
         |RightParen [43..44)
         |Ident(==) [45..47)
         |Ident(b) [48..49)
         |Indentation.Outdent [50..50)
         |KwThen [55..59)
         |Constant.Int(1) [60..61)
         |KwElse [62..66)
         |Constant.Int(2) [67..68)
         |LF [68..69)
         |Ident(r) [73..74)
         |Indentation.Outdent [74..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with `==` after the parentheses in an indented `val` body, a comment line, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      if (a) == b
         |      // c
         |    then 1 else 2
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwIf [38..40)
         |LeftParen [41..42)
         |Ident(a) [42..43)
         |RightParen [43..44)
         |Ident(==) [45..47)
         |Ident(b) [48..49)
         |Indentation.Outdent [60..60)
         |KwThen [65..69)
         |Constant.Int(1) [70..71)
         |KwElse [72..76)
         |Constant.Int(2) [77..78)
         |LF [78..79)
         |Ident(r) [83..84)
         |Indentation.Outdent [84..84)
         |Indentation.Outdent [84..84)
         |EOF [85..85)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with `==` after the parentheses in an indented `val` body in braces, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A {
         |  def f = {
         |    val r =
         |      if (a) == b
         |    then 1 else 2
         |    r
         |  }
         |}
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |LeftBrace [9..10)
         |KwDef [13..16)
         |Ident(f) [17..18)
         |Equals [19..20)
         |LeftBrace [21..22)
         |KwVal [27..30)
         |Ident(r) [31..32)
         |Equals [33..34)
         |Indentation.Indent [34..34)
         |KwIf [41..43)
         |LeftParen [44..45)
         |Ident(a) [45..46)
         |RightParen [46..47)
         |Ident(==) [48..50)
         |Ident(b) [51..52)
         |Indentation.Outdent [52..52)
         |KwThen [57..61)
         |Constant.Int(1) [62..63)
         |KwElse [64..68)
         |Constant.Int(2) [69..70)
         |LF [70..71)
         |Ident(r) [75..76)
         |LF [76..77)
         |RightBrace [79..80)
         |LF [80..81)
         |RightBrace [81..82)
         |EOF [83..83)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `}` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: a statement and an `if` with `==` after the parentheses in an indented `val` body, `then` and `else` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      e
         |      if (a) == b
         |    then 1 else 2
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |Ident(e) [38..39)
         |LF [39..40)
         |KwIf [46..48)
         |LeftParen [49..50)
         |Ident(a) [50..51)
         |RightParen [51..52)
         |Ident(==) [53..55)
         |Ident(b) [56..57)
         |Indentation.Outdent [57..57)
         |KwThen [62..66)
         |Constant.Int(1) [67..68)
         |KwElse [69..73)
         |Constant.Int(2) [74..75)
         |LF [75..76)
         |Ident(r) [80..81)
         |Indentation.Outdent [81..81)
         |Indentation.Outdent [81..81)
         |EOF [82..82)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:6: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` with `==` after the parentheses in an indented `def` body, `then` and `else` on the next line at the column of the `def`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    def h =
         |      if (a) == b
         |    then 1 else 2
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwDef [24..27)
         |Ident(h) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwIf [38..40)
         |LeftParen [41..42)
         |Ident(a) [42..43)
         |RightParen [43..44)
         |Ident(==) [45..47)
         |Ident(b) [48..49)
         |Indentation.Outdent [49..49)
         |KwThen [54..58)
         |Constant.Int(1) [59..60)
         |KwElse [61..65)
         |Constant.Int(2) [66..67)
         |LF [67..68)
         |Ident(h) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:5: error: `outdent` expected but `then` found
         |    then 1 else 2
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `for` with a guard with `==` after the parentheses in an indented `val` body, `yield` on the next line at the column of the `val`, next statement") {
    val code =
      """|object A:
         |  def f =
         |    val r =
         |      for x <- xs if (a) == b
         |    yield x
         |    r
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwVal [24..27)
         |Ident(r) [28..29)
         |Equals [30..31)
         |Indentation.Indent [31..31)
         |KwFor [38..41)
         |Ident(x) [42..43)
         |LeftArrow [44..46)
         |Ident(xs) [47..49)
         |KwIf [50..52)
         |LeftParen [53..54)
         |Ident(a) [54..55)
         |RightParen [55..56)
         |Ident(==) [57..59)
         |Ident(b) [60..61)
         |Indentation.Outdent [61..61)
         |KwYield [66..71)
         |Ident(x) [72..73)
         |LF [73..74)
         |Ident(r) [78..79)
         |Indentation.Outdent [79..79)
         |Indentation.Outdent [79..79)
         |EOF [80..80)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:4: error: illegal start of simple expression
         |      for x <- xs if (a) == b
         |                             ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("nested condition: `if` on a deeper statement line, the condition on the next line at the column of the `if`, a continuation line, a colon argument, leading `&&` at the column of the continuation") {
    val code =
      """|object A:
         |  def f =
         |    a
         |      if
         |      x
         |        + y && foo:
         |            z
         |        && w
         |      then c else d
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = {
         |    a
         |    if (x + y && foo {
         |      z
         |    } && w) c else d
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tname("a"),
          tif(
            tinfix(
              tinfix(tinfix(tname("x"), "+", tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, shallower leading `&&` on the next line, a blank line, leading `||` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |      && w
         |
         |      || v
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z && w
         |  } || v) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(
                tapply(tname("x"), tname("y")),
                "&&",
                tapply(tname("foo"), blk(tinfix(tname("z"), "&&", tname("w")))),
              ),
              "||",
              tname("v"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line with a comment, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z // c
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |Indentation.Outdent [53..53)
         |Ident(&&) [60..62)
         |Ident(w) [63..64)
         |KwThen [69..73)
         |Ident(c) [74..75)
         |KwElse [76..80)
         |Ident(d) [81..82)
         |LF [82..83)
         |KwDef [85..88)
         |Ident(g) [89..90)
         |Equals [91..92)
         |Constant.Int(1) [93..94)
         |Indentation.Outdent [94..94)
         |EOF [95..95)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z // c
         |
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(
                tapply(tname("x"), tname("y")),
                "&&",
                tapply(tname("foo"), blk(tnameComments("z")()("// c"))),
              ),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, two blank lines, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |Indentation.Outdent [49..49)
         |Ident(&&) [56..58)
         |Ident(w) [59..60)
         |KwThen [65..69)
         |Ident(c) [70..71)
         |KwElse [72..76)
         |Ident(d) [77..78)
         |LF [78..79)
         |KwDef [81..84)
         |Ident(g) [85..86)
         |Equals [87..88)
         |Constant.Int(1) [89..90)
         |Indentation.Outdent [90..90)
         |EOF [91..91)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z
         |  } && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument after the parentheses, the argument on the next line, a blank line, a comment line, a blank line, shallower leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f = if (x)(y) && foo:
         |        z
         |
         |      // c
         |
         |      && w
         |    then c else d
         |  def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |KwIf [20..22)
         |LeftParen [23..24)
         |Ident(x) [24..25)
         |RightParen [25..26)
         |LeftParen [26..27)
         |Ident(y) [27..28)
         |RightParen [28..29)
         |Ident(&&) [30..32)
         |Ident(foo) [33..36)
         |Colon [36..37)
         |Indentation.Indent [37..37)
         |Ident(z) [46..47)
         |Indentation.Outdent [48..48)
         |Ident(&&) [67..69)
         |Ident(w) [70..71)
         |KwThen [76..80)
         |Ident(c) [81..82)
         |KwElse [83..87)
         |Ident(d) [88..89)
         |LF [89..90)
         |KwDef [92..95)
         |Ident(g) [96..97)
         |Equals [98..99)
         |Constant.Int(1) [100..101)
         |Indentation.Outdent [101..101)
         |EOF [102..102)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = if (x(y) && foo {
         |    z
         |  }
         |    // c
         |
         |    && w) c else d
         |  def g = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("f"),
          Nil,
          None,
          tif(
            tinfix(
              tinfix(tapply(tname("x"), tname("y")), "&&", tapply(tname("foo"), blk(tname("z")))),
              Term.Name.newBuilder("&&").begComment(detachedComments("// c")).result(),
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
        ),
        Defn.Def(Nil, tname("g"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a colon argument, the argument on the next line, two blank lines, leading `&&` at the column of the `if`, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if try foo:
         |        z
         |
         |
         |    && w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |Ident(foo) [31..34)
         |Colon [34..35)
         |Indentation.Indent [35..35)
         |Ident(z) [44..45)
         |Indentation.Outdent [47..47)
         |Ident(&&) [52..54)
         |Ident(w) [55..56)
         |KwThen [61..65)
         |Ident(c) [66..67)
         |KwElse [68..72)
         |Ident(d) [73..74)
         |LF [74..75)
         |Ident(g) [79..80)
         |Indentation.Outdent [80..80)
         |Indentation.Outdent [80..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (try foo {
         |      z
         |    } && w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with an `if` whose `then` body holds `try` and a colon argument, the argument on the next line, a blank line, leading `&&` on the next line, `else` and the outer `then` on later lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if a then try foo:
         |          z
         |
         |        && w
         |      else d
         |    then c else e
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |KwThen [32..36)
         |KwTry [37..40)
         |Ident(foo) [41..44)
         |Colon [44..45)
         |Indentation.Indent [45..45)
         |Ident(z) [56..57)
         |Indentation.Outdent [58..58)
         |InfixLF [58..59)
         |Ident(&&) [67..69)
         |Ident(w) [70..71)
         |KwElse [78..82)
         |Ident(d) [83..84)
         |KwThen [89..93)
         |Ident(c) [94..95)
         |KwElse [96..100)
         |Ident(e) [101..102)
         |LF [102..103)
         |Ident(g) [107..108)
         |Indentation.Outdent [108..108)
         |Indentation.Outdent [108..108)
         |EOF [109..109)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) try foo {
         |      z
         |    } && w else d) c else e
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tname("a"),
              Term.Try(tinfix(tapply(tname("foo"), blk(tname("z"))), "&&", tname("w")), None, None),
              tname("d"),
            ),
            tname("c"),
            tname("e"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with an `if` whose `then` body holds `try`, a deeper line in parentheses, `else` and the outer `then` on later lines, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if if a then try b
         |          (c)
         |      else d
         |    then e else k
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |KwThen [32..36)
         |KwTry [37..40)
         |Ident(b) [41..42)
         |LeftParen [53..54)
         |Ident(c) [54..55)
         |RightParen [55..56)
         |KwElse [63..67)
         |Ident(d) [68..69)
         |KwThen [74..78)
         |Ident(e) [79..80)
         |KwElse [81..85)
         |Ident(k) [86..87)
         |LF [87..88)
         |Ident(g) [92..93)
         |Indentation.Outdent [93..93)
         |Indentation.Outdent [93..93)
         |EOF [94..94)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) try b(c) else d) e else k
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(tname("a"), Term.Try(tapply(tname("b"), tname("c")), None, None), tname("d")),
            tname("e"),
            tname("k"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `if` with `try` and a `catch` case on its line, a colon argument in the case body, the argument on the next line, a blank line, leading `&&` on the next line, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if try a catch case _: E => foo:
         |            z
         |
         |      && w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwTry [27..30)
         |Ident(a) [31..32)
         |KwCatch [33..38)
         |Indentation.Indent [39..39)
         |KwCase [39..43)
         |Underscore [44..45)
         |Colon [45..46)
         |Ident(E) [47..48)
         |RightArrow [49..51)
         |Ident(foo) [52..55)
         |Colon [55..56)
         |Indentation.Indent [56..56)
         |Ident(z) [69..70)
         |Indentation.Outdent [71..71)
         |LFLF [71..72)
         |Ident(&&) [78..80)
         |Ident(w) [81..82)
         |Indentation.Outdent [82..82)
         |KwThen [87..91)
         |Ident(c) [92..93)
         |KwElse [94..98)
         |Ident(d) [99..100)
         |LF [100..101)
         |Ident(g) [105..106)
         |Indentation.Outdent [106..106)
         |Indentation.Outdent [106..106)
         |EOF [107..107)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (try a catch {
         |      case _: E =>
         |        foo {
         |          z
         |        }
         |        &&w
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Try(
              tname("a"),
              List(Case(
                Pat.Typed(Pat.Wildcard(), pname("E")),
                None,
                blk(tapply(tname("foo"), blk(tname("z"))), tpostfix(tname("&&"), "w")),
              )),
              None,
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `&&` and a colon argument, the argument on the next line ending in `;`, `+` and an expression between the columns of the `if` and the argument, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if a && foo:
         |        z;
         |      + w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(a) [27..28)
         |Ident(&&) [29..31)
         |Ident(foo) [32..35)
         |Colon [35..36)
         |Indentation.Indent [36..36)
         |Ident(z) [45..46)
         |Semicolon [46..47)
         |InfixLF [47..48)
         |Ident(+) [54..55)
         |Ident(w) [56..57)
         |Indentation.Outdent [57..57)
         |KwThen [62..66)
         |Ident(c) [67..68)
         |KwElse [69..73)
         |Ident(d) [74..75)
         |LF [75..76)
         |Ident(g) [80..81)
         |Indentation.Outdent [81..81)
         |Indentation.Outdent [81..81)
         |EOF [82..82)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (a && foo {
         |      z
         |      +w
         |    }) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(tname("a"), "&&", tapply(tname("foo"), blk(tname("z"), unary("+", tname("w"))))),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `match` with cases on the next lines, leading `&&` between the columns of the `if` and the cases, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if x match
         |        case 1 => true
         |        case _ => false
         |      && w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |Ident(x) [27..28)
         |KwMatch [29..34)
         |Indentation.Indent [34..34)
         |KwCase [43..47)
         |Constant.Int(1) [48..49)
         |RightArrow [50..52)
         |KwTrue [53..57)
         |LF [57..58)
         |KwCase [66..70)
         |Underscore [71..72)
         |RightArrow [73..75)
         |KwFalse [76..81)
         |Indentation.Outdent [81..81)
         |InfixLF [81..82)
         |Ident(&&) [88..90)
         |Ident(w) [91..92)
         |KwThen [97..101)
         |Ident(c) [102..103)
         |KwElse [104..108)
         |Ident(d) [109..110)
         |LF [110..111)
         |Ident(g) [115..116)
         |Indentation.Outdent [116..116)
         |Indentation.Outdent [116..116)
         |EOF [117..117)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if ((x match {
         |      case 1 => true
         |      case _ => false
         |    }) && w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              tmatch(tname("x"), Case(int(1), None, bool(true)), Case(patwildcard, None, bool(false))),
              "&&",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("nested condition: `new` with a template body after a colon, the body on the next line, leading `==` between the columns of the `if` and the body, `then` and `else` on the next line, next statement") {
    val code =
      """|object A:
         |  def f =
         |    if new B:
         |         def z = 1
         |       == w
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwNew [27..30)
         |Ident(B) [31..32)
         |Colon [32..33)
         |Indentation.Indent [33..33)
         |KwDef [43..46)
         |Ident(z) [47..48)
         |Equals [49..50)
         |Constant.Int(1) [51..52)
         |Indentation.Outdent [52..52)
         |InfixLF [52..53)
         |Ident(==) [60..62)
         |Ident(w) [63..64)
         |KwThen [69..73)
         |Ident(c) [74..75)
         |KwElse [76..80)
         |Ident(d) [81..82)
         |LF [82..83)
         |Ident(g) [87..88)
         |Indentation.Outdent [88..88)
         |Indentation.Outdent [88..88)
         |EOF [89..89)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (new B { def z = 1 } == w) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tinfix(
              Term.NewAnonymous(
                tpl(List(init(pname("B"))), List(Defn.Def(Nil, tname("z"), Nil, None, int(1)))),
              ),
              "==",
              tname("w"),
            ),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

}
