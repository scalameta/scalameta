package scala.meta.tests.semanticdb

class PatternDesugaringSuite extends SemanticdbSuite {

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  private def check(
      name: String,
      code: String,
  )(occurrences: String, symbols: String, synthetics: String)(implicit loc: munit.Location): Unit =
    test(name) {
      val payload = computePayloadFromSnippet(code)
      assertNoDiff(computeSectionFromPayload(payload, "Occurrences"), occurrences)
      assertNoDiff(computeSectionFromPayload(payload, "Symbols"), symbols)
      assertNoDiff(computeSectionFromPayload(payload, "Synthetics"), synthetics)
    }

  symbols(
    "tuple pattern in a method",
    """|object a {
       |  def m(): Int = {
       |    val (x, y) = (1, 2)
       |    x + y
       |  }
       |}
       |""".stripMargin,
    """|_empty_/a. => final object a extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/a.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "tuple pattern in a method",
    """|object a {
       |  def m(): Int = {
       |    val (x, y) = (1, 2)
       |    x + y
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): a <= _empty_/a.
       |[1:6..1:7): m <= _empty_/a.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:9..2:10): x <= local0
       |[2:12..2:13): y <= local1
       |[3:4..3:5): x => local0
       |[3:6..3:7): + => scala/Int#`+`(+4).
       |[3:8..3:9): y => local1
       |""".stripMargin,
  )

  symbols(
    "extractor pattern in a method",
    """|object b {
       |  def m(): Int = {
       |    val Some(s) = Some(1)
       |    s
       |  }
       |}
       |""".stripMargin,
    """|_empty_/b. => final object b extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/b.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local s: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "extractor pattern in a method",
    """|object b {
       |  def m(): Int = {
       |    val Some(s) = Some(1)
       |    s
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): b <= _empty_/b.
       |[1:6..1:7): m <= _empty_/b.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:8..2:12): Some => scala/Some.
       |[2:13..2:14): s <= local0
       |[2:18..2:22): Some => scala/Some.
       |[3:4..3:5): s => local0
       |""".stripMargin,
  )

  symbols(
    "sequence pattern in a method",
    """|object c {
       |  def m(): Int = {
       |    val List(x, y) = List(1, 2)
       |    x + y
       |  }
       |}
       |""".stripMargin,
    """|_empty_/c. => final object c extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/c.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "sequence pattern in a method",
    """|object c {
       |  def m(): Int = {
       |    val List(x, y) = List(1, 2)
       |    x + y
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:8): c <= _empty_/c.
         |[1:6..1:7): m <= _empty_/c.m().
         |[1:11..1:14): Int => scala/Int#
         |[2:8..2:12): List => scala/collection/immutable/List.
         |[2:13..2:14): x <= local0
         |[2:16..2:17): y <= local1
         |[2:21..2:25): List => scala/collection/immutable/List.
         |[3:4..3:5): x => local0
         |[3:6..3:7): + => scala/Int#`+`(+4).
         |[3:8..3:9): y => local1
         |""".stripMargin
    else
      """|[0:7..0:8): c <= _empty_/c.
         |[1:6..1:7): m <= _empty_/c.m().
         |[1:11..1:14): Int => scala/Int#
         |[2:8..2:12): List => scala/package.List.
         |[2:13..2:14): x <= local0
         |[2:16..2:17): y <= local1
         |[2:21..2:25): List => scala/package.List.
         |[3:4..3:5): x => local0
         |[3:6..3:7): + => scala/Int#`+`(+4).
         |[3:8..3:9): y => local1
         |""".stripMargin,
  )

  symbols(
    "nested extractor pattern in a method",
    """|object d {
       |  def m(): Int = {
       |    val Some(Some(s)) = Some(Some(1))
       |    s
       |  }
       |}
       |""".stripMargin,
    """|_empty_/d. => final object d extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/d.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local s: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "nested extractor pattern in a method",
    """|object d {
       |  def m(): Int = {
       |    val Some(Some(s)) = Some(Some(1))
       |    s
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): d <= _empty_/d.
       |[1:6..1:7): m <= _empty_/d.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:8..2:12): Some => scala/Some.
       |[2:13..2:17): Some => scala/Some.
       |[2:18..2:19): s <= local0
       |[2:24..2:28): Some => scala/Some.
       |[2:29..2:33): Some => scala/Some.
       |[3:4..3:5): s => local0
       |""".stripMargin,
  )

  symbols(
    "tuple inside an extractor in a method",
    """|object e {
       |  def m(): Int = {
       |    val Some((x, y)) = Some((1, 2))
       |    x + y
       |  }
       |}
       |""".stripMargin,
    """|_empty_/e. => final object e extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/e.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "tuple inside an extractor in a method",
    """|object e {
       |  def m(): Int = {
       |    val Some((x, y)) = Some((1, 2))
       |    x + y
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): e <= _empty_/e.
       |[1:6..1:7): m <= _empty_/e.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:8..2:12): Some => scala/Some.
       |[2:14..2:15): x <= local0
       |[2:17..2:18): y <= local1
       |[2:23..2:27): Some => scala/Some.
       |[3:4..3:5): x => local0
       |[3:6..3:7): + => scala/Int#`+`(+4).
       |[3:8..3:9): y => local1
       |""".stripMargin,
  )

  symbols(
    "typed binder in an extractor in a method",
    """|object f {
       |  def m(): Int = {
       |    val Some(s: Int) = Option[Any](1)
       |    s
       |  }
       |}
       |""".stripMargin,
    """|_empty_/f. => final object f extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/f.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local s: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "typed binder in an extractor in a method",
    """|object f {
       |  def m(): Int = {
       |    val Some(s: Int) = Option[Any](1)
       |    s
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): f <= _empty_/f.
       |[1:6..1:7): m <= _empty_/f.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:8..2:12): Some => scala/Some.
       |[2:13..2:14): s <= local0
       |[2:16..2:19): Int => scala/Int#
       |[2:23..2:29): Option => scala/Option.
       |[2:30..2:33): Any => scala/Any#
       |[3:4..3:5): s => local0
       |""".stripMargin,
  )

  symbols(
    "typed binder in a tuple in a method",
    """|object g {
       |  def m(): Int = {
       |    val (x: Int, y) = (1, 2)
       |    x + y
       |  }
       |}
       |""".stripMargin,
    """|_empty_/g. => final object g extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/g.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "typed binder in a tuple in a method",
    """|object g {
       |  def m(): Int = {
       |    val (x: Int, y) = (1, 2)
       |    x + y
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): g <= _empty_/g.
       |[1:6..1:7): m <= _empty_/g.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:9..2:10): x <= local0
       |[2:12..2:15): Int => scala/Int#
       |[2:17..2:18): y <= local1
       |[3:4..3:5): x => local0
       |[3:6..3:7): + => scala/Int#`+`(+4).
       |[3:8..3:9): y => local1
       |""".stripMargin,
  )

  occurrences(
    "refinement types in patterns with an access qualifier",
    """|object rb {
       |  private[rb] val Some(a: AnyRef { val x: Int }) = Some(new AnyRef { val x = 1 })
       |  private[rb] val Some(b: AnyRef { def y: Int }) = Some(new AnyRef { def y = 1 })
       |}
       |""".stripMargin,
    """|[0:7..0:9): rb <= _empty_/rb.
       |[1:10..1:12): rb => _empty_/rb.
       |[1:18..1:22): Some => scala/Some.
       |[1:23..1:24): a <= _empty_/rb.a.
       |[1:26..1:32): AnyRef => scala/AnyRef#
       |[1:39..1:40): x <= local2
       |[1:42..1:45): Int => scala/Int#
       |[1:51..1:55): Some => scala/Some.
       |[1:60..1:60):  <= local1
       |[1:60..1:66): AnyRef => scala/AnyRef#
       |[1:66..1:66):  => java/lang/Object#`<init>`().
       |[1:73..1:74): x <= local0
       |[2:10..2:12): rb => _empty_/rb.
       |[2:18..2:22): Some => scala/Some.
       |[2:23..2:24): b <= _empty_/rb.b.
       |[2:26..2:32): AnyRef => scala/AnyRef#
       |[2:39..2:40): y <= local5
       |[2:42..2:45): Int => scala/Int#
       |[2:51..2:55): Some => scala/Some.
       |[2:60..2:60):  <= local4
       |[2:60..2:66): AnyRef => scala/AnyRef#
       |[2:66..2:66):  => java/lang/Object#`<init>`().
       |[2:73..2:74): y <= local3
       |""".stripMargin,
  )

  occurrences(
    "patterns in types of pattern definitions",
    """|object rf {
       |  class ann(x: Any) extends scala.annotation.StaticAnnotation
       |  val (a: Int @ann((1, 2) match { case (b, c) => b })) = 1
       |  val (d, e): (Int, Int) @ann({ class L { private[L] val (f, g) = (1, 2) }; new L().hashCode }) = (1, 2)
       |}
       |""".stripMargin,
    """|[0:7..0:9): rf <= _empty_/rf.
       |[1:8..1:11): ann <= _empty_/rf.ann#
       |[1:11..1:11):  <= _empty_/rf.ann#`<init>`().
       |[1:12..1:13): x <= _empty_/rf.ann#`<init>`().(x)
       |[1:12..1:13): x <= _empty_/rf.ann#x.
       |[1:15..1:18): Any => scala/Any#
       |[1:28..1:33): scala => scala/
       |[1:34..1:44): annotation => scala/annotation/
       |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
       |[2:7..2:8): a <= _empty_/rf.a.
       |[2:10..2:13): Int => scala/Int#
       |[2:15..2:18): ann => _empty_/rf.ann#
       |[2:18..2:18):  => _empty_/rf.ann#`<init>`().
       |[2:40..2:41): b <= local0
       |[2:43..2:44): c <= local1
       |[2:49..2:50): b => local0
       |[3:7..3:8): d <= _empty_/rf.d.
       |[3:10..3:11): e <= _empty_/rf.e.
       |[3:15..3:18): Int => scala/Int#
       |[3:20..3:23): Int => scala/Int#
       |[3:26..3:29): ann => _empty_/rf.ann#
       |[3:29..3:29):  => _empty_/rf.ann#`<init>`().
       |[3:38..3:39): L <= local2
       |[3:40..3:40):  <= local3
       |[3:50..3:51): L => local2
       |[3:58..3:59): f <= local5
       |[3:61..3:62): g <= local6
       |[3:80..3:81): L => local2
       |[3:81..3:81):  => local3
       |[3:84..3:92): hashCode => java/lang/Object#hashCode().
       |""".stripMargin,
  )

  occurrences(
    "refinement types in patterns of definitions, a case and a for",
    """|object rg {
       |  private[rg] val Some(a: AnyRef { var x: Int }) = Some(new AnyRef { var x = 1 })
       |  private[rg] val Some(b: AnyRef { val x, y: Int }) = Some(new AnyRef { val x = 1; val y = 2 })
       |  private[rg] val (c: AnyRef { val x: Int }) = new AnyRef { val x = 1 }
       |  val Some(d: AnyRef { val x: Int }) = Some(new AnyRef { val x = 1 })
       |  Option(Option(new AnyRef { val x = 1 })) match { case Some(Some(e: AnyRef { val x: Int })) => e }
       |  for (Some(f: AnyRef { val x: Int }) <- Option(Option(new AnyRef { val x = 1 }))) yield f
       |  def m = { val Some(g: AnyRef { val x: Int }) = Some(new AnyRef { val x = 1 }); g }
       |}
       |""".stripMargin,
    """|[0:7..0:9): rg <= _empty_/rg.
       |[1:10..1:12): rg => _empty_/rg.
       |[1:18..1:22): Some => scala/Some.
       |[1:23..1:24): a <= _empty_/rg.a.
       |[1:26..1:32): AnyRef => scala/AnyRef#
       |[1:39..1:40): x <= local4
       |[1:42..1:45): Int => scala/Int#
       |[1:51..1:55): Some => scala/Some.
       |[1:60..1:60):  <= local3
       |[1:60..1:66): AnyRef => scala/AnyRef#
       |[1:66..1:66):  => java/lang/Object#`<init>`().
       |[1:73..1:74): x <= local0
       |[2:10..2:12): rg => _empty_/rg.
       |[2:18..2:22): Some => scala/Some.
       |[2:23..2:24): b <= _empty_/rg.b.
       |[2:26..2:32): AnyRef => scala/AnyRef#
       |[2:39..2:40): x <= local10
       |[2:42..2:43): y <= local11
       |[2:45..2:48): Int => scala/Int#
       |[2:54..2:58): Some => scala/Some.
       |[2:63..2:63):  <= local9
       |[2:63..2:69): AnyRef => scala/AnyRef#
       |[2:69..2:69):  => java/lang/Object#`<init>`().
       |[2:76..2:77): x <= local7
       |[2:87..2:88): y <= local8
       |[3:10..3:12): rg => _empty_/rg.
       |[3:19..3:20): c <= _empty_/rg.c.
       |[3:22..3:28): AnyRef => scala/AnyRef#
       |[3:35..3:36): x <= local14
       |[3:38..3:41): Int => scala/Int#
       |[3:51..3:51):  <= local12
       |[3:51..3:57): AnyRef => scala/AnyRef#
       |[3:57..3:57):  => java/lang/Object#`<init>`().
       |[3:64..3:65): x <= local13
       |[4:6..4:10): Some => scala/Some.
       |[4:11..4:12): d <= _empty_/rg.d.
       |[4:14..4:20): AnyRef => scala/AnyRef#
       |[4:27..4:28): x <= local17
       |[4:30..4:33): Int => scala/Int#
       |[4:39..4:43): Some => scala/Some.
       |[4:48..4:48):  <= local16
       |[4:48..4:54): AnyRef => scala/AnyRef#
       |[4:54..4:54):  => java/lang/Object#`<init>`().
       |[4:61..4:62): x <= local15
       |[5:2..5:8): Option => scala/Option.
       |[5:9..5:15): Option => scala/Option.
       |[5:20..5:20):  <= local19
       |[5:20..5:26): AnyRef => scala/AnyRef#
       |[5:26..5:26):  => java/lang/Object#`<init>`().
       |[5:33..5:34): x <= local18
       |[5:56..5:60): Some => scala/Some.
       |[5:61..5:65): Some => scala/Some.
       |[5:66..5:67): e <= local20
       |[5:69..5:75): AnyRef => scala/AnyRef#
       |[5:82..5:83): x <= local21
       |[5:85..5:88): Int => scala/Int#
       |[5:96..5:97): e => local20
       |[6:7..6:11): Some => scala/Some.
       |[6:12..6:13): f <= local26
       |[6:15..6:21): AnyRef => scala/AnyRef#
       |[6:28..6:29): x <= local27
       |[6:31..6:34): Int => scala/Int#
       |[6:41..6:47): Option => scala/Option.
       |[6:48..6:54): Option => scala/Option.
       |[6:59..6:59):  <= local25
       |[6:59..6:65): AnyRef => scala/AnyRef#
       |[6:65..6:65):  => java/lang/Object#`<init>`().
       |[6:72..6:73): x <= local23
       |[6:89..6:90): f => local26
       |[7:6..7:7): m <= _empty_/rg.m().
       |[7:16..7:20): Some => scala/Some.
       |[7:21..7:22): g <= local31
       |[7:24..7:30): AnyRef => scala/AnyRef#
       |[7:37..7:38): x <= local28
       |[7:40..7:43): Int => scala/Int#
       |[7:49..7:53): Some => scala/Some.
       |[7:58..7:58):  <= local30
       |[7:58..7:64): AnyRef => scala/AnyRef#
       |[7:64..7:64):  => java/lang/Object#`<init>`().
       |[7:71..7:72): x <= local29
       |[7:81..7:82): g => local31
       |""".stripMargin,
  )

  test("a qualified pattern definition in a type of a qualified pattern") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object rh {
         |  class ann(x: Any) extends scala.annotation.StaticAnnotation
         |  private[rh] val (a: Int @ann({ class L { private[L] val (b, c) = (1, 2) }; 1 }), d) = (1, 2)
         |}
         |""".stripMargin,
      "Occurrences",
    )
    assertNoDiff(
      load(),
      """|[0:7..0:9): rh <= _empty_/rh.
         |[1:8..1:11): ann <= _empty_/rh.ann#
         |[1:11..1:11):  <= _empty_/rh.ann#`<init>`().
         |[1:12..1:13): x <= _empty_/rh.ann#`<init>`().(x)
         |[1:12..1:13): x <= _empty_/rh.ann#x.
         |[1:15..1:18): Any => scala/Any#
         |[1:28..1:33): scala => scala/
         |[1:34..1:44): annotation => scala/annotation/
         |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
         |[2:10..2:12): rh => _empty_/rh.
         |[2:19..2:20): a <= _empty_/rh.a.
         |[2:22..2:25): Int => scala/Int#
         |[2:27..2:30): ann => _empty_/rh.ann#
         |[2:30..2:30):  => _empty_/rh.ann#`<init>`().
         |[2:39..2:40): L <= local0
         |[2:41..2:41):  <= local5
         |[2:51..2:52): L => local0
         |[2:59..2:60): b <= local3
         |[2:62..2:63): c <= local4
         |[2:83..2:84): d <= _empty_/rh.d.
         |""".stripMargin,
    )
  }

  occurrences(
    "typed pattern in parens in a template",
    """|object tp1 {
       |  val (a: Option[Int]) = None
       |  var (b: Int) = 1
       |  lazy val (c: Int) = 1
       |  val ((d: Int)) = 1
       |  val (e /* c */ : Int) = 1
       |  val (f: Int), (g: Int) = 1
       |  a.get + b + c + d + e + f + g
       |}
       |""".stripMargin,
    """|[0:7..0:10): tp1 <= _empty_/tp1.
       |[1:7..1:8): a <= _empty_/tp1.a.
       |[1:10..1:16): Option => scala/Option#
       |[1:17..1:20): Int => scala/Int#
       |[1:25..1:29): None => scala/None.
       |[2:7..2:8): b <= _empty_/tp1.b().
       |[2:10..2:13): Int => scala/Int#
       |[3:12..3:13): c <= _empty_/tp1.c.
       |[3:15..3:18): Int => scala/Int#
       |[4:8..4:9): d <= _empty_/tp1.d.
       |[4:11..4:14): Int => scala/Int#
       |[5:7..5:8): e <= _empty_/tp1.e.
       |[5:19..5:22): Int => scala/Int#
       |[6:7..6:8): f <= _empty_/tp1.f.
       |[6:10..6:13): Int => scala/Int#
       |[6:17..6:18): g <= _empty_/tp1.g.
       |[6:20..6:23): Int => scala/Int#
       |[7:2..7:3): a => _empty_/tp1.a.
       |[7:4..7:7): get => scala/Option#get().
       |[7:8..7:9): + => scala/Int#`+`(+4).
       |[7:10..7:11): b => _empty_/tp1.b().
       |[7:12..7:13): + => scala/Int#`+`(+4).
       |[7:14..7:15): c => _empty_/tp1.c.
       |[7:16..7:17): + => scala/Int#`+`(+4).
       |[7:18..7:19): d => _empty_/tp1.d.
       |[7:20..7:21): + => scala/Int#`+`(+4).
       |[7:22..7:23): e => _empty_/tp1.e.
       |[7:24..7:25): + => scala/Int#`+`(+4).
       |[7:26..7:27): f => _empty_/tp1.f.
       |[7:28..7:29): + => scala/Int#`+`(+4).
       |[7:30..7:31): g => _empty_/tp1.g.
       |""".stripMargin,
  )

  occurrences(
    "typed pattern in parens in a method",
    """|object tp2 {
       |  def m(): Int = {
       |    val (a: Int) = 1
       |    var (b: Int) = 1
       |    lazy val (c: Int) = 1
       |    a + b + c
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:10): tp2 <= _empty_/tp2.
       |[1:6..1:7): m <= _empty_/tp2.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:9..2:10): a <= local0
       |[2:12..2:15): Int => scala/Int#
       |[3:9..3:10): b <= local1
       |[3:12..3:15): Int => scala/Int#
       |[4:14..4:15): c <= local2
       |[4:17..4:20): Int => scala/Int#
       |[5:4..5:5): a => local0
       |[5:6..5:7): + => scala/Int#`+`(+4).
       |[5:8..5:9): b => local1
       |[5:10..5:11): + => scala/Int#`+`(+4).
       |[5:12..5:13): c => local2
       |""".stripMargin,
  )

  occurrences(
    "typed pattern in parens in a local class",
    """|object tp3 {
       |  def m(): Int = {
       |    class L { val (a: Int) = 1; var (b: Int) = 1 }
       |    new L().a + new L().b
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:10): tp3 <= _empty_/tp3.
       |[1:6..1:7): m <= _empty_/tp3.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:10..2:11): L <= local0
       |[2:12..2:12):  <= local1
       |[2:19..2:20): a <= local2
       |[2:22..2:25): Int => scala/Int#
       |[2:37..2:38): b <= local3
       |[2:40..2:43): Int => scala/Int#
       |[3:8..3:9): L => local0
       |[3:9..3:9):  => local1
       |[3:12..3:13): a => local2
       |[3:14..3:15): + => scala/Int#`+`(+4).
       |[3:20..3:21): L => local0
       |[3:21..3:21):  => local1
       |[3:24..3:25): b => local3
       |""".stripMargin,
  )

  occurrences(
    "typed pattern in parens in a for and a case",
    """|object tp4 {
       |  for ((x: Int) <- Option(1); (y: Int) = x) yield y
       |  1 match { case (z: Int) => z }
       |}
       |""".stripMargin,
    """|[0:7..0:10): tp4 <= _empty_/tp4.
       |[1:8..1:9): x <= local1
       |[1:11..1:14): Int => scala/Int#
       |[1:19..1:25): Option => scala/Option.
       |[1:31..1:32): y <= local3
       |[1:34..1:37): Int => scala/Int#
       |[1:41..1:42): x => local1
       |[1:50..1:51): y => local3
       |[2:18..2:19): z <= local4
       |[2:21..2:24): Int => scala/Int#
       |[2:29..2:30): z => local4
       |""".stripMargin,
  )

  occurrences(
    "typed pattern in parens with an access qualifier",
    """|object tp5 {
       |  protected[tp5] val (a: Int) = 1
       |  private[tp5] var (b: Int) = 1
       |  private[tp5] val ((c: Int)) = 1
       |  protected[tp5] lazy val (d: Int) = 1
       |}
       |""".stripMargin,
    """|[0:7..0:10): tp5 <= _empty_/tp5.
       |[1:12..1:15): tp5 => _empty_/tp5.
       |[1:22..1:23): a <= _empty_/tp5.a.
       |[1:25..1:28): Int => scala/Int#
       |[2:10..2:13): tp5 => _empty_/tp5.
       |[2:20..2:21): b <= _empty_/tp5.b().
       |[2:23..2:26): Int => scala/Int#
       |[3:10..3:13): tp5 => _empty_/tp5.
       |[3:21..3:22): c <= _empty_/tp5.c.
       |[3:24..3:27): Int => scala/Int#
       |[4:12..4:15): tp5 => _empty_/tp5.
       |[4:27..4:28): d <= _empty_/tp5.d.
       |[4:30..4:33): Int => scala/Int#
       |""".stripMargin,
  )

  check(
    "typed pattern in parens with a line break or a comment by the name",
    """|object tp6 {
       |  val (a // c
       |    : Int) = 1
       |  val (b
       |    : Int) = 1
       |  val ( // c
       |    c: Int) = 1
       |  def m = {
       |    val (d // c
       |      : Int) = 1
       |    val (e
       |      : Int) = 1
       |    val ( // c
       |      f: Int) = 1
       |    val (g /* c */ : Int) = 1
       |    val ( /* c */ h: Int) = 1
       |    d + e + f + g + h
       |  }
       |}
       |""".stripMargin,
  )(
    """|[0:7..0:10): tp6 <= _empty_/tp6.
       |[1:7..1:8): a <= _empty_/tp6.a.
       |[2:6..2:9): Int => scala/Int#
       |[3:7..3:8): b <= _empty_/tp6.b.
       |[4:6..4:9): Int => scala/Int#
       |[6:4..6:5): c <= _empty_/tp6.c.
       |[6:7..6:10): Int => scala/Int#
       |[7:6..7:7): m <= _empty_/tp6.m().
       |[8:9..8:10): d <= local0
       |[9:8..9:11): Int => scala/Int#
       |[10:9..10:10): e <= local1
       |[11:8..11:11): Int => scala/Int#
       |[13:6..13:7): f <= local2
       |[13:9..13:12): Int => scala/Int#
       |[14:9..14:10): g <= local3
       |[14:21..14:24): Int => scala/Int#
       |[15:18..15:19): h <= local4
       |[15:21..15:24): Int => scala/Int#
       |[16:4..16:5): d => local0
       |[16:6..16:7): + => scala/Int#`+`(+4).
       |[16:8..16:9): e => local1
       |[16:10..16:11): + => scala/Int#`+`(+4).
       |[16:12..16:13): f => local2
       |[16:14..16:15): + => scala/Int#`+`(+4).
       |[16:16..16:17): g => local3
       |[16:18..16:19): + => scala/Int#`+`(+4).
       |[16:20..16:21): h => local4
       |""".stripMargin,
    """|_empty_/tp6. => final object tp6 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/tp6.a. => val method a: Int
       |  Int => scala/Int#
       |_empty_/tp6.b. => val method b: Int
       |  Int => scala/Int#
       |_empty_/tp6.c. => val method c: Int
       |  Int => scala/Int#
       |_empty_/tp6.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => val local d: Int
       |  Int => scala/Int#
       |local1 => val local e: Int
       |  Int => scala/Int#
       |local2 => val local f: Int
       |  Int => scala/Int#
       |local3 => val local g: Int
       |  Int => scala/Int#
       |local4 => val local h: Int
       |  Int => scala/Int#
       |""".stripMargin,
    "",
  )

  check(
    "typed pattern in parens with a line break or a comment by the name, in copies",
    """|object tp7 {
       |  def p1(x: Int = {
       |    val (a // c
       |      : Int) = 1
       |    val (b
       |      : Int) = 1
       |    val ( // c
       |      c: Int) = 1
       |    val (d /* c */ : Int) = 1
       |    val ( /* c */ e: Int) = 1
       |    a + b + c + d + e
       |  }) = x
       |  def p2 = {
       |    val v, w = {
       |      val (a // c
       |        : Int) = 1
       |      val (b
       |        : Int) = 1
       |      val ( // c
       |        c: Int) = 1
       |      a + b + c
       |    }
       |    v + w
       |  }
       |  val p3: PartialFunction[Int, Int] = {
       |    case x if {
       |          val (a // c
       |            : Int) = 1
       |          val (b
       |            : Int) = 1
       |          val ( // c
       |            c: Int) = 1
       |          a + b + c
       |        } > x => x
       |  }
       |}
       |""".stripMargin,
  )(
    """|[0:7..0:10): tp7 <= _empty_/tp7.
       |[1:6..1:8): p1 <= _empty_/tp7.p1().
       |[1:9..1:10): x <= _empty_/tp7.p1().(x)
       |[1:12..1:15): Int => scala/Int#
       |[2:9..2:10): a <= local0
       |[3:8..3:11): Int => scala/Int#
       |[4:9..4:10): b <= local1
       |[5:8..5:11): Int => scala/Int#
       |[7:6..7:7): c <= local2
       |[7:9..7:12): Int => scala/Int#
       |[8:9..8:10): d <= local3
       |[8:21..8:24): Int => scala/Int#
       |[9:18..9:19): e <= local4
       |[9:21..9:24): Int => scala/Int#
       |[10:4..10:5): a => local0
       |[10:6..10:7): + => scala/Int#`+`(+4).
       |[10:8..10:9): b => local1
       |[10:10..10:11): + => scala/Int#`+`(+4).
       |[10:12..10:13): c => local2
       |[10:14..10:15): + => scala/Int#`+`(+4).
       |[10:16..10:17): d => local3
       |[10:18..10:19): + => scala/Int#`+`(+4).
       |[10:20..10:21): e => local4
       |[11:7..11:8): x => _empty_/tp7.p1().(x)
       |[12:6..12:8): p2 <= _empty_/tp7.p2().
       |[13:8..13:9): v <= local8
       |[13:11..13:12): w <= local9
       |[14:11..14:12): a <= local5
       |[15:10..15:13): Int => scala/Int#
       |[16:11..16:12): b <= local6
       |[17:10..17:13): Int => scala/Int#
       |[19:8..19:9): c <= local7
       |[19:11..19:14): Int => scala/Int#
       |[20:6..20:7): a => local5
       |[20:8..20:9): + => scala/Int#`+`(+4).
       |[20:10..20:11): b => local6
       |[20:12..20:13): + => scala/Int#`+`(+4).
       |[20:14..20:15): c => local7
       |[22:4..22:5): v => local8
       |[22:6..22:7): + => scala/Int#`+`(+4).
       |[22:8..22:9): w => local9
       |[24:6..24:8): p3 <= _empty_/tp7.p3.
       |[24:10..24:25): PartialFunction => scala/PartialFunction#
       |[24:26..24:29): Int => scala/Int#
       |[24:31..24:34): Int => scala/Int#
       |[24:38..24:38):  <= local10
       |[25:9..25:10): x <= local17
       |[26:15..26:16): a <= local18
       |[27:14..27:17): Int => scala/Int#
       |[28:15..28:16): b <= local19
       |[29:14..29:17): Int => scala/Int#
       |[31:12..31:13): c <= local20
       |[31:15..31:18): Int => scala/Int#
       |[32:10..32:11): a => local18
       |[32:12..32:13): + => scala/Int#`+`(+4).
       |[32:14..32:15): b => local19
       |[32:16..32:17): + => scala/Int#`+`(+4).
       |[32:18..32:19): c => local20
       |[33:10..33:11): > => scala/Int#`>`(+3).
       |[33:12..33:13): x => local17
       |[33:17..33:18): x => local17
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/tp7. => final object tp7 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/tp7.p1$default$1(). => synthetic method p1$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/tp7.p1(). => method p1(x: Int): Int
         |  x => _empty_/tp7.p1().(x)
         |  Int => scala/Int#
         |_empty_/tp7.p1().(x) => default param x: Int
         |  Int => scala/Int#
         |_empty_/tp7.p2(). => method p2: Int
         |  Int => scala/Int#
         |_empty_/tp7.p3. => val method p3: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => val local a: Int
         |  Int => scala/Int#
         |local1 => val local b: Int
         |  Int => scala/Int#
         |local2 => val local c: Int
         |  Int => scala/Int#
         |local3 => val local d: Int
         |  Int => scala/Int#
         |local4 => val local e: Int
         |  Int => scala/Int#
         |local5 => val local a: Int
         |  Int => scala/Int#
         |local6 => val local b: Int
         |  Int => scala/Int#
         |local7 => val local c: Int
         |  Int => scala/Int#
         |local8 => val local v: Int
         |  Int => scala/Int#
         |local9 => val local w: Int
         |  Int => scala/Int#
         |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local11 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local16
         |  Int => scala/Int#
         |  B1 => local15
         |  x1 => local13
         |  default => local14
         |  Function1 => scala/Function1#
         |local12 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local21
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local13 => synthetic param x1: A1
         |  A1 => local16
         |local14 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local16
         |  B1 => local15
         |local15 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local16 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local17 => val local x: Int
         |  Int => scala/Int#
         |local18 => val local a: Int
         |  Int => scala/Int#
         |local19 => val local b: Int
         |  Int => scala/Int#
         |local20 => val local c: Int
         |  Int => scala/Int#
         |local21 => synthetic param x1: Int
         |  Int => scala/Int#
         |""".stripMargin
    else if (ScalaVersion.atLeast213_17)
      """|_empty_/tp7. => final object tp7 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/tp7.p1$default$1(). => synthetic method p1$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/tp7.p1(). => method p1(x: Int): Int
         |  x => _empty_/tp7.p1().(x)
         |  Int => scala/Int#
         |_empty_/tp7.p1().(x) => default param x: Int
         |  Int => scala/Int#
         |_empty_/tp7.p2(). => method p2: Int
         |  Int => scala/Int#
         |_empty_/tp7.p3. => val method p3: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => val local a: Int
         |  Int => scala/Int#
         |local1 => val local b: Int
         |  Int => scala/Int#
         |local2 => val local c: Int
         |  Int => scala/Int#
         |local3 => val local d: Int
         |  Int => scala/Int#
         |local4 => val local e: Int
         |  Int => scala/Int#
         |local5 => val local a: Int
         |  Int => scala/Int#
         |local6 => val local b: Int
         |  Int => scala/Int#
         |local7 => val local c: Int
         |  Int => scala/Int#
         |local8 => val local v: Int
         |  Int => scala/Int#
         |local9 => val local w: Int
         |  Int => scala/Int#
         |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local11 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local16
         |  Int => scala/Int#
         |  B1 => local15
         |  x1 => local13
         |  default => local14
         |  Function1 => scala/Function1#
         |local12 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local21
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local13 => synthetic param x1: A1
         |  A1 => local16
         |local14 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local16
         |  B1 => local15
         |local15 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local16 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local17 => val local x: Int
         |  Int => scala/Int#
         |local18 => val local a: Int
         |  Int => scala/Int#
         |local19 => val local b: Int
         |  Int => scala/Int#
         |local20 => val local c: Int
         |  Int => scala/Int#
         |local21 => synthetic param x1: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/tp7. => final object tp7 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/tp7.p1$default$1(). => synthetic method p1$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/tp7.p1(). => method p1(x: Int): Int
         |  x => _empty_/tp7.p1().(x)
         |  Int => scala/Int#
         |_empty_/tp7.p1().(x) => default param x: Int
         |  Int => scala/Int#
         |_empty_/tp7.p2(). => method p2: Int
         |  Int => scala/Int#
         |_empty_/tp7.p3. => val method p3: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => val local a: Int
         |  Int => scala/Int#
         |local1 => val local b: Int
         |  Int => scala/Int#
         |local2 => val local c: Int
         |  Int => scala/Int#
         |local3 => val local d: Int
         |  Int => scala/Int#
         |local4 => val local e: Int
         |  Int => scala/Int#
         |local5 => val local a: Int
         |  Int => scala/Int#
         |local6 => val local b: Int
         |  Int => scala/Int#
         |local7 => val local c: Int
         |  Int => scala/Int#
         |local8 => val local v: Int
         |  Int => scala/Int#
         |local9 => val local w: Int
         |  Int => scala/Int#
         |local10 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local11 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local16
         |  Int => scala/Int#
         |  B1 => local15
         |  x1 => local13
         |  default => local14
         |  Function1 => scala/Function1#
         |local12 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local21
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local13 => synthetic param x1: A1
         |  A1 => local16
         |local14 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local16
         |  B1 => local15
         |local15 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local16 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local17 => val local x: Int
         |  Int => scala/Int#
         |local18 => val local a: Int
         |  Int => scala/Int#
         |local19 => val local b: Int
         |  Int => scala/Int#
         |local20 => val local c: Int
         |  Int => scala/Int#
         |local21 => synthetic param x1: Int
         |  Int => scala/Int#
         |""".stripMargin,
    """|[24:38..34:3): {
       |    case x if {
       |          val (a // c
       |            : Int) = 1
       |          val (b
       |            : Int) = 1
       |          val ( // c
       |            c: Int) = 1
       |          a + b + c
       |        } > x => x
       |  } => *.apply
       |  apply => scala/Function1#apply().
       |""".stripMargin,
  )

  occurrences(
    "typed pattern in parens with comment marks in a comment by the name, in copies",
    """|object tp8 {
       |  def p1(x: Int = {
       |    val (a /* c // d */ : Int) = 1
       |    val (b // c */
       |      : Int) = 1
       |    val (c /* c /* d */ e */ : Int) = 1
       |    a + b + c
       |  }) = x
       |}
       |""".stripMargin,
    """|[0:7..0:10): tp8 <= _empty_/tp8.
       |[1:6..1:8): p1 <= _empty_/tp8.p1().
       |[1:9..1:10): x <= _empty_/tp8.p1().(x)
       |[1:12..1:15): Int => scala/Int#
       |[2:9..2:10): a <= local0
       |[2:26..2:29): Int => scala/Int#
       |[3:9..3:10): b <= local1
       |[4:8..4:11): Int => scala/Int#
       |[5:9..5:10): c <= local2
       |[5:31..5:34): Int => scala/Int#
       |[6:4..6:5): a => local0
       |[6:6..6:7): + => scala/Int#`+`(+4).
       |[6:8..6:9): b => local1
       |[6:10..6:11): + => scala/Int#`+`(+4).
       |[6:12..6:13): c => local2
       |[7:7..7:8): x => _empty_/tp8.p1().(x)
       |""".stripMargin,
  )

  occurrences(
    "patterns with an access qualifier",
    """|object pq {
       |  private[pq] val Some(a) = Some(1)
       |  private[pq] val b, c = 1
       |  private[pq] val (d: Int), e = 1
       |  private[pq] val f = 1
       |}
       |""".stripMargin,
    """|[0:7..0:9): pq <= _empty_/pq.
       |[1:10..1:12): pq => _empty_/pq.
       |[1:18..1:22): Some => scala/Some.
       |[1:23..1:24): a <= _empty_/pq.a.
       |[1:28..1:32): Some => scala/Some.
       |[2:10..2:12): pq => _empty_/pq.
       |[2:18..2:19): b <= _empty_/pq.b.
       |[2:21..2:22): c <= _empty_/pq.c.
       |[3:10..3:12): pq => _empty_/pq.
       |[3:19..3:20): d <= _empty_/pq.d.
       |[3:22..3:25): Int => scala/Int#
       |[3:28..3:29): e <= _empty_/pq.e.
       |[4:10..4:12): pq => _empty_/pq.
       |[4:18..4:19): f <= _empty_/pq.f.
       |""".stripMargin,
  )

  occurrences(
    "nested patterns with an access qualifier",
    """|object pn {
       |  object ~ { def unapply(x: Int): Option[(Int, Int)] = Some((x, x)) }
       |  private[pn] val Some(Some(a)) = Some(Some(1))
       |  private[pn] val (b, c) = (1, 2)
       |  private[pn] val d ~ _ = 1
       |}
       |""".stripMargin,
    """|[0:7..0:9): pn <= _empty_/pn.
       |[1:9..1:10): ~ <= _empty_/pn.`~`.
       |[1:17..1:24): unapply <= _empty_/pn.`~`.unapply().
       |[1:25..1:26): x <= _empty_/pn.`~`.unapply().(x)
       |[1:28..1:31): Int => scala/Int#
       |[1:34..1:40): Option => scala/Option#
       |[1:42..1:45): Int => scala/Int#
       |[1:47..1:50): Int => scala/Int#
       |[1:55..1:59): Some => scala/Some.
       |[1:61..1:62): x => _empty_/pn.`~`.unapply().(x)
       |[1:64..1:65): x => _empty_/pn.`~`.unapply().(x)
       |[2:10..2:12): pn => _empty_/pn.
       |[2:18..2:22): Some => scala/Some.
       |[2:23..2:27): Some => scala/Some.
       |[2:28..2:29): a <= _empty_/pn.a.
       |[2:34..2:38): Some => scala/Some.
       |[2:39..2:43): Some => scala/Some.
       |[3:10..3:12): pn => _empty_/pn.
       |[3:19..3:20): b <= _empty_/pn.b.
       |[3:22..3:23): c <= _empty_/pn.c.
       |[4:10..4:12): pn => _empty_/pn.
       |[4:18..4:19): d <= _empty_/pn.d.
       |[4:20..4:21): ~ => _empty_/pn.`~`.
       |""".stripMargin,
  )

  occurrences(
    "extractor and infix patterns with an access qualifier",
    """|object px {
       |  case class ~[A, B](l: A, r: B)
       |  private[px] val Some(Some(Some(a))) = Some(Some(Some(1)))
       |  private[px] val Tuple2(b ~ 0, c) = (new ~(1, 0), 2)
       |  private[px] val Tuple2(d ~ 0, _) = (new ~(1, 0), 2)
       |  private[px] val Some(e) ~ 0 = new ~(Some(1), 0)
       |  private[px] val Some(f) ~ g = new ~(Some(1), 0)
       |  private[px] val h ~ 0 = new ~(1, 0)
       |  private[px] val Some((i, j)) = Some((1, 2))
       |}
       |""".stripMargin,
    """|[0:7..0:9): px <= _empty_/px.
       |[1:13..1:14): ~ <= _empty_/px.`~`#
       |[1:15..1:16): A <= _empty_/px.`~`#[A]
       |[1:18..1:19): B <= _empty_/px.`~`#[B]
       |[1:20..1:20):  <= _empty_/px.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/px.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/px.`~`#l.
       |[1:24..1:25): A => _empty_/px.`~`#[A]
       |[1:27..1:28): r <= _empty_/px.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/px.`~`#r.
       |[1:30..1:31): B => _empty_/px.`~`#[B]
       |[2:10..2:12): px => _empty_/px.
       |[2:18..2:22): Some => scala/Some.
       |[2:23..2:27): Some => scala/Some.
       |[2:28..2:32): Some => scala/Some.
       |[2:33..2:34): a <= _empty_/px.a.
       |[2:40..2:44): Some => scala/Some.
       |[2:45..2:49): Some => scala/Some.
       |[2:50..2:54): Some => scala/Some.
       |[3:10..3:12): px => _empty_/px.
       |[3:18..3:24): Tuple2 => scala/Tuple2.
       |[3:25..3:26): b <= _empty_/px.b.
       |[3:27..3:28): ~ => _empty_/px.`~`.
       |[3:32..3:33): c <= _empty_/px.c.
       |[3:42..3:43): ~ => _empty_/px.`~`#
       |[3:43..3:43):  => _empty_/px.`~`#`<init>`().
       |[4:10..4:12): px => _empty_/px.
       |[4:18..4:24): Tuple2 => scala/Tuple2.
       |[4:25..4:26): d <= _empty_/px.d.
       |[4:27..4:28): ~ => _empty_/px.`~`.
       |[4:42..4:43): ~ => _empty_/px.`~`#
       |[4:43..4:43):  => _empty_/px.`~`#`<init>`().
       |[5:10..5:12): px => _empty_/px.
       |[5:18..5:22): Some => scala/Some.
       |[5:23..5:24): e <= _empty_/px.e.
       |[5:26..5:27): ~ => _empty_/px.`~`.
       |[5:36..5:37): ~ => _empty_/px.`~`#
       |[5:37..5:37):  => _empty_/px.`~`#`<init>`().
       |[5:38..5:42): Some => scala/Some.
       |[6:10..6:12): px => _empty_/px.
       |[6:18..6:22): Some => scala/Some.
       |[6:23..6:24): f <= _empty_/px.f.
       |[6:26..6:27): ~ => _empty_/px.`~`.
       |[6:28..6:29): g <= _empty_/px.g.
       |[6:36..6:37): ~ => _empty_/px.`~`#
       |[6:37..6:37):  => _empty_/px.`~`#`<init>`().
       |[6:38..6:42): Some => scala/Some.
       |[7:10..7:12): px => _empty_/px.
       |[7:18..7:19): h <= _empty_/px.h.
       |[7:20..7:21): ~ => _empty_/px.`~`.
       |[7:30..7:31): ~ => _empty_/px.`~`#
       |[7:31..7:31):  => _empty_/px.`~`#`<init>`().
       |[8:10..8:12): px => _empty_/px.
       |[8:18..8:22): Some => scala/Some.
       |[8:24..8:25): i <= _empty_/px.i.
       |[8:27..8:28): j <= _empty_/px.j.
       |[8:33..8:37): Some => scala/Some.
       |""".stripMargin,
  )

  occurrences(
    "extractor patterns of one variable with an access qualifier",
    """|object p1 {
       |  private[p1] val Tuple2(a, 0) = (1, 0)
       |  protected[p1] val Tuple2(_, b) = (1, 2)
       |  private[p1] val Tuple2(Tuple2(_, c), _) = ((1, 2), 3)
       |  private[p1] val Some(Tuple2(d, _)) = Some((1, 2))
       |  private[p1] val Array(e, _*) = Array(1)
       |  private[p1] val Array(_, f @ _*) = Array(1)
       |  private[p1] val g @ Some(_) = Some(1)
       |}
       |""".stripMargin,
    """|[0:7..0:9): p1 <= _empty_/p1.
       |[1:10..1:12): p1 => _empty_/p1.
       |[1:18..1:24): Tuple2 => scala/Tuple2.
       |[1:25..1:26): a <= _empty_/p1.a.
       |[2:12..2:14): p1 => _empty_/p1.
       |[2:20..2:26): Tuple2 => scala/Tuple2.
       |[2:30..2:31): b <= _empty_/p1.b.
       |[3:10..3:12): p1 => _empty_/p1.
       |[3:18..3:24): Tuple2 => scala/Tuple2.
       |[3:25..3:31): Tuple2 => scala/Tuple2.
       |[3:35..3:36): c <= _empty_/p1.c.
       |[4:10..4:12): p1 => _empty_/p1.
       |[4:18..4:22): Some => scala/Some.
       |[4:23..4:29): Tuple2 => scala/Tuple2.
       |[4:30..4:31): d <= _empty_/p1.d.
       |[4:39..4:43): Some => scala/Some.
       |[5:10..5:12): p1 => _empty_/p1.
       |[5:18..5:23): Array => scala/Array.
       |[5:24..5:25): e <= _empty_/p1.e.
       |[5:33..5:38): Array => scala/Array.
       |[6:10..6:12): p1 => _empty_/p1.
       |[6:18..6:23): Array => scala/Array.
       |[6:27..6:28): f <= _empty_/p1.f.
       |[6:37..6:42): Array => scala/Array.
       |[7:10..7:12): p1 => _empty_/p1.
       |[7:18..7:19): g <= _empty_/p1.g.
       |[7:22..7:26): Some => scala/Some.
       |[7:32..7:36): Some => scala/Some.
       |""".stripMargin,
  )

  occurrences(
    "tuple patterns of one variable with an access qualifier",
    """|object p2 {
       |  private[p2] val (a, 0) = (1, 0)
       |  protected[p2] val (_, b) = (1, 2)
       |  private[p2] val (_, (c, _)) = (1, (2, 3))
       |  private[p2] val (_, Some(d)) = (1, Some(2))
       |  private[p2] val ((e: Int), 0) = (1, 0)
       |  private[p2] val ((f, _)) = (1, 2)
       |}
       |""".stripMargin,
    """|[0:7..0:9): p2 <= _empty_/p2.
       |[1:10..1:12): p2 => _empty_/p2.
       |[1:19..1:20): a <= _empty_/p2.a.
       |[2:12..2:14): p2 => _empty_/p2.
       |[2:24..2:25): b <= _empty_/p2.b.
       |[3:10..3:12): p2 => _empty_/p2.
       |[3:23..3:24): c <= _empty_/p2.c.
       |[4:10..4:12): p2 => _empty_/p2.
       |[4:22..4:26): Some => scala/Some.
       |[4:27..4:28): d <= _empty_/p2.d.
       |[4:37..4:41): Some => scala/Some.
       |[5:10..5:12): p2 => _empty_/p2.
       |[5:20..5:21): e <= _empty_/p2.e.
       |[5:23..5:26): Int => scala/Int#
       |[6:10..6:12): p2 => _empty_/p2.
       |[6:20..6:21): f <= _empty_/p2.f.
       |""".stripMargin,
  )

  occurrences(
    "infix patterns of one variable with an access qualifier",
    """|object p3 {
       |  case class ~[A, B](l: A, r: B)
       |  private[p3] val Tuple2(a, _) ~ 0 = new ~((1, 2), 0)
       |  private[p3] val (b, _) ~ 0 = new ~((1, 2), 0)
       |  private[p3] val 0 ~ c = new ~(0, 1)
       |  private[p3] val _ ~ d = new ~(0, 1)
       |  private[p3] val (0 ~ e) ~ 0 = new ~(new ~(0, 1), 0)
       |  private[p3] val 0 ~ Tuple2(f, _) = new ~(0, (1, 2))
       |  private[p3] val g ~ _ = new ~(1, 2)
       |}
       |""".stripMargin,
    """|[0:7..0:9): p3 <= _empty_/p3.
       |[1:13..1:14): ~ <= _empty_/p3.`~`#
       |[1:15..1:16): A <= _empty_/p3.`~`#[A]
       |[1:18..1:19): B <= _empty_/p3.`~`#[B]
       |[1:20..1:20):  <= _empty_/p3.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/p3.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/p3.`~`#l.
       |[1:24..1:25): A => _empty_/p3.`~`#[A]
       |[1:27..1:28): r <= _empty_/p3.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/p3.`~`#r.
       |[1:30..1:31): B => _empty_/p3.`~`#[B]
       |[2:10..2:12): p3 => _empty_/p3.
       |[2:18..2:24): Tuple2 => scala/Tuple2.
       |[2:25..2:26): a <= _empty_/p3.a.
       |[2:31..2:32): ~ => _empty_/p3.`~`.
       |[2:41..2:42): ~ => _empty_/p3.`~`#
       |[2:42..2:42):  => _empty_/p3.`~`#`<init>`().
       |[3:10..3:12): p3 => _empty_/p3.
       |[3:19..3:20): b <= _empty_/p3.b.
       |[3:25..3:26): ~ => _empty_/p3.`~`.
       |[3:35..3:36): ~ => _empty_/p3.`~`#
       |[3:36..3:36):  => _empty_/p3.`~`#`<init>`().
       |[4:10..4:12): p3 => _empty_/p3.
       |[4:20..4:21): ~ => _empty_/p3.`~`.
       |[4:22..4:23): c <= _empty_/p3.c.
       |[4:30..4:31): ~ => _empty_/p3.`~`#
       |[4:31..4:31):  => _empty_/p3.`~`#`<init>`().
       |[5:10..5:12): p3 => _empty_/p3.
       |[5:20..5:21): ~ => _empty_/p3.`~`.
       |[5:22..5:23): d <= _empty_/p3.d.
       |[5:30..5:31): ~ => _empty_/p3.`~`#
       |[5:31..5:31):  => _empty_/p3.`~`#`<init>`().
       |[6:10..6:12): p3 => _empty_/p3.
       |[6:21..6:22): ~ => _empty_/p3.`~`.
       |[6:23..6:24): e <= _empty_/p3.e.
       |[6:26..6:27): ~ => _empty_/p3.`~`.
       |[6:36..6:37): ~ => _empty_/p3.`~`#
       |[6:37..6:37):  => _empty_/p3.`~`#`<init>`().
       |[6:42..6:43): ~ => _empty_/p3.`~`#
       |[6:43..6:43):  => _empty_/p3.`~`#`<init>`().
       |[7:10..7:12): p3 => _empty_/p3.
       |[7:20..7:21): ~ => _empty_/p3.`~`.
       |[7:22..7:28): Tuple2 => scala/Tuple2.
       |[7:29..7:30): f <= _empty_/p3.f.
       |[7:41..7:42): ~ => _empty_/p3.`~`#
       |[7:42..7:42):  => _empty_/p3.`~`#`<init>`().
       |[8:10..8:12): p3 => _empty_/p3.
       |[8:18..8:19): g <= _empty_/p3.g.
       |[8:20..8:21): ~ => _empty_/p3.`~`.
       |[8:30..8:31): ~ => _empty_/p3.`~`#
       |[8:31..8:31):  => _empty_/p3.`~`#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "var and lazy val patterns of one variable with an access qualifier in a trait",
    """|trait p4 {
       |  case class ~[A, B](l: A, r: B)
       |  private[p4] var Tuple2(a ~ 0, _) = (new ~(1, 0), 2)
       |  private[p4] lazy val Tuple2(b ~ 0, _) = (new ~(1, 0), 2)
       |  private[p4] val Tuple2(c ~ 0, _) = (new ~(1, 0), 2)
       |  private[p4] var (d, _) = (1, 2)
       |  private[p4] lazy val (e, _) = (1, 2)
       |  private[p4] var _ ~ f = new ~(0, 1)
       |  private[p4] lazy val 0 ~ g = new ~(0, 1)
       |}
       |""".stripMargin,
    """|[0:6..0:8): p4 <= _empty_/p4#
       |[0:9..0:9):  <= _empty_/p4#`<init>`().
       |[1:13..1:14): ~ <= _empty_/p4#`~`#
       |[1:15..1:16): A <= _empty_/p4#`~`#[A]
       |[1:18..1:19): B <= _empty_/p4#`~`#[B]
       |[1:20..1:20):  <= _empty_/p4#`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/p4#`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/p4#`~`#l.
       |[1:24..1:25): A => _empty_/p4#`~`#[A]
       |[1:27..1:28): r <= _empty_/p4#`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/p4#`~`#r.
       |[1:30..1:31): B => _empty_/p4#`~`#[B]
       |[2:10..2:12): p4 => _empty_/p4#
       |[2:18..2:24): Tuple2 => scala/Tuple2.
       |[2:25..2:26): a <= _empty_/p4#a().
       |[2:27..2:28): ~ => _empty_/p4#`~`.
       |[2:42..2:43): ~ => _empty_/p4#`~`#
       |[2:43..2:43):  => _empty_/p4#`~`#`<init>`().
       |[3:10..3:12): p4 => _empty_/p4#
       |[3:23..3:29): Tuple2 => scala/Tuple2.
       |[3:30..3:31): b <= _empty_/p4#b.
       |[3:32..3:33): ~ => _empty_/p4#`~`.
       |[3:47..3:48): ~ => _empty_/p4#`~`#
       |[3:48..3:48):  => _empty_/p4#`~`#`<init>`().
       |[4:10..4:12): p4 => _empty_/p4#
       |[4:18..4:24): Tuple2 => scala/Tuple2.
       |[4:25..4:26): c <= _empty_/p4#c.
       |[4:27..4:28): ~ => _empty_/p4#`~`.
       |[4:42..4:43): ~ => _empty_/p4#`~`#
       |[4:43..4:43):  => _empty_/p4#`~`#`<init>`().
       |[5:10..5:12): p4 => _empty_/p4#
       |[5:19..5:20): d <= _empty_/p4#d().
       |[6:10..6:12): p4 => _empty_/p4#
       |[6:24..6:25): e <= _empty_/p4#e.
       |[7:10..7:12): p4 => _empty_/p4#
       |[7:20..7:21): ~ => _empty_/p4#`~`.
       |[7:22..7:23): f <= _empty_/p4#f().
       |[7:30..7:31): ~ => _empty_/p4#`~`#
       |[7:31..7:31):  => _empty_/p4#`~`#`<init>`().
       |[8:10..8:12): p4 => _empty_/p4#
       |[8:25..8:26): ~ => _empty_/p4#`~`.
       |[8:27..8:28): g <= _empty_/p4#g.
       |[8:35..8:36): ~ => _empty_/p4#`~`#
       |[8:36..8:36):  => _empty_/p4#`~`#`<init>`().
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "interpolated patterns of one variable with an access qualifier",
    """|object p6 {
       |  private[p6] val s"$a!" = "1!"
       |  private[p6] val s"$b-$_" = "1-2"
       |}
       |""".stripMargin,
    """|[0:7..0:9): p6 <= _empty_/p6.
       |[1:10..1:12): p6 => _empty_/p6.
       |[1:18..1:19): s => scala/StringContext#s.
       |[1:21..1:22): a <= _empty_/p6.a.
       |[2:10..2:12): p6 => _empty_/p6.
       |[2:18..2:19): s => scala/StringContext#s.
       |[2:21..2:22): b <= _empty_/p6.b.
       |""".stripMargin,
  )

  occurrences(
    "patterns of one variable in a method",
    """|object p5 {
       |  case class ~[A, B](l: A, r: B)
       |  def m(): Int = {
       |    val Tuple2(a, _) = (1, 2)
       |    val (b, _) = (1, 2)
       |    val Tuple2(c, _) ~ 0 = new ~((1, 2), 0)
       |    val 0 ~ d = new ~(0, 1)
       |    val _ ~ e = new ~(0, 1)
       |    var (f, _) = (1, 2)
       |    lazy val Tuple2(g, _) = (1, 2)
       |    a + b + c + d + e + f + g
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): p5 <= _empty_/p5.
       |[1:13..1:14): ~ <= _empty_/p5.`~`#
       |[1:15..1:16): A <= _empty_/p5.`~`#[A]
       |[1:18..1:19): B <= _empty_/p5.`~`#[B]
       |[1:20..1:20):  <= _empty_/p5.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/p5.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/p5.`~`#l.
       |[1:24..1:25): A => _empty_/p5.`~`#[A]
       |[1:27..1:28): r <= _empty_/p5.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/p5.`~`#r.
       |[1:30..1:31): B => _empty_/p5.`~`#[B]
       |[2:6..2:7): m <= _empty_/p5.m().
       |[2:11..2:14): Int => scala/Int#
       |[3:8..3:14): Tuple2 => scala/Tuple2.
       |[3:15..3:16): a <= local0
       |[4:9..4:10): b <= local1
       |[5:8..5:14): Tuple2 => scala/Tuple2.
       |[5:15..5:16): c <= local2
       |[5:21..5:22): ~ => _empty_/p5.`~`.
       |[5:31..5:32): ~ => _empty_/p5.`~`#
       |[5:32..5:32):  => _empty_/p5.`~`#`<init>`().
       |[6:10..6:11): ~ => _empty_/p5.`~`.
       |[6:12..6:13): d <= local3
       |[6:20..6:21): ~ => _empty_/p5.`~`#
       |[6:21..6:21):  => _empty_/p5.`~`#`<init>`().
       |[7:10..7:11): ~ => _empty_/p5.`~`.
       |[7:12..7:13): e <= local4
       |[7:20..7:21): ~ => _empty_/p5.`~`#
       |[7:21..7:21):  => _empty_/p5.`~`#`<init>`().
       |[8:9..8:10): f <= local5
       |[9:13..9:19): Tuple2 => scala/Tuple2.
       |[9:20..9:21): g <= local6
       |[10:4..10:5): a => local0
       |[10:6..10:7): + => scala/Int#`+`(+4).
       |[10:8..10:9): b => local1
       |[10:10..10:11): + => scala/Int#`+`(+4).
       |[10:12..10:13): c => local2
       |[10:14..10:15): + => scala/Int#`+`(+4).
       |[10:16..10:17): d => local3
       |[10:18..10:19): + => scala/Int#`+`(+4).
       |[10:20..10:21): e => local4
       |[10:22..10:23): + => scala/Int#`+`(+4).
       |[10:24..10:25): f => local5
       |[10:26..10:27): + => scala/Int#`+`(+4).
       |[10:28..10:29): g => local6
       |""".stripMargin,
  )

  occurrences(
    "extractor and infix patterns in a method",
    """|object pm {
       |  case class ~[A, B](l: A, r: B)
       |  def m(): Int = {
       |    val Some(Some(Some(a))) = Some(Some(Some(1)))
       |    val Tuple2(b ~ 0, c) = (new ~(1, 0), 2)
       |    val Tuple2(d ~ 0, _) = (new ~(1, 0), 2)
       |    val Some(e) ~ 0 = new ~(Some(1), 0)
       |    val Some(f) ~ g = new ~(Some(1), 0)
       |    val h ~ 0 = new ~(1, 0)
       |    val Some((i, j)) = Some((1, 2))
       |    a + b + c + d + e + f + g + h + i + j
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): pm <= _empty_/pm.
       |[1:13..1:14): ~ <= _empty_/pm.`~`#
       |[1:15..1:16): A <= _empty_/pm.`~`#[A]
       |[1:18..1:19): B <= _empty_/pm.`~`#[B]
       |[1:20..1:20):  <= _empty_/pm.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/pm.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/pm.`~`#l.
       |[1:24..1:25): A => _empty_/pm.`~`#[A]
       |[1:27..1:28): r <= _empty_/pm.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/pm.`~`#r.
       |[1:30..1:31): B => _empty_/pm.`~`#[B]
       |[2:6..2:7): m <= _empty_/pm.m().
       |[2:11..2:14): Int => scala/Int#
       |[3:8..3:12): Some => scala/Some.
       |[3:13..3:17): Some => scala/Some.
       |[3:18..3:22): Some => scala/Some.
       |[3:23..3:24): a <= local0
       |[3:30..3:34): Some => scala/Some.
       |[3:35..3:39): Some => scala/Some.
       |[3:40..3:44): Some => scala/Some.
       |[4:8..4:14): Tuple2 => scala/Tuple2.
       |[4:15..4:16): b <= local1
       |[4:17..4:18): ~ => _empty_/pm.`~`.
       |[4:22..4:23): c <= local2
       |[4:32..4:33): ~ => _empty_/pm.`~`#
       |[4:33..4:33):  => _empty_/pm.`~`#`<init>`().
       |[5:8..5:14): Tuple2 => scala/Tuple2.
       |[5:15..5:16): d <= local3
       |[5:17..5:18): ~ => _empty_/pm.`~`.
       |[5:32..5:33): ~ => _empty_/pm.`~`#
       |[5:33..5:33):  => _empty_/pm.`~`#`<init>`().
       |[6:8..6:12): Some => scala/Some.
       |[6:13..6:14): e <= local4
       |[6:16..6:17): ~ => _empty_/pm.`~`.
       |[6:26..6:27): ~ => _empty_/pm.`~`#
       |[6:27..6:27):  => _empty_/pm.`~`#`<init>`().
       |[6:28..6:32): Some => scala/Some.
       |[7:8..7:12): Some => scala/Some.
       |[7:13..7:14): f <= local5
       |[7:16..7:17): ~ => _empty_/pm.`~`.
       |[7:18..7:19): g <= local6
       |[7:26..7:27): ~ => _empty_/pm.`~`#
       |[7:27..7:27):  => _empty_/pm.`~`#`<init>`().
       |[7:28..7:32): Some => scala/Some.
       |[8:8..8:9): h <= local7
       |[8:10..8:11): ~ => _empty_/pm.`~`.
       |[8:20..8:21): ~ => _empty_/pm.`~`#
       |[8:21..8:21):  => _empty_/pm.`~`#`<init>`().
       |[9:8..9:12): Some => scala/Some.
       |[9:14..9:15): i <= local8
       |[9:17..9:18): j <= local9
       |[9:23..9:27): Some => scala/Some.
       |[10:4..10:5): a => local0
       |[10:6..10:7): + => scala/Int#`+`(+4).
       |[10:8..10:9): b => local1
       |[10:10..10:11): + => scala/Int#`+`(+4).
       |[10:12..10:13): c => local2
       |[10:14..10:15): + => scala/Int#`+`(+4).
       |[10:16..10:17): d => local3
       |[10:18..10:19): + => scala/Int#`+`(+4).
       |[10:20..10:21): e => local4
       |[10:22..10:23): + => scala/Int#`+`(+4).
       |[10:24..10:25): f => local5
       |[10:26..10:27): + => scala/Int#`+`(+4).
       |[10:28..10:29): g => local6
       |[10:30..10:31): + => scala/Int#`+`(+4).
       |[10:32..10:33): h => local7
       |[10:34..10:35): + => scala/Int#`+`(+4).
       |[10:36..10:37): i => local8
       |[10:38..10:39): + => scala/Int#`+`(+4).
       |[10:40..10:41): j => local9
       |""".stripMargin,
  )

  occurrences(
    "var patterns with an access qualifier",
    """|object pv {
       |  private[pv] var Some(a) = Some(1)
       |  private[pv] var b, c = 1
       |  private[pv] var (d, e) = (1, 2)
       |  private[pv] var f = 1
       |}
       |""".stripMargin,
    """|[0:7..0:9): pv <= _empty_/pv.
       |[1:10..1:12): pv => _empty_/pv.
       |[1:18..1:22): Some => scala/Some.
       |[1:23..1:24): a <= _empty_/pv.a().
       |[1:28..1:32): Some => scala/Some.
       |[2:10..2:12): pv => _empty_/pv.
       |[2:18..2:19): b <= _empty_/pv.b().
       |[2:21..2:22): c <= _empty_/pv.c().
       |[3:10..3:12): pv => _empty_/pv.
       |[3:19..3:20): d <= _empty_/pv.d().
       |[3:22..3:23): e <= _empty_/pv.e().
       |[4:10..4:12): pv => _empty_/pv.
       |[4:18..4:19): f <= _empty_/pv.f().
       |""".stripMargin,
  )

  occurrences(
    "lazy val patterns with an access qualifier",
    """|object pl {
       |  private[pl] lazy val Some(a) = Some(1)
       |  private[pl] lazy val b, c = 1
       |  private[pl] lazy val (d, e) = (1, 2)
       |  private[pl] lazy val f = 1
       |}
       |""".stripMargin,
    """|[0:7..0:9): pl <= _empty_/pl.
       |[1:10..1:12): pl => _empty_/pl.
       |[1:23..1:27): Some => scala/Some.
       |[1:28..1:29): a <= _empty_/pl.a.
       |[1:33..1:37): Some => scala/Some.
       |[2:10..2:12): pl => _empty_/pl.
       |[2:23..2:24): b <= _empty_/pl.b.
       |[2:26..2:27): c <= _empty_/pl.c.
       |[3:10..3:12): pl => _empty_/pl.
       |[3:24..3:25): d <= _empty_/pl.d.
       |[3:27..3:28): e <= _empty_/pl.e.
       |[4:10..4:12): pl => _empty_/pl.
       |[4:23..4:24): f <= _empty_/pl.f.
       |""".stripMargin,
  )

  occurrences(
    "patterns with an access qualifier in a trait",
    """|trait pr {
       |  private[pr] val Some(a) = Some(1)
       |  private[pr] val b, c = 1
       |  private[pr] val (d, e) = (1, 2)
       |  private[pr] val f = 1
       |}
       |""".stripMargin,
    """|[0:6..0:8): pr <= _empty_/pr#
       |[0:9..0:9):  <= _empty_/pr#`<init>`().
       |[1:10..1:12): pr => _empty_/pr#
       |[1:18..1:22): Some => scala/Some.
       |[1:23..1:24): a <= _empty_/pr#a.
       |[1:28..1:32): Some => scala/Some.
       |[2:10..2:12): pr => _empty_/pr#
       |[2:18..2:19): b <= _empty_/pr#b.
       |[2:21..2:22): c <= _empty_/pr#c.
       |[3:10..3:12): pr => _empty_/pr#
       |[3:19..3:20): d <= _empty_/pr#d.
       |[3:22..3:23): e <= _empty_/pr#e.
       |[4:10..4:12): pr => _empty_/pr#
       |[4:18..4:19): f <= _empty_/pr#f.
       |""".stripMargin,
  )

  occurrences(
    "a pattern with an access qualifier beside unqualified patterns",
    """|object pu {
       |  private[pu] val (a, b) = (1, 2) match { case (x, y) => (y, x) }
       |  val (d, e) = (a, b)
       |}
       |""".stripMargin,
    """|[0:7..0:9): pu <= _empty_/pu.
       |[1:10..1:12): pu => _empty_/pu.
       |[1:19..1:20): a <= _empty_/pu.a.
       |[1:22..1:23): b <= _empty_/pu.b.
       |[1:48..1:49): x <= local0
       |[1:51..1:52): y <= local1
       |[1:58..1:59): y => local1
       |[1:61..1:62): x => local0
       |[2:7..2:8): d <= _empty_/pu.d.
       |[2:10..2:11): e <= _empty_/pu.e.
       |[2:16..2:17): a => _empty_/pu.a.
       |[2:19..2:20): b => _empty_/pu.b.
       |""".stripMargin,
  )

  occurrences(
    "declarations with an access qualifier",
    """|abstract class pd {
       |  protected[pd] val a, b: Int
       |  protected[pd] var c, d: Int
       |  protected[pd] val e: Int
       |}
       |""".stripMargin,
    """|[0:15..0:17): pd <= _empty_/pd#
       |[0:18..0:18):  <= _empty_/pd#`<init>`().
       |[1:12..1:14): pd => _empty_/pd#
       |[1:20..1:21): a <= _empty_/pd#a.
       |[1:23..1:24): b <= _empty_/pd#b.
       |[1:26..1:29): Int => scala/Int#
       |[2:12..2:14): pd => _empty_/pd#
       |[2:20..2:21): c <= _empty_/pd#c().
       |[2:23..2:24): d <= _empty_/pd#d().
       |[2:26..2:29): Int => scala/Int#
       |[3:12..3:14): pd => _empty_/pd#
       |[3:20..3:21): e <= _empty_/pd#e.
       |[3:23..3:26): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "patterns with a this qualifier",
    """|object pt {
       |  private[this] val Some(a) = Some(1)
       |  protected[this] val b, c = 1
       |  private[this] val f = 1
       |}
       |""".stripMargin,
    """|[0:7..0:9): pt <= _empty_/pt.
       |[1:20..1:24): Some => scala/Some.
       |[1:25..1:26): a <= _empty_/pt.a.
       |[1:30..1:34): Some => scala/Some.
       |[2:22..2:23): b <= _empty_/pt.b.
       |[2:25..2:26): c <= _empty_/pt.c.
       |[3:20..3:21): f <= _empty_/pt.f.
       |""".stripMargin,
  )

  occurrences(
    "typed patterns at depth in a template",
    """|object td1 {
       |  case class ~[A, B](l: A, r: B)
       |  val (a: Int) ~ 0 = new ~(1, 0)
       |  val Some(b: Int) = Some(1)
       |  val Some((c: Int)) = Some(1)
       |  val (j @ _) = 1
       |  val (k @ (_: Int)) = 1
       |  val (l: Int @unchecked) = 1
       |}
       |""".stripMargin,
    """|[0:7..0:10): td1 <= _empty_/td1.
       |[1:13..1:14): ~ <= _empty_/td1.`~`#
       |[1:15..1:16): A <= _empty_/td1.`~`#[A]
       |[1:18..1:19): B <= _empty_/td1.`~`#[B]
       |[1:20..1:20):  <= _empty_/td1.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/td1.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/td1.`~`#l.
       |[1:24..1:25): A => _empty_/td1.`~`#[A]
       |[1:27..1:28): r <= _empty_/td1.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/td1.`~`#r.
       |[1:30..1:31): B => _empty_/td1.`~`#[B]
       |[2:7..2:8): a <= _empty_/td1.a.
       |[2:10..2:13): Int => scala/Int#
       |[2:15..2:16): ~ => _empty_/td1.`~`.
       |[2:25..2:26): ~ => _empty_/td1.`~`#
       |[2:26..2:26):  => _empty_/td1.`~`#`<init>`().
       |[3:6..3:10): Some => scala/Some.
       |[3:11..3:12): b <= _empty_/td1.b.
       |[3:14..3:17): Int => scala/Int#
       |[3:21..3:25): Some => scala/Some.
       |[4:6..4:10): Some => scala/Some.
       |[4:12..4:13): c <= _empty_/td1.c.
       |[4:15..4:18): Int => scala/Int#
       |[4:23..4:27): Some => scala/Some.
       |[5:7..5:8): j <= _empty_/td1.j.
       |[6:7..6:8): k <= _empty_/td1.k.
       |[6:15..6:18): Int => scala/Int#
       |[7:7..7:8): l <= _empty_/td1.l.
       |[7:10..7:13): Int => scala/Int#
       |[7:15..7:24): unchecked => scala/unchecked#
       |[7:24..7:24):  => scala/unchecked#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "typed patterns at depth in a method",
    """|object td2 {
       |  case class ~[A, B](l: A, r: B)
       |  def m(): Unit = {
       |    val (a: Int) ~ 0 = new ~(1, 0)
       |    val Some(b: Int) = Some(1)
       |    val (j @ _) = 1
       |    var (k @ (_: Int)) = 1
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:10): td2 <= _empty_/td2.
       |[1:13..1:14): ~ <= _empty_/td2.`~`#
       |[1:15..1:16): A <= _empty_/td2.`~`#[A]
       |[1:18..1:19): B <= _empty_/td2.`~`#[B]
       |[1:20..1:20):  <= _empty_/td2.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/td2.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/td2.`~`#l.
       |[1:24..1:25): A => _empty_/td2.`~`#[A]
       |[1:27..1:28): r <= _empty_/td2.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/td2.`~`#r.
       |[1:30..1:31): B => _empty_/td2.`~`#[B]
       |[2:6..2:7): m <= _empty_/td2.m().
       |[2:11..2:15): Unit => scala/Unit#
       |[3:9..3:10): a <= local0
       |[3:12..3:15): Int => scala/Int#
       |[3:17..3:18): ~ => _empty_/td2.`~`.
       |[3:27..3:28): ~ => _empty_/td2.`~`#
       |[3:28..3:28):  => _empty_/td2.`~`#`<init>`().
       |[4:8..4:12): Some => scala/Some.
       |[4:13..4:14): b <= local1
       |[4:16..4:19): Int => scala/Int#
       |[4:23..4:27): Some => scala/Some.
       |[5:9..5:10): j <= local2
       |[6:9..6:10): k <= local3
       |[6:17..6:20): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "binders and infix patterns with an access qualifier",
    """|object qs {
       |  case class ~[A, B](l: A, r: B)
       |  private[qs] val x @ Some(y) = Some(1)
       |  private[qs] val Some(a @ _) = Some(1)
       |  private[qs] val Some(b), Some(c) = Some(1)
       |  private[qs] val (d: Int), (e: Int) = 1
       |  private[qs] val Some(h ~ 0) = Some(new ~(1, 0))
       |  private[qs] val (i, (j: Int)) = (1, 2)
       |  private[qs] val k ~ l ~ 0 = new ~(new ~(1, 2), 0)
       |  private[qs] var m ~ 0 = new ~(1, 0)
       |  private[qs] lazy val n ~ 0 = new ~(1, 0)
       |  private[qs] val (o @ _) = 1
       |}
       |""".stripMargin,
    """|[0:7..0:9): qs <= _empty_/qs.
       |[1:13..1:14): ~ <= _empty_/qs.`~`#
       |[1:15..1:16): A <= _empty_/qs.`~`#[A]
       |[1:18..1:19): B <= _empty_/qs.`~`#[B]
       |[1:20..1:20):  <= _empty_/qs.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/qs.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/qs.`~`#l.
       |[1:24..1:25): A => _empty_/qs.`~`#[A]
       |[1:27..1:28): r <= _empty_/qs.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/qs.`~`#r.
       |[1:30..1:31): B => _empty_/qs.`~`#[B]
       |[2:10..2:12): qs => _empty_/qs.
       |[2:18..2:19): x <= _empty_/qs.x.
       |[2:22..2:26): Some => scala/Some.
       |[2:27..2:28): y <= _empty_/qs.y.
       |[2:32..2:36): Some => scala/Some.
       |[3:10..3:12): qs => _empty_/qs.
       |[3:18..3:22): Some => scala/Some.
       |[3:23..3:24): a <= _empty_/qs.a.
       |[3:32..3:36): Some => scala/Some.
       |[4:10..4:12): qs => _empty_/qs.
       |[4:18..4:22): Some => scala/Some.
       |[4:23..4:24): b <= _empty_/qs.b.
       |[4:27..4:31): Some => scala/Some.
       |[4:32..4:33): c <= _empty_/qs.c.
       |[4:37..4:41): Some => scala/Some.
       |[5:10..5:12): qs => _empty_/qs.
       |[5:19..5:20): d <= _empty_/qs.d.
       |[5:22..5:25): Int => scala/Int#
       |[5:29..5:30): e <= _empty_/qs.e.
       |[5:32..5:35): Int => scala/Int#
       |[6:10..6:12): qs => _empty_/qs.
       |[6:18..6:22): Some => scala/Some.
       |[6:23..6:24): h <= _empty_/qs.h.
       |[6:25..6:26): ~ => _empty_/qs.`~`.
       |[6:32..6:36): Some => scala/Some.
       |[6:41..6:42): ~ => _empty_/qs.`~`#
       |[6:42..6:42):  => _empty_/qs.`~`#`<init>`().
       |[7:10..7:12): qs => _empty_/qs.
       |[7:19..7:20): i <= _empty_/qs.i.
       |[7:23..7:24): j <= _empty_/qs.j.
       |[7:26..7:29): Int => scala/Int#
       |[8:10..8:12): qs => _empty_/qs.
       |[8:18..8:19): k <= _empty_/qs.k.
       |[8:20..8:21): ~ => _empty_/qs.`~`.
       |[8:22..8:23): l <= _empty_/qs.l.
       |[8:24..8:25): ~ => _empty_/qs.`~`.
       |[8:34..8:35): ~ => _empty_/qs.`~`#
       |[8:35..8:35):  => _empty_/qs.`~`#`<init>`().
       |[8:40..8:41): ~ => _empty_/qs.`~`#
       |[8:41..8:41):  => _empty_/qs.`~`#`<init>`().
       |[9:10..9:12): qs => _empty_/qs.
       |[9:18..9:19): m <= _empty_/qs.m().
       |[9:20..9:21): ~ => _empty_/qs.`~`.
       |[9:30..9:31): ~ => _empty_/qs.`~`#
       |[9:31..9:31):  => _empty_/qs.`~`#`<init>`().
       |[10:10..10:12): qs => _empty_/qs.
       |[10:23..10:24): n <= _empty_/qs.n.
       |[10:25..10:26): ~ => _empty_/qs.`~`.
       |[10:35..10:36): ~ => _empty_/qs.`~`#
       |[10:36..10:36):  => _empty_/qs.`~`#`<init>`().
       |[11:10..11:12): qs => _empty_/qs.
       |[11:19..11:20): o <= _empty_/qs.o.
       |""".stripMargin,
  )

  occurrences(
    "typed patterns with an access qualifier",
    """|object qt {
       |  case class ~[A, B](l: A, r: B)
       |  private[qt] val (a: Int) ~ 0 = new ~(1, 0)
       |  private[qt] var (b: Int) ~ 0 = new ~(1, 0)
       |  private[qt] val (c: Int) ~ (d: Int) = new ~(1, 0)
       |  private[qt] val Some(e: Int) = Some(1)
       |  private[qt] val Tuple1(f: Int) = Tuple1(1)
       |  private[qt] val (g, _: Int) = (1, 2)
       |  private[qt] val Some((h: Int) ~ 0) = Some(new ~(1, 0))
       |  private[qt] val (i: Int) ~ 0, (j: Int) ~ 0 = new ~(1, 0)
       |}
       |""".stripMargin,
    """|[0:7..0:9): qt <= _empty_/qt.
       |[1:13..1:14): ~ <= _empty_/qt.`~`#
       |[1:15..1:16): A <= _empty_/qt.`~`#[A]
       |[1:18..1:19): B <= _empty_/qt.`~`#[B]
       |[1:20..1:20):  <= _empty_/qt.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/qt.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/qt.`~`#l.
       |[1:24..1:25): A => _empty_/qt.`~`#[A]
       |[1:27..1:28): r <= _empty_/qt.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/qt.`~`#r.
       |[1:30..1:31): B => _empty_/qt.`~`#[B]
       |[2:10..2:12): qt => _empty_/qt.
       |[2:19..2:20): a <= _empty_/qt.a.
       |[2:22..2:25): Int => scala/Int#
       |[2:27..2:28): ~ => _empty_/qt.`~`.
       |[2:37..2:38): ~ => _empty_/qt.`~`#
       |[2:38..2:38):  => _empty_/qt.`~`#`<init>`().
       |[3:10..3:12): qt => _empty_/qt.
       |[3:19..3:20): b <= _empty_/qt.b().
       |[3:22..3:25): Int => scala/Int#
       |[3:27..3:28): ~ => _empty_/qt.`~`.
       |[3:37..3:38): ~ => _empty_/qt.`~`#
       |[3:38..3:38):  => _empty_/qt.`~`#`<init>`().
       |[4:10..4:12): qt => _empty_/qt.
       |[4:19..4:20): c <= _empty_/qt.c.
       |[4:22..4:25): Int => scala/Int#
       |[4:27..4:28): ~ => _empty_/qt.`~`.
       |[4:30..4:31): d <= _empty_/qt.d.
       |[4:33..4:36): Int => scala/Int#
       |[4:44..4:45): ~ => _empty_/qt.`~`#
       |[4:45..4:45):  => _empty_/qt.`~`#`<init>`().
       |[5:10..5:12): qt => _empty_/qt.
       |[5:18..5:22): Some => scala/Some.
       |[5:23..5:24): e <= _empty_/qt.e.
       |[5:26..5:29): Int => scala/Int#
       |[5:33..5:37): Some => scala/Some.
       |[6:10..6:12): qt => _empty_/qt.
       |[6:18..6:24): Tuple1 => scala/Tuple1.
       |[6:25..6:26): f <= _empty_/qt.f.
       |[6:28..6:31): Int => scala/Int#
       |[6:35..6:41): Tuple1 => scala/Tuple1.
       |[7:10..7:12): qt => _empty_/qt.
       |[7:19..7:20): g <= _empty_/qt.g.
       |[7:25..7:28): Int => scala/Int#
       |[8:10..8:12): qt => _empty_/qt.
       |[8:18..8:22): Some => scala/Some.
       |[8:24..8:25): h <= _empty_/qt.h.
       |[8:27..8:30): Int => scala/Int#
       |[8:32..8:33): ~ => _empty_/qt.`~`.
       |[8:39..8:43): Some => scala/Some.
       |[8:48..8:49): ~ => _empty_/qt.`~`#
       |[8:49..8:49):  => _empty_/qt.`~`#`<init>`().
       |[9:10..9:12): qt => _empty_/qt.
       |[9:19..9:20): i <= _empty_/qt.i.
       |[9:22..9:25): Int => scala/Int#
       |[9:27..9:28): ~ => _empty_/qt.`~`.
       |[9:33..9:34): j <= _empty_/qt.j.
       |[9:36..9:39): Int => scala/Int#
       |[9:41..9:42): ~ => _empty_/qt.`~`.
       |[9:51..9:52): ~ => _empty_/qt.`~`#
       |[9:52..9:52):  => _empty_/qt.`~`#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "a variable beside a wildcard with an access qualifier",
    """|object qw {
       |  case class ~[A, B](l: A, r: B)
       |  private[qw] val (a, _) = (1, 2)
       |  private[qw] val Tuple2(b, _) = (1, 2)
       |  private[qw] val (_, (c: Int)) = (1, 2)
       |  private[qw] val (d: Int) ~ _ = new ~(1, 0)
       |  private[qw] val ((e: Int), _) = (1, 2)
       |  private[qw] val (f, _), (g, _) = (1, 2)
       |}
       |""".stripMargin,
    """|[0:7..0:9): qw <= _empty_/qw.
       |[1:13..1:14): ~ <= _empty_/qw.`~`#
       |[1:15..1:16): A <= _empty_/qw.`~`#[A]
       |[1:18..1:19): B <= _empty_/qw.`~`#[B]
       |[1:20..1:20):  <= _empty_/qw.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/qw.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/qw.`~`#l.
       |[1:24..1:25): A => _empty_/qw.`~`#[A]
       |[1:27..1:28): r <= _empty_/qw.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/qw.`~`#r.
       |[1:30..1:31): B => _empty_/qw.`~`#[B]
       |[2:10..2:12): qw => _empty_/qw.
       |[2:19..2:20): a <= _empty_/qw.a.
       |[3:10..3:12): qw => _empty_/qw.
       |[3:18..3:24): Tuple2 => scala/Tuple2.
       |[3:25..3:26): b <= _empty_/qw.b.
       |[4:10..4:12): qw => _empty_/qw.
       |[4:23..4:24): c <= _empty_/qw.c.
       |[4:26..4:29): Int => scala/Int#
       |[5:10..5:12): qw => _empty_/qw.
       |[5:19..5:20): d <= _empty_/qw.d.
       |[5:22..5:25): Int => scala/Int#
       |[5:27..5:28): ~ => _empty_/qw.`~`.
       |[5:37..5:38): ~ => _empty_/qw.`~`#
       |[5:38..5:38):  => _empty_/qw.`~`#`<init>`().
       |[6:10..6:12): qw => _empty_/qw.
       |[6:20..6:21): e <= _empty_/qw.e.
       |[6:23..6:26): Int => scala/Int#
       |[7:10..7:12): qw => _empty_/qw.
       |[7:19..7:20): f <= _empty_/qw.f.
       |[7:27..7:28): g <= _empty_/qw.g.
       |""".stripMargin,
  )

  occurrences(
    "typed variables at depth with an access qualifier in a trait",
    """|trait qd {
       |  case class ~[A, B](l: A, r: B)
       |  private[qd] val ((a: Int) ~ 0) ~ 0 = new ~(new ~(1, 0), 0)
       |  private[qd] var 0 ~ (b: Int) = new ~(0, 1)
       |  private[qd] lazy val (c: Int) ~ 0 = new ~(1, 0)
       |  private[qd] val Tuple2((d: Int) ~ 0, _) = (new ~(1, 0), 2)
       |  private[qd] val (_, (e: Int) ~ 0) = (1, new ~(2, 0))
       |  private[qd] var Some(Some(f: Int)) = Some(Some(1))
       |  private[qd] lazy val ((g: Int), _) ~ 0 = new ~((1, 2), 0)
       |  private[qd] val (h @ (_: Int)) ~ 0 = new ~(1, 0)
       |  private[qd] val ((i: Int) ~ 0): (Int ~ Int) = new ~(1, 0)
       |  private[qd] var (j: Int) ~ _ ~ 0 = new ~(new ~(1, 2), 0)
       |  private[qd] lazy val Some((k: Int)) = Some(1)
       |}
       |""".stripMargin,
    """|[0:6..0:8): qd <= _empty_/qd#
       |[0:9..0:9):  <= _empty_/qd#`<init>`().
       |[1:13..1:14): ~ <= _empty_/qd#`~`#
       |[1:15..1:16): A <= _empty_/qd#`~`#[A]
       |[1:18..1:19): B <= _empty_/qd#`~`#[B]
       |[1:20..1:20):  <= _empty_/qd#`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/qd#`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/qd#`~`#l.
       |[1:24..1:25): A => _empty_/qd#`~`#[A]
       |[1:27..1:28): r <= _empty_/qd#`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/qd#`~`#r.
       |[1:30..1:31): B => _empty_/qd#`~`#[B]
       |[2:10..2:12): qd => _empty_/qd#
       |[2:20..2:21): a <= _empty_/qd#a.
       |[2:23..2:26): Int => scala/Int#
       |[2:28..2:29): ~ => _empty_/qd#`~`.
       |[2:33..2:34): ~ => _empty_/qd#`~`.
       |[2:43..2:44): ~ => _empty_/qd#`~`#
       |[2:44..2:44):  => _empty_/qd#`~`#`<init>`().
       |[2:49..2:50): ~ => _empty_/qd#`~`#
       |[2:50..2:50):  => _empty_/qd#`~`#`<init>`().
       |[3:10..3:12): qd => _empty_/qd#
       |[3:20..3:21): ~ => _empty_/qd#`~`.
       |[3:23..3:24): b <= _empty_/qd#b().
       |[3:26..3:29): Int => scala/Int#
       |[3:37..3:38): ~ => _empty_/qd#`~`#
       |[3:38..3:38):  => _empty_/qd#`~`#`<init>`().
       |[4:10..4:12): qd => _empty_/qd#
       |[4:24..4:25): c <= _empty_/qd#c.
       |[4:27..4:30): Int => scala/Int#
       |[4:32..4:33): ~ => _empty_/qd#`~`.
       |[4:42..4:43): ~ => _empty_/qd#`~`#
       |[4:43..4:43):  => _empty_/qd#`~`#`<init>`().
       |[5:10..5:12): qd => _empty_/qd#
       |[5:18..5:24): Tuple2 => scala/Tuple2.
       |[5:26..5:27): d <= _empty_/qd#d.
       |[5:29..5:32): Int => scala/Int#
       |[5:34..5:35): ~ => _empty_/qd#`~`.
       |[5:49..5:50): ~ => _empty_/qd#`~`#
       |[5:50..5:50):  => _empty_/qd#`~`#`<init>`().
       |[6:10..6:12): qd => _empty_/qd#
       |[6:23..6:24): e <= _empty_/qd#e.
       |[6:26..6:29): Int => scala/Int#
       |[6:31..6:32): ~ => _empty_/qd#`~`.
       |[6:46..6:47): ~ => _empty_/qd#`~`#
       |[6:47..6:47):  => _empty_/qd#`~`#`<init>`().
       |[7:10..7:12): qd => _empty_/qd#
       |[7:18..7:22): Some => scala/Some.
       |[7:23..7:27): Some => scala/Some.
       |[7:28..7:29): f <= _empty_/qd#f().
       |[7:31..7:34): Int => scala/Int#
       |[7:39..7:43): Some => scala/Some.
       |[7:44..7:48): Some => scala/Some.
       |[8:10..8:12): qd => _empty_/qd#
       |[8:25..8:26): g <= _empty_/qd#g.
       |[8:28..8:31): Int => scala/Int#
       |[8:37..8:38): ~ => _empty_/qd#`~`.
       |[8:47..8:48): ~ => _empty_/qd#`~`#
       |[8:48..8:48):  => _empty_/qd#`~`#`<init>`().
       |[9:10..9:12): qd => _empty_/qd#
       |[9:19..9:20): h <= _empty_/qd#h.
       |[9:27..9:30): Int => scala/Int#
       |[9:33..9:34): ~ => _empty_/qd#`~`.
       |[9:43..9:44): ~ => _empty_/qd#`~`#
       |[9:44..9:44):  => _empty_/qd#`~`#`<init>`().
       |[10:10..10:12): qd => _empty_/qd#
       |[10:20..10:21): i <= _empty_/qd#i.
       |[10:23..10:26): Int => scala/Int#
       |[10:28..10:29): ~ => _empty_/qd#`~`.
       |[10:35..10:38): Int => scala/Int#
       |[10:39..10:40): ~ => _empty_/qd#`~`#
       |[10:41..10:44): Int => scala/Int#
       |[10:52..10:53): ~ => _empty_/qd#`~`#
       |[10:53..10:53):  => _empty_/qd#`~`#`<init>`().
       |[11:10..11:12): qd => _empty_/qd#
       |[11:19..11:20): j <= _empty_/qd#j().
       |[11:22..11:25): Int => scala/Int#
       |[11:27..11:28): ~ => _empty_/qd#`~`.
       |[11:31..11:32): ~ => _empty_/qd#`~`.
       |[11:41..11:42): ~ => _empty_/qd#`~`#
       |[11:42..11:42):  => _empty_/qd#`~`#`<init>`().
       |[11:47..11:48): ~ => _empty_/qd#`~`#
       |[11:48..11:48):  => _empty_/qd#`~`#`<init>`().
       |[12:10..12:12): qd => _empty_/qd#
       |[12:23..12:27): Some => scala/Some.
       |[12:29..12:30): k <= _empty_/qd#k.
       |[12:32..12:35): Int => scala/Int#
       |[12:40..12:44): Some => scala/Some.
       |""".stripMargin,
  )

  occurrences(
    "typed variables at depth with an access qualifier in a local class",
    """|object qe {
       |  case class ~[A, B](l: A, r: B)
       |  def m(): Int = {
       |    class L {
       |      private[L] val (a: Int) ~ 0 = new ~(1, 0)
       |      private[L] var 0 ~ (b: Int) = new ~(0, 1)
       |      private[L] lazy val (c: Int) ~ 0 = new ~(1, 0)
       |      private[L] val (_, (d: Int)) = (1, 2)
       |      private[L] val Some((e: Int) ~ 0) = Some(new ~(1, 0))
       |    }
       |    new L().hashCode
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): qe <= _empty_/qe.
       |[1:13..1:14): ~ <= _empty_/qe.`~`#
       |[1:15..1:16): A <= _empty_/qe.`~`#[A]
       |[1:18..1:19): B <= _empty_/qe.`~`#[B]
       |[1:20..1:20):  <= _empty_/qe.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/qe.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/qe.`~`#l.
       |[1:24..1:25): A => _empty_/qe.`~`#[A]
       |[1:27..1:28): r <= _empty_/qe.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/qe.`~`#r.
       |[1:30..1:31): B => _empty_/qe.`~`#[B]
       |[2:6..2:7): m <= _empty_/qe.m().
       |[2:11..2:14): Int => scala/Int#
       |[3:10..3:11): L <= local0
       |[3:12..3:12):  <= local1
       |[4:14..4:15): L => local0
       |[4:22..4:23): a <= local2
       |[4:25..4:28): Int => scala/Int#
       |[4:30..4:31): ~ => _empty_/qe.`~`.
       |[4:40..4:41): ~ => _empty_/qe.`~`#
       |[4:41..4:41):  => _empty_/qe.`~`#`<init>`().
       |[5:14..5:15): L => local0
       |[5:23..5:24): ~ => _empty_/qe.`~`.
       |[5:26..5:27): b <= local3
       |[5:29..5:32): Int => scala/Int#
       |[5:40..5:41): ~ => _empty_/qe.`~`#
       |[5:41..5:41):  => _empty_/qe.`~`#`<init>`().
       |[6:14..6:15): L => local0
       |[6:27..6:28): c <= local5
       |[6:30..6:33): Int => scala/Int#
       |[6:35..6:36): ~ => _empty_/qe.`~`.
       |[6:45..6:46): ~ => _empty_/qe.`~`#
       |[6:46..6:46):  => _empty_/qe.`~`#`<init>`().
       |[7:14..7:15): L => local0
       |[7:26..7:27): d <= local6
       |[7:29..7:32): Int => scala/Int#
       |[8:14..8:15): L => local0
       |[8:21..8:25): Some => scala/Some.
       |[8:27..8:28): e <= local7
       |[8:30..8:33): Int => scala/Int#
       |[8:35..8:36): ~ => _empty_/qe.`~`.
       |[8:42..8:46): Some => scala/Some.
       |[8:51..8:52): ~ => _empty_/qe.`~`#
       |[8:52..8:52):  => _empty_/qe.`~`#`<init>`().
       |[10:8..10:9): L => local0
       |[10:9..10:9):  => local1
       |[10:12..10:20): hashCode => java/lang/Object#hashCode().
       |""".stripMargin,
  )

  occurrences(
    "typed variables at depth in a method",
    """|object me {
       |  case class ~[A, B](l: A, r: B)
       |  def m(): Int = {
       |    val ((a: Int) ~ 0) ~ 0 = new ~(new ~(1, 0), 0)
       |    var 0 ~ (b: Int) = new ~(0, 1)
       |    lazy val (c: Int) ~ 0 = new ~(1, 0)
       |    val (_, (d: Int) ~ 0) = (1, new ~(2, 0))
       |    val ((e: Int) ~ 0): (Int ~ Int) = new ~(1, 0)
       |    a + b + c + d + e
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): me <= _empty_/me.
       |[1:13..1:14): ~ <= _empty_/me.`~`#
       |[1:15..1:16): A <= _empty_/me.`~`#[A]
       |[1:18..1:19): B <= _empty_/me.`~`#[B]
       |[1:20..1:20):  <= _empty_/me.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/me.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/me.`~`#l.
       |[1:24..1:25): A => _empty_/me.`~`#[A]
       |[1:27..1:28): r <= _empty_/me.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/me.`~`#r.
       |[1:30..1:31): B => _empty_/me.`~`#[B]
       |[2:6..2:7): m <= _empty_/me.m().
       |[2:11..2:14): Int => scala/Int#
       |[3:10..3:11): a <= local0
       |[3:13..3:16): Int => scala/Int#
       |[3:18..3:19): ~ => _empty_/me.`~`.
       |[3:23..3:24): ~ => _empty_/me.`~`.
       |[3:33..3:34): ~ => _empty_/me.`~`#
       |[3:34..3:34):  => _empty_/me.`~`#`<init>`().
       |[3:39..3:40): ~ => _empty_/me.`~`#
       |[3:40..3:40):  => _empty_/me.`~`#`<init>`().
       |[4:10..4:11): ~ => _empty_/me.`~`.
       |[4:13..4:14): b <= local1
       |[4:16..4:19): Int => scala/Int#
       |[4:27..4:28): ~ => _empty_/me.`~`#
       |[4:28..4:28):  => _empty_/me.`~`#`<init>`().
       |[5:14..5:15): c <= local2
       |[5:17..5:20): Int => scala/Int#
       |[5:22..5:23): ~ => _empty_/me.`~`.
       |[5:32..5:33): ~ => _empty_/me.`~`#
       |[5:33..5:33):  => _empty_/me.`~`#`<init>`().
       |[6:13..6:14): d <= local3
       |[6:16..6:19): Int => scala/Int#
       |[6:21..6:22): ~ => _empty_/me.`~`.
       |[6:36..6:37): ~ => _empty_/me.`~`#
       |[6:37..6:37):  => _empty_/me.`~`#`<init>`().
       |[7:10..7:11): e <= local4
       |[7:13..7:16): Int => scala/Int#
       |[7:18..7:19): ~ => _empty_/me.`~`.
       |[7:25..7:28): Int => scala/Int#
       |[7:29..7:30): ~ => _empty_/me.`~`#
       |[7:31..7:34): Int => scala/Int#
       |[7:42..7:43): ~ => _empty_/me.`~`#
       |[7:43..7:43):  => _empty_/me.`~`#`<init>`().
       |[8:4..8:5): a => local0
       |[8:6..8:7): + => scala/Int#`+`(+4).
       |[8:8..8:9): b => local1
       |[8:10..8:11): + => scala/Int#`+`(+4).
       |[8:12..8:13): c => local2
       |[8:14..8:15): + => scala/Int#`+`(+4).
       |[8:16..8:17): d => local3
       |[8:18..8:19): + => scala/Int#`+`(+4).
       |[8:20..8:21): e => local4
       |""".stripMargin,
  )

  occurrences(
    "patterns with an access qualifier in a local class",
    """|object ql {
       |  def m(): Int = {
       |    class L {
       |      private[L] val Some(a) = Some(1)
       |      private[L] val (b: Int) = 1
       |      private[L] val c, d = 1
       |      private[L] val e = 1
       |    }
       |    new L().hashCode
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): ql <= _empty_/ql.
       |[1:6..1:7): m <= _empty_/ql.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:10..2:11): L <= local0
       |[2:12..2:12):  <= local1
       |[3:14..3:15): L => local0
       |[3:21..3:25): Some => scala/Some.
       |[3:26..3:27): a <= local2
       |[3:31..3:35): Some => scala/Some.
       |[4:14..4:15): L => local0
       |[4:22..4:23): b <= local3
       |[4:25..4:28): Int => scala/Int#
       |[5:14..5:15): L => local0
       |[5:21..5:22): c <= local4
       |[5:24..5:25): d <= local5
       |[6:14..6:15): L => local0
       |[6:21..6:22): e <= local6
       |[8:8..8:9): L => local0
       |[8:9..8:9):  => local1
       |[8:12..8:20): hashCode => java/lang/Object#hashCode().
       |""".stripMargin,
  )

  occurrences(
    "infix and typed patterns in a for and a case",
    """|object cf {
       |  case class ~[A, B](l: A, r: B)
       |  for (Some(a ~ 0) <- Option(Option(new ~(1, 0)))) yield a
       |  for { x <- Option(1); (b ~ 0, c) = (new ~(x, 0), x); (d: Int) = c } yield b + c + d
       |  (new ~(1, 0): Any) match {
       |    case (e: Int) ~ 0 => e
       |    case Some(f ~ 0) => f
       |    case (g ~ 0, h) => h
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): cf <= _empty_/cf.
       |[1:13..1:14): ~ <= _empty_/cf.`~`#
       |[1:15..1:16): A <= _empty_/cf.`~`#[A]
       |[1:18..1:19): B <= _empty_/cf.`~`#[B]
       |[1:20..1:20):  <= _empty_/cf.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/cf.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/cf.`~`#l.
       |[1:24..1:25): A => _empty_/cf.`~`#[A]
       |[1:27..1:28): r <= _empty_/cf.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/cf.`~`#r.
       |[1:30..1:31): B => _empty_/cf.`~`#[B]
       |[2:7..2:11): Some => scala/Some.
       |[2:12..2:13): a <= local2
       |[2:14..2:15): ~ => _empty_/cf.`~`.
       |[2:22..2:28): Option => scala/Option.
       |[2:29..2:35): Option => scala/Option.
       |[2:40..2:41): ~ => _empty_/cf.`~`#
       |[2:41..2:41):  => _empty_/cf.`~`#`<init>`().
       |[2:57..2:58): a => local2
       |[3:8..3:9): x <= local3
       |[3:13..3:19): Option => scala/Option.
       |[3:25..3:26): b <= local5
       |[3:27..3:28): ~ => _empty_/cf.`~`.
       |[3:32..3:33): c <= local6
       |[3:42..3:43): ~ => _empty_/cf.`~`#
       |[3:43..3:43):  => _empty_/cf.`~`#`<init>`().
       |[3:44..3:45): x => local3
       |[3:51..3:52): x => local3
       |[3:56..3:57): d <= local7
       |[3:59..3:62): Int => scala/Int#
       |[3:66..3:67): c => local6
       |[3:76..3:77): b => local5
       |[3:78..3:79): + => scala/Int#`+`(+4).
       |[3:80..3:81): c => local6
       |[3:82..3:83): + => scala/Int#`+`(+4).
       |[3:84..3:85): d => local7
       |[4:7..4:8): ~ => _empty_/cf.`~`#
       |[4:8..4:8):  => _empty_/cf.`~`#`<init>`().
       |[4:16..4:19): Any => scala/Any#
       |[5:10..5:11): e <= local8
       |[5:13..5:16): Int => scala/Int#
       |[5:18..5:19): ~ => _empty_/cf.`~`.
       |[5:25..5:26): e => local8
       |[6:9..6:13): Some => scala/Some.
       |[6:14..6:15): f <= local9
       |[6:16..6:17): ~ => _empty_/cf.`~`.
       |[6:24..6:25): f => local9
       |[7:10..7:11): g <= local10
       |[7:12..7:13): ~ => _empty_/cf.`~`.
       |[7:17..7:18): h <= local11
       |[7:23..7:24): h => local11
       |""".stripMargin,
  )

  occurrences(
    "empty and nested argument clauses in a val, a for and a case",
    """|object an {
       |  case class ~[A, B](l: A, r: B)
       |  case class E()
       |  val E() = E()
       |  val Some(Some((a: Int) ~ 0)) = Some(Some(new ~(1, 0)))
       |  val (Some(b ~ 0), (c, Some(_))) = (Some(new ~(1, 0)), (2, Some(3)))
       |  for (E() <- Option(E()); Some((d: Int) ~ 0) <- Option(Some(new ~(1, 0)))) yield d
       |  for { x <- Option(1); (Some(e ~ 0), f) = (Some(new ~(x, 0)), x) } yield e + f
       |  (new ~(1, 0): Any) match {
       |    case E() => 0
       |    case Some(Some((g: Int) ~ 0)) => g
       |    case (Some(h ~ 0), (i, Some(_))) => i
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): an <= _empty_/an.
       |[1:13..1:14): ~ <= _empty_/an.`~`#
       |[1:15..1:16): A <= _empty_/an.`~`#[A]
       |[1:18..1:19): B <= _empty_/an.`~`#[B]
       |[1:20..1:20):  <= _empty_/an.`~`#`<init>`().
       |[1:21..1:22): l <= _empty_/an.`~`#`<init>`().(l)
       |[1:21..1:22): l <= _empty_/an.`~`#l.
       |[1:24..1:25): A => _empty_/an.`~`#[A]
       |[1:27..1:28): r <= _empty_/an.`~`#`<init>`().(r)
       |[1:27..1:28): r <= _empty_/an.`~`#r.
       |[1:30..1:31): B => _empty_/an.`~`#[B]
       |[2:13..2:14): E <= _empty_/an.E#
       |[2:14..2:14):  <= _empty_/an.E#`<init>`().
       |[3:6..3:7): E => _empty_/an.E.
       |[3:12..3:13): E => _empty_/an.E.
       |[4:6..4:10): Some => scala/Some.
       |[4:11..4:15): Some => scala/Some.
       |[4:17..4:18): a <= _empty_/an.a.
       |[4:20..4:23): Int => scala/Int#
       |[4:25..4:26): ~ => _empty_/an.`~`.
       |[4:33..4:37): Some => scala/Some.
       |[4:38..4:42): Some => scala/Some.
       |[4:47..4:48): ~ => _empty_/an.`~`#
       |[4:48..4:48):  => _empty_/an.`~`#`<init>`().
       |[5:7..5:11): Some => scala/Some.
       |[5:12..5:13): b <= _empty_/an.b.
       |[5:14..5:15): ~ => _empty_/an.`~`.
       |[5:21..5:22): c <= _empty_/an.c.
       |[5:24..5:28): Some => scala/Some.
       |[5:37..5:41): Some => scala/Some.
       |[5:46..5:47): ~ => _empty_/an.`~`#
       |[5:47..5:47):  => _empty_/an.`~`#`<init>`().
       |[5:60..5:64): Some => scala/Some.
       |[6:7..6:8): E => _empty_/an.E.
       |[6:14..6:20): Option => scala/Option.
       |[6:21..6:22): E => _empty_/an.E.
       |[6:27..6:31): Some => scala/Some.
       |[6:33..6:34): d <= local4
       |[6:36..6:39): Int => scala/Int#
       |[6:41..6:42): ~ => _empty_/an.`~`.
       |[6:49..6:55): Option => scala/Option.
       |[6:56..6:60): Some => scala/Some.
       |[6:65..6:66): ~ => _empty_/an.`~`#
       |[6:66..6:66):  => _empty_/an.`~`#`<init>`().
       |[6:82..6:83): d => local4
       |[7:8..7:9): x <= local5
       |[7:13..7:19): Option => scala/Option.
       |[7:25..7:29): Some => scala/Some.
       |[7:30..7:31): e <= local7
       |[7:32..7:33): ~ => _empty_/an.`~`.
       |[7:38..7:39): f <= local8
       |[7:44..7:48): Some => scala/Some.
       |[7:53..7:54): ~ => _empty_/an.`~`#
       |[7:54..7:54):  => _empty_/an.`~`#`<init>`().
       |[7:55..7:56): x => local5
       |[7:63..7:64): x => local5
       |[7:74..7:75): e => local7
       |[7:76..7:77): + => scala/Int#`+`(+4).
       |[7:78..7:79): f => local8
       |[8:7..8:8): ~ => _empty_/an.`~`#
       |[8:8..8:8):  => _empty_/an.`~`#`<init>`().
       |[8:16..8:19): Any => scala/Any#
       |[9:9..9:10): E => _empty_/an.E.
       |[10:9..10:13): Some => scala/Some.
       |[10:14..10:18): Some => scala/Some.
       |[10:20..10:21): g <= local9
       |[10:23..10:26): Int => scala/Int#
       |[10:28..10:29): ~ => _empty_/an.`~`.
       |[10:37..10:38): g => local9
       |[11:10..11:14): Some => scala/Some.
       |[11:15..11:16): h <= local10
       |[11:17..11:18): ~ => _empty_/an.`~`.
       |[11:24..11:25): i <= local11
       |[11:27..11:31): Some => scala/Some.
       |[11:40..11:41): i => local11
       |""".stripMargin,
  )

  occurrences(
    "patterns with an access qualifier in an annotation of a pattern",
    """|object ra {
       |  class ann(x: Any) extends scala.annotation.StaticAnnotation
       |  @ann({ object o { private[o] val (a, b) = (1, 2) }; o }) val (c, d) = (1, 2)
       |  @ann({ object q { private[q] val (e, f) = (1, 2) }; q }) val g = 1
       |}
       |""".stripMargin,
    """|[0:7..0:9): ra <= _empty_/ra.
       |[1:8..1:11): ann <= _empty_/ra.ann#
       |[1:11..1:11):  <= _empty_/ra.ann#`<init>`().
       |[1:12..1:13): x <= _empty_/ra.ann#`<init>`().(x)
       |[1:12..1:13): x <= _empty_/ra.ann#x.
       |[1:15..1:18): Any => scala/Any#
       |[1:28..1:33): scala => scala/
       |[1:34..1:44): annotation => scala/annotation/
       |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
       |[2:3..2:6): ann => _empty_/ra.ann#
       |[2:6..2:6):  => _empty_/ra.ann#`<init>`().
       |[2:16..2:17): o <= local0
       |[2:28..2:29): o => local0
       |[2:36..2:37): a <= local2
       |[2:39..2:40): b <= local3
       |[2:54..2:55): o => local0
       |[2:64..2:65): c <= _empty_/ra.c.
       |[2:67..2:68): d <= _empty_/ra.d.
       |[3:3..3:6): ann => _empty_/ra.ann#
       |[3:6..3:6):  => _empty_/ra.ann#`<init>`().
       |[3:16..3:17): q <= local4
       |[3:28..3:29): q => local4
       |[3:36..3:37): e <= local6
       |[3:39..3:40): f <= local7
       |[3:54..3:55): q => local4
       |[3:63..3:64): g <= _empty_/ra.g.
       |""".stripMargin,
  )

  occurrences(
    "local definitions in early definitions named like members",
    """|trait rw
       |class si extends { val a = { val a = 1; a } } with rw
       |class sj extends { val a = { def a(x: Int) = x; a(1) } } with rw
       |class sk extends { val (b, c) = { val b = 1; (b, 2) } } with rw
       |class sl extends { val a = { val Some(a) = Option(1); a } } with rw
       |""".stripMargin,
    """|[0:6..0:8): rw <= _empty_/rw#
       |[1:6..1:8): si <= _empty_/si#
       |[1:9..1:9):  <= _empty_/si#`<init>`().
       |[1:23..1:24): a <= _empty_/si#a.
       |[1:33..1:34): a <= local0
       |[1:40..1:41): a => local0
       |[1:51..1:53): rw => _empty_/rw#
       |[1:53..1:53):  => java/lang/Object#`<init>`().
       |[2:6..2:8): sj <= _empty_/sj#
       |[2:9..2:9):  <= _empty_/sj#`<init>`().
       |[2:23..2:24): a <= _empty_/sj#a.
       |[2:33..2:34): a <= local1
       |[2:35..2:36): x <= local2
       |[2:38..2:41): Int => scala/Int#
       |[2:45..2:46): x => local2
       |[2:48..2:49): a => local1
       |[2:62..2:64): rw => _empty_/rw#
       |[2:64..2:64):  => java/lang/Object#`<init>`().
       |[3:6..3:8): sk <= _empty_/sk#
       |[3:9..3:9):  <= _empty_/sk#`<init>`().
       |[3:24..3:25): b <= _empty_/sk#b.
       |[3:27..3:28): c <= _empty_/sk#c.
       |[3:38..3:39): b <= local3
       |[3:46..3:47): b => local3
       |[3:61..3:63): rw => _empty_/rw#
       |[3:63..3:63):  => java/lang/Object#`<init>`().
       |[4:6..4:8): sl <= _empty_/sl#
       |[4:9..4:9):  <= _empty_/sl#`<init>`().
       |[4:23..4:24): a <= _empty_/sl#a.
       |[4:33..4:37): Some => scala/Some.
       |[4:38..4:39): a <= local4
       |[4:43..4:49): Option => scala/Option.
       |[4:54..4:55): a => local4
       |[4:65..4:67): rw => _empty_/rw#
       |[4:67..4:67):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "matches and patterns in early definitions named like members",
    """|trait rw
       |class sm extends { val a = 1 match { case a => a } } with rw
       |class sn extends { val a = { val (a, b) = (1, 2); a + b } } with rw
       |class so extends { val Some(a) = { val a = 1; Option(a) } } with rw
       |class sp extends { val Some(a) = 1 match { case a => Option(a) } } with rw
       |class sq extends { val (b, c) = { val x = 1; (x, 2) } } with rw { val x = 3 }
       |class sr extends { val (b, c) = { val c = 1; (1, c) } } with rw
       |""".stripMargin,
    """|[0:6..0:8): rw <= _empty_/rw#
       |[1:6..1:8): sm <= _empty_/sm#
       |[1:9..1:9):  <= _empty_/sm#`<init>`().
       |[1:23..1:24): a <= _empty_/sm#a.
       |[1:42..1:43): a <= local0
       |[1:47..1:48): a => local0
       |[1:58..1:60): rw => _empty_/rw#
       |[1:60..1:60):  => java/lang/Object#`<init>`().
       |[2:6..2:8): sn <= _empty_/sn#
       |[2:9..2:9):  <= _empty_/sn#`<init>`().
       |[2:23..2:24): a <= _empty_/sn#a.
       |[2:34..2:35): a <= local1
       |[2:37..2:38): b <= local2
       |[2:50..2:51): a => local1
       |[2:52..2:53): + => scala/Int#`+`(+4).
       |[2:54..2:55): b => local2
       |[2:65..2:67): rw => _empty_/rw#
       |[2:67..2:67):  => java/lang/Object#`<init>`().
       |[3:6..3:8): so <= _empty_/so#
       |[3:9..3:9):  <= _empty_/so#`<init>`().
       |[3:23..3:27): Some => scala/Some.
       |[3:28..3:29): a <= _empty_/so#a.
       |[3:39..3:40): a <= local3
       |[3:46..3:52): Option => scala/Option.
       |[3:53..3:54): a => local3
       |[3:65..3:67): rw => _empty_/rw#
       |[3:67..3:67):  => java/lang/Object#`<init>`().
       |[4:6..4:8): sp <= _empty_/sp#
       |[4:9..4:9):  <= _empty_/sp#`<init>`().
       |[4:23..4:27): Some => scala/Some.
       |[4:28..4:29): a <= _empty_/sp#a.
       |[4:48..4:49): a <= local4
       |[4:53..4:59): Option => scala/Option.
       |[4:60..4:61): a => local4
       |[4:72..4:74): rw => _empty_/rw#
       |[4:74..4:74):  => java/lang/Object#`<init>`().
       |[5:6..5:8): sq <= _empty_/sq#
       |[5:9..5:9):  <= _empty_/sq#`<init>`().
       |[5:24..5:25): b <= _empty_/sq#b.
       |[5:27..5:28): c <= _empty_/sq#c.
       |[5:38..5:39): x <= local5
       |[5:46..5:47): x => local5
       |[5:61..5:63): rw => _empty_/rw#
       |[5:63..5:63):  => java/lang/Object#`<init>`().
       |[5:70..5:71): x <= _empty_/sq#x.
       |[6:6..6:8): sr <= _empty_/sr#
       |[6:9..6:9):  <= _empty_/sr#`<init>`().
       |[6:24..6:25): b <= _empty_/sr#b.
       |[6:27..6:28): c <= _empty_/sr#c.
       |[6:38..6:39): c <= local6
       |[6:49..6:50): c => local6
       |[6:61..6:63): rw => _empty_/rw#
       |[6:63..6:63):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "other local definitions in early definitions named like members",
    """|trait rw
       |class ss extends { val a = { lazy val a = 1; a } } with rw
       |class st extends { var a = { val a = 1; a } } with rw
       |class su extends { val a: Int = { val a = 1; a } } with rw
       |class sv extends { val a: Int = { object a; 1 } } with rw
       |""".stripMargin,
    """|[0:6..0:8): rw <= _empty_/rw#
       |[1:6..1:8): ss <= _empty_/ss#
       |[1:9..1:9):  <= _empty_/ss#`<init>`().
       |[1:23..1:24): a <= _empty_/ss#a.
       |[1:38..1:39): a <= local0
       |[1:45..1:46): a => local0
       |[1:56..1:58): rw => _empty_/rw#
       |[1:58..1:58):  => java/lang/Object#`<init>`().
       |[2:6..2:8): st <= _empty_/st#
       |[2:9..2:9):  <= _empty_/st#`<init>`().
       |[2:23..2:24): a <= _empty_/st#a().
       |[2:33..2:34): a <= local1
       |[2:40..2:41): a => local1
       |[2:51..2:53): rw => _empty_/rw#
       |[2:53..2:53):  => java/lang/Object#`<init>`().
       |[3:6..3:8): su <= _empty_/su#
       |[3:9..3:9):  <= _empty_/su#`<init>`().
       |[3:23..3:24): a <= _empty_/su#a.
       |[3:26..3:29): Int => scala/Int#
       |[3:38..3:39): a <= local2
       |[3:45..3:46): a => local2
       |[3:56..3:58): rw => _empty_/rw#
       |[3:58..3:58):  => java/lang/Object#`<init>`().
       |[4:6..4:8): sv <= _empty_/sv#
       |[4:9..4:9):  <= _empty_/sv#`<init>`().
       |[4:23..4:24): a <= _empty_/sv#a.
       |[4:26..4:29): Int => scala/Int#
       |[4:41..4:42): a <= local3
       |[4:55..4:57): rw => _empty_/rw#
       |[4:57..4:57):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "local definitions in early definitions of an object, a trait and an anonymous class",
    """|trait rw
       |object sw extends { val a = { val a = 1; a } } with rw
       |trait sx extends { val a = { val a = 1; a } } with rw
       |object sy { val x = new { val a = { val a = 1; a } } with rw }
       |""".stripMargin,
    """|[0:6..0:8): rw <= _empty_/rw#
       |[1:7..1:9): sw <= _empty_/sw.
       |[1:24..1:25): a <= _empty_/sw.a.
       |[1:34..1:35): a <= local0
       |[1:41..1:42): a => local0
       |[1:52..1:54): rw => _empty_/rw#
       |[1:54..1:54):  => java/lang/Object#`<init>`().
       |[2:6..2:8): sx <= _empty_/sx#
       |[2:9..2:9):  <= _empty_/sx#`<init>`().
       |[2:23..2:24): a <= _empty_/sx#a.
       |[2:33..2:34): a <= local1
       |[2:40..2:41): a => local1
       |[2:51..2:53): rw => _empty_/rw#
       |[3:7..3:9): sy <= _empty_/sy.
       |[3:16..3:17): x <= _empty_/sy.x.
       |[3:24..3:24):  <= local2
       |[3:30..3:31): a <= local3
       |[3:40..3:41): a <= local4
       |[3:47..3:48): a => local4
       |[3:58..3:60): rw => _empty_/rw#
       |[3:60..3:60):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "lambdas, local classes and nested blocks in early definitions",
    """|trait rw
       |class sz extends { val a = Option(1).map(a => a) } with rw
       |class ta extends { val a = { class C { val a = 1 }; new C().a } } with rw
       |class tb extends { val a = { def b = { val a = 1; a }; b } } with rw
       |class tc extends { val a = { val b = { val a = 1; a }; b } } with rw
       |""".stripMargin,
    """|[0:6..0:8): rw <= _empty_/rw#
       |[1:6..1:8): sz <= _empty_/sz#
       |[1:9..1:9):  <= _empty_/sz#`<init>`().
       |[1:23..1:24): a <= _empty_/sz#a.
       |[1:27..1:33): Option => scala/Option.
       |[1:37..1:40): map => scala/Option#map().
       |[1:41..1:42): a <= local0
       |[1:46..1:47): a => local0
       |[1:56..1:58): rw => _empty_/rw#
       |[1:58..1:58):  => java/lang/Object#`<init>`().
       |[2:6..2:8): ta <= _empty_/ta#
       |[2:9..2:9):  <= _empty_/ta#`<init>`().
       |[2:23..2:24): a <= _empty_/ta#a.
       |[2:35..2:36): C <= local1
       |[2:37..2:37):  <= local2
       |[2:43..2:44): a <= local3
       |[2:56..2:57): C => local1
       |[2:57..2:57):  => local2
       |[2:60..2:61): a => local3
       |[2:71..2:73): rw => _empty_/rw#
       |[2:73..2:73):  => java/lang/Object#`<init>`().
       |[3:6..3:8): tb <= _empty_/tb#
       |[3:9..3:9):  <= _empty_/tb#`<init>`().
       |[3:23..3:24): a <= _empty_/tb#a.
       |[3:33..3:34): b <= local4
       |[3:43..3:44): a <= local5
       |[3:50..3:51): a => local5
       |[3:55..3:56): b => local4
       |[3:66..3:68): rw => _empty_/rw#
       |[3:68..3:68):  => java/lang/Object#`<init>`().
       |[4:6..4:8): tc <= _empty_/tc#
       |[4:9..4:9):  <= _empty_/tc#`<init>`().
       |[4:23..4:24): a <= _empty_/tc#a.
       |[4:33..4:34): b <= local7
       |[4:43..4:44): a <= local6
       |[4:50..4:51): a => local6
       |[4:55..4:56): b => local7
       |[4:66..4:68): rw => _empty_/rw#
       |[4:68..4:68):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "patterns in early definitions",
    """|trait rt
       |class rc extends { val a = 1; val (b, c) = (1, 2); val (d: Int) = 1 } with rt {
       |  def f = a + b + c + d
       |}
       |""".stripMargin,
    """|[0:6..0:8): rt <= _empty_/rt#
       |[1:6..1:8): rc <= _empty_/rc#
       |[1:9..1:9):  <= _empty_/rc#`<init>`().
       |[1:23..1:24): a <= _empty_/rc#a.
       |[1:35..1:36): b <= _empty_/rc#b.
       |[1:38..1:39): c <= _empty_/rc#c.
       |[1:56..1:57): d <= _empty_/rc#d.
       |[1:59..1:62): Int => scala/Int#
       |[1:75..1:77): rt => _empty_/rt#
       |[1:77..1:77):  => java/lang/Object#`<init>`().
       |[2:6..2:7): f <= _empty_/rc#f().
       |[2:10..2:11): a => _empty_/rc#a.
       |[2:12..2:13): + => scala/Int#`+`(+4).
       |[2:14..2:15): b => _empty_/rc#b.
       |[2:16..2:17): + => scala/Int#`+`(+4).
       |[2:18..2:19): c => _empty_/rc#c.
       |[2:20..2:21): + => scala/Int#`+`(+4).
       |[2:22..2:23): d => _empty_/rc#d.
       |""".stripMargin,
  )

  occurrences(
    "var pattern of one variable in early definitions",
    """|trait vt
       |class vc extends { var Some(a) = Option(1) } with vt
       |""".stripMargin,
    """|[0:6..0:8): vt <= _empty_/vt#
       |[1:6..1:8): vc <= _empty_/vc#
       |[1:9..1:9):  <= _empty_/vc#`<init>`().
       |[1:23..1:27): Some => scala/Some.
       |[1:28..1:29): a <= _empty_/vc#a().
       |[1:33..1:39): Option => scala/Option.
       |[1:50..1:52): vt => _empty_/vt#
       |[1:52..1:52):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "var patterns in early definitions",
    """|trait vt
       |class ve1 extends { var a :: Nil = List(1) } with vt
       |class ve2 extends { var Tuple2(a, _) = (1, 2) } with vt
       |class ve3 extends { var (a, _) = (1, 2) } with vt
       |class ve4 extends { var (a, b) = (1, 2) } with vt
       |""".stripMargin,
    s"""|[0:6..0:8): vt <= _empty_/vt#
        |[1:6..1:9): ve1 <= _empty_/ve1#
        |[1:10..1:10):  <= _empty_/ve1#`<init>`().
        |[1:24..1:25): a <= _empty_/ve1#a().
        |[1:26..1:28): :: => scala/package.`::`.
        |[1:29..1:32): Nil => ${if (ScalaVersion.is212) "scala/collection/immutable/Nil." else "scala/package.Nil."}
        |[1:35..1:39): List => ${if (ScalaVersion.is212) "scala/collection/immutable/List." else "scala/package.List."}
        |[1:50..1:52): vt => _empty_/vt#
        |[1:52..1:52):  => java/lang/Object#`<init>`().
        |[2:6..2:9): ve2 <= _empty_/ve2#
        |[2:10..2:10):  <= _empty_/ve2#`<init>`().
        |[2:24..2:30): Tuple2 => scala/Tuple2.
        |[2:31..2:32): a <= _empty_/ve2#a().
        |[2:53..2:55): vt => _empty_/vt#
        |[2:55..2:55):  => java/lang/Object#`<init>`().
        |[3:6..3:9): ve3 <= _empty_/ve3#
        |[3:10..3:10):  <= _empty_/ve3#`<init>`().
        |[3:25..3:26): a <= _empty_/ve3#a().
        |[3:47..3:49): vt => _empty_/vt#
        |[3:49..3:49):  => java/lang/Object#`<init>`().
        |[4:6..4:9): ve4 <= _empty_/ve4#
        |[4:10..4:10):  <= _empty_/ve4#`<init>`().
        |[4:25..4:26): a <= _empty_/ve4#a().
        |[4:28..4:29): b <= _empty_/ve4#b().
        |[4:47..4:49): vt => _empty_/vt#
        |[4:49..4:49):  => java/lang/Object#`<init>`().
        |""".stripMargin,
  )

  occurrences(
    "var pattern of one variable in early definitions of an object, a trait and local classes",
    """|trait vt
       |object vo extends { var Some(a) = Option(1) } with vt
       |trait vr extends { var Some(a) = Option(1) } with vt
       |object vn {
       |  val x = new { var Some(a) = Option(1) } with vt
       |  def f = { class vl extends { var Some(a) = Option(1) } with vt; new vl }
       |}
       |""".stripMargin,
    """|[0:6..0:8): vt <= _empty_/vt#
       |[1:7..1:9): vo <= _empty_/vo.
       |[1:24..1:28): Some => scala/Some.
       |[1:29..1:30): a <= _empty_/vo.a().
       |[1:34..1:40): Option => scala/Option.
       |[1:51..1:53): vt => _empty_/vt#
       |[1:53..1:53):  => java/lang/Object#`<init>`().
       |[2:6..2:8): vr <= _empty_/vr#
       |[2:9..2:9):  <= _empty_/vr#`<init>`().
       |[2:23..2:27): Some => scala/Some.
       |[2:28..2:29): a <= _empty_/vr#a().
       |[2:33..2:39): Option => scala/Option.
       |[2:50..2:52): vt => _empty_/vt#
       |[3:7..3:9): vn <= _empty_/vn.
       |[4:6..4:7): x <= _empty_/vn.x.
       |[4:14..4:14):  <= local0
       |[4:20..4:24): Some => scala/Some.
       |[4:25..4:26): a <= local1
       |[4:30..4:36): Option => scala/Option.
       |[4:47..4:49): vt => _empty_/vt#
       |[4:49..4:49):  => java/lang/Object#`<init>`().
       |[5:6..5:7): f <= _empty_/vn.f().
       |[5:18..5:20): vl <= local7
       |[5:21..5:21):  <= local8
       |[5:35..5:39): Some => scala/Some.
       |[5:40..5:41): a <= local4
       |[5:45..5:51): Option => scala/Option.
       |[5:62..5:64): vt => _empty_/vt#
       |[5:64..5:64):  => java/lang/Object#`<init>`().
       |[5:70..5:72): vl => local7
       |[5:72..5:72):  => local8
       |""".stripMargin,
  )

  occurrences(
    "patterns in annotations of pattern definitions in a trait",
    """|trait rd {
       |  class ann(x: Any) extends scala.annotation.StaticAnnotation
       |  @ann({ object o { private[o] val (a, b) = (1, 2) }; o }) var (c, d) = (1, 2)
       |  @ann({ object q { private[q] val (e, f) = (1, 2) }; q }) val g, h: Int
       |  @ann((1, 2) match { case (i, j) => i }) val (k, l) = (1, 2)
       |  @ann(Option(1) match { case Some(m) => m }) val Some(n) = Some(1)
       |}
       |""".stripMargin,
    """|[0:6..0:8): rd <= _empty_/rd#
       |[0:9..0:9):  <= _empty_/rd#`<init>`().
       |[1:8..1:11): ann <= _empty_/rd#ann#
       |[1:11..1:11):  <= _empty_/rd#ann#`<init>`().
       |[1:12..1:13): x <= _empty_/rd#ann#`<init>`().(x)
       |[1:12..1:13): x <= _empty_/rd#ann#x.
       |[1:15..1:18): Any => scala/Any#
       |[1:28..1:33): scala => scala/
       |[1:34..1:44): annotation => scala/annotation/
       |[1:45..1:61): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
       |[2:3..2:6): ann => _empty_/rd#ann#
       |[2:6..2:6):  => _empty_/rd#ann#`<init>`().
       |[2:16..2:17): o <= local0
       |[2:28..2:29): o => local0
       |[2:36..2:37): a <= local2
       |[2:39..2:40): b <= local3
       |[2:54..2:55): o => local0
       |[2:64..2:65): c <= _empty_/rd#c().
       |[2:67..2:68): d <= _empty_/rd#d().
       |[3:3..3:6): ann => _empty_/rd#ann#
       |[3:6..3:6):  => _empty_/rd#ann#`<init>`().
       |[3:16..3:17): q <= local4
       |[3:28..3:29): q => local4
       |[3:36..3:37): e <= local6
       |[3:39..3:40): f <= local7
       |[3:54..3:55): q => local4
       |[3:63..3:64): g <= _empty_/rd#g.
       |[3:66..3:67): h <= _empty_/rd#h.
       |[3:69..3:72): Int => scala/Int#
       |[4:3..4:6): ann => _empty_/rd#ann#
       |[4:6..4:6):  => _empty_/rd#ann#`<init>`().
       |[4:28..4:29): i <= local8
       |[4:31..4:32): j <= local9
       |[4:37..4:38): i => local8
       |[4:47..4:48): k <= _empty_/rd#k.
       |[4:50..4:51): l <= _empty_/rd#l.
       |[5:3..5:6): ann => _empty_/rd#ann#
       |[5:6..5:6):  => _empty_/rd#ann#`<init>`().
       |[5:7..5:13): Option => scala/Option.
       |[5:30..5:34): Some => scala/Some.
       |[5:35..5:36): m <= local10
       |[5:41..5:42): m => local10
       |[5:50..5:54): Some => scala/Some.
       |[5:55..5:56): n <= _empty_/rd#n.
       |[5:60..5:64): Some => scala/Some.
       |""".stripMargin,
  )

  symbols(
    "patterns in early definitions",
    """|trait rt
       |class rc extends { val a = 1; val (b, c) = (1, 2); val (d: Int) = 1 } with rt {
       |  def f = a + b + c + d
       |}
       |""".stripMargin,
    """|_empty_/rc# => class rc extends AnyRef with rt { +7 decls }
       |  AnyRef => scala/AnyRef#
       |  rt => _empty_/rt#
       |_empty_/rc#`<init>`(). => primary ctor <init>()
       |_empty_/rc#a. => val method a: Int
       |  Int => scala/Int#
       |_empty_/rc#b. => val method b: Int
       |  Int => scala/Int#
       |_empty_/rc#c. => val method c: Int
       |  Int => scala/Int#
       |_empty_/rc#d. => val method d: Int
       |  Int => scala/Int#
       |_empty_/rc#f(). => method f: Int
       |  Int => scala/Int#
       |_empty_/rc#x$1. => private[this] synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/rt# => trait rt extends AnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  occurrences(
    "patterns in early definitions of an object, a trait and an anonymous class",
    """|trait ru
       |object ro extends { val (a, b) = (1, 2); val Some(c) = Some(1); var (d, e) = (1, 2) } with ru {
       |  def f = a + b + c + d + e
       |}
       |trait rv extends { val (a, b) = (1, 2) } with ru { def f = a + b }
       |object rn { val x = new { val (a, b) = (1, 2) } with ru { def f = a + b } }
       |""".stripMargin,
    """|[0:6..0:8): ru <= _empty_/ru#
       |[1:7..1:9): ro <= _empty_/ro.
       |[1:25..1:26): a <= _empty_/ro.a.
       |[1:28..1:29): b <= _empty_/ro.b.
       |[1:45..1:49): Some => scala/Some.
       |[1:50..1:51): c <= _empty_/ro.c.
       |[1:55..1:59): Some => scala/Some.
       |[1:69..1:70): d <= _empty_/ro.d().
       |[1:72..1:73): e <= _empty_/ro.e().
       |[1:91..1:93): ru => _empty_/ru#
       |[1:93..1:93):  => java/lang/Object#`<init>`().
       |[2:6..2:7): f <= _empty_/ro.f().
       |[2:10..2:11): a => _empty_/ro.a.
       |[2:12..2:13): + => scala/Int#`+`(+4).
       |[2:14..2:15): b => _empty_/ro.b.
       |[2:16..2:17): + => scala/Int#`+`(+4).
       |[2:18..2:19): c => _empty_/ro.c.
       |[2:20..2:21): + => scala/Int#`+`(+4).
       |[2:22..2:23): d => _empty_/ro.d().
       |[2:24..2:25): + => scala/Int#`+`(+4).
       |[2:26..2:27): e => _empty_/ro.e().
       |[4:6..4:8): rv <= _empty_/rv#
       |[4:9..4:9):  <= _empty_/rv#`<init>`().
       |[4:24..4:25): a <= _empty_/rv#a.
       |[4:27..4:28): b <= _empty_/rv#b.
       |[4:46..4:48): ru => _empty_/ru#
       |[4:55..4:56): f <= _empty_/rv#f().
       |[4:59..4:60): a => _empty_/rv#a.
       |[4:61..4:62): + => scala/Int#`+`(+4).
       |[4:63..4:64): b => _empty_/rv#b.
       |[5:7..5:9): rn <= _empty_/rn.
       |[5:16..5:17): x <= _empty_/rn.x.
       |[5:24..5:24):  <= local0
       |[5:31..5:32): a <= local2
       |[5:34..5:35): b <= local3
       |[5:53..5:55): ru => _empty_/ru#
       |[5:55..5:55):  => java/lang/Object#`<init>`().
       |[5:62..5:63): f <= local4
       |[5:66..5:67): a => local2
       |[5:68..5:69): + => scala/Int#`+`(+4).
       |[5:70..5:71): b => local3
       |""".stripMargin,
  )

  symbols(
    "patterns in early definitions of an object, a trait and an anonymous class",
    """|trait ru
       |object ro extends { val (a, b) = (1, 2); val Some(c) = Some(1); var (d, e) = (1, 2) } with ru {
       |  def f = a + b + c + d + e
       |}
       |trait rv extends { val (a, b) = (1, 2) } with ru { def f = a + b }
       |object rn { val x = new { val (a, b) = (1, 2) } with ru { def f = a + b } }
       |""".stripMargin,
    """|_empty_/rn. => final object rn extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/rn.x. => val method x: AnyRef with ru { val def a: Int; val def b: Int; def f: Int }
       |  AnyRef => scala/AnyRef#
       |  ru => _empty_/ru#
       |  a => local2
       |  Int => scala/Int#
       |  b => local3
       |  f => local4
       |_empty_/ro. => final object ro extends AnyRef with ru { +10 decls }
       |  AnyRef => scala/AnyRef#
       |  ru => _empty_/ru#
       |_empty_/ro.`d_=`(). => var method d_=(x$1: Int): Unit
       |  x$1 => _empty_/ro.`d_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/ro.`d_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/ro.`e_=`(). => var method e_=(x$1: Int): Unit
       |  x$1 => _empty_/ro.`e_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/ro.`e_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/ro.a. => val method a: Int
       |  Int => scala/Int#
       |_empty_/ro.b. => val method b: Int
       |  Int => scala/Int#
       |_empty_/ro.c. => val method c: Int
       |  Int => scala/Int#
       |_empty_/ro.d(). => var method d: Int
       |  Int => scala/Int#
       |_empty_/ro.e(). => var method e: Int
       |  Int => scala/Int#
       |_empty_/ro.f(). => method f: Int
       |  Int => scala/Int#
       |_empty_/ro.x$1. => private[this] synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/ro.x$2. => private[this] synthetic val method x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/ru# => trait ru extends AnyRef
       |  AnyRef => scala/AnyRef#
       |_empty_/rv# => trait rv extends AnyRef with ru { +5 decls }
       |  AnyRef => scala/AnyRef#
       |  ru => _empty_/ru#
       |_empty_/rv#`<init>`(). => primary ctor <init>()
       |_empty_/rv#a. => val method a: Int
       |  Int => scala/Int#
       |_empty_/rv#b. => val method b: Int
       |  Int => scala/Int#
       |_empty_/rv#f(). => method f: Int
       |  Int => scala/Int#
       |_empty_/rv#x$3. => private[this] synthetic val method x$3: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local0 => final class $anon extends AnyRef with ru { +4 decls }
       |  AnyRef => scala/AnyRef#
       |  ru => _empty_/ru#
       |local1 => private[this] synthetic val method x$4: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val method a: Int
       |  Int => scala/Int#
       |local3 => val method b: Int
       |  Int => scala/Int#
       |local4 => method f: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "references between early definitions",
    """|trait rw
       |class rx extends { val a = 1; val b = a + 1; val Some(c) = Some(b) } with rw {
       |  def f = c
       |}
       |""".stripMargin,
    """|[0:6..0:8): rw <= _empty_/rw#
       |[1:6..1:8): rx <= _empty_/rx#
       |[1:9..1:9):  <= _empty_/rx#`<init>`().
       |[1:23..1:24): a <= _empty_/rx#a.
       |[1:34..1:35): b <= _empty_/rx#b.
       |[1:38..1:39): a => _empty_/rx#a.
       |[1:40..1:41): + => scala/Int#`+`(+4).
       |[1:49..1:53): Some => scala/Some.
       |[1:54..1:55): c <= _empty_/rx#c.
       |[1:59..1:63): Some => scala/Some.
       |[1:64..1:65): b => _empty_/rx#b.
       |[1:74..1:76): rw => _empty_/rw#
       |[1:76..1:76):  => java/lang/Object#`<init>`().
       |[2:6..2:7): f <= _empty_/rx#f().
       |[2:10..2:11): c => _empty_/rx#c.
       |""".stripMargin,
  )

  occurrences(
    "early definitions beside methods of the same name",
    """|trait rw
       |class ry extends { val a = 1; val (b, c) = (a, 2) } with rw {
       |  def a(x: Int): Int = x
       |  def b(x: Int): Int = x
       |}
       |object rz { val x = new { var d = 1; val e = d } with rw { def d(x: Int): Int = x } }
       |""".stripMargin,
    """|[0:6..0:8): rw <= _empty_/rw#
       |[1:6..1:8): ry <= _empty_/ry#
       |[1:9..1:9):  <= _empty_/ry#`<init>`().
       |[1:23..1:24): a <= _empty_/ry#a.
       |[1:35..1:36): b <= _empty_/ry#b.
       |[1:38..1:39): c <= _empty_/ry#c.
       |[1:44..1:45): a => _empty_/ry#a.
       |[1:57..1:59): rw => _empty_/rw#
       |[1:59..1:59):  => java/lang/Object#`<init>`().
       |[2:6..2:7): a <= _empty_/ry#a().
       |[2:8..2:9): x <= _empty_/ry#a().(x)
       |[2:11..2:14): Int => scala/Int#
       |[2:17..2:20): Int => scala/Int#
       |[2:23..2:24): x => _empty_/ry#a().(x)
       |[3:6..3:7): b <= _empty_/ry#b().
       |[3:8..3:9): x <= _empty_/ry#b().(x)
       |[3:11..3:14): Int => scala/Int#
       |[3:17..3:20): Int => scala/Int#
       |[3:23..3:24): x => _empty_/ry#b().(x)
       |[5:7..5:9): rz <= _empty_/rz.
       |[5:16..5:17): x <= _empty_/rz.x.
       |[5:24..5:24):  <= local0
       |[5:30..5:31): d <= local1
       |[5:41..5:42): e <= local3
       |[5:45..5:46): d => local1
       |[5:54..5:56): rw => _empty_/rw#
       |[5:56..5:56):  => java/lang/Object#`<init>`().
       |[5:63..5:64): d <= local4
       |[5:65..5:66): x <= local6
       |[5:68..5:71): Int => scala/Int#
       |[5:74..5:77): Int => scala/Int#
       |[5:80..5:81): x => local6
       |""".stripMargin,
  )

  occurrences(
    "early definitions of a trait, an object and a local class beside methods of the same name",
    """|trait rw
       |trait sa extends { val a = 1; val (b, c) = (a, 2) } with rw {
       |  def a(x: Int): Int = x
       |  def b(x: Int): Int = x
       |}
       |object sb extends { var a = 1; val Some(b) = Some(a); val (c: Int) = a } with rw {
       |  def a(x: Int): Int = x
       |  def b(x: Int): Int = x
       |  def c(x: Int): Int = x
       |}
       |object sc {
       |  def m = {
       |    class sd extends { val a = 1; val (b, c) = (a, 2) } with rw {
       |      def a(x: Int): Int = x
       |      def b(x: Int): Int = x
       |    }
       |    new sd
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:8): rw <= _empty_/rw#
       |[1:6..1:8): sa <= _empty_/sa#
       |[1:9..1:9):  <= _empty_/sa#`<init>`().
       |[1:23..1:24): a <= _empty_/sa#a.
       |[1:35..1:36): b <= _empty_/sa#b.
       |[1:38..1:39): c <= _empty_/sa#c.
       |[1:44..1:45): a => _empty_/sa#a.
       |[1:57..1:59): rw => _empty_/rw#
       |[2:6..2:7): a <= _empty_/sa#a().
       |[2:8..2:9): x <= _empty_/sa#a().(x)
       |[2:11..2:14): Int => scala/Int#
       |[2:17..2:20): Int => scala/Int#
       |[2:23..2:24): x => _empty_/sa#a().(x)
       |[3:6..3:7): b <= _empty_/sa#b().
       |[3:8..3:9): x <= _empty_/sa#b().(x)
       |[3:11..3:14): Int => scala/Int#
       |[3:17..3:20): Int => scala/Int#
       |[3:23..3:24): x => _empty_/sa#b().(x)
       |[5:7..5:9): sb <= _empty_/sb.
       |[5:24..5:25): a <= _empty_/sb.a().
       |[5:35..5:39): Some => scala/Some.
       |[5:40..5:41): b <= _empty_/sb.b.
       |[5:45..5:49): Some => scala/Some.
       |[5:50..5:51): a => _empty_/sb.a().
       |[5:59..5:60): c <= _empty_/sb.c.
       |[5:62..5:65): Int => scala/Int#
       |[5:69..5:70): a => _empty_/sb.a().
       |[5:78..5:80): rw => _empty_/rw#
       |[5:80..5:80):  => java/lang/Object#`<init>`().
       |[6:6..6:7): a <= _empty_/sb.a(+1).
       |[6:8..6:9): x <= _empty_/sb.a(+1).(x)
       |[6:11..6:14): Int => scala/Int#
       |[6:17..6:20): Int => scala/Int#
       |[6:23..6:24): x => _empty_/sb.a(+1).(x)
       |[7:6..7:7): b <= _empty_/sb.b().
       |[7:8..7:9): x <= _empty_/sb.b().(x)
       |[7:11..7:14): Int => scala/Int#
       |[7:17..7:20): Int => scala/Int#
       |[7:23..7:24): x => _empty_/sb.b().(x)
       |[8:6..8:7): c <= _empty_/sb.c().
       |[8:8..8:9): x <= _empty_/sb.c().(x)
       |[8:11..8:14): Int => scala/Int#
       |[8:17..8:20): Int => scala/Int#
       |[8:23..8:24): x => _empty_/sb.c().(x)
       |[10:7..10:9): sc <= _empty_/sc.
       |[11:6..11:7): m <= _empty_/sc.m().
       |[12:10..12:12): sd <= local7
       |[12:13..12:13):  <= local9
       |[12:27..12:28): a <= local0
       |[12:39..12:40): b <= local1
       |[12:42..12:43): c <= local2
       |[12:48..12:49): a => local0
       |[12:61..12:63): rw => _empty_/rw#
       |[12:63..12:63):  => java/lang/Object#`<init>`().
       |[13:10..13:11): a <= local3
       |[13:12..13:13): x <= local4
       |[13:15..13:18): Int => scala/Int#
       |[13:21..13:24): Int => scala/Int#
       |[13:27..13:28): x => local4
       |[14:10..14:11): b <= local5
       |[14:12..14:13): x <= local6
       |[14:15..14:18): Int => scala/Int#
       |[14:21..14:24): Int => scala/Int#
       |[14:27..14:28): x => local6
       |[16:8..16:10): sd => local7
       |[16:10..16:10):  => local9
       |""".stripMargin,
  )

  symbols(
    "early definitions of a trait, an object and a local class beside methods of the same name",
    """|trait rw
       |trait sa extends { val a = 1; val (b, c) = (a, 2) } with rw {
       |  def a(x: Int): Int = x
       |  def b(x: Int): Int = x
       |}
       |object sb extends { var a = 1; val Some(b) = Some(a); val (c: Int) = a } with rw {
       |  def a(x: Int): Int = x
       |  def b(x: Int): Int = x
       |  def c(x: Int): Int = x
       |}
       |object sc {
       |  def m = {
       |    class sd extends { val a = 1; val (b, c) = (a, 2) } with rw {
       |      def a(x: Int): Int = x
       |      def b(x: Int): Int = x
       |    }
       |    new sd
       |  }
       |}
       |""".stripMargin,
    """|_empty_/rw# => trait rw extends AnyRef
       |  AnyRef => scala/AnyRef#
       |_empty_/sa# => trait sa extends AnyRef with rw { +7 decls }
       |  AnyRef => scala/AnyRef#
       |  rw => _empty_/rw#
       |_empty_/sa#`<init>`(). => primary ctor <init>()
       |_empty_/sa#a(). => method a(x: Int): Int
       |  x => _empty_/sa#a().(x)
       |  Int => scala/Int#
       |_empty_/sa#a().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/sa#a. => val method a: Int
       |  Int => scala/Int#
       |_empty_/sa#b(). => method b(x: Int): Int
       |  x => _empty_/sa#b().(x)
       |  Int => scala/Int#
       |_empty_/sa#b().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/sa#b. => val method b: Int
       |  Int => scala/Int#
       |_empty_/sa#c. => val method c: Int
       |  Int => scala/Int#
       |_empty_/sa#x$1. => private[this] synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/sb. => final object sb extends AnyRef with rw { +7 decls }
       |  AnyRef => scala/AnyRef#
       |  rw => _empty_/rw#
       |_empty_/sb.`a_=`(). => var method a_=(x$1: Int): Unit
       |  x$1 => _empty_/sb.`a_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/sb.`a_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/sb.a(). => var method a: Int
       |  Int => scala/Int#
       |_empty_/sb.a(+1). => method a(x: Int): Int
       |  x => _empty_/sb.a(+1).(x)
       |  Int => scala/Int#
       |_empty_/sb.a(+1).(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/sb.b(). => method b(x: Int): Int
       |  x => _empty_/sb.b().(x)
       |  Int => scala/Int#
       |_empty_/sb.b().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/sb.b. => val method b: Int
       |  Int => scala/Int#
       |_empty_/sb.c(). => method c(x: Int): Int
       |  x => _empty_/sb.c().(x)
       |  Int => scala/Int#
       |_empty_/sb.c().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/sb.c. => val method c: Int
       |  Int => scala/Int#
       |_empty_/sc. => final object sc extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sc.m(). => method m: AnyRef with rw { val def a: Int; val def b: Int; val def c: Int; def a(x: Int): Int; def b(x: Int): Int }
       |  AnyRef => scala/AnyRef#
       |  rw => _empty_/rw#
       |  a => local0
       |  Int => scala/Int#
       |  b => local1
       |  c => local2
       |  a => local3
       |  x => local4
       |  b => local5
       |  x => local6
       |local0 => val method a: Int
       |  Int => scala/Int#
       |local1 => val method b: Int
       |  Int => scala/Int#
       |local2 => val method c: Int
       |  Int => scala/Int#
       |local3 => method a(x: Int): Int
       |  x => local4
       |  Int => scala/Int#
       |local4 => param x: Int
       |  Int => scala/Int#
       |local5 => method b(x: Int): Int
       |  x => local6
       |  Int => scala/Int#
       |local6 => param x: Int
       |  Int => scala/Int#
       |local7 => class sd extends AnyRef with rw { +7 decls }
       |  AnyRef => scala/AnyRef#
       |  rw => _empty_/rw#
       |local8 => private[this] synthetic val method x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local9 => primary ctor <init>()
       |""".stripMargin,
  )

  occurrences(
    "early definitions with modifiers beside methods of the same name",
    """|trait rw
       |class se extends { private[this] val a = 1; final val b = a; protected val c = b; lazy val d = c } with rw {
       |  def a(x: Int): Int = x
       |  def b(x: Int): Int = x
       |  def c(x: Int): Int = x
       |  def d(x: Int): Int = x
       |  def f = a + b + c + d
       |}
       |trait sg extends { private[this] val a = 1; val b = a } with rw { def a(x: Int): Int = x }
       |""".stripMargin,
    """|[0:6..0:8): rw <= _empty_/rw#
       |[1:6..1:8): se <= _empty_/se#
       |[1:9..1:9):  <= _empty_/se#`<init>`().
       |[1:37..1:38): a <= _empty_/se#a.
       |[1:54..1:55): b <= _empty_/se#b.
       |[1:58..1:59): a => _empty_/se#a.
       |[1:75..1:76): c <= _empty_/se#c.
       |[1:79..1:80): b => _empty_/se#b.
       |[1:91..1:92): d <= _empty_/se#d.
       |[1:95..1:96): c => _empty_/se#c.
       |[1:104..1:106): rw => _empty_/rw#
       |[1:106..1:106):  => java/lang/Object#`<init>`().
       |[2:6..2:7): a <= _empty_/se#a().
       |[2:8..2:9): x <= _empty_/se#a().(x)
       |[2:11..2:14): Int => scala/Int#
       |[2:17..2:20): Int => scala/Int#
       |[2:23..2:24): x => _empty_/se#a().(x)
       |[3:6..3:7): b <= _empty_/se#b().
       |[3:8..3:9): x <= _empty_/se#b().(x)
       |[3:11..3:14): Int => scala/Int#
       |[3:17..3:20): Int => scala/Int#
       |[3:23..3:24): x => _empty_/se#b().(x)
       |[4:6..4:7): c <= _empty_/se#c().
       |[4:8..4:9): x <= _empty_/se#c().(x)
       |[4:11..4:14): Int => scala/Int#
       |[4:17..4:20): Int => scala/Int#
       |[4:23..4:24): x => _empty_/se#c().(x)
       |[5:6..5:7): d <= _empty_/se#d().
       |[5:8..5:9): x <= _empty_/se#d().(x)
       |[5:11..5:14): Int => scala/Int#
       |[5:17..5:20): Int => scala/Int#
       |[5:23..5:24): x => _empty_/se#d().(x)
       |[6:6..6:7): f <= _empty_/se#f().
       |[6:10..6:11): a => _empty_/se#a.
       |[6:12..6:13): + => scala/Int#`+`(+4).
       |[6:14..6:15): b => _empty_/se#b.
       |[6:16..6:17): + => scala/Int#`+`(+4).
       |[6:18..6:19): c => _empty_/se#c.
       |[6:20..6:21): + => scala/Int#`+`(+4).
       |[6:22..6:23): d => _empty_/se#d.
       |[8:6..8:8): sg <= _empty_/sg#
       |[8:9..8:9):  <= _empty_/sg#`<init>`().
       |[8:37..8:38): a <= _empty_/sg#a.
       |[8:48..8:49): b <= _empty_/sg#b.
       |[8:52..8:53): a => _empty_/sg#a.
       |[8:61..8:63): rw => _empty_/rw#
       |[8:70..8:71): a <= _empty_/sg#a().
       |[8:72..8:73): x <= _empty_/sg#a().(x)
       |[8:75..8:78): Int => scala/Int#
       |[8:81..8:84): Int => scala/Int#
       |[8:87..8:88): x => _empty_/sg#a().(x)
       |""".stripMargin,
  )

  symbols(
    "early definitions with modifiers beside methods of the same name",
    """|trait rw
       |class se extends { private[this] val a = 1; final val b = a; protected val c = b; lazy val d = c } with rw {
       |  def a(x: Int): Int = x
       |  def b(x: Int): Int = x
       |  def c(x: Int): Int = x
       |  def d(x: Int): Int = x
       |  def f = a + b + c + d
       |}
       |trait sg extends { private[this] val a = 1; val b = a } with rw { def a(x: Int): Int = x }
       |""".stripMargin,
    """|_empty_/rw# => trait rw extends AnyRef
       |  AnyRef => scala/AnyRef#
       |_empty_/se# => class se extends AnyRef with rw { +10 decls }
       |  AnyRef => scala/AnyRef#
       |  rw => _empty_/rw#
       |_empty_/se#`<init>`(). => primary ctor <init>()
       |_empty_/se#a(). => method a(x: Int): Int
       |  x => _empty_/se#a().(x)
       |  Int => scala/Int#
       |_empty_/se#a().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/se#a. => private[this] val method a: Int
       |  Int => scala/Int#
       |_empty_/se#b(). => method b(x: Int): Int
       |  x => _empty_/se#b().(x)
       |  Int => scala/Int#
       |_empty_/se#b().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/se#b. => final val method b: Int
       |  Int => scala/Int#
       |_empty_/se#c(). => method c(x: Int): Int
       |  x => _empty_/se#c().(x)
       |  Int => scala/Int#
       |_empty_/se#c().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/se#c. => protected val method c: Int
       |  Int => scala/Int#
       |_empty_/se#d(). => method d(x: Int): Int
       |  x => _empty_/se#d().(x)
       |  Int => scala/Int#
       |_empty_/se#d().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/se#d. => lazy val method d: Int
       |  Int => scala/Int#
       |_empty_/se#f(). => method f: Int
       |  Int => scala/Int#
       |_empty_/sg# => trait sg extends AnyRef with rw { +4 decls }
       |  AnyRef => scala/AnyRef#
       |  rw => _empty_/rw#
       |_empty_/sg#`<init>`(). => primary ctor <init>()
       |_empty_/sg#a(). => method a(x: Int): Int
       |  x => _empty_/sg#a().(x)
       |  Int => scala/Int#
       |_empty_/sg#a().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/sg#a. => private[this] val method a: Int
       |  Int => scala/Int#
       |_empty_/sg#b. => val method b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "early definitions with modifiers of an anonymous class and a local class",
    """|trait rw
       |object ub {
       |  val x = new { private[this] val a = 1; var b = a; lazy val c = b; val Some(d) = Option(c) } with rw { def f = a + b + c + d }
       |  def m = { class uc extends { private[this] val a = 1; var b = a; lazy val c = b } with rw { def f = a + b + c }; new uc().b }
       |}
       |""".stripMargin,
    """|[0:6..0:8): rw <= _empty_/rw#
       |[1:7..1:9): ub <= _empty_/ub.
       |[2:6..2:7): x <= _empty_/ub.x.
       |[2:14..2:14):  <= local0
       |[2:34..2:35): a <= local1
       |[2:45..2:46): b <= local2
       |[2:49..2:50): a => local1
       |[2:61..2:62): c <= local4
       |[2:65..2:66): b => local2
       |[2:72..2:76): Some => scala/Some.
       |[2:77..2:78): d <= local5
       |[2:82..2:88): Option => scala/Option.
       |[2:89..2:90): c => local4
       |[2:99..2:101): rw => _empty_/rw#
       |[2:101..2:101):  => java/lang/Object#`<init>`().
       |[2:108..2:109): f <= local6
       |[2:112..2:113): a => local1
       |[2:114..2:115): + => scala/Int#`+`(+4).
       |[2:116..2:117): b => local2
       |[2:118..2:119): + => scala/Int#`+`(+4).
       |[2:120..2:121): c => local4
       |[2:122..2:123): + => scala/Int#`+`(+4).
       |[2:124..2:125): d => local5
       |[3:6..3:7): m <= _empty_/ub.m().
       |[3:18..3:20): uc <= local8
       |[3:21..3:21):  <= local13
       |[3:49..3:50): a <= local9
       |[3:60..3:61): b <= local10
       |[3:64..3:65): a => local9
       |[3:76..3:77): c <= local12
       |[3:80..3:81): b => local10
       |[3:89..3:91): rw => _empty_/rw#
       |[3:91..3:91):  => java/lang/Object#`<init>`().
       |[3:98..3:99): f <= local14
       |[3:102..3:103): a => local9
       |[3:104..3:105): + => scala/Int#`+`(+4).
       |[3:106..3:107): b => local10
       |[3:108..3:109): + => scala/Int#`+`(+4).
       |[3:110..3:111): c => local12
       |[3:119..3:121): uc => local8
       |[3:121..3:121):  => local13
       |[3:124..3:125): b => local10
       |""".stripMargin,
  )

  symbols(
    "early definitions with modifiers of an anonymous class and a local class",
    """|trait rw
       |object ub {
       |  val x = new { private[this] val a = 1; var b = a; lazy val c = b; val Some(d) = Option(c) } with rw { def f = a + b + c + d }
       |  def m = { class uc extends { private[this] val a = 1; var b = a; lazy val c = b } with rw { def f = a + b + c }; new uc().b }
       |}
       |""".stripMargin,
    """|_empty_/rw# => trait rw extends AnyRef
       |  AnyRef => scala/AnyRef#
       |_empty_/ub. => final object ub extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ub.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/ub.x. => val method x: AnyRef with rw { var def b: Int; var def b_=(x$1: Int): Unit; lazy val def c: Int; val def d: Int; def f: Int }
       |  AnyRef => scala/AnyRef#
       |  rw => _empty_/rw#
       |  b => local2
       |  Int => scala/Int#
       |  b_= => local3
       |  x$1 => local7
       |  Unit => scala/Unit#
       |  c => local4
       |  d => local5
       |  f => local6
       |local0 => final class $anon extends AnyRef with rw { +6 decls }
       |  AnyRef => scala/AnyRef#
       |  rw => _empty_/rw#
       |local1 => private[this] val method a: Int
       |  Int => scala/Int#
       |local2 => var method b: Int
       |  Int => scala/Int#
       |local3 => var method b_=(x$1: Int): Unit
       |  x$1 => local7
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local4 => lazy val method c: Int
       |  Int => scala/Int#
       |local5 => val method d: Int
       |  Int => scala/Int#
       |local6 => method f: Int
       |  Int => scala/Int#
       |local7 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local8 => class uc extends AnyRef with rw { +6 decls }
       |  AnyRef => scala/AnyRef#
       |  rw => _empty_/rw#
       |local9 => private[this] val method a: Int
       |  Int => scala/Int#
       |local10 => var method b: Int
       |  Int => scala/Int#
       |local11 => var method b_=(x$1: Int): Unit
       |  x$1 => local15
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local12 => lazy val method c: Int
       |  Int => scala/Int#
       |local13 => primary ctor <init>()
       |local14 => method f: Int
       |  Int => scala/Int#
       |local15 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "lazy values in constructors beside members of the same name",
    """|class sh(x: Int = { lazy val y = 1; y }) {
       |  val y = x
       |  def this(s: String) = { this(2); lazy val y = s; println(y) }
       |}
       |""".stripMargin,
    """|[0:6..0:8): sh <= _empty_/sh#
       |[0:8..0:8):  <= _empty_/sh#`<init>`().
       |[0:9..0:10): x <= _empty_/sh#`<init>`().(x)
       |[0:9..0:10): x <= _empty_/sh#x.
       |[0:12..0:15): Int => scala/Int#
       |[0:29..0:30): y <= local0
       |[0:36..0:37): y => local0
       |[1:6..1:7): y <= _empty_/sh#y.
       |[1:10..1:11): x => _empty_/sh#x.
       |[2:6..2:10): this <= _empty_/sh#`<init>`(+1).
       |[2:11..2:12): s <= _empty_/sh#`<init>`(+1).(s)
       |[2:14..2:20): String => scala/Predef.String#
       |[2:30..2:30):  => _empty_/sh#`<init>`().
       |[2:44..2:45): y <= local1
       |[2:48..2:49): s => _empty_/sh#`<init>`(+1).(s)
       |[2:51..2:58): println => scala/Predef.println(+1).
       |[2:59..2:60): y => local1
       |""".stripMargin,
  )

  symbols(
    "lazy values in constructors beside members of the same name",
    """|class sh(x: Int = { lazy val y = 1; y }) {
       |  val y = x
       |  def this(s: String) = { this(2); lazy val y = s; println(y) }
       |}
       |""".stripMargin,
    """|_empty_/sh# => class sh extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sh#`<init>`(). => primary ctor <init>(x: Int)
       |  x => _empty_/sh#`<init>`().(x)
       |  Int => scala/Int#
       |_empty_/sh#`<init>`().(x) => default param x: Int
       |  Int => scala/Int#
       |_empty_/sh#`<init>`(+1). => ctor <init>(s: String)
       |  s => _empty_/sh#`<init>`(+1).(s)
       |  String => scala/Predef.String#
       |_empty_/sh#`<init>`(+1).(s) => param s: String
       |  String => scala/Predef.String#
       |_empty_/sh#x. => private[this] val method x: Int
       |  Int => scala/Int#
       |_empty_/sh#y. => val method y: Int
       |  Int => scala/Int#
       |_empty_/sh. => synthetic final object sh extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sh.`<init>$default$1`(). => synthetic method <init>$default$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |local0 => lazy val local y: Int
       |  Int => scala/Int#
       |local1 => lazy val local y: String
       |  String => scala/Predef.String#
       |""".stripMargin,
  )

  occurrences(
    "local definitions in default arguments and constructors named like their owners",
    """|class td(x: Int = { val x = 1; x }) {
       |  def f(y: Int = { val y = 1; y }) = y
       |  def this(s: String) = { this(1); val x = 2; println(x) }
       |}
       |""".stripMargin,
    """|[0:6..0:8): td <= _empty_/td#
       |[0:8..0:8):  <= _empty_/td#`<init>`().
       |[0:9..0:10): x <= _empty_/td#`<init>`().(x)
       |[0:9..0:10): x <= _empty_/td#x.
       |[0:12..0:15): Int => scala/Int#
       |[0:24..0:25): x <= local0
       |[0:31..0:32): x => local0
       |[1:6..1:7): f <= _empty_/td#f().
       |[1:8..1:9): y <= _empty_/td#f().(y)
       |[1:11..1:14): Int => scala/Int#
       |[1:23..1:24): y <= local1
       |[1:30..1:31): y => local1
       |[1:37..1:38): y => _empty_/td#f().(y)
       |[2:6..2:10): this <= _empty_/td#`<init>`(+1).
       |[2:11..2:12): s <= _empty_/td#`<init>`(+1).(s)
       |[2:14..2:20): String => scala/Predef.String#
       |[2:30..2:30):  => _empty_/td#`<init>`().
       |[2:39..2:40): x <= local2
       |[2:46..2:53): println => scala/Predef.println(+1).
       |[2:54..2:55): x => local2
       |""".stripMargin,
  )

  check(
    "an implicit argument and a view from members of local classes",
    """|object sq {
       |  def m: Unit = {
       |    def use = k
       |    def k(implicit c: C[Int]) = c
       |    class C[T]
       |    object C { implicit def c[T]: C[T] = new C[T] }
       |  }
       |  def n: Unit = {
       |    class L { val s: String = 1; implicit val f: C = new C }
       |    class C extends (Int => String) { def apply(i: Int) = i.toString }
       |  }
       |}
       |""".stripMargin,
  )(
    """|[0:7..0:9): sq <= _empty_/sq.
       |[1:6..1:7): m <= _empty_/sq.m().
       |[1:9..1:13): Unit => scala/Unit#
       |[2:8..2:11): use <= local0
       |[2:14..2:15): k => local4
       |[3:8..3:9): k <= local4
       |[3:19..3:20): c <= local5
       |[3:22..3:23): C => local1
       |[3:24..3:27): Int => scala/Int#
       |[3:32..3:33): c => local5
       |[4:10..4:11): C <= local1
       |[4:12..4:13): T <= local7
       |[4:14..4:14):  <= local6
       |[5:11..5:12): C <= local3
       |[5:28..5:29): c <= local2
       |[5:30..5:31): T <= local8
       |[5:34..5:35): C => local1
       |[5:36..5:37): T => local8
       |[5:45..5:46): C => local1
       |[5:47..5:48): T => local8
       |[5:49..5:49):  => local6
       |[7:6..7:7): n <= _empty_/sq.n().
       |[7:9..7:13): Unit => scala/Unit#
       |[8:10..8:11): L <= local9
       |[8:12..8:12):  <= local10
       |[8:18..8:19): s <= local11
       |[8:21..8:27): String => scala/Predef.String#
       |[8:46..8:47): f <= local12
       |[8:49..8:50): C => local15
       |[8:57..8:58): C => local15
       |[8:58..8:58):  => local14
       |[9:10..9:11): C <= local15
       |[9:12..9:12):  <= local14
       |[9:21..9:24): Int => scala/Int#
       |[9:28..9:34): String => scala/Predef.String#
       |[9:42..9:47): apply <= local13
       |[9:48..9:49): i <= local16
       |[9:51..9:54): Int => scala/Int#
       |[9:58..9:59): i => local16
       |[9:60..9:68): toString => scala/Any#toString().
       |""".stripMargin,
    """|_empty_/sq. => final object sq extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sq.m(). => method m: Unit
       |  Unit => scala/Unit#
       |_empty_/sq.n(). => method n: Unit
       |  Unit => scala/Unit#
       |local0 => method use: C[Int]
       |  C => local1
       |  Int => scala/Int#
       |local1 => class C[T] extends AnyRef { +1 decls }
       |  T => local7
       |  AnyRef => scala/AnyRef#
       |local2 => implicit method c[T]: C[T]
       |  T => local8
       |  C => local1
       |local3 => final object C extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |local4 => method k(implicit c: C[Int]): C[Int]
       |  c => local5
       |  C => local1
       |  Int => scala/Int#
       |local5 => implicit param c: C[Int]
       |  C => local1
       |  Int => scala/Int#
       |local6 => primary ctor <init>()
       |local7 => typeparam T
       |local8 => typeparam T
       |local9 => class L extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local10 => primary ctor <init>()
       |local11 => val method s: String
       |  String => scala/Predef.String#
       |local12 => implicit val method f: C
       |  C => local15
       |local13 => method apply(i: Int): String <: scala/Function1#apply().
       |  i => local16
       |  Int => scala/Int#
       |  String => java/lang/String#
       |local14 => primary ctor <init>()
       |local15 => class C extends AnyRef with Function1[Int, String] { +2 decls }
       |  AnyRef => scala/AnyRef#
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |local16 => param i: Int
       |  Int => scala/Int#
       |""".stripMargin,
    """|[2:14..2:15): k => *(C.c[Int])
       |  C => local3
       |  c => local2
       |  Int => scala/Int#
       |[8:30..8:31): 1 => f.apply(*)
       |  f => local12
       |  apply => local13
       |""".stripMargin,
  )

  symbols(
    "ascribed extractor pattern in a method",
    """|object h {
       |  def m(): Int = {
       |    val Some(s): Option[Int] = Some(1)
       |    s
       |  }
       |}
       |""".stripMargin,
    """|_empty_/h. => final object h extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/h.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local s: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "ascribed extractor pattern in a method",
    """|object h {
       |  def m(): Int = {
       |    val Some(s): Option[Int] = Some(1)
       |    s
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): h <= _empty_/h.
       |[1:6..1:7): m <= _empty_/h.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:8..2:12): Some => scala/Some.
       |[2:13..2:14): s <= local0
       |[2:17..2:23): Option => scala/Option#
       |[2:24..2:27): Int => scala/Int#
       |[2:31..2:35): Some => scala/Some.
       |[3:4..3:5): s => local0
       |""".stripMargin,
  )

  symbols(
    "var patterns in a method",
    """|object i {
       |  def m(): Int = {
       |    var Some(s) = Some(1)
       |    var (x, y) = (1, 2)
       |    s + x + y
       |  }
       |}
       |""".stripMargin,
    """|_empty_/i. => final object i extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/i.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => var local s: Int
       |  Int => scala/Int#
       |local1 => var local x: Int
       |  Int => scala/Int#
       |local2 => var local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "var patterns in a method",
    """|object i {
       |  def m(): Int = {
       |    var Some(s) = Some(1)
       |    var (x, y) = (1, 2)
       |    s + x + y
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): i <= _empty_/i.
       |[1:6..1:7): m <= _empty_/i.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:8..2:12): Some => scala/Some.
       |[2:13..2:14): s <= local0
       |[2:18..2:22): Some => scala/Some.
       |[3:9..3:10): x <= local1
       |[3:12..3:13): y <= local2
       |[4:4..4:5): s => local0
       |[4:6..4:7): + => scala/Int#`+`(+4).
       |[4:8..4:9): x => local1
       |[4:10..4:11): + => scala/Int#`+`(+4).
       |[4:12..4:13): y => local2
       |""".stripMargin,
  )

  symbols(
    "lazy val patterns in a method",
    """|object j {
       |  def m(): Int = {
       |    lazy val Some(s) = Some(1)
       |    lazy val (x, y) = (1, 2)
       |    s + x + y
       |  }
       |}
       |""".stripMargin,
    """|_empty_/j. => final object j extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/j.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => lazy val local s: Int
       |  Int => scala/Int#
       |local1 => lazy val local x: Int
       |  Int => scala/Int#
       |local2 => lazy val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "lazy val patterns in a method",
    """|object j {
       |  def m(): Int = {
       |    lazy val Some(s) = Some(1)
       |    lazy val (x, y) = (1, 2)
       |    s + x + y
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): j <= _empty_/j.
       |[1:6..1:7): m <= _empty_/j.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:13..2:17): Some => scala/Some.
       |[2:18..2:19): s <= local0
       |[2:23..2:27): Some => scala/Some.
       |[3:14..3:15): x <= local1
       |[3:17..3:18): y <= local2
       |[4:4..4:5): s => local0
       |[4:6..4:7): + => scala/Int#`+`(+4).
       |[4:8..4:9): x => local1
       |[4:10..4:11): + => scala/Int#`+`(+4).
       |[4:12..4:13): y => local2
       |""".stripMargin,
  )

  symbols(
    "infix patterns in a method",
    """|object k {
       |  def m(): Int = {
       |    val a :: Nil = List(1)
       |    val b :: c = List(1, 2)
       |    a + b + c.size
       |  }
       |}
       |""".stripMargin,
    """|_empty_/k. => final object k extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/k.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local a: Int
       |  Int => scala/Int#
       |local1 => val local b: Int
       |  Int => scala/Int#
       |local2 => val local c: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "infix patterns in a method",
    """|object k {
       |  def m(): Int = {
       |    val a :: Nil = List(1)
       |    val b :: c = List(1, 2)
       |    a + b + c.size
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:8): k <= _empty_/k.
         |[1:6..1:7): m <= _empty_/k.m().
         |[1:11..1:14): Int => scala/Int#
         |[2:8..2:9): a <= local0
         |[2:10..2:12): :: => scala/package.`::`.
         |[2:13..2:16): Nil => scala/collection/immutable/Nil.
         |[2:19..2:23): List => scala/collection/immutable/List.
         |[3:8..3:9): b <= local1
         |[3:10..3:12): :: => scala/package.`::`.
         |[3:13..3:14): c <= local2
         |[3:17..3:21): List => scala/collection/immutable/List.
         |[4:4..4:5): a => local0
         |[4:6..4:7): + => scala/Int#`+`(+4).
         |[4:8..4:9): b => local1
         |[4:10..4:11): + => scala/Int#`+`(+4).
         |[4:12..4:13): c => local2
         |[4:14..4:18): size => scala/collection/SeqLike#size().
         |""".stripMargin
    else
      """|[0:7..0:8): k <= _empty_/k.
         |[1:6..1:7): m <= _empty_/k.m().
         |[1:11..1:14): Int => scala/Int#
         |[2:8..2:9): a <= local0
         |[2:10..2:12): :: => scala/package.`::`.
         |[2:13..2:16): Nil => scala/package.Nil.
         |[2:19..2:23): List => scala/package.List.
         |[3:8..3:9): b <= local1
         |[3:10..3:12): :: => scala/package.`::`.
         |[3:13..3:14): c <= local2
         |[3:17..3:21): List => scala/package.List.
         |[4:4..4:5): a => local0
         |[4:6..4:7): + => scala/Int#`+`(+4).
         |[4:8..4:9): b => local1
         |[4:10..4:11): + => scala/Int#`+`(+4).
         |[4:12..4:13): c => local2
         |[4:14..4:18): size => scala/collection/SeqOps#size().
         |""".stripMargin,
  )

  symbols(
    "bound extractor pattern in a method",
    """|object l {
       |  def m(): Int = {
       |    val s @ Some(_) = Some(1)
       |    s.get
       |  }
       |}
       |""".stripMargin,
    """|_empty_/l. => final object l extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/l.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local s: Some[Int]
       |  Some => scala/Some#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "bound extractor pattern in a method",
    """|object l {
       |  def m(): Int = {
       |    val s @ Some(_) = Some(1)
       |    s.get
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): l <= _empty_/l.
       |[1:6..1:7): m <= _empty_/l.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:8..2:9): s <= local0
       |[2:12..2:16): Some => scala/Some.
       |[2:22..2:26): Some => scala/Some.
       |[3:4..3:5): s => local0
       |[3:6..3:9): get => scala/Some#get().
       |""".stripMargin,
  )

  symbols(
    "two extractor patterns in a method",
    """|object n {
       |  def m(): Int = {
       |    val Some(s), Some(t) = Some(1)
       |    s + t
       |  }
       |}
       |""".stripMargin,
    """|_empty_/n. => final object n extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/n.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local s: Int
       |  Int => scala/Int#
       |local1 => val local t: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "two extractor patterns in a method",
    """|object n {
       |  def m(): Int = {
       |    val Some(s), Some(t) = Some(1)
       |    s + t
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): n <= _empty_/n.
       |[1:6..1:7): m <= _empty_/n.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:8..2:12): Some => scala/Some.
       |[2:13..2:14): s <= local0
       |[2:17..2:21): Some => scala/Some.
       |[2:22..2:23): t <= local1
       |[2:27..2:31): Some => scala/Some.
       |[3:4..3:5): s => local0
       |[3:6..3:7): + => scala/Int#`+`(+4).
       |[3:8..3:9): t => local1
       |""".stripMargin,
  )

  symbols(
    "patterns in a template",
    """|object o {
       |  val Some(s) = Some(1)
       |  val (x, y) = (1, 2)
       |  var Some(v) = Some(1)
       |  lazy val Some(z) = Some(1)
       |}
       |""".stripMargin,
    """|_empty_/o. => final object o extends AnyRef { +7 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/o.`v_=`(). => var method v_=(x$1: Int): Unit
       |  x$1 => _empty_/o.`v_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/o.`v_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/o.s. => val method s: Int
       |  Int => scala/Int#
       |_empty_/o.v(). => var method v: Int
       |  Int => scala/Int#
       |_empty_/o.x$1. => private[this] synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/o.x. => val method x: Int
       |  Int => scala/Int#
       |_empty_/o.y. => val method y: Int
       |  Int => scala/Int#
       |_empty_/o.z. => lazy val method z: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "patterns in a template",
    """|object o {
       |  val Some(s) = Some(1)
       |  val (x, y) = (1, 2)
       |  var Some(v) = Some(1)
       |  lazy val Some(z) = Some(1)
       |}
       |""".stripMargin,
    """|[0:7..0:8): o <= _empty_/o.
       |[1:6..1:10): Some => scala/Some.
       |[1:11..1:12): s <= _empty_/o.s.
       |[1:16..1:20): Some => scala/Some.
       |[2:7..2:8): x <= _empty_/o.x.
       |[2:10..2:11): y <= _empty_/o.y.
       |[3:6..3:10): Some => scala/Some.
       |[3:11..3:12): v <= _empty_/o.v().
       |[3:16..3:20): Some => scala/Some.
       |[4:11..4:15): Some => scala/Some.
       |[4:16..4:17): z <= _empty_/o.z.
       |[4:21..4:25): Some => scala/Some.
       |""".stripMargin,
  )

  symbols(
    "tuple patterns in a template",
    """|class z {
       |  lazy val (a, b) = (1, 2)
       |  var (c, d) = (1, 2)
       |  val (x, y) = (1, 2)
       |  def x(i: Int): Int = i
       |}
       |""".stripMargin,
    """|_empty_/z# => class z extends AnyRef { +13 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/z#`<init>`(). => primary ctor <init>()
       |_empty_/z#`c_=`(). => var method c_=(x$1: Int): Unit
       |  x$1 => _empty_/z#`c_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/z#`c_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/z#`d_=`(). => var method d_=(x$1: Int): Unit
       |  x$1 => _empty_/z#`d_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/z#`d_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/z#a. => lazy val method a: Int
       |  Int => scala/Int#
       |_empty_/z#b. => lazy val method b: Int
       |  Int => scala/Int#
       |_empty_/z#c(). => var method c: Int
       |  Int => scala/Int#
       |_empty_/z#d(). => var method d: Int
       |  Int => scala/Int#
       |_empty_/z#x$1. => private[this] synthetic lazy val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/z#x$2. => private[this] synthetic val method x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/z#x$3. => private[this] synthetic val method x$3: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/z#x(). => method x(i: Int): Int
       |  i => _empty_/z#x().(i)
       |  Int => scala/Int#
       |_empty_/z#x().(i) => param i: Int
       |  Int => scala/Int#
       |_empty_/z#x. => val method x: Int
       |  Int => scala/Int#
       |_empty_/z#y. => val method y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "tuple patterns in a template",
    """|class z {
       |  lazy val (a, b) = (1, 2)
       |  var (c, d) = (1, 2)
       |  val (x, y) = (1, 2)
       |  def x(i: Int): Int = i
       |}
       |""".stripMargin,
    """|[0:6..0:7): z <= _empty_/z#
       |[0:8..0:8):  <= _empty_/z#`<init>`().
       |[1:12..1:13): a <= _empty_/z#a.
       |[1:15..1:16): b <= _empty_/z#b.
       |[2:7..2:8): c <= _empty_/z#c().
       |[2:10..2:11): d <= _empty_/z#d().
       |[3:7..3:8): x <= _empty_/z#x.
       |[3:10..3:11): y <= _empty_/z#y.
       |[4:6..4:7): x <= _empty_/z#x().
       |[4:8..4:9): i <= _empty_/z#x().(i)
       |[4:11..4:14): Int => scala/Int#
       |[4:17..4:20): Int => scala/Int#
       |[4:23..4:24): i => _empty_/z#x().(i)
       |""".stripMargin,
  )

  symbols(
    "patterns in a template block",
    """|class p {
       |  {
       |    val Some(s) = Some(1)
       |    val (x, y) = (1, 2)
       |    println(s + x + y)
       |  }
       |}
       |""".stripMargin,
    """|_empty_/p# => class p extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/p#`<init>`(). => primary ctor <init>()
       |local0 => val local s: Int
       |  Int => scala/Int#
       |local1 => val local x: Int
       |  Int => scala/Int#
       |local2 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "patterns in a template block",
    """|class p {
       |  {
       |    val Some(s) = Some(1)
       |    val (x, y) = (1, 2)
       |    println(s + x + y)
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:7): p <= _empty_/p#
       |[0:8..0:8):  <= _empty_/p#`<init>`().
       |[2:8..2:12): Some => scala/Some.
       |[2:13..2:14): s <= local0
       |[2:18..2:22): Some => scala/Some.
       |[3:9..3:10): x <= local1
       |[3:12..3:13): y <= local2
       |[4:4..4:11): println => scala/Predef.println(+1).
       |[4:12..4:13): s => local0
       |[4:14..4:15): + => scala/Int#`+`(+4).
       |[4:16..4:17): x => local1
       |[4:18..4:19): + => scala/Int#`+`(+4).
       |[4:20..4:21): y => local2
       |""".stripMargin,
  )

  symbols(
    "patterns in a lambda",
    """|object q {
       |  val f = (n: Int) => {
       |    val Some(s) = Some(n)
       |    val (x, y) = (n, n)
       |    s + x + y
       |  }
       |}
       |""".stripMargin,
    """|_empty_/q. => final object q extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/q.f. => val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |local0 => param n: Int
       |  Int => scala/Int#
       |local1 => val local s: Int
       |  Int => scala/Int#
       |local2 => val local x: Int
       |  Int => scala/Int#
       |local3 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "patterns in a lambda",
    """|object q {
       |  val f = (n: Int) => {
       |    val Some(s) = Some(n)
       |    val (x, y) = (n, n)
       |    s + x + y
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): q <= _empty_/q.
       |[1:6..1:7): f <= _empty_/q.f.
       |[1:11..1:12): n <= local0
       |[1:14..1:17): Int => scala/Int#
       |[2:8..2:12): Some => scala/Some.
       |[2:13..2:14): s <= local1
       |[2:18..2:22): Some => scala/Some.
       |[2:23..2:24): n => local0
       |[3:9..3:10): x <= local2
       |[3:12..3:13): y <= local3
       |[3:18..3:19): n => local0
       |[3:21..3:22): n => local0
       |[4:4..4:5): s => local1
       |[4:6..4:7): + => scala/Int#`+`(+4).
       |[4:8..4:9): x => local2
       |[4:10..4:11): + => scala/Int#`+`(+4).
       |[4:12..4:13): y => local3
       |""".stripMargin,
  )

  symbols(
    "match binder named after its val in a method",
    """|object r {
       |  def m(): Int = {
       |    val s = Option(1) match { case Some(s) => s }
       |    s
       |  }
       |}
       |""".stripMargin,
    """|_empty_/r. => final object r extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/r.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => val local s: Int
       |  Int => scala/Int#
       |local1 => val local s: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "match binder named after its val in a method",
    """|object r {
       |  def m(): Int = {
       |    val s = Option(1) match { case Some(s) => s }
       |    s
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:8): r <= _empty_/r.
       |[1:6..1:7): m <= _empty_/r.m().
       |[1:11..1:14): Int => scala/Int#
       |[2:8..2:9): s <= local1
       |[2:12..2:18): Option => scala/Option.
       |[2:35..2:39): Some => scala/Some.
       |[2:40..2:41): s <= local0
       |[2:46..2:47): s => local0
       |[3:4..3:5): s => local1
       |""".stripMargin,
  )

  occurrences(
    "match binders and patterns named like the value of their match",
    """|object te {
       |  val a = 1 match { case a => a }
       |  def b = 1 match { case b => b }
       |  val Some(c) = 1 match { case c => Option(c) }
       |  def m = { val d = 1 match { case d => d }; d }
       |  val Some(e) = { val (e, f) = (1, 2); Option(e + f) }
       |}
       |""".stripMargin,
    """|[0:7..0:9): te <= _empty_/te.
       |[1:6..1:7): a <= _empty_/te.a.
       |[1:25..1:26): a <= local0
       |[1:30..1:31): a => local0
       |[2:6..2:7): b <= _empty_/te.b().
       |[2:25..2:26): b <= local1
       |[2:30..2:31): b => local1
       |[3:6..3:10): Some => scala/Some.
       |[3:11..3:12): c <= _empty_/te.c.
       |[3:31..3:32): c <= local2
       |[3:36..3:42): Option => scala/Option.
       |[3:43..3:44): c => local2
       |[4:6..4:7): m <= _empty_/te.m().
       |[4:16..4:17): d <= local4
       |[4:35..4:36): d <= local3
       |[4:40..4:41): d => local3
       |[4:45..4:46): d => local4
       |[5:6..5:10): Some => scala/Some.
       |[5:11..5:12): e <= _empty_/te.e.
       |[5:23..5:24): e <= local5
       |[5:26..5:27): f <= local6
       |[5:39..5:45): Option => scala/Option.
       |[5:46..5:47): e => local5
       |[5:48..5:49): + => scala/Int#`+`(+4).
       |[5:50..5:51): f => local6
       |""".stripMargin,
  )

  symbols(
    "extractor value definition in a for",
    """|object t {
       |  val r = for {
       |    x <- List(1)
       |    Some(s) = Option(x)
       |  } yield s
       |}
       |""".stripMargin,
    """|_empty_/t. => final object t extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t.r. => val method r: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, Some[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Some => scala/Some#
       |local2 => val local s: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "extractor value definition in a for",
    """|object t {
       |  val r = for {
       |    x <- List(1)
       |    Some(s) = Option(x)
       |  } yield s
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:8): t <= _empty_/t.
         |[1:6..1:7): r <= _empty_/t.r.
         |[2:4..2:5): x <= local0
         |[2:9..2:13): List => scala/collection/immutable/List.
         |[3:4..3:8): Some => scala/Some.
         |[3:9..3:10): s <= local2
         |[3:14..3:20): Option => scala/Option.
         |[3:21..3:22): x => local0
         |[4:10..4:11): s => local2
         |""".stripMargin
    else
      """|[0:7..0:8): t <= _empty_/t.
         |[1:6..1:7): r <= _empty_/t.r.
         |[2:4..2:5): x <= local0
         |[2:9..2:13): List => scala/package.List.
         |[3:4..3:8): Some => scala/Some.
         |[3:9..3:10): s <= local2
         |[3:14..3:20): Option => scala/Option.
         |[3:21..3:22): x => local0
         |[4:10..4:11): s => local2
         |""".stripMargin,
  )

  symbols(
    "infix value definition in a for",
    """|object u {
       |  val r = for {
       |    x <- List(1)
       |    a :: y = List(x)
       |  } yield y
       |}
       |""".stripMargin,
    """|_empty_/u. => final object u extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/u.r. => val method r: List[List[Int]]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, ::[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  :: => scala/collection/immutable/`::`#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "infix value definition in a for",
    """|object u {
       |  val r = for {
       |    x <- List(1)
       |    a :: y = List(x)
       |  } yield y
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:8): u <= _empty_/u.
         |[1:6..1:7): r <= _empty_/u.r.
         |[2:4..2:5): x <= local0
         |[2:9..2:13): List => scala/collection/immutable/List.
         |[3:4..3:5): a <= local2
         |[3:6..3:8): :: => scala/package.`::`.
         |[3:9..3:10): y <= local3
         |[3:13..3:17): List => scala/collection/immutable/List.
         |[3:18..3:19): x => local0
         |[4:10..4:11): y => local3
         |""".stripMargin
    else
      """|[0:7..0:8): u <= _empty_/u.
         |[1:6..1:7): r <= _empty_/u.r.
         |[2:4..2:5): x <= local0
         |[2:9..2:13): List => scala/package.List.
         |[3:4..3:5): a <= local2
         |[3:6..3:8): :: => scala/package.`::`.
         |[3:9..3:10): y <= local3
         |[3:13..3:17): List => scala/package.List.
         |[3:18..3:19): x => local0
         |[4:10..4:11): y => local3
         |""".stripMargin,
  )

  symbols(
    "simple value definition in a for",
    """|object v {
       |  val r = for {
       |    a <- List(1)
       |    y = a
       |  } yield y
       |}
       |""".stripMargin,
    """|_empty_/v. => final object v extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/v.r. => val method r: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => val local a: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "simple value definition in a for",
    """|object v {
       |  val r = for {
       |    a <- List(1)
       |    y = a
       |  } yield y
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:8): v <= _empty_/v.
         |[1:6..1:7): r <= _empty_/v.r.
         |[2:4..2:5): a <= local0
         |[2:9..2:13): List => scala/collection/immutable/List.
         |[3:4..3:5): y <= local2
         |[3:8..3:9): a => local0
         |[4:10..4:11): y => local2
         |""".stripMargin
    else
      """|[0:7..0:8): v <= _empty_/v.
         |[1:6..1:7): r <= _empty_/v.r.
         |[2:4..2:5): a <= local0
         |[2:9..2:13): List => scala/package.List.
         |[3:4..3:5): y <= local2
         |[3:8..3:9): a => local0
         |[4:10..4:11): y => local2
         |""".stripMargin,
  )

  check(
    "value definition with an infix case class pattern",
    """|object v1 {
       |  for (x <- List(1); a :: y = List(x)) yield y
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): v1 <= _empty_/v1.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:21..1:22): a <= local2
         |[1:23..1:25): :: => scala/package.`::`.
         |[1:26..1:27): y <= local3
         |[1:30..1:34): List => scala/collection/immutable/List.
         |[1:35..1:36): x => local0
         |[1:45..1:46): y => local3
         |""".stripMargin
    else
      """|[0:7..0:9): v1 <= _empty_/v1.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:21..1:22): a <= local2
         |[1:23..1:25): :: => scala/package.`::`.
         |[1:26..1:27): y <= local3
         |[1:30..1:34): List => scala/package.List.
         |[1:35..1:36): x => local0
         |[1:45..1:46): y => local3
         |""".stripMargin,
    """|_empty_/v1. => final object v1 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, ::[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  :: => scala/collection/immutable/`::`#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:46): for (x <- List(1); a :: y = List(x)) yield y => orig(List(1)).map[Tuple2[Int, ::[Int]], List[Tuple2[Int, ::[Int]]]]({(x) => orig(a :: y = List(x))})(List.canBuildFrom[Tuple2[Int, ::[Int]]]).map[List[Int], List[List[Int]]]({(x$3) => orig(y)})(List.canBuildFrom[List[Int]])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  :: => scala/collection/immutable/`::`#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$3 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:21..1:27): a :: y => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[1:30..1:34): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:46): for (x <- List(1); a :: y = List(x)) yield y => orig(List(1)).map[Tuple2[Int, ::[Int]]]({(x) => orig(a :: y = List(x))}).map[List[Int]]({(x$3) => orig(y)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  :: => scala/collection/immutable/`::`#
         |  x => local0
         |  List => scala/collection/immutable/List#
         |  x$3 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:21..1:27): a :: y => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[1:30..1:34): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "value definition with an infix extractor pattern",
    """|object v2 {
       |  object ~ { def unapply(p: (Int, Int)): Option[(Int, Int)] = Some(p) }
       |  for (x <- List(1); a ~ b = (x, x)) yield a + b
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): v2 <= _empty_/v2.
         |[1:9..1:10): ~ <= _empty_/v2.`~`.
         |[1:17..1:24): unapply <= _empty_/v2.`~`.unapply().
         |[1:25..1:26): p <= _empty_/v2.`~`.unapply().(p)
         |[1:29..1:32): Int => scala/Int#
         |[1:34..1:37): Int => scala/Int#
         |[1:41..1:47): Option => scala/Option#
         |[1:49..1:52): Int => scala/Int#
         |[1:54..1:57): Int => scala/Int#
         |[1:62..1:66): Some => scala/Some.
         |[1:67..1:68): p => _empty_/v2.`~`.unapply().(p)
         |[2:7..2:8): x <= local0
         |[2:12..2:16): List => scala/collection/immutable/List.
         |[2:21..2:22): a <= local2
         |[2:23..2:24): ~ => _empty_/v2.`~`.
         |[2:25..2:26): b <= local3
         |[2:30..2:31): x => local0
         |[2:33..2:34): x => local0
         |[2:43..2:44): a => local2
         |[2:45..2:46): + => scala/Int#`+`(+4).
         |[2:47..2:48): b => local3
         |""".stripMargin
    else
      """|[0:7..0:9): v2 <= _empty_/v2.
         |[1:9..1:10): ~ <= _empty_/v2.`~`.
         |[1:17..1:24): unapply <= _empty_/v2.`~`.unapply().
         |[1:25..1:26): p <= _empty_/v2.`~`.unapply().(p)
         |[1:29..1:32): Int => scala/Int#
         |[1:34..1:37): Int => scala/Int#
         |[1:41..1:47): Option => scala/Option#
         |[1:49..1:52): Int => scala/Int#
         |[1:54..1:57): Int => scala/Int#
         |[1:62..1:66): Some => scala/Some.
         |[1:67..1:68): p => _empty_/v2.`~`.unapply().(p)
         |[2:7..2:8): x <= local0
         |[2:12..2:16): List => scala/package.List.
         |[2:21..2:22): a <= local2
         |[2:23..2:24): ~ => _empty_/v2.`~`.
         |[2:25..2:26): b <= local3
         |[2:30..2:31): x => local0
         |[2:33..2:34): x => local0
         |[2:43..2:44): a => local2
         |[2:45..2:46): + => scala/Int#`+`(+4).
         |[2:47..2:48): b => local3
         |""".stripMargin,
    """|_empty_/v2. => final object v2 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/v2.`~`. => final object ~ extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/v2.`~`.unapply(). => method unapply(p: Tuple2[Int, Int]): Option[Tuple2[Int, Int]]
       |  p => _empty_/v2.`~`.unapply().(p)
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Option => scala/Option#
       |_empty_/v2.`~`.unapply().(p) => param p: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, Tuple2[Int, Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local b: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:62..1:66): Some => *.apply[Tuple2[Int, Int]]
         |  apply => scala/Some.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[2:2..2:48): for (x <- List(1); a ~ b = (x, x)) yield a + b => orig(List(1)).map[Tuple2[Int, Tuple2[Int, Int]], List[Tuple2[Int, Tuple2[Int, Int]]]]({(x) => orig(a ~ b = (x, x))})(List.canBuildFrom[Tuple2[Int, Tuple2[Int, Int]]]).map[Int, List[Int]]({(x$3) => orig(a + b)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$3 => local1
         |[2:12..2:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[2:21..2:26): a ~ b => *.apply[Tuple2[Int, Int], Int, Int]
         |  apply => scala/Tuple3.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:62..1:66): Some => *.apply[Tuple2[Int, Int]]
         |  apply => scala/Some.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[2:2..2:48): for (x <- List(1); a ~ b = (x, x)) yield a + b => orig(List(1)).map[Tuple2[Int, Tuple2[Int, Int]]]({(x) => orig(a ~ b = (x, x))}).map[Int]({(x$3) => orig(a + b)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  x => local0
         |  x$3 => local1
         |[2:12..2:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[2:21..2:26): a ~ b => *.apply[Tuple2[Int, Int], Int, Int]
         |  apply => scala/Tuple3.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "value definition with a constructor pattern",
    """|object v3 {
       |  for (x <- List(1); Some(a) = Option(x)) yield a
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): v3 <= _empty_/v3.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:21..1:25): Some => scala/Some.
         |[1:26..1:27): a <= local2
         |[1:31..1:37): Option => scala/Option.
         |[1:38..1:39): x => local0
         |[1:48..1:49): a => local2
         |""".stripMargin
    else
      """|[0:7..0:9): v3 <= _empty_/v3.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:21..1:25): Some => scala/Some.
         |[1:26..1:27): a <= local2
         |[1:31..1:37): Option => scala/Option.
         |[1:38..1:39): x => local0
         |[1:48..1:49): a => local2
         |""".stripMargin,
    """|_empty_/v3. => final object v3 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, Some[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Some => scala/Some#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:49): for (x <- List(1); Some(a) = Option(x)) yield a => orig(List(1)).map[Tuple2[Int, Some[Int]], List[Tuple2[Int, Some[Int]]]]({(x) => orig(Some(a) = Option(x))})(List.canBuildFrom[Tuple2[Int, Some[Int]]]).map[Int, List[Int]]({(x$3) => orig(a)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Some => scala/Some#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$3 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:21..1:28): Some(a) => *.apply[Some[Int], Int]
         |  apply => scala/Tuple2.apply().
         |  Some => scala/Some#
         |  Int => scala/Int#
         |[1:31..1:37): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:49): for (x <- List(1); Some(a) = Option(x)) yield a => orig(List(1)).map[Tuple2[Int, Some[Int]]]({(x) => orig(Some(a) = Option(x))}).map[Int]({(x$3) => orig(a)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Some => scala/Some#
         |  x => local0
         |  x$3 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:21..1:28): Some(a) => *.apply[Some[Int], Int]
         |  apply => scala/Tuple2.apply().
         |  Some => scala/Some#
         |  Int => scala/Int#
         |[1:31..1:37): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "value definition with a nested infix pattern",
    """|object v5 {
       |  for (x <- List(1); Some(a :: y) = Option(List(x))) yield y
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): v5 <= _empty_/v5.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:21..1:25): Some => scala/Some.
         |[1:26..1:27): a <= local2
         |[1:28..1:30): :: => scala/package.`::`.
         |[1:31..1:32): y <= local3
         |[1:36..1:42): Option => scala/Option.
         |[1:43..1:47): List => scala/collection/immutable/List.
         |[1:48..1:49): x => local0
         |[1:59..1:60): y => local3
         |""".stripMargin
    else
      """|[0:7..0:9): v5 <= _empty_/v5.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:21..1:25): Some => scala/Some.
         |[1:26..1:27): a <= local2
         |[1:28..1:30): :: => scala/package.`::`.
         |[1:31..1:32): y <= local3
         |[1:36..1:42): Option => scala/Option.
         |[1:43..1:47): List => scala/package.List.
         |[1:48..1:49): x => local0
         |[1:59..1:60): y => local3
         |""".stripMargin,
    """|_empty_/v5. => final object v5 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, Some[List[Int]]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Some => scala/Some#
       |  List => scala/collection/immutable/List#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:60): for (x <- List(1); Some(a :: y) = Option(List(x))) yield y => orig(List(1)).map[Tuple2[Int, Some[List[Int]]], List[Tuple2[Int, Some[List[Int]]]]]({(x) => orig(Some(a :: y) = Option(List(x)))})(List.canBuildFrom[Tuple2[Int, Some[List[Int]]]]).map[List[Int], List[List[Int]]]({(x$3) => orig(y)})(List.canBuildFrom[List[Int]])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Some => scala/Some#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$3 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:21..1:33): Some(a :: y) => *.apply[Some[List[Int]], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  Some => scala/Some#
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:36..1:42): Option => *.apply[List[Int]]
         |  apply => scala/Option.apply().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:43..1:47): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:60): for (x <- List(1); Some(a :: y) = Option(List(x))) yield y => orig(List(1)).map[Tuple2[Int, Some[List[Int]]]]({(x) => orig(Some(a :: y) = Option(List(x)))}).map[List[Int]]({(x$3) => orig(y)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Some => scala/Some#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  x$3 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:21..1:33): Some(a :: y) => *.apply[Some[List[Int]], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  Some => scala/Some#
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:36..1:42): Option => *.apply[List[Int]]
         |  apply => scala/Option.apply().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:43..1:47): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "value definition with a bound infix pattern",
    """|object v6 {
       |  for (x <- List(1); z @ (a :: y) = List(x)) yield z
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): v6 <= _empty_/v6.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:21..1:22): z <= local2
         |[1:26..1:27): a <= local3
         |[1:28..1:30): :: => scala/package.`::`.
         |[1:31..1:32): y <= local4
         |[1:36..1:40): List => scala/collection/immutable/List.
         |[1:41..1:42): x => local0
         |[1:51..1:52): z => local2
         |""".stripMargin
    else
      """|[0:7..0:9): v6 <= _empty_/v6.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:21..1:22): z <= local2
         |[1:26..1:27): a <= local3
         |[1:28..1:30): :: => scala/package.`::`.
         |[1:31..1:32): y <= local4
         |[1:36..1:40): List => scala/package.List.
         |[1:41..1:42): x => local0
         |[1:51..1:52): z => local2
         |""".stripMargin,
    """|_empty_/v6. => final object v6 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$2: Tuple2[Int, ::[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  :: => scala/collection/immutable/`::`#
       |local2 => val local z: ::[Int]
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |local3 => val local a: Int
       |  Int => scala/Int#
       |local4 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:52): for (x <- List(1); z @ (a :: y) = List(x)) yield z => orig(List(1)).map[Tuple2[Int, ::[Int]], List[Tuple2[Int, ::[Int]]]]({(x) => orig(z @ (a :: y) = List(x))})(List.canBuildFrom[Tuple2[Int, ::[Int]]]).map[::[Int], List[::[Int]]]({(x$2) => orig(z)})(List.canBuildFrom[::[Int]])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  :: => scala/collection/immutable/`::`#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$2 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:21..1:32): z @ (a :: y => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[1:36..1:40): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:52): for (x <- List(1); z @ (a :: y) = List(x)) yield z => orig(List(1)).map[Tuple2[Int, ::[Int]]]({(x) => orig(z @ (a :: y) = List(x))}).map[::[Int]]({(x$2) => orig(z)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  :: => scala/collection/immutable/`::`#
         |  x => local0
         |  x$2 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:21..1:32): z @ (a :: y => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[1:36..1:40): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "value definition with a variable pattern",
    """|object v7 {
       |  for (x <- List(1); a = x) yield a
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): v7 <= _empty_/v7.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:21..1:22): a <= local2
         |[1:25..1:26): x => local0
         |[1:34..1:35): a => local2
         |""".stripMargin
    else
      """|[0:7..0:9): v7 <= _empty_/v7.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:21..1:22): a <= local2
         |[1:25..1:26): x => local0
         |[1:34..1:35): a => local2
         |""".stripMargin,
    """|_empty_/v7. => final object v7 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:35): for (x <- List(1); a = x) yield a => orig(List(1)).map[Tuple2[Int, Int], List[Tuple2[Int, Int]]]({(x) => orig(a = x)})(List.canBuildFrom[Tuple2[Int, Int]]).map[Int, List[Int]]({(x$1) => orig(a)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$1 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:35): for (x <- List(1); a = x) yield a => orig(List(1)).map[Tuple2[Int, Int]]({(x) => orig(a = x)}).map[Int]({(x$1) => orig(a)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  x => local0
         |  x$1 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "value definition with an infix case class pattern in a loop",
    """|object v9 {
       |  for (x <- List(1); a :: y = List(x)) println(y)
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): v9 <= _empty_/v9.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:21..1:22): a <= local2
         |[1:23..1:25): :: => scala/package.`::`.
         |[1:26..1:27): y <= local3
         |[1:30..1:34): List => scala/collection/immutable/List.
         |[1:35..1:36): x => local0
         |[1:39..1:46): println => scala/Predef.println(+1).
         |[1:47..1:48): y => local3
         |""".stripMargin
    else
      """|[0:7..0:9): v9 <= _empty_/v9.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:21..1:22): a <= local2
         |[1:23..1:25): :: => scala/package.`::`.
         |[1:26..1:27): y <= local3
         |[1:30..1:34): List => scala/package.List.
         |[1:35..1:36): x => local0
         |[1:39..1:46): println => scala/Predef.println(+1).
         |[1:47..1:48): y => local3
         |""".stripMargin,
    """|_empty_/v9. => final object v9 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, ::[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  :: => scala/collection/immutable/`::`#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:49): for (x <- List(1); a :: y = List(x)) println(y) => orig(List(1)).map[Tuple2[Int, ::[Int]], List[Tuple2[Int, ::[Int]]]]({(x) => orig(a :: y = List(x))})(List.canBuildFrom[Tuple2[Int, ::[Int]]]).foreach[Unit]({(x$3) => orig(println(y))})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  :: => scala/collection/immutable/`::`#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  foreach => scala/collection/immutable/List#foreach().
         |  Unit => scala/Unit#
         |  x$3 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:21..1:27): a :: y => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[1:30..1:34): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:49): for (x <- List(1); a :: y = List(x)) println(y) => orig(List(1)).map[Tuple2[Int, ::[Int]]]({(x) => orig(a :: y = List(x))}).foreach[Unit]({(x$3) => orig(println(y))})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  :: => scala/collection/immutable/`::`#
         |  x => local0
         |  foreach => scala/collection/immutable/List#foreach().
         |  Unit => scala/Unit#
         |  x$3 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:21..1:27): a :: y => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[1:30..1:34): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "two value definitions with infix case class patterns",
    """|object v10 {
       |  for (x <- List(1); a :: y = List(x); b :: z = y) yield z
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:10): v10 <= _empty_/v10.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:21..1:22): a <= local2
         |[1:23..1:25): :: => scala/package.`::`.
         |[1:26..1:27): y <= local3
         |[1:30..1:34): List => scala/collection/immutable/List.
         |[1:35..1:36): x => local0
         |[1:39..1:40): b <= local4
         |[1:41..1:43): :: => scala/package.`::`.
         |[1:44..1:45): z <= local5
         |[1:48..1:49): y => local3
         |[1:57..1:58): z => local5
         |""".stripMargin
    else
      """|[0:7..0:10): v10 <= _empty_/v10.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:21..1:22): a <= local2
         |[1:23..1:25): :: => scala/package.`::`.
         |[1:26..1:27): y <= local3
         |[1:30..1:34): List => scala/package.List.
         |[1:35..1:36): x => local0
         |[1:39..1:40): b <= local4
         |[1:41..1:43): :: => scala/package.`::`.
         |[1:44..1:45): z <= local5
         |[1:48..1:49): y => local3
         |[1:57..1:58): z => local5
         |""".stripMargin,
    """|_empty_/v10. => final object v10 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$5: Tuple3[Int, ::[Int], ::[Int]]
       |  Tuple3 => scala/Tuple3#
       |  Int => scala/Int#
       |  :: => scala/collection/immutable/`::`#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local4 => val local b: Int
       |  Int => scala/Int#
       |local5 => val local z: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:58): for (x <- List(1); a :: y = List(x); b :: z = y) yield z => orig(List(1)).map[Tuple3[Int, ::[Int], ::[Int]], List[Tuple3[Int, ::[Int], ::[Int]]]]({(x) => orig(a :: y = List(x); b :: z = y)})(List.canBuildFrom[Tuple3[Int, ::[Int], ::[Int]]]).map[List[Int], List[List[Int]]]({(x$5) => orig(z)})(List.canBuildFrom[List[Int]])
         |  map => scala/collection/immutable/List#map().
         |  Tuple3 => scala/Tuple3#
         |  Int => scala/Int#
         |  :: => scala/collection/immutable/`::`#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$5 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:21..1:27): a :: y => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[1:30..1:34): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:39..1:45): b :: z => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |""".stripMargin
    else
      """|[1:2..1:58): for (x <- List(1); a :: y = List(x); b :: z = y) yield z => orig(List(1)).map[Tuple3[Int, ::[Int], ::[Int]]]({(x) => orig(a :: y = List(x); b :: z = y)}).map[List[Int]]({(x$5) => orig(z)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple3 => scala/Tuple3#
         |  Int => scala/Int#
         |  :: => scala/collection/immutable/`::`#
         |  x => local0
         |  List => scala/collection/immutable/List#
         |  x$5 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:21..1:27): a :: y => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[1:30..1:34): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:39..1:45): b :: z => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |""".stripMargin,
  )

  check(
    "value definition with an infix case class pattern and a guard",
    """|object v11 {
       |  for (x <- List(1); a :: y = List(x) if a > 0) yield y
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:10): v11 <= _empty_/v11.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:21..1:22): a <= local3
         |[1:23..1:25): :: => scala/package.`::`.
         |[1:26..1:27): y <= local4
         |[1:30..1:34): List => scala/collection/immutable/List.
         |[1:35..1:36): x => local0
         |[1:41..1:42): a => local3
         |[1:43..1:44): > => scala/Int#`>`(+3).
         |[1:54..1:55): y => local4
         |""".stripMargin
    else
      """|[0:7..0:10): v11 <= _empty_/v11.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:21..1:22): a <= local3
         |[1:23..1:25): :: => scala/package.`::`.
         |[1:26..1:27): y <= local4
         |[1:30..1:34): List => scala/package.List.
         |[1:35..1:36): x => local0
         |[1:41..1:42): a => local3
         |[1:43..1:44): > => scala/Int#`>`(+3).
         |[1:54..1:55): y => local4
         |""".stripMargin,
    """|_empty_/v11. => final object v11 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, ::[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  :: => scala/collection/immutable/`::`#
       |local2 => synthetic param x$4: Tuple2[Int, ::[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  :: => scala/collection/immutable/`::`#
       |local3 => val local a: Int
       |  Int => scala/Int#
       |local4 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:55): for (x <- List(1); a :: y = List(x) if a > 0) yield y => orig(List(1)).map[Tuple2[Int, ::[Int]], List[Tuple2[Int, ::[Int]]]]({(x) => orig(a :: y = List(x))})(List.canBuildFrom[Tuple2[Int, ::[Int]]]).withFilter({(x$3) => orig(a > 0)}).map[List[Int], List[List[Int]]]({(x$4) => orig(y)})(List.canBuildFrom[List[Int]])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  :: => scala/collection/immutable/`::`#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  x$3 => local1
         |  map => scala/collection/generic/FilterMonadic#map().
         |  x$4 => local2
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:21..1:27): a :: y => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[1:30..1:34): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:55): for (x <- List(1); a :: y = List(x) if a > 0) yield y => orig(List(1)).map[Tuple2[Int, ::[Int]]]({(x) => orig(a :: y = List(x))}).withFilter({(x$3) => orig(a > 0)}).map[List[Int]]({(x$4) => orig(y)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  :: => scala/collection/immutable/`::`#
         |  x => local0
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  x$3 => local1
         |  map => scala/collection/WithFilter#map().
         |  List => scala/collection/immutable/List#
         |  x$4 => local2
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:21..1:27): a :: y => *.apply[::[Int], Int, List[Int]]
         |  apply => scala/Tuple3.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[1:30..1:34): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "value definition with an infix pattern of a local case class",
    """|object v12 {
       |  case class ~(a: Int, b: Int)
       |  for (x <- List(1); a ~ b = new ~(x, x)) yield a + b
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:10): v12 <= _empty_/v12.
         |[1:13..1:14): ~ <= _empty_/v12.`~`#
         |[1:14..1:14):  <= _empty_/v12.`~`#`<init>`().
         |[1:15..1:16): a <= _empty_/v12.`~`#`<init>`().(a)
         |[1:15..1:16): a <= _empty_/v12.`~`#a.
         |[1:18..1:21): Int => scala/Int#
         |[1:23..1:24): b <= _empty_/v12.`~`#`<init>`().(b)
         |[1:23..1:24): b <= _empty_/v12.`~`#b.
         |[1:26..1:29): Int => scala/Int#
         |[2:7..2:8): x <= local0
         |[2:12..2:16): List => scala/collection/immutable/List.
         |[2:21..2:22): a <= local2
         |[2:23..2:24): ~ => _empty_/v12.`~`.
         |[2:25..2:26): b <= local3
         |[2:33..2:34): ~ => _empty_/v12.`~`#
         |[2:34..2:34):  => _empty_/v12.`~`#`<init>`().
         |[2:35..2:36): x => local0
         |[2:38..2:39): x => local0
         |[2:48..2:49): a => local2
         |[2:50..2:51): + => scala/Int#`+`(+4).
         |[2:52..2:53): b => local3
         |""".stripMargin
    else
      """|[0:7..0:10): v12 <= _empty_/v12.
         |[1:13..1:14): ~ <= _empty_/v12.`~`#
         |[1:14..1:14):  <= _empty_/v12.`~`#`<init>`().
         |[1:15..1:16): a <= _empty_/v12.`~`#`<init>`().(a)
         |[1:15..1:16): a <= _empty_/v12.`~`#a.
         |[1:18..1:21): Int => scala/Int#
         |[1:23..1:24): b <= _empty_/v12.`~`#`<init>`().(b)
         |[1:23..1:24): b <= _empty_/v12.`~`#b.
         |[1:26..1:29): Int => scala/Int#
         |[2:7..2:8): x <= local0
         |[2:12..2:16): List => scala/package.List.
         |[2:21..2:22): a <= local2
         |[2:23..2:24): ~ => _empty_/v12.`~`.
         |[2:25..2:26): b <= local3
         |[2:33..2:34): ~ => _empty_/v12.`~`#
         |[2:34..2:34):  => _empty_/v12.`~`#`<init>`().
         |[2:35..2:36): x => local0
         |[2:38..2:39): x => local0
         |[2:48..2:49): a => local2
         |[2:50..2:51): + => scala/Int#`+`(+4).
         |[2:52..2:53): b => local3
         |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/v12. => final object v12 extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/v12.`~`# => case class ~ extends AnyRef with Product with Serializable { +14 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |_empty_/v12.`~`#`<init>`(). => primary ctor <init>(val a: Int, val b: Int)
         |  a => _empty_/v12.`~`#`<init>`().(a)
         |  Int => scala/Int#
         |  b => _empty_/v12.`~`#`<init>`().(b)
         |_empty_/v12.`~`#`<init>`().(a) => val param a: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#`<init>`().(b) => val param b: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#b. => val method b: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/v12.`~`#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/v12.`~`#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/v12.`~`#copy$default$1(). => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/v12.`~`#copy$default$2(). => synthetic method copy$default$2: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/v12.`~`#copy(). => synthetic method copy(a: Int, b: Int): ~
         |  a => _empty_/v12.`~`#copy().(a)
         |  Int => scala/Int#
         |  b => _empty_/v12.`~`#copy().(b)
         |  ~ => _empty_/v12.`~`#
         |_empty_/v12.`~`#copy().(a) => default param a: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#copy().(b) => default param b: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/v12.`~`#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/v12.`~`#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/v12.`~`#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/v12.`~`#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/v12.`~`#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/v12.`~`#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/v12.`~`#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/v12.`~`#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/v12.`~`#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/v12.`~`. => synthetic final object ~ extends AbstractFunction2[Int, Int, ~] with Serializable { +4 decls }
         |  AbstractFunction2 => scala/runtime/AbstractFunction2#
         |  Int => scala/Int#
         |  ~ => _empty_/v12.`~`#
         |  Serializable => scala/Serializable#
         |_empty_/v12.`~`.apply(). => synthetic method apply(a: Int, b: Int): ~ <: scala/Function2#apply().
         |  a => _empty_/v12.`~`.apply().(a)
         |  Int => scala/Int#
         |  b => _empty_/v12.`~`.apply().(b)
         |  ~ => _empty_/v12.`~`#
         |_empty_/v12.`~`.apply().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`.apply().(b) => param b: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`.readResolve(). => private synthetic method readResolve(): Object
         |  Object => java/lang/Object#
         |_empty_/v12.`~`.toString(). => synthetic final method toString(): String <: scala/Function2#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/v12.`~`.unapply(). => synthetic method unapply(x$0: ~): Option[Tuple2[Int, Int]]
         |  x$0 => _empty_/v12.`~`.unapply().(x$0)
         |  ~ => _empty_/v12.`~`#
         |  Option => scala/Option#
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/v12.`~`.unapply().(x$0) => synthetic param x$0: ~
         |  ~ => _empty_/v12.`~`#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local1 => synthetic param x$3: Tuple2[Int, ~]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  ~ => _empty_/v12.`~`#
         |local2 => val local a: Int
         |  Int => scala/Int#
         |local3 => val local b: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/v12. => final object v12 extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/v12.`~`# => case class ~ extends AnyRef with Product with Serializable { +15 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |_empty_/v12.`~`#`<init>`(). => primary ctor <init>(val a: Int, val b: Int)
         |  a => _empty_/v12.`~`#`<init>`().(a)
         |  Int => scala/Int#
         |  b => _empty_/v12.`~`#`<init>`().(b)
         |_empty_/v12.`~`#`<init>`().(a) => val param a: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#`<init>`().(b) => val param b: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#b. => val method b: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/v12.`~`#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/v12.`~`#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/v12.`~`#copy$default$1(). => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/v12.`~`#copy$default$2(). => synthetic method copy$default$2: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/v12.`~`#copy(). => synthetic method copy(a: Int, b: Int): ~
         |  a => _empty_/v12.`~`#copy().(a)
         |  Int => scala/Int#
         |  b => _empty_/v12.`~`#copy().(b)
         |  ~ => _empty_/v12.`~`#
         |_empty_/v12.`~`#copy().(a) => default param a: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#copy().(b) => default param b: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/v12.`~`#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/v12.`~`#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/v12.`~`#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/v12.`~`#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/v12.`~`#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/v12.`~`#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/v12.`~`#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#productElementName(). => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => _empty_/v12.`~`#productElementName().(x$1)
         |  Int => scala/Int#
         |  String => java/lang/String#
         |_empty_/v12.`~`#productElementName().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/v12.`~`#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/v12.`~`#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/v12.`~`. => synthetic final object ~ extends AbstractFunction2[Int, Int, ~] with Serializable { +4 decls }
         |  AbstractFunction2 => scala/runtime/AbstractFunction2#
         |  Int => scala/Int#
         |  ~ => _empty_/v12.`~`#
         |  Serializable => java/io/Serializable#
         |_empty_/v12.`~`.apply(). => synthetic method apply(a: Int, b: Int): ~ <: scala/Function2#apply().
         |  a => _empty_/v12.`~`.apply().(a)
         |  Int => scala/Int#
         |  b => _empty_/v12.`~`.apply().(b)
         |  ~ => _empty_/v12.`~`#
         |_empty_/v12.`~`.apply().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`.apply().(b) => param b: Int
         |  Int => scala/Int#
         |_empty_/v12.`~`.toString(). => synthetic final method toString(): String <: scala/Function2#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/v12.`~`.unapply(). => synthetic method unapply(x$0: ~): Option[Tuple2[Int, Int]]
         |  x$0 => _empty_/v12.`~`.unapply().(x$0)
         |  ~ => _empty_/v12.`~`#
         |  Option => scala/Option#
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/v12.`~`.unapply().(x$0) => synthetic param x$0: ~
         |  ~ => _empty_/v12.`~`#
         |_empty_/v12.`~`.writeReplace(). => private synthetic method writeReplace(): Object
         |  Object => java/lang/Object#
         |local0 => val local x: Int
         |  Int => scala/Int#
         |local1 => synthetic param x$3: Tuple2[Int, ~]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  ~ => _empty_/v12.`~`#
         |local2 => val local a: Int
         |  Int => scala/Int#
         |local3 => val local b: Int
         |  Int => scala/Int#
         |""".stripMargin,
    if (ScalaVersion.is212)
      """|[2:2..2:53): for (x <- List(1); a ~ b = new ~(x, x)) yield a + b => orig(List(1)).map[Tuple2[Int, ~], List[Tuple2[Int, ~]]]({(x) => orig(a ~ b = new ~(x, x))})(List.canBuildFrom[Tuple2[Int, ~]]).map[Int, List[Int]]({(x$3) => orig(a + b)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  ~ => _empty_/v12.`~`#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$3 => local1
         |[2:12..2:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[2:21..2:26): a ~ b => *.apply[~, Int, Int]
         |  apply => scala/Tuple3.apply().
         |  ~ => _empty_/v12.`~`#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[2:2..2:53): for (x <- List(1); a ~ b = new ~(x, x)) yield a + b => orig(List(1)).map[Tuple2[Int, ~]]({(x) => orig(a ~ b = new ~(x, x))}).map[Int]({(x$3) => orig(a + b)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  ~ => _empty_/v12.`~`#
         |  x => local0
         |  x$3 => local1
         |[2:12..2:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[2:21..2:26): a ~ b => *.apply[~, Int, Int]
         |  apply => scala/Tuple3.apply().
         |  ~ => _empty_/v12.`~`#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "value definition with a wildcard pattern",
    """|object v13 {
       |  for (x <- List(1); _ = println(x)) yield x
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:10): v13 <= _empty_/v13.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:21..1:22): _ <= local2
         |[1:25..1:32): println => scala/Predef.println(+1).
         |[1:33..1:34): x => local0
         |[1:43..1:44): x => local0
         |""".stripMargin
    else
      """|[0:7..0:10): v13 <= _empty_/v13.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:21..1:22): _ <= local2
         |[1:25..1:32): println => scala/Predef.println(+1).
         |[1:33..1:34): x => local0
         |[1:43..1:44): x => local0
         |""".stripMargin,
    """|_empty_/v13. => final object v13 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$2: Tuple2[Int, Unit]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local2 => val local x$1: Unit
       |  Unit => scala/Unit#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:44): for (x <- List(1); _ = println(x)) yield x => orig(List(1)).map[Tuple2[Int, Unit], List[Tuple2[Int, Unit]]]({(x) => orig(_ = println(x))})(List.canBuildFrom[Tuple2[Int, Unit]]).map[Int, List[Int]]({(x$2) => orig(x)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$2 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:44): for (x <- List(1); _ = println(x)) yield x => orig(List(1)).map[Tuple2[Int, Unit]]({(x) => orig(_ = println(x))}).map[Int]({(x$2) => orig(x)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |  x => local0
         |  x$2 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "generator with a tuple pattern and a guard",
    """|object g7 {
       |  for ((a, b) <- List((1, 2)) if a > b) yield a
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): g7 <= _empty_/g7.
         |[1:8..1:9): a <= local3
         |[1:11..1:12): b <= local4
         |[1:17..1:21): List => scala/collection/immutable/List.
         |[1:33..1:34): a => local3
         |[1:35..1:36): > => scala/Int#`>`(+3).
         |[1:37..1:38): b => local4
         |[1:46..1:47): a => local3
         |""".stripMargin
    else
      """|[0:7..0:9): g7 <= _empty_/g7.
         |[1:8..1:9): a <= local3
         |[1:11..1:12): b <= local4
         |[1:17..1:21): List => scala/package.List.
         |[1:33..1:34): a => local3
         |[1:35..1:36): > => scala/Int#`>`(+3).
         |[1:37..1:38): b => local4
         |[1:46..1:47): a => local3
         |""".stripMargin,
    """|_empty_/g7. => final object g7 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => synthetic param check$ifrefutable$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => synthetic param x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local3 => val local a: Int
       |  Int => scala/Int#
       |local4 => val local b: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:47): for ((a, b) <- List((1, 2)) if a > b) yield a => orig(List((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).withFilter({(x$1) => orig(a > b)}).map[Int, List[Int]]({(x$2) => orig(a)})(List.canBuildFrom[Int])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  withFilter => scala/collection/generic/FilterMonadic#withFilter().
         |  x$1 => local1
         |  map => scala/collection/generic/FilterMonadic#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x$2 => local2
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[1:17..1:21): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/immutable/List.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:47): for ((a, b) <- List((1, 2)) if a > b) yield a => orig(List((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).withFilter({(x$1) => orig(a > b)}).map[Int]({(x$2) => orig(a)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  withFilter => scala/collection/WithFilter#withFilter().
         |  x$1 => local1
         |  map => scala/collection/WithFilter#map().
         |  Int => scala/Int#
         |  x$2 => local2
         |[1:17..1:21): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/IterableFactory#apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "function with a wildcard parameter",
    """|object f2 {
       |  val f: Int => Int = _ => 1
       |}
       |""".stripMargin,
  )(
    """|[0:7..0:9): f2 <= _empty_/f2.
       |[1:6..1:7): f <= _empty_/f2.f.
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:22..1:23): _ <= local0
       |""".stripMargin,
    """|_empty_/f2. => final object f2 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/f2.f. => val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |local0 => param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
    """|
       |""".stripMargin,
  )

  check(
    "self type with a wildcard name",
    """|class s2 { _: AnyRef => }
       |""".stripMargin,
  )(
    """|[0:6..0:8): s2 <= _empty_/s2#
       |[0:9..0:9):  <= _empty_/s2#`<init>`().
       |[0:14..0:20): AnyRef => scala/AnyRef#
       |""".stripMargin,
    """|_empty_/s2# => class s2 extends AnyRef { self: AnyRef => +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s2#`<init>`(). => primary ctor <init>()
       |""".stripMargin,
    """|
       |""".stripMargin,
  )

  check(
    "generator with a wildcard pattern",
    """|object g10 {
       |  for (_ <- List(1)) yield 1
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:10): g10 <= _empty_/g10.
         |[1:7..1:8): _ <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |""".stripMargin
    else
      """|[0:7..0:10): g10 <= _empty_/g10.
         |[1:7..1:8): _ <= local0
         |[1:12..1:16): List => scala/package.List.
         |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/g10. => final object g10 extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => param _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/g10. => final object g10 extends AnyRef
         |  AnyRef => scala/AnyRef#
         |local0 => synthetic param x$1: Int
         |  Int => scala/Int#
         |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:28): for (_ <- List(1)) yield 1 => orig(List(1)).map[Int, List[Int]]({(_) => orig(1)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  _ => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:28): for (_ <- List(1)) yield 1 => orig(List(1)).map[Int]({(x$1) => orig(1)})
         |  map => scala/collection/immutable/List#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  symbols(
    "extractor generator in a for",
    """|object y {
       |  val r = for {
       |    Some(s) <- List(Option(1))
       |  } yield s
       |}
       |""".stripMargin,
    """|_empty_/y. => final object y extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/y.r. => val method r: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => synthetic param check$ifrefutable$1: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local2 => val local s: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "extractor generator in a for",
    """|object y {
       |  val r = for {
       |    Some(s) <- List(Option(1))
       |  } yield s
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:8): y <= _empty_/y.
         |[1:6..1:7): r <= _empty_/y.r.
         |[2:4..2:8): Some => scala/Some.
         |[2:9..2:10): s <= local2
         |[2:15..2:19): List => scala/collection/immutable/List.
         |[2:20..2:26): Option => scala/Option.
         |[3:10..3:11): s => local2
         |""".stripMargin
    else
      """|[0:7..0:8): y <= _empty_/y.
         |[1:6..1:7): r <= _empty_/y.r.
         |[2:4..2:8): Some => scala/Some.
         |[2:9..2:10): s <= local2
         |[2:15..2:19): List => scala/package.List.
         |[2:20..2:26): Option => scala/Option.
         |[3:10..3:11): s => local2
         |""".stripMargin,
  )

  check(
    "generator with an infix case class pattern",
    """|object g1 {
       |  for (a :: y <- List(List(1))) yield y
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): g1 <= _empty_/g1.
         |[1:7..1:8): a <= local2
         |[1:9..1:11): :: => scala/package.`::`.
         |[1:12..1:13): y <= local3
         |[1:17..1:21): List => scala/collection/immutable/List.
         |[1:22..1:26): List => scala/collection/immutable/List.
         |[1:38..1:39): y => local3
         |""".stripMargin
    else
      """|[0:7..0:9): g1 <= _empty_/g1.
         |[1:7..1:8): a <= local2
         |[1:9..1:11): :: => scala/package.`::`.
         |[1:12..1:13): y <= local3
         |[1:17..1:21): List => scala/package.List.
         |[1:22..1:26): List => scala/package.List.
         |[1:38..1:39): y => local3
         |""".stripMargin,
    """|_empty_/g1. => final object g1 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => synthetic param check$ifrefutable$1: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:39): for (a :: y <- List(List(1))) yield y => orig(List(List(1))).withFilter({(check$ifrefutable$1) => orig()}).map[List[Int], List[List[Int]]]({(x$1) => orig(y)})(List.canBuildFrom[List[Int]])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/generic/FilterMonadic#map().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |  x$1 => local1
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[1:17..1:21): List => *.apply[List[Int]]
         |  apply => scala/collection/immutable/List.apply().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:22..1:26): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:39): for (a :: y <- List(List(1))) yield y => orig(List(List(1))).withFilter({(check$ifrefutable$1) => orig()}).map[List[Int]]({(x$1) => orig(y)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/WithFilter#map().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |  x$1 => local1
         |[1:17..1:21): List => *.apply[List[Int]]
         |  apply => scala/collection/IterableFactory#apply().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:22..1:26): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "generator with an infix extractor pattern",
    """|object g2 {
       |  object ~ { def unapply(p: (Int, Int)): Option[(Int, Int)] = Some(p) }
       |  for (a ~ b <- List((1, 2))) yield a + b
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): g2 <= _empty_/g2.
         |[1:9..1:10): ~ <= _empty_/g2.`~`.
         |[1:17..1:24): unapply <= _empty_/g2.`~`.unapply().
         |[1:25..1:26): p <= _empty_/g2.`~`.unapply().(p)
         |[1:29..1:32): Int => scala/Int#
         |[1:34..1:37): Int => scala/Int#
         |[1:41..1:47): Option => scala/Option#
         |[1:49..1:52): Int => scala/Int#
         |[1:54..1:57): Int => scala/Int#
         |[1:62..1:66): Some => scala/Some.
         |[1:67..1:68): p => _empty_/g2.`~`.unapply().(p)
         |[2:7..2:8): a <= local2
         |[2:9..2:10): ~ => _empty_/g2.`~`.
         |[2:11..2:12): b <= local3
         |[2:16..2:20): List => scala/collection/immutable/List.
         |[2:36..2:37): a => local2
         |[2:38..2:39): + => scala/Int#`+`(+4).
         |[2:40..2:41): b => local3
         |""".stripMargin
    else
      """|[0:7..0:9): g2 <= _empty_/g2.
         |[1:9..1:10): ~ <= _empty_/g2.`~`.
         |[1:17..1:24): unapply <= _empty_/g2.`~`.unapply().
         |[1:25..1:26): p <= _empty_/g2.`~`.unapply().(p)
         |[1:29..1:32): Int => scala/Int#
         |[1:34..1:37): Int => scala/Int#
         |[1:41..1:47): Option => scala/Option#
         |[1:49..1:52): Int => scala/Int#
         |[1:54..1:57): Int => scala/Int#
         |[1:62..1:66): Some => scala/Some.
         |[1:67..1:68): p => _empty_/g2.`~`.unapply().(p)
         |[2:7..2:8): a <= local2
         |[2:9..2:10): ~ => _empty_/g2.`~`.
         |[2:11..2:12): b <= local3
         |[2:16..2:20): List => scala/package.List.
         |[2:36..2:37): a => local2
         |[2:38..2:39): + => scala/Int#`+`(+4).
         |[2:40..2:41): b => local3
         |""".stripMargin,
    """|_empty_/g2. => final object g2 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/g2.`~`. => final object ~ extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/g2.`~`.unapply(). => method unapply(p: Tuple2[Int, Int]): Option[Tuple2[Int, Int]]
       |  p => _empty_/g2.`~`.unapply().(p)
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Option => scala/Option#
       |_empty_/g2.`~`.unapply().(p) => param p: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local0 => synthetic param check$ifrefutable$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local b: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:62..1:66): Some => *.apply[Tuple2[Int, Int]]
         |  apply => scala/Some.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[2:2..2:41): for (a ~ b <- List((1, 2))) yield a + b => orig(List((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).map[Int, List[Int]]({(x$1) => orig(a + b)})(List.canBuildFrom[Int])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/generic/FilterMonadic#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x$1 => local1
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[2:16..2:20): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/immutable/List.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:62..1:66): Some => *.apply[Tuple2[Int, Int]]
         |  apply => scala/Some.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[2:2..2:41): for (a ~ b <- List((1, 2))) yield a + b => orig(List((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(a + b)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/WithFilter#map().
         |  Int => scala/Int#
         |  x$1 => local1
         |[2:16..2:20): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/IterableFactory#apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "generator with a constructor pattern",
    """|object g3 {
       |  for (Some(a) <- List(Option(1))) yield a
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): g3 <= _empty_/g3.
         |[1:7..1:11): Some => scala/Some.
         |[1:12..1:13): a <= local2
         |[1:18..1:22): List => scala/collection/immutable/List.
         |[1:23..1:29): Option => scala/Option.
         |[1:41..1:42): a => local2
         |""".stripMargin
    else
      """|[0:7..0:9): g3 <= _empty_/g3.
         |[1:7..1:11): Some => scala/Some.
         |[1:12..1:13): a <= local2
         |[1:18..1:22): List => scala/package.List.
         |[1:23..1:29): Option => scala/Option.
         |[1:41..1:42): a => local2
         |""".stripMargin,
    """|_empty_/g3. => final object g3 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => synthetic param check$ifrefutable$1: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:42): for (Some(a) <- List(Option(1))) yield a => orig(List(Option(1))).withFilter({(check$ifrefutable$1) => orig()}).map[Int, List[Int]]({(x$1) => orig(a)})(List.canBuildFrom[Int])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/generic/FilterMonadic#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x$1 => local1
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[1:18..1:22): List => *.apply[Option[Int]]
         |  apply => scala/collection/immutable/List.apply().
         |  Option => scala/Option#
         |  Int => scala/Int#
         |[1:23..1:29): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:42): for (Some(a) <- List(Option(1))) yield a => orig(List(Option(1))).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(a)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/WithFilter#map().
         |  Int => scala/Int#
         |  x$1 => local1
         |[1:18..1:22): List => *.apply[Option[Int]]
         |  apply => scala/collection/IterableFactory#apply().
         |  Option => scala/Option#
         |  Int => scala/Int#
         |[1:23..1:29): Option => *.apply[Int]
         |  apply => scala/Option.apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "generator with a tuple pattern",
    """|object g4 {
       |  for ((a, b) <- List((1, 2))) yield a + b
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): g4 <= _empty_/g4.
         |[1:8..1:9): a <= local2
         |[1:11..1:12): b <= local3
         |[1:17..1:21): List => scala/collection/immutable/List.
         |[1:37..1:38): a => local2
         |[1:39..1:40): + => scala/Int#`+`(+4).
         |[1:41..1:42): b => local3
         |""".stripMargin
    else
      """|[0:7..0:9): g4 <= _empty_/g4.
         |[1:8..1:9): a <= local2
         |[1:11..1:12): b <= local3
         |[1:17..1:21): List => scala/package.List.
         |[1:37..1:38): a => local2
         |[1:39..1:40): + => scala/Int#`+`(+4).
         |[1:41..1:42): b => local3
         |""".stripMargin,
    """|_empty_/g4. => final object g4 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => synthetic param check$ifrefutable$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local b: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:42): for ((a, b) <- List((1, 2))) yield a + b => orig(List((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).map[Int, List[Int]]({(x$1) => orig(a + b)})(List.canBuildFrom[Int])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/generic/FilterMonadic#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x$1 => local1
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[1:17..1:21): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/immutable/List.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:42): for ((a, b) <- List((1, 2))) yield a + b => orig(List((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x$1) => orig(a + b)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/WithFilter#map().
         |  Int => scala/Int#
         |  x$1 => local1
         |[1:17..1:21): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/IterableFactory#apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "generator with a nested infix pattern",
    """|object g5 {
       |  for (Some(a :: y) <- List(Option(List(1)))) yield y
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): g5 <= _empty_/g5.
         |[1:7..1:11): Some => scala/Some.
         |[1:12..1:13): a <= local2
         |[1:14..1:16): :: => scala/package.`::`.
         |[1:17..1:18): y <= local3
         |[1:23..1:27): List => scala/collection/immutable/List.
         |[1:28..1:34): Option => scala/Option.
         |[1:35..1:39): List => scala/collection/immutable/List.
         |[1:52..1:53): y => local3
         |""".stripMargin
    else
      """|[0:7..0:9): g5 <= _empty_/g5.
         |[1:7..1:11): Some => scala/Some.
         |[1:12..1:13): a <= local2
         |[1:14..1:16): :: => scala/package.`::`.
         |[1:17..1:18): y <= local3
         |[1:23..1:27): List => scala/package.List.
         |[1:28..1:34): Option => scala/Option.
         |[1:35..1:39): List => scala/package.List.
         |[1:52..1:53): y => local3
         |""".stripMargin,
    """|_empty_/g5. => final object g5 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => synthetic param check$ifrefutable$1: Option[List[Int]]
       |  Option => scala/Option#
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Option[List[Int]]
       |  Option => scala/Option#
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:53): for (Some(a :: y) <- List(Option(List(1)))) yield y => orig(List(Option(List(1)))).withFilter({(check$ifrefutable$1) => orig()}).map[List[Int], List[List[Int]]]({(x$1) => orig(y)})(List.canBuildFrom[List[Int]])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/generic/FilterMonadic#map().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |  x$1 => local1
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[1:23..1:27): List => *.apply[Option[List[Int]]]
         |  apply => scala/collection/immutable/List.apply().
         |  Option => scala/Option#
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:28..1:34): Option => *.apply[List[Int]]
         |  apply => scala/Option.apply().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:35..1:39): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:53): for (Some(a :: y) <- List(Option(List(1)))) yield y => orig(List(Option(List(1)))).withFilter({(check$ifrefutable$1) => orig()}).map[List[Int]]({(x$1) => orig(y)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/WithFilter#map().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |  x$1 => local1
         |[1:23..1:27): List => *.apply[Option[List[Int]]]
         |  apply => scala/collection/IterableFactory#apply().
         |  Option => scala/Option#
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:28..1:34): Option => *.apply[List[Int]]
         |  apply => scala/Option.apply().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:35..1:39): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "generator with a bound infix pattern",
    """|object g6 {
       |  for (z @ (a :: y) <- List(List(1))) yield z
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): g6 <= _empty_/g6.
         |[1:7..1:8): z <= local2
         |[1:12..1:13): a <= local3
         |[1:14..1:16): :: => scala/package.`::`.
         |[1:17..1:18): y <= local4
         |[1:23..1:27): List => scala/collection/immutable/List.
         |[1:28..1:32): List => scala/collection/immutable/List.
         |[1:44..1:45): z => local2
         |""".stripMargin
    else
      """|[0:7..0:9): g6 <= _empty_/g6.
         |[1:7..1:8): z <= local2
         |[1:12..1:13): a <= local3
         |[1:14..1:16): :: => scala/package.`::`.
         |[1:17..1:18): y <= local4
         |[1:23..1:27): List => scala/package.List.
         |[1:28..1:32): List => scala/package.List.
         |[1:44..1:45): z => local2
         |""".stripMargin,
    """|_empty_/g6. => final object g6 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => synthetic param check$ifrefutable$1: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local2 => val local z: ::[Int]
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |local3 => val local a: Int
       |  Int => scala/Int#
       |local4 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:45): for (z @ (a :: y) <- List(List(1))) yield z => orig(List(List(1))).withFilter({(check$ifrefutable$1) => orig()}).map[::[Int], List[::[Int]]]({(x$1) => orig(z)})(List.canBuildFrom[::[Int]])
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/generic/FilterMonadic#map().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x$1 => local1
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[1:23..1:27): List => *.apply[List[Int]]
         |  apply => scala/collection/immutable/List.apply().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:28..1:32): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:45): for (z @ (a :: y) <- List(List(1))) yield z => orig(List(List(1))).withFilter({(check$ifrefutable$1) => orig()}).map[::[Int]]({(x$1) => orig(z)})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  map => scala/collection/WithFilter#map().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |  x$1 => local1
         |[1:23..1:27): List => *.apply[List[Int]]
         |  apply => scala/collection/IterableFactory#apply().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[1:28..1:32): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "generator with a tuple pattern in a loop",
    """|object g8 {
       |  for ((a, b) <- List((1, 2))) println(a + b)
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): g8 <= _empty_/g8.
         |[1:8..1:9): a <= local2
         |[1:11..1:12): b <= local3
         |[1:17..1:21): List => scala/collection/immutable/List.
         |[1:31..1:38): println => scala/Predef.println(+1).
         |[1:39..1:40): a => local2
         |[1:41..1:42): + => scala/Int#`+`(+4).
         |[1:43..1:44): b => local3
         |""".stripMargin
    else
      """|[0:7..0:9): g8 <= _empty_/g8.
         |[1:8..1:9): a <= local2
         |[1:11..1:12): b <= local3
         |[1:17..1:21): List => scala/package.List.
         |[1:31..1:38): println => scala/Predef.println(+1).
         |[1:39..1:40): a => local2
         |[1:41..1:42): + => scala/Int#`+`(+4).
         |[1:43..1:44): b => local3
         |""".stripMargin,
    """|_empty_/g8. => final object g8 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => synthetic param check$ifrefutable$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local b: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:45): for ((a, b) <- List((1, 2))) println(a + b) => orig(List((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).foreach[Unit]({(x$1) => orig(println(a + b))})
         |  withFilter => scala/collection/TraversableLike#withFilter().
         |  check$ifrefutable$1 => local0
         |  foreach => scala/collection/generic/FilterMonadic#foreach().
         |  Unit => scala/Unit#
         |  x$1 => local1
         |[1:17..1:21): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/immutable/List.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:45): for ((a, b) <- List((1, 2))) println(a + b) => orig(List((1, 2))).withFilter({(check$ifrefutable$1) => orig()}).foreach[Unit]({(x$1) => orig(println(a + b))})
         |  withFilter => scala/collection/IterableOps#withFilter().
         |  check$ifrefutable$1 => local0
         |  foreach => scala/collection/WithFilter#foreach().
         |  Unit => scala/Unit#
         |  x$1 => local1
         |[1:17..1:21): List => *.apply[Tuple2[Int, Int]]
         |  apply => scala/collection/IterableFactory#apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "generator with a parameter named like a fresh name",
    """|object g9 {
       |  for (x$1 <- List(1)) yield x$1
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): g9 <= _empty_/g9.
         |[1:7..1:10): x$1 <= local0
         |[1:14..1:18): List => scala/collection/immutable/List.
         |[1:29..1:32): x$1 => local0
         |""".stripMargin
    else
      """|[0:7..0:9): g9 <= _empty_/g9.
         |[1:7..1:10): x$1 <= local0
         |[1:14..1:18): List => scala/package.List.
         |[1:29..1:32): x$1 => local0
         |""".stripMargin,
    """|_empty_/g9. => final object g9 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:32): for (x$1 <- List(1)) yield x$1 => orig(List(1)).map[Int, List[Int]]({(x$1) => orig(x$1)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x$1 => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |[1:14..1:18): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:32): for (x$1 <- List(1)) yield x$1 => orig(List(1)).map[Int]({(x$1) => orig(x$1)})
         |  map => scala/collection/immutable/List#map().
         |  Int => scala/Int#
         |  x$1 => local0
         |[1:14..1:18): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "generator with a typed pattern",
    """|object g11 {
       |  for (x: Int <- Option(1)) yield x
       |  for (x: Int <- Option(1)) println(x)
       |  Option(1).withFilter { case x: Int => true; case _ => false }
       |}
       |""".stripMargin,
  )(
    """|[0:7..0:10): g11 <= _empty_/g11.
       |[1:7..1:8): x <= local1
       |[1:10..1:13): Int => scala/Int#
       |[1:17..1:23): Option => scala/Option.
       |[1:34..1:35): x => local1
       |[2:7..2:8): x <= local3
       |[2:10..2:13): Int => scala/Int#
       |[2:17..2:23): Option => scala/Option.
       |[2:28..2:35): println => scala/Predef.println(+1).
       |[2:36..2:37): x => local3
       |[3:2..3:8): Option => scala/Option.
       |[3:12..3:22): withFilter => scala/Option#withFilter().
       |[3:30..3:31): x <= local4
       |[3:33..3:36): Int => scala/Int#
       |""".stripMargin,
    """|_empty_/g11. => final object g11 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => synthetic param check$ifrefutable$1: Int
       |  Int => scala/Int#
       |local1 => param x: Int
       |  Int => scala/Int#
       |local2 => synthetic param check$ifrefutable$2: Int
       |  Int => scala/Int#
       |local3 => param x: Int
       |  Int => scala/Int#
       |local4 => val local x: Int
       |  Int => scala/Int#
       |""".stripMargin,
    """|[1:2..1:35): for (x: Int <- Option(1)) yield x => orig(Option(1)).withFilter({(check$ifrefutable$1) => orig()}).map[Int]({(x) => orig(x)})
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$1 => local0
       |  map => scala/Option#WithFilter#map().
       |  Int => scala/Int#
       |  x => local1
       |[1:17..1:23): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[2:2..2:38): for (x: Int <- Option(1)) println(x) => orig(Option(1)).withFilter({(check$ifrefutable$2) => orig()}).foreach[Unit]({(x) => orig(println(x))})
       |  withFilter => scala/Option#withFilter().
       |  check$ifrefutable$2 => local2
       |  foreach => scala/Option#WithFilter#foreach().
       |  Unit => scala/Unit#
       |  x => local3
       |[2:17..2:23): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |[3:2..3:8): Option => *.apply[Int]
       |  apply => scala/Option.apply().
       |  Int => scala/Int#
       |""".stripMargin,
  )

  check(
    "self type named like a fresh name",
    """|class s1 { x$1: AnyRef =>
       |  def f = x$1
       |}
       |""".stripMargin,
  )(
    """|[0:6..0:8): s1 <= _empty_/s1#
       |[0:9..0:9):  <= _empty_/s1#`<init>`().
       |[0:11..0:14): x$1 <= local0
       |[0:16..0:22): AnyRef => scala/AnyRef#
       |[1:6..1:7): f <= _empty_/s1#f().
       |[1:10..1:13): x$1 => local0
       |""".stripMargin,
    """|_empty_/s1# => class s1 extends AnyRef { self: AnyRef => +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s1#`<init>`(). => primary ctor <init>()
       |_empty_/s1#f(). => method f: s1 with AnyRef {}
       |  s1 => _empty_/s1#
       |  AnyRef => scala/AnyRef#
       |local0 => selfparam x$1: AnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
    """|
       |""".stripMargin,
  )

  check(
    "backquoted self type named like a fresh name",
    """|class s3 { `x$1`: AnyRef =>
       |  def f = `x$1`
       |}
       |""".stripMargin,
  )(
    """|[0:6..0:8): s3 <= _empty_/s3#
       |[0:9..0:9):  <= _empty_/s3#`<init>`().
       |[0:11..0:16): `x$1` <= local0
       |[0:18..0:24): AnyRef => scala/AnyRef#
       |[1:6..1:7): f <= _empty_/s3#f().
       |[1:10..1:15): `x$1` => local0
       |""".stripMargin,
    """|_empty_/s3# => class s3 extends AnyRef { self: AnyRef => +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s3#`<init>`(). => primary ctor <init>()
       |_empty_/s3#f(). => method f: s3 with AnyRef {}
       |  s3 => _empty_/s3#
       |  AnyRef => scala/AnyRef#
       |local0 => selfparam x$1: AnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
    """|
       |""".stripMargin,
  )

  check(
    "function with a parameter named like a fresh name",
    """|object f1 {
       |  val f = (x$1: Int) => x$1
       |}
       |""".stripMargin,
  )(
    """|[0:7..0:9): f1 <= _empty_/f1.
       |[1:6..1:7): f <= _empty_/f1.f.
       |[1:11..1:14): x$1 <= local0
       |[1:16..1:19): Int => scala/Int#
       |[1:24..1:27): x$1 => local0
       |""".stripMargin,
    """|_empty_/f1. => final object f1 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/f1.f. => val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |local0 => param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
    """|
       |""".stripMargin,
  )

  symbols(
    "function with a backquoted parameter named like a fresh name",
    """|object f3 {
       |  val f = (`x$1`: Int) => `x$1`
       |}
       |""".stripMargin,
    """|_empty_/f3. => final object f3 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/f3.f. => val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |local0 => param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "extractor pattern in a val whose type has a val of the binder's name",
    """|class r1 { val x = 1 }
       |object r2 {
       |  val p: r1 = {
       |    val Some(x) = Option(1)
       |    new r1
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:8): r1 <= _empty_/r1#
       |[0:9..0:9):  <= _empty_/r1#`<init>`().
       |[0:15..0:16): x <= _empty_/r1#x.
       |[1:7..1:9): r2 <= _empty_/r2.
       |[2:6..2:7): p <= _empty_/r2.p.
       |[2:9..2:11): r1 => _empty_/r1#
       |[3:8..3:12): Some => scala/Some.
       |[3:13..3:14): x <= local0
       |[3:18..3:24): Option => scala/Option.
       |[4:8..4:10): r1 => _empty_/r1#
       |[4:10..4:10):  => _empty_/r1#`<init>`().
       |""".stripMargin,
  )

  symbols(
    "extractor pattern in a val whose type has a val of the binder's name",
    """|class r1 { val x = 1 }
       |object r2 {
       |  val p: r1 = {
       |    val Some(x) = Option(1)
       |    new r1
       |  }
       |}
       |""".stripMargin,
    """|_empty_/r1# => class r1 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/r1#`<init>`(). => primary ctor <init>()
       |_empty_/r1#x. => val method x: Int
       |  Int => scala/Int#
       |_empty_/r2. => final object r2 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/r2.p. => val method p: r1
       |  r1 => _empty_/r1#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "extractor pattern in a local val whose type has a val of the binder's name",
    """|object r3 {
       |  def m = {
       |    val s: Some[Int] = {
       |      val Some(value) = Option(1)
       |      Some(value)
       |    }
       |    s
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): r3 <= _empty_/r3.
       |[1:6..1:7): m <= _empty_/r3.m().
       |[2:8..2:9): s <= local1
       |[2:11..2:15): Some => scala/Some#
       |[2:16..2:19): Int => scala/Int#
       |[3:10..3:14): Some => scala/Some.
       |[3:15..3:20): value <= local0
       |[3:24..3:30): Option => scala/Option.
       |[4:6..4:10): Some => scala/Some.
       |[4:11..4:16): value => local0
       |[6:4..6:5): s => local1
       |""".stripMargin,
  )

  symbols(
    "extractor pattern in a local val whose type has a val of the binder's name",
    """|object r3 {
       |  def m = {
       |    val s: Some[Int] = {
       |      val Some(value) = Option(1)
       |      Some(value)
       |    }
       |    s
       |  }
       |}
       |""".stripMargin,
    """|_empty_/r3. => final object r3 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/r3.m(). => method m: Some[Int]
       |  Some => scala/Some#
       |  Int => scala/Int#
       |local0 => val local value: Int
       |  Int => scala/Int#
       |local1 => val local s: Some[Int]
       |  Some => scala/Some#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "extractor pattern in a method whose type has a val of the binder's name",
    """|class r4 { val x = 1 }
       |object r5 {
       |  def m: r4 = {
       |    val Some(x) = Option(1)
       |    new r4
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:8): r4 <= _empty_/r4#
       |[0:9..0:9):  <= _empty_/r4#`<init>`().
       |[0:15..0:16): x <= _empty_/r4#x.
       |[1:7..1:9): r5 <= _empty_/r5.
       |[2:6..2:7): m <= _empty_/r5.m().
       |[2:9..2:11): r4 => _empty_/r4#
       |[3:8..3:12): Some => scala/Some.
       |[3:13..3:14): x <= local0
       |[3:18..3:24): Option => scala/Option.
       |[4:8..4:10): r4 => _empty_/r4#
       |[4:10..4:10):  => _empty_/r4#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "extractor pattern named `_1` in a tuple val",
    """|object r6 {
       |  val t: (Int, Int) = {
       |    val Some(_1) = Option(1)
       |    (_1, 2)
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): r6 <= _empty_/r6.
       |[1:6..1:7): t <= _empty_/r6.t.
       |[1:10..1:13): Int => scala/Int#
       |[1:15..1:18): Int => scala/Int#
       |[2:8..2:12): Some => scala/Some.
       |[2:13..2:15): _1 <= local0
       |[2:19..2:25): Option => scala/Option.
       |[3:5..3:7): _1 => local0
       |""".stripMargin,
  )

  symbols(
    "extractor pattern named `_1` in a tuple val",
    """|object r6 {
       |  val t: (Int, Int) = {
       |    val Some(_1) = Option(1)
       |    (_1, 2)
       |  }
       |}
       |""".stripMargin,
    """|_empty_/r6. => final object r6 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/r6.t. => val method t: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local0 => val local _1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "local val in a val whose type has a val of the same name",
    """|class r7 { val x = 1 }
       |object r8 {
       |  val p: r7 = {
       |    val x = 1
       |    new r7
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:8): r7 <= _empty_/r7#
       |[0:9..0:9):  <= _empty_/r7#`<init>`().
       |[0:15..0:16): x <= _empty_/r7#x.
       |[1:7..1:9): r8 <= _empty_/r8.
       |[2:6..2:7): p <= _empty_/r8.p.
       |[2:9..2:11): r7 => _empty_/r7#
       |[3:8..3:9): x <= local0
       |[4:8..4:10): r7 => _empty_/r7#
       |[4:10..4:10):  => _empty_/r7#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    "local val in a method whose type has a val of the same name",
    """|class r9 { val x = 1 }
       |object r10 {
       |  def m: r9 = {
       |    val x = 1
       |    new r9
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:8): r9 <= _empty_/r9#
       |[0:9..0:9):  <= _empty_/r9#`<init>`().
       |[0:15..0:16): x <= _empty_/r9#x.
       |[1:7..1:10): r10 <= _empty_/r10.
       |[2:6..2:7): m <= _empty_/r10.m().
       |[2:9..2:11): r9 => _empty_/r9#
       |[3:8..3:9): x <= local0
       |[4:8..4:10): r9 => _empty_/r9#
       |[4:10..4:10):  => _empty_/r9#`<init>`().
       |""".stripMargin,
  )

  symbols(
    "wildcard value definition in a block",
    """|object w2 {
       |  def m = {
       |    val _ = 1
       |    2
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/w2. => final object w2 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/w2.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => val local _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/w2. => final object w2 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/w2.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => synthetic val local x$1: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  symbols(
    "tuple value definition in a for",
    """|object w {
       |  val r = for {
       |    x <- List(1)
       |    (a, b) = (x, x)
       |  } yield a + b
       |}
       |""".stripMargin,
    """|_empty_/w. => final object w extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/w.r. => val method r: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, Tuple2[Int, Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "tuple value definition in a for",
    """|object w {
       |  val r = for {
       |    x <- List(1)
       |    (a, b) = (x, x)
       |  } yield a + b
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:8): w <= _empty_/w.
         |[1:6..1:7): r <= _empty_/w.r.
         |[2:4..2:5): x <= local0
         |[2:9..2:13): List => scala/collection/immutable/List.
         |[3:5..3:6): a <= local2
         |[3:8..3:9): b <= local3
         |[3:14..3:15): x => local0
         |[3:17..3:18): x => local0
         |[4:10..4:11): a => local2
         |[4:12..4:13): + => scala/Int#`+`(+4).
         |[4:14..4:15): b => local3
         |""".stripMargin
    else
      """|[0:7..0:8): w <= _empty_/w.
         |[1:6..1:7): r <= _empty_/w.r.
         |[2:4..2:5): x <= local0
         |[2:9..2:13): List => scala/package.List.
         |[3:5..3:6): a <= local2
         |[3:8..3:9): b <= local3
         |[3:14..3:15): x => local0
         |[3:17..3:18): x => local0
         |[4:10..4:11): a => local2
         |[4:12..4:13): + => scala/Int#`+`(+4).
         |[4:14..4:15): b => local3
         |""".stripMargin,
  )

  check(
    "value definition with a tuple pattern",
    """|object v4 {
       |  for (x <- List(1); (a, b) = (x, x)) yield a + b
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): v4 <= _empty_/v4.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:22..1:23): a <= local2
         |[1:25..1:26): b <= local3
         |[1:31..1:32): x => local0
         |[1:34..1:35): x => local0
         |[1:44..1:45): a => local2
         |[1:46..1:47): + => scala/Int#`+`(+4).
         |[1:48..1:49): b => local3
         |""".stripMargin
    else
      """|[0:7..0:9): v4 <= _empty_/v4.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:22..1:23): a <= local2
         |[1:25..1:26): b <= local3
         |[1:31..1:32): x => local0
         |[1:34..1:35): x => local0
         |[1:44..1:45): a => local2
         |[1:46..1:47): + => scala/Int#`+`(+4).
         |[1:48..1:49): b => local3
         |""".stripMargin,
    """|_empty_/v4. => final object v4 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, Tuple2[Int, Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local b: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:49): for (x <- List(1); (a, b) = (x, x)) yield a + b => orig(List(1)).map[Tuple2[Int, Tuple2[Int, Int]], List[Tuple2[Int, Tuple2[Int, Int]]]]({(x) => orig((a, b) = (x, x))})(List.canBuildFrom[Tuple2[Int, Tuple2[Int, Int]]]).map[Int, List[Int]]({(x$3) => orig(a + b)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$3 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:21..1:27): (a, b) => *.apply[Tuple2[Int, Int], Int, Int]
         |  apply => scala/Tuple3.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:49): for (x <- List(1); (a, b) = (x, x)) yield a + b => orig(List(1)).map[Tuple2[Int, Tuple2[Int, Int]]]({(x) => orig((a, b) = (x, x))}).map[Int]({(x$3) => orig(a + b)})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  x => local0
         |  x$3 => local1
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:21..1:27): (a, b) => *.apply[Tuple2[Int, Int], Int, Int]
         |  apply => scala/Tuple3.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin,
  )

  check(
    "value definition before a generator",
    """|object v8 {
       |  for (x <- List(1); (a, b) = (x, x); y <- List(a)) yield b + y
       |}
       |""".stripMargin,
  )(
    if (ScalaVersion.is212)
      """|[0:7..0:9): v8 <= _empty_/v8.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/collection/immutable/List.
         |[1:22..1:23): a <= local3
         |[1:25..1:26): b <= local4
         |[1:31..1:32): x => local0
         |[1:34..1:35): x => local0
         |[1:38..1:39): y <= local2
         |[1:43..1:47): List => scala/collection/immutable/List.
         |[1:48..1:49): a => local3
         |[1:58..1:59): b => local4
         |[1:60..1:61): + => scala/Int#`+`(+4).
         |[1:62..1:63): y => local2
         |""".stripMargin
    else
      """|[0:7..0:9): v8 <= _empty_/v8.
         |[1:7..1:8): x <= local0
         |[1:12..1:16): List => scala/package.List.
         |[1:22..1:23): a <= local3
         |[1:25..1:26): b <= local4
         |[1:31..1:32): x => local0
         |[1:34..1:35): x => local0
         |[1:38..1:39): y <= local2
         |[1:43..1:47): List => scala/package.List.
         |[1:48..1:49): a => local3
         |[1:58..1:59): b => local4
         |[1:60..1:61): + => scala/Int#`+`(+4).
         |[1:62..1:63): y => local2
         |""".stripMargin,
    """|_empty_/v8. => final object v8 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[Int, Tuple2[Int, Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => param y: Int
       |  Int => scala/Int#
       |local3 => val local a: Int
       |  Int => scala/Int#
       |local4 => val local b: Int
       |  Int => scala/Int#
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:2..1:63): for (x <- List(1); (a, b) = (x, x); y <- List(a)) yield b + y => orig(List(1)).map[Tuple2[Int, Tuple2[Int, Int]], List[Tuple2[Int, Tuple2[Int, Int]]]]({(x) => orig((a, b) = (x, x))})(List.canBuildFrom[Tuple2[Int, Tuple2[Int, Int]]]).flatMap[Int, List[Int]]({(x$3) => orig(List(a)).map[Int, List[Int]]({(y) => orig(b + y)})(List.canBuildFrom[Int])})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  flatMap => scala/collection/immutable/List#flatMap().
         |  x$3 => local1
         |  y => local2
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:21..1:27): (a, b) => *.apply[Tuple2[Int, Int], Int, Int]
         |  apply => scala/Tuple3.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[1:43..1:47): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:2..1:63): for (x <- List(1); (a, b) = (x, x); y <- List(a)) yield b + y => orig(List(1)).map[Tuple2[Int, Tuple2[Int, Int]]]({(x) => orig((a, b) = (x, x))}).flatMap[Int]({(x$3) => orig(List(a)).map[Int]({(y) => orig(b + y)})})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  x => local0
         |  flatMap => scala/collection/immutable/List#flatMap().
         |  x$3 => local1
         |  y => local2
         |[1:12..1:16): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:21..1:27): (a, b) => *.apply[Tuple2[Int, Int], Int, Int]
         |  apply => scala/Tuple3.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[1:43..1:47): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  symbols(
    "infix value definition in a for with a binder prefixed like a fresh name",
    """|object p3 {
       |  for (a <- List(List(1)); x$1a :: y = a) yield y
       |}
       |""".stripMargin,
    """|_empty_/p3. => final object p3 extends AnyRef
       |  AnyRef => scala/AnyRef#
       |local0 => val local a: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local1 => synthetic param x$3: Tuple2[List[Int], ::[Int]]
       |  Tuple2 => scala/Tuple2#
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |  :: => scala/collection/immutable/`::`#
       |local2 => val local x$1a: Int
       |  Int => scala/Int#
       |local3 => val local y: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "local value named like a fresh name",
    """|object w3 {
       |  def m = {
       |    val x$1 = 1
       |    x$1
       |  }
       |}
       |""".stripMargin,
    """|_empty_/w3. => final object w3 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/w3.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => val local x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "backquoted local value named like a fresh name",
    """|object w4 {
       |  def m = {
       |    val `x$1` = 1
       |    `x$1`
       |  }
       |}
       |""".stripMargin,
    """|_empty_/w4. => final object w4 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/w4.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => val local x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "private[this] tuple patterns in a template",
    """|class p2 {
       |  private[this] val (a, b) = (1, 2)
       |  def f = a + b
       |}
       |""".stripMargin,
    """|[0:6..0:8): p2 <= _empty_/p2#
       |[0:9..0:9):  <= _empty_/p2#`<init>`().
       |[1:21..1:22): a <= _empty_/p2#a.
       |[1:24..1:25): b <= _empty_/p2#b.
       |[2:6..2:7): f <= _empty_/p2#f().
       |[2:10..2:11): a => _empty_/p2#a.
       |[2:12..2:13): + => scala/Int#`+`(+4).
       |[2:14..2:15): b => _empty_/p2#b.
       |""".stripMargin,
  )

  occurrences(
    "private[this] extractor pattern in a template",
    """|class p6 {
       |  private[this] val Some(a) = Option(1)
       |  def f = a
       |}
       |""".stripMargin,
    """|[0:6..0:8): p6 <= _empty_/p6#
       |[0:9..0:9):  <= _empty_/p6#`<init>`().
       |[1:20..1:24): Some => scala/Some.
       |[1:25..1:26): a <= _empty_/p6#a.
       |[1:30..1:36): Option => scala/Option.
       |[2:6..2:7): f <= _empty_/p6#f().
       |[2:10..2:11): a => _empty_/p6#a.
       |""".stripMargin,
  )

  occurrences(
    "var with a setter annotation in a class",
    """|object sk {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  class K {
       |    @(sa @setter) var x = 1
       |    @(sa @setter) private var w = 1
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): sk <= _empty_/sk.
       |[1:9..1:14): scala => scala/
       |[1:15..1:25): annotation => scala/annotation/
       |[1:26..1:30): meta => scala/annotation/meta/
       |[1:31..1:37): setter => scala/annotation/meta/setter#
       |[2:8..2:10): sa <= _empty_/sk.sa#
       |[2:11..2:11):  <= _empty_/sk.sa#`<init>`().
       |[2:19..2:24): scala => scala/
       |[2:25..2:35): annotation => scala/annotation/
       |[2:36..2:52): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:52..2:52):  => scala/annotation/Annotation#`<init>`().
       |[3:8..3:9): K <= _empty_/sk.K#
       |[3:10..3:10):  <= _empty_/sk.K#`<init>`().
       |[4:6..4:8): sa => _empty_/sk.sa#
       |[4:10..4:16): setter => scala/annotation/meta/setter#
       |[4:16..4:16):  => scala/annotation/meta/setter#`<init>`().
       |[4:17..4:17):  => _empty_/sk.sa#`<init>`().
       |[4:22..4:23): x <= _empty_/sk.K#x().
       |[5:6..5:8): sa => _empty_/sk.sa#
       |[5:10..5:16): setter => scala/annotation/meta/setter#
       |[5:16..5:16):  => scala/annotation/meta/setter#`<init>`().
       |[5:17..5:17):  => _empty_/sk.sa#`<init>`().
       |[5:30..5:31): w <= _empty_/sk.K#w().
       |""".stripMargin,
  )

  symbols(
    "var with a setter annotation in a class",
    """|object sk {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  class K {
       |    @(sa @setter) var x = 1
       |    @(sa @setter) private var w = 1
       |  }
       |}
       |""".stripMargin,
    """|_empty_/sk. => final object sk extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sk.K# => class K extends AnyRef { +5 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sk.K#`<init>`(). => primary ctor <init>()
       |_empty_/sk.K#`w_=`(). => @sa @setter private var method w_=(x$1: Int): Unit
       |  sa => _empty_/sk.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => _empty_/sk.K#`w_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/sk.K#`w_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/sk.K#`x_=`(). => @sa @setter var method x_=(x$1: Int): Unit
       |  sa => _empty_/sk.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => _empty_/sk.K#`x_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/sk.K#`x_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/sk.K#w(). => private var method w: Int
       |  Int => scala/Int#
       |_empty_/sk.K#x(). => var method x: Int
       |  Int => scala/Int#
       |_empty_/sk.sa# => class sa extends Annotation with StaticAnnotation { +1 decls }
       |  Annotation => scala/annotation/Annotation#
       |  StaticAnnotation => scala/annotation/StaticAnnotation#
       |_empty_/sk.sa#`<init>`(). => primary ctor <init>()
       |""".stripMargin,
  )

  occurrences(
    "class parameter var with a setter annotation",
    """|object sp {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  class P(@(sa @setter) var y: Int, @(sa @setter) private var v: Int)
       |}
       |""".stripMargin,
    """|[0:7..0:9): sp <= _empty_/sp.
       |[1:9..1:14): scala => scala/
       |[1:15..1:25): annotation => scala/annotation/
       |[1:26..1:30): meta => scala/annotation/meta/
       |[1:31..1:37): setter => scala/annotation/meta/setter#
       |[2:8..2:10): sa <= _empty_/sp.sa#
       |[2:11..2:11):  <= _empty_/sp.sa#`<init>`().
       |[2:19..2:24): scala => scala/
       |[2:25..2:35): annotation => scala/annotation/
       |[2:36..2:52): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:52..2:52):  => scala/annotation/Annotation#`<init>`().
       |[3:8..3:9): P <= _empty_/sp.P#
       |[3:9..3:9):  <= _empty_/sp.P#`<init>`().
       |[3:12..3:14): sa => _empty_/sp.sa#
       |[3:16..3:22): setter => scala/annotation/meta/setter#
       |[3:22..3:22):  => scala/annotation/meta/setter#`<init>`().
       |[3:23..3:23):  => _empty_/sp.sa#`<init>`().
       |[3:28..3:29): y <= _empty_/sp.P#`<init>`().(y)
       |[3:28..3:29): y <= _empty_/sp.P#y().
       |[3:31..3:34): Int => scala/Int#
       |[3:38..3:40): sa => _empty_/sp.sa#
       |[3:42..3:48): setter => scala/annotation/meta/setter#
       |[3:48..3:48):  => scala/annotation/meta/setter#`<init>`().
       |[3:49..3:49):  => _empty_/sp.sa#`<init>`().
       |[3:62..3:63): v <= _empty_/sp.P#`<init>`().(v)
       |[3:62..3:63): v <= _empty_/sp.P#v().
       |[3:65..3:68): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "var pattern with a setter annotation in a class",
    """|object sz {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  class Z {
       |    @(sa @setter) var Some(z) = Option(1)
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): sz <= _empty_/sz.
       |[1:9..1:14): scala => scala/
       |[1:15..1:25): annotation => scala/annotation/
       |[1:26..1:30): meta => scala/annotation/meta/
       |[1:31..1:37): setter => scala/annotation/meta/setter#
       |[2:8..2:10): sa <= _empty_/sz.sa#
       |[2:11..2:11):  <= _empty_/sz.sa#`<init>`().
       |[2:19..2:24): scala => scala/
       |[2:25..2:35): annotation => scala/annotation/
       |[2:36..2:52): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:52..2:52):  => scala/annotation/Annotation#`<init>`().
       |[3:8..3:9): Z <= _empty_/sz.Z#
       |[3:10..3:10):  <= _empty_/sz.Z#`<init>`().
       |[4:6..4:8): sa => _empty_/sz.sa#
       |[4:10..4:16): setter => scala/annotation/meta/setter#
       |[4:16..4:16):  => scala/annotation/meta/setter#`<init>`().
       |[4:17..4:17):  => _empty_/sz.sa#`<init>`().
       |[4:22..4:26): Some => scala/Some.
       |[4:27..4:28): z <= _empty_/sz.Z#z().
       |[4:32..4:38): Option => scala/Option.
       |""".stripMargin,
  )

  symbols(
    "var pattern with a setter annotation in a class",
    """|object sz {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  class Z {
       |    @(sa @setter) var Some(z) = Option(1)
       |  }
       |}
       |""".stripMargin,
    """|_empty_/sz. => final object sz extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sz.Z# => class Z extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sz.Z#`<init>`(). => primary ctor <init>()
       |_empty_/sz.Z#`z_=`(). => @sa @setter var method z_=(x$1: Int): Unit
       |  sa => _empty_/sz.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => _empty_/sz.Z#`z_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/sz.Z#`z_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/sz.Z#z(). => var method z: Int
       |  Int => scala/Int#
       |_empty_/sz.sa# => class sa extends Annotation with StaticAnnotation { +1 decls }
       |  Annotation => scala/annotation/Annotation#
       |  StaticAnnotation => scala/annotation/StaticAnnotation#
       |_empty_/sz.sa#`<init>`(). => primary ctor <init>()
       |""".stripMargin,
  )

  occurrences(
    "var patterns of other shapes with a setter annotation in a class",
    """|object sv {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  class V {
       |    @(sa @setter) var (t, u) = (1, 2)
       |    @(sa @setter) var (s: Int) = 1
       |    @(sa @setter) var r :: _ = Option(1).toList
       |    @(sa @setter) var Tuple2(q, _) = (1, 2)
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): sv <= _empty_/sv.
       |[1:9..1:14): scala => scala/
       |[1:15..1:25): annotation => scala/annotation/
       |[1:26..1:30): meta => scala/annotation/meta/
       |[1:31..1:37): setter => scala/annotation/meta/setter#
       |[2:8..2:10): sa <= _empty_/sv.sa#
       |[2:11..2:11):  <= _empty_/sv.sa#`<init>`().
       |[2:19..2:24): scala => scala/
       |[2:25..2:35): annotation => scala/annotation/
       |[2:36..2:52): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:52..2:52):  => scala/annotation/Annotation#`<init>`().
       |[3:8..3:9): V <= _empty_/sv.V#
       |[3:10..3:10):  <= _empty_/sv.V#`<init>`().
       |[4:6..4:8): sa => _empty_/sv.sa#
       |[4:10..4:16): setter => scala/annotation/meta/setter#
       |[4:16..4:16):  => scala/annotation/meta/setter#`<init>`().
       |[4:17..4:17):  => _empty_/sv.sa#`<init>`().
       |[4:23..4:24): t <= _empty_/sv.V#t().
       |[4:26..4:27): u <= _empty_/sv.V#u().
       |[5:6..5:8): sa => _empty_/sv.sa#
       |[5:10..5:16): setter => scala/annotation/meta/setter#
       |[5:16..5:16):  => scala/annotation/meta/setter#`<init>`().
       |[5:17..5:17):  => _empty_/sv.sa#`<init>`().
       |[5:23..5:24): s <= _empty_/sv.V#s().
       |[5:26..5:29): Int => scala/Int#
       |[6:6..6:8): sa => _empty_/sv.sa#
       |[6:10..6:16): setter => scala/annotation/meta/setter#
       |[6:16..6:16):  => scala/annotation/meta/setter#`<init>`().
       |[6:17..6:17):  => _empty_/sv.sa#`<init>`().
       |[6:22..6:23): r <= _empty_/sv.V#r().
       |[6:24..6:26): :: => scala/package.`::`.
       |[6:31..6:37): Option => scala/Option.
       |[6:41..6:47): toList => scala/Option#toList().
       |[7:6..7:8): sa => _empty_/sv.sa#
       |[7:10..7:16): setter => scala/annotation/meta/setter#
       |[7:16..7:16):  => scala/annotation/meta/setter#`<init>`().
       |[7:17..7:17):  => _empty_/sv.sa#`<init>`().
       |[7:22..7:28): Tuple2 => scala/Tuple2.
       |[7:29..7:30): q <= _empty_/sv.V#q().
       |""".stripMargin,
  )

  symbols(
    "var patterns of other shapes with a setter annotation in a class",
    """|object sv {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  class V {
       |    @(sa @setter) var (t, u) = (1, 2)
       |    @(sa @setter) var (s: Int) = 1
       |    @(sa @setter) var r :: _ = Option(1).toList
       |    @(sa @setter) var Tuple2(q, _) = (1, 2)
       |  }
       |}
       |""".stripMargin,
    """|_empty_/sv. => final object sv extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sv.V# => class V extends AnyRef { +12 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sv.V#`<init>`(). => primary ctor <init>()
       |_empty_/sv.V#`q_=`(). => @sa @setter var method q_=(x$1: Int): Unit
       |  sa => _empty_/sv.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => _empty_/sv.V#`q_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/sv.V#`q_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/sv.V#`r_=`(). => @sa @setter var method r_=(x$1: Int): Unit
       |  sa => _empty_/sv.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => _empty_/sv.V#`r_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/sv.V#`r_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/sv.V#`s_=`(). => @sa @setter var method s_=(x$1: Int): Unit
       |  sa => _empty_/sv.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => _empty_/sv.V#`s_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/sv.V#`s_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/sv.V#`t_=`(). => @sa @setter var method t_=(x$1: Int): Unit
       |  sa => _empty_/sv.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => _empty_/sv.V#`t_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/sv.V#`t_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/sv.V#`u_=`(). => @sa @setter var method u_=(x$1: Int): Unit
       |  sa => _empty_/sv.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => _empty_/sv.V#`u_=`().(x$1)
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |_empty_/sv.V#`u_=`().(x$1) => synthetic param x$1: Int
       |  Int => scala/Int#
       |_empty_/sv.V#q(). => var method q: Int
       |  Int => scala/Int#
       |_empty_/sv.V#r(). => var method r: Int
       |  Int => scala/Int#
       |_empty_/sv.V#s(). => var method s: Int
       |  Int => scala/Int#
       |_empty_/sv.V#t(). => var method t: Int
       |  Int => scala/Int#
       |_empty_/sv.V#u(). => var method u: Int
       |  Int => scala/Int#
       |_empty_/sv.V#x$1. => private[this] synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/sv.sa# => class sa extends Annotation with StaticAnnotation { +1 decls }
       |  Annotation => scala/annotation/Annotation#
       |  StaticAnnotation => scala/annotation/StaticAnnotation#
       |_empty_/sv.sa#`<init>`(). => primary ctor <init>()
       |""".stripMargin,
  )

  occurrences(
    "vars with a setter annotation in local classes",
    """|object sl {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  def m(): Unit = {
       |    class K {
       |      @(sa @setter) var x = 1
       |      @(sa @setter) private var w = 1
       |      @(sa @setter) var Some(z) = Option(1)
       |    }
       |    class P(@(sa @setter) var y: Int)
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): sl <= _empty_/sl.
       |[1:9..1:14): scala => scala/
       |[1:15..1:25): annotation => scala/annotation/
       |[1:26..1:30): meta => scala/annotation/meta/
       |[1:31..1:37): setter => scala/annotation/meta/setter#
       |[2:8..2:10): sa <= _empty_/sl.sa#
       |[2:11..2:11):  <= _empty_/sl.sa#`<init>`().
       |[2:19..2:24): scala => scala/
       |[2:25..2:35): annotation => scala/annotation/
       |[2:36..2:52): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:52..2:52):  => scala/annotation/Annotation#`<init>`().
       |[3:6..3:7): m <= _empty_/sl.m().
       |[3:11..3:15): Unit => scala/Unit#
       |[4:10..4:11): K <= local0
       |[4:12..4:12):  <= local1
       |[5:8..5:10): sa => _empty_/sl.sa#
       |[5:12..5:18): setter => scala/annotation/meta/setter#
       |[5:18..5:18):  => scala/annotation/meta/setter#`<init>`().
       |[5:19..5:19):  => _empty_/sl.sa#`<init>`().
       |[5:24..5:25): x <= local2
       |[6:8..6:10): sa => _empty_/sl.sa#
       |[6:12..6:18): setter => scala/annotation/meta/setter#
       |[6:18..6:18):  => scala/annotation/meta/setter#`<init>`().
       |[6:19..6:19):  => _empty_/sl.sa#`<init>`().
       |[6:32..6:33): w <= local4
       |[7:8..7:10): sa => _empty_/sl.sa#
       |[7:12..7:18): setter => scala/annotation/meta/setter#
       |[7:18..7:18):  => scala/annotation/meta/setter#`<init>`().
       |[7:19..7:19):  => _empty_/sl.sa#`<init>`().
       |[7:24..7:28): Some => scala/Some.
       |[7:29..7:30): z <= local6
       |[7:34..7:40): Option => scala/Option.
       |[9:10..9:11): P <= local11
       |[9:11..9:11):  <= local14
       |[9:14..9:16): sa => _empty_/sl.sa#
       |[9:18..9:24): setter => scala/annotation/meta/setter#
       |[9:24..9:24):  => scala/annotation/meta/setter#`<init>`().
       |[9:25..9:25):  => _empty_/sl.sa#`<init>`().
       |[9:30..9:31): y <= local12
       |[9:33..9:36): Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "vars with a setter annotation in local classes",
    """|object sl {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  def m(): Unit = {
       |    class K {
       |      @(sa @setter) var x = 1
       |      @(sa @setter) private var w = 1
       |      @(sa @setter) var Some(z) = Option(1)
       |    }
       |    class P(@(sa @setter) var y: Int)
       |  }
       |}
       |""".stripMargin,
    """|_empty_/sl. => final object sl extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/sl.m(). => method m(): Unit
       |  Unit => scala/Unit#
       |_empty_/sl.sa# => class sa extends Annotation with StaticAnnotation { +1 decls }
       |  Annotation => scala/annotation/Annotation#
       |  StaticAnnotation => scala/annotation/StaticAnnotation#
       |_empty_/sl.sa#`<init>`(). => primary ctor <init>()
       |local0 => class K extends AnyRef { +7 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => primary ctor <init>()
       |local2 => var method x: Int
       |  Int => scala/Int#
       |local3 => @sa @setter var method x_=(x$1: Int): Unit
       |  sa => _empty_/sl.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => local8
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local4 => private var method w: Int
       |  Int => scala/Int#
       |local5 => @sa @setter private var method w_=(x$1: Int): Unit
       |  sa => _empty_/sl.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => local9
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local6 => var method z: Int
       |  Int => scala/Int#
       |local7 => @sa @setter var method z_=(x$1: Int): Unit
       |  sa => _empty_/sl.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => local10
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local8 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local9 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local10 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local11 => class P extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local12 => var param y: Int
       |  Int => scala/Int#
       |local13 => @sa @setter var method y_=(x$1: Int): Unit
       |  sa => _empty_/sl.sa#
       |  setter => scala/annotation/meta/setter#
       |  x$1 => local15
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local14 => primary ctor <init>(var y: Int)
       |  y => local12
       |  Int => scala/Int#
       |local15 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "var with a getter annotation in a class",
    """|object sg {
       |  import scala.annotation.meta.getter
       |  class sa extends scala.annotation.StaticAnnotation
       |  class K {
       |    @(sa @getter) var x = 1
       |  }
       |  class P(@(sa @getter) var y: Int)
       |}
       |""".stripMargin,
    """|[0:7..0:9): sg <= _empty_/sg.
       |[1:9..1:14): scala => scala/
       |[1:15..1:25): annotation => scala/annotation/
       |[1:26..1:30): meta => scala/annotation/meta/
       |[1:31..1:37): getter => scala/annotation/meta/getter#
       |[2:8..2:10): sa <= _empty_/sg.sa#
       |[2:11..2:11):  <= _empty_/sg.sa#`<init>`().
       |[2:19..2:24): scala => scala/
       |[2:25..2:35): annotation => scala/annotation/
       |[2:36..2:52): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:52..2:52):  => scala/annotation/Annotation#`<init>`().
       |[3:8..3:9): K <= _empty_/sg.K#
       |[3:10..3:10):  <= _empty_/sg.K#`<init>`().
       |[4:6..4:8): sa => _empty_/sg.sa#
       |[4:10..4:16): getter => scala/annotation/meta/getter#
       |[4:16..4:16):  => scala/annotation/meta/getter#`<init>`().
       |[4:17..4:17):  => _empty_/sg.sa#`<init>`().
       |[4:22..4:23): x <= _empty_/sg.K#x().
       |[6:8..6:9): P <= _empty_/sg.P#
       |[6:9..6:9):  <= _empty_/sg.P#`<init>`().
       |[6:12..6:14): sa => _empty_/sg.sa#
       |[6:16..6:22): getter => scala/annotation/meta/getter#
       |[6:22..6:22):  => scala/annotation/meta/getter#`<init>`().
       |[6:23..6:23):  => _empty_/sg.sa#`<init>`().
       |[6:28..6:29): y <= _empty_/sg.P#`<init>`().(y)
       |[6:28..6:29): y <= _empty_/sg.P#y().
       |[6:31..6:34): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "var with a setter annotation in a trait and an object",
    """|object st {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  trait T {
       |    @(sa @setter) var x = 1
       |  }
       |  object O {
       |    @(sa @setter) var y = 1
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): st <= _empty_/st.
       |[1:9..1:14): scala => scala/
       |[1:15..1:25): annotation => scala/annotation/
       |[1:26..1:30): meta => scala/annotation/meta/
       |[1:31..1:37): setter => scala/annotation/meta/setter#
       |[2:8..2:10): sa <= _empty_/st.sa#
       |[2:11..2:11):  <= _empty_/st.sa#`<init>`().
       |[2:19..2:24): scala => scala/
       |[2:25..2:35): annotation => scala/annotation/
       |[2:36..2:52): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:52..2:52):  => scala/annotation/Annotation#`<init>`().
       |[3:8..3:9): T <= _empty_/st.T#
       |[3:10..3:10):  <= _empty_/st.T#`<init>`().
       |[4:6..4:8): sa => _empty_/st.sa#
       |[4:10..4:16): setter => scala/annotation/meta/setter#
       |[4:16..4:16):  => scala/annotation/meta/setter#`<init>`().
       |[4:17..4:17):  => _empty_/st.sa#`<init>`().
       |[4:22..4:23): x <= _empty_/st.T#x().
       |[6:9..6:10): O <= _empty_/st.O.
       |[7:6..7:8): sa => _empty_/st.sa#
       |[7:10..7:16): setter => scala/annotation/meta/setter#
       |[7:16..7:16):  => scala/annotation/meta/setter#`<init>`().
       |[7:17..7:17):  => _empty_/st.sa#`<init>`().
       |[7:22..7:23): y <= _empty_/st.O.y().
       |""".stripMargin,
  )

  occurrences(
    "var with a setter and getter annotation in a class",
    """|object sb {
       |  import scala.annotation.meta.{getter, setter}
       |  class sa extends scala.annotation.StaticAnnotation
       |  class K {
       |    @(sa @setter @getter) var x = 1
       |  }
       |  class P(@(sa @setter @getter) var y: Int)
       |}
       |""".stripMargin,
    """|[0:7..0:9): sb <= _empty_/sb.
       |[1:9..1:14): scala => scala/
       |[1:15..1:25): annotation => scala/annotation/
       |[1:26..1:30): meta => scala/annotation/meta/
       |[1:32..1:38): getter => scala/annotation/meta/getter#
       |[1:40..1:46): setter => scala/annotation/meta/setter#
       |[2:8..2:10): sa <= _empty_/sb.sa#
       |[2:11..2:11):  <= _empty_/sb.sa#`<init>`().
       |[2:19..2:24): scala => scala/
       |[2:25..2:35): annotation => scala/annotation/
       |[2:36..2:52): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:52..2:52):  => scala/annotation/Annotation#`<init>`().
       |[3:8..3:9): K <= _empty_/sb.K#
       |[3:10..3:10):  <= _empty_/sb.K#`<init>`().
       |[4:6..4:8): sa => _empty_/sb.sa#
       |[4:10..4:16): setter => scala/annotation/meta/setter#
       |[4:16..4:16):  => scala/annotation/meta/setter#`<init>`().
       |[4:18..4:24): getter => scala/annotation/meta/getter#
       |[4:24..4:24):  => scala/annotation/meta/getter#`<init>`().
       |[4:25..4:25):  => _empty_/sb.sa#`<init>`().
       |[4:30..4:31): x <= _empty_/sb.K#x().
       |[6:8..6:9): P <= _empty_/sb.P#
       |[6:9..6:9):  <= _empty_/sb.P#`<init>`().
       |[6:12..6:14): sa => _empty_/sb.sa#
       |[6:16..6:22): setter => scala/annotation/meta/setter#
       |[6:22..6:22):  => scala/annotation/meta/setter#`<init>`().
       |[6:24..6:30): getter => scala/annotation/meta/getter#
       |[6:30..6:30):  => scala/annotation/meta/getter#`<init>`().
       |[6:31..6:31):  => _empty_/sb.sa#`<init>`().
       |[6:36..6:37): y <= _empty_/sb.P#`<init>`().(y)
       |[6:36..6:37): y <= _empty_/sb.P#y().
       |[6:39..6:42): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "private[this] var with a setter annotation in a class",
    """|object sh {
       |  import scala.annotation.meta.setter
       |  class sa extends scala.annotation.StaticAnnotation
       |  class K {
       |    @(sa @setter) private[this] var x = 1
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): sh <= _empty_/sh.
       |[1:9..1:14): scala => scala/
       |[1:15..1:25): annotation => scala/annotation/
       |[1:26..1:30): meta => scala/annotation/meta/
       |[1:31..1:37): setter => scala/annotation/meta/setter#
       |[2:8..2:10): sa <= _empty_/sh.sa#
       |[2:11..2:11):  <= _empty_/sh.sa#`<init>`().
       |[2:19..2:24): scala => scala/
       |[2:25..2:35): annotation => scala/annotation/
       |[2:36..2:52): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:52..2:52):  => scala/annotation/Annotation#`<init>`().
       |[3:8..3:9): K <= _empty_/sh.K#
       |[3:10..3:10):  <= _empty_/sh.K#`<init>`().
       |[4:6..4:8): sa => _empty_/sh.sa#
       |[4:10..4:16): setter => scala/annotation/meta/setter#
       |[4:16..4:16):  => scala/annotation/meta/setter#`<init>`().
       |[4:17..4:17):  => _empty_/sh.sa#`<init>`().
       |[4:36..4:37): x <= _empty_/sh.K#x().
       |""".stripMargin,
  )

  occurrences(
    "var with an annotated type",
    """|object sy {
       |  class sa extends scala.annotation.StaticAnnotation
       |  class K { var x: Int @sa = 1 }
       |  trait T { var y: Int @sa = 1 }
       |  def m(): Unit = { class L { var z: Int @sa = 1 } }
       |}
       |""".stripMargin,
    """|[0:7..0:9): sy <= _empty_/sy.
       |[1:8..1:10): sa <= _empty_/sy.sa#
       |[1:11..1:11):  <= _empty_/sy.sa#`<init>`().
       |[1:19..1:24): scala => scala/
       |[1:25..1:35): annotation => scala/annotation/
       |[1:36..1:52): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[1:52..1:52):  => scala/annotation/Annotation#`<init>`().
       |[2:8..2:9): K <= _empty_/sy.K#
       |[2:10..2:10):  <= _empty_/sy.K#`<init>`().
       |[2:16..2:17): x <= _empty_/sy.K#x().
       |[2:19..2:22): Int => scala/Int#
       |[2:24..2:26): sa => _empty_/sy.sa#
       |[2:26..2:26):  => _empty_/sy.sa#`<init>`().
       |[3:8..3:9): T <= _empty_/sy.T#
       |[3:10..3:10):  <= _empty_/sy.T#`<init>`().
       |[3:16..3:17): y <= _empty_/sy.T#y().
       |[3:19..3:22): Int => scala/Int#
       |[3:24..3:26): sa => _empty_/sy.sa#
       |[3:26..3:26):  => _empty_/sy.sa#`<init>`().
       |[4:6..4:7): m <= _empty_/sy.m().
       |[4:11..4:15): Unit => scala/Unit#
       |[4:26..4:27): L <= local0
       |[4:28..4:28):  <= local1
       |[4:34..4:35): z <= local2
       |[4:37..4:40): Int => scala/Int#
       |[4:42..4:44): sa => _empty_/sy.sa#
       |[4:44..4:44):  => _empty_/sy.sa#`<init>`().
       |""".stripMargin,
  )

}
