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
       |local2 => synthetic val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
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
       |local2 => synthetic val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
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
       |local2 => synthetic val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
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
       |local2 => synthetic val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
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
       |local3 => synthetic val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
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
       |local3 => synthetic lazy val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
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
       |local3 => synthetic val local x$1: Tuple2[Int, List[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
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
       |_empty_/o.x$1. => synthetic val method x$1: Tuple2[Int, Int]
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
       |_empty_/z#x$1. => synthetic lazy val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/z#x$2. => synthetic val method x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/z#x$3. => synthetic val method x$3: Tuple2[Int, Int]
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
       |local3 => synthetic val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
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
       |local4 => synthetic val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
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
       |local3 => synthetic val local x$2: Tuple2[Some[Int], Int]
       |  Tuple2 => scala/Tuple2#
       |  Some => scala/Some#
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
       |local4 => synthetic val local x$2: Tuple3[::[Int], Int, List[Int]]
       |  Tuple3 => scala/Tuple3#
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
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
       |local4 => synthetic val local x$2: Tuple3[::[Int], Int, List[Int]]
       |  Tuple3 => scala/Tuple3#
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
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
       |local4 => synthetic val local x$2: Tuple3[Tuple2[Int, Int], Int, Int]
       |  Tuple3 => scala/Tuple3#
       |  Tuple2 => scala/Tuple2#
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
       |local3 => synthetic val local x$2: Tuple2[Some[Int], Int]
       |  Tuple2 => scala/Tuple2#
       |  Some => scala/Some#
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
       |local4 => synthetic val local x$2: Tuple3[Some[List[Int]], Int, List[Int]]
       |  Tuple3 => scala/Tuple3#
       |  Some => scala/Some#
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
       |local5 => synthetic val local x$1: Tuple3[::[Int], Int, List[Int]]
       |  Tuple3 => scala/Tuple3#
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
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
       |local4 => synthetic val local x$2: Tuple3[::[Int], Int, List[Int]]
       |  Tuple3 => scala/Tuple3#
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
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
         |[1:39..1:40): b <= local5
         |[1:41..1:43): :: => scala/package.`::`.
         |[1:44..1:45): z <= local6
         |[1:48..1:49): y => local3
         |[1:57..1:58): z => local6
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
         |[1:39..1:40): b <= local5
         |[1:41..1:43): :: => scala/package.`::`.
         |[1:44..1:45): z <= local6
         |[1:48..1:49): y => local3
         |[1:57..1:58): z => local6
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
       |local4 => synthetic val local x$3: Tuple3[::[Int], Int, List[Int]]
       |  Tuple3 => scala/Tuple3#
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
       |local5 => val local b: Int
       |  Int => scala/Int#
       |local6 => val local z: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local7 => synthetic val local x$4: Tuple3[::[Int], Int, List[Int]]
       |  Tuple3 => scala/Tuple3#
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
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
       |local5 => synthetic val local x$2: Tuple3[::[Int], Int, List[Int]]
       |  Tuple3 => scala/Tuple3#
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
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
         |[2:7..2:8): x <= local2
         |[2:12..2:16): List => scala/collection/immutable/List.
         |[2:21..2:22): a <= local4
         |[2:23..2:24): ~ => _empty_/v12.`~`.
         |[2:25..2:26): b <= local5
         |[2:33..2:34): ~ => _empty_/v12.`~`#
         |[2:34..2:34):  => _empty_/v12.`~`#`<init>`().
         |[2:35..2:36): x => local2
         |[2:38..2:39): x => local2
         |[2:48..2:49): a => local4
         |[2:50..2:51): + => scala/Int#`+`(+4).
         |[2:52..2:53): b => local5
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
         |[2:7..2:8): x <= local2
         |[2:12..2:16): List => scala/package.List.
         |[2:21..2:22): a <= local4
         |[2:23..2:24): ~ => _empty_/v12.`~`.
         |[2:25..2:26): b <= local5
         |[2:33..2:34): ~ => _empty_/v12.`~`#
         |[2:34..2:34):  => _empty_/v12.`~`#`<init>`().
         |[2:35..2:36): x => local2
         |[2:38..2:39): x => local2
         |[2:48..2:49): a => local4
         |[2:50..2:51): + => scala/Int#`+`(+4).
         |[2:52..2:53): b => local5
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
         |local0 => synthetic var local acc: Int
         |  Int => scala/Int#
         |local1 => synthetic val local ~$1: ~
         |  ~ => _empty_/v12.`~`#
         |local2 => val local x: Int
         |  Int => scala/Int#
         |local3 => synthetic param x$3: Tuple2[Int, ~]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  ~ => _empty_/v12.`~`#
         |local4 => val local a: Int
         |  Int => scala/Int#
         |local5 => val local b: Int
         |  Int => scala/Int#
         |local6 => synthetic val local x$2: Tuple3[~, Int, Int]
         |  Tuple3 => scala/Tuple3#
         |  ~ => _empty_/v12.`~`#
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
         |local0 => synthetic var local acc: Int
         |  Int => scala/Int#
         |local1 => synthetic val local ~$1: ~
         |  ~ => _empty_/v12.`~`#
         |local2 => val local x: Int
         |  Int => scala/Int#
         |local3 => synthetic param x$3: Tuple2[Int, ~]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  ~ => _empty_/v12.`~`#
         |local4 => val local a: Int
         |  Int => scala/Int#
         |local5 => val local b: Int
         |  Int => scala/Int#
         |local6 => synthetic val local x$2: Tuple3[~, Int, Int]
         |  Tuple3 => scala/Tuple3#
         |  ~ => _empty_/v12.`~`#
         |  Int => scala/Int#
         |""".stripMargin,
    if (ScalaVersion.is212)
      """|[2:2..2:53): for (x <- List(1); a ~ b = new ~(x, x)) yield a + b => orig(List(1)).map[Tuple2[Int, ~], List[Tuple2[Int, ~]]]({(x) => orig(a ~ b = new ~(x, x))})(List.canBuildFrom[Tuple2[Int, ~]]).map[Int, List[Int]]({(x$3) => orig(a + b)})(List.canBuildFrom[Int])
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  ~ => _empty_/v12.`~`#
         |  List => scala/collection/immutable/List#
         |  x => local2
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$3 => local3
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
         |  x => local2
         |  x$3 => local3
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
       |local4 => synthetic val local x$2: Tuple3[Tuple2[Int, Int], Int, Int]
       |  Tuple3 => scala/Tuple3#
       |  Tuple2 => scala/Tuple2#
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
       |local4 => synthetic val local x$2: Tuple3[Tuple2[Int, Int], Int, Int]
       |  Tuple3 => scala/Tuple3#
       |  Tuple2 => scala/Tuple2#
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
       |local5 => synthetic val local x$2: Tuple3[Tuple2[Int, Int], Int, Int]
       |  Tuple3 => scala/Tuple3#
       |  Tuple2 => scala/Tuple2#
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
       |local4 => synthetic val local x$2: Tuple3[::[Int], Int, List[Int]]
       |  Tuple3 => scala/Tuple3#
       |  :: => scala/collection/immutable/`::`#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
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

}
