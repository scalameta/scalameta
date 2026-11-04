package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}

import scala.collection.mutable

class SamLambdaSuite extends SemanticdbSuite {

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  // for each name, the occurrences at its exact range, as scalafix looks them up
  private def names(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = test(s"$name: names") {
    import scala.meta._
    val occurrences = computeDatabaseSectionFromSnippet(code, "Occurrences").linesIterator.toList
    val obtained = dialects.Scala213(code).parse[Source].get.collect { case n: Name =>
      val p = n.pos
      val range = s"[${p.startLine}:${p.startColumn}..${p.endLine}:${p.endColumn}):"
      val found = occurrences.filter(_.startsWith(range))
      if (found.isEmpty) s"$range ${p.text}".trim else found.mkString("\n")
    }
    assertNoDiff(obtained.mkString("\n"), expected)
  }

  // for each tree, the first occurrence at its exact range, which scalafix reads as `tree.symbol`
  private def treeSymbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = test(s"$name: tree symbols") {
    import scala.meta._
    val occurrences = computeDatabaseFromSnippet(code).occurrences
    val obtained = mutable.LinkedHashSet.empty[String]
    dialects.Scala213(code).parse[Source].get.traverse { case t =>
      val p = t.pos
      val (bl, bc, el, ec) = (p.startLine, p.startColumn, p.endLine, p.endColumn)
      occurrences.find(_.range.contains(s.Range(bl, bc, el, ec))).foreach { o =>
        val role = if (o.role.isDefinition) "<=" else "=>"
        obtained += s"[$bl:$bc..$el:$ec): ${p.text} $role ${o.symbol}"
      }
    }
    assertNoDiff(obtained.mkString("\n"), expected)
  }

  symbols(
    "lambda of a SAM class",
    """|abstract class F { def apply(x: Int): Int }
       |class t20 {
       |  val v: F = x => x
       |  def m = { val f: F = x => x + 1; f }
       |}
       |""".stripMargin,
    """|_empty_/F# => abstract class F extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/F#`<init>`(). => primary ctor <init>()
       |_empty_/F#apply(). => abstract method apply(x: Int): Int
       |  x => _empty_/F#apply().(x)
       |  Int => scala/Int#
       |_empty_/F#apply().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/t20# => class t20 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t20#`<init>`(). => primary ctor <init>()
       |_empty_/t20#m(). => method m: F
       |  F => _empty_/F#
       |_empty_/t20#v. => val method v: F
       |  F => _empty_/F#
       |local0 => synthetic class $anon extends Object with F { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |local1 => synthetic method apply(x: Int): Int <: _empty_/F#apply().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with F { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |local4 => synthetic method apply(x: Int): Int <: _empty_/F#apply().
       |  x => local5
       |  Int => scala/Int#
       |local5 => param x: Int
       |  Int => scala/Int#
       |local6 => val local f: F
       |  F => _empty_/F#
       |""".stripMargin,
  )

  targeted(
    """|abstract class F { def apply(x: Int): Int }
       |class t20 {
       |  val v: F = x => x
       |  def m = { val f: F = x => x + 1; f }
       |}
       |""".stripMargin,
    (doc: s.TextDocument) => {
      val decls = doc.symbols.filter(_.symbol.startsWith("local")).flatMap(_.signature match {
        case sig: s.ClassSignature => sig.declarations.map(_.symlinks)
        case _ => None
      })
      assertEquals(decls.toList, List(Seq("local1"), Seq("local4")))
    },
  )

  occurrences(
    "SAM lambda with a placeholder",
    """|trait S { def f(a: Int): Int }
       |object t60 {
       |  def use(s: S) = s.f(0)
       |  def m = use(_ + 1)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t60 <= _empty_/t60.
       |[2:6..2:9): use <= _empty_/t60.use().
       |[2:10..2:11): s <= _empty_/t60.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t60.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t60.m().
       |[3:10..3:13): use => _empty_/t60.use().
       |[3:14..3:15): _ <= local2
       |[3:14..3:19): _ + 1 <= local0
       |[3:14..3:19): _ + 1 <= local1
       |[3:16..3:17): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda with a placeholder",
    """|trait S { def f(a: Int): Int }
       |object t60 {
       |  def use(s: S) = s.f(0)
       |  def m = use(_ + 1)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t60 <= _empty_/t60.
       |[2:6..2:9): use <= _empty_/t60.use().
       |[2:10..2:11): s <= _empty_/t60.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t60.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t60.m().
       |[3:10..3:13): use => _empty_/t60.use().
       |[3:14..3:19): _ + 1 <= local0
       |[3:14..3:15): _ <= local2
       |[3:16..3:17): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  symbols(
    "SAM lambda with a placeholder",
    """|trait S { def f(a: Int): Int }
       |object t60 {
       |  def use(s: S) = s.f(0)
       |  def m = use(_ + 1)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t60. => final object t60 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t60.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t60.use(). => method use(s: S): Int
       |  s => _empty_/t60.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t60.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): Int <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda with a typed parameter",
    """|trait S { def f(a: Int): Int }
       |object t61 {
       |  def use(s: S) = s.f(0)
       |  def m = use((x: Int) => x - 1)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t61 <= _empty_/t61.
       |[2:6..2:9): use <= _empty_/t61.use().
       |[2:10..2:11): s <= _empty_/t61.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t61.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t61.m().
       |[3:10..3:13): use => _empty_/t61.use().
       |[3:14..3:31): (x: Int) => x - 1 <= local0
       |[3:14..3:31): (x: Int) => x - 1 <= local1
       |[3:15..3:16): x <= local2
       |[3:18..3:21): Int => scala/Int#
       |[3:26..3:27): x => local2
       |[3:28..3:29): - => scala/Int#`-`(+3).
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda with a typed parameter",
    """|trait S { def f(a: Int): Int }
       |object t61 {
       |  def use(s: S) = s.f(0)
       |  def m = use((x: Int) => x - 1)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t61 <= _empty_/t61.
       |[2:6..2:9): use <= _empty_/t61.use().
       |[2:10..2:11): s <= _empty_/t61.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t61.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t61.m().
       |[3:10..3:13): use => _empty_/t61.use().
       |[3:14..3:31): (x: Int) => x - 1 <= local0
       |[3:15..3:16): x <= local2
       |[3:18..3:21): Int => scala/Int#
       |[3:26..3:27): x => local2
       |[3:28..3:29): - => scala/Int#`-`(+3).
       |""".stripMargin,
  )

  symbols(
    "SAM lambda with a typed parameter",
    """|trait S { def f(a: Int): Int }
       |object t61 {
       |  def use(s: S) = s.f(0)
       |  def m = use((x: Int) => x - 1)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t61. => final object t61 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t61.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t61.use(). => method use(s: S): Int
       |  s => _empty_/t61.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t61.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda with an untyped parameter",
    """|trait S { def f(a: Int): Int }
       |object t62 {
       |  def use(s: S) = s.f(0)
       |  def m = use(x => x - 1)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t62 <= _empty_/t62.
       |[2:6..2:9): use <= _empty_/t62.use().
       |[2:10..2:11): s <= _empty_/t62.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t62.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t62.m().
       |[3:10..3:13): use => _empty_/t62.use().
       |[3:14..3:15): x <= local2
       |[3:14..3:24): x => x - 1 <= local0
       |[3:14..3:24): x => x - 1 <= local1
       |[3:19..3:20): x => local2
       |[3:21..3:22): - => scala/Int#`-`(+3).
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda with an untyped parameter",
    """|trait S { def f(a: Int): Int }
       |object t62 {
       |  def use(s: S) = s.f(0)
       |  def m = use(x => x - 1)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t62 <= _empty_/t62.
       |[2:6..2:9): use <= _empty_/t62.use().
       |[2:10..2:11): s <= _empty_/t62.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t62.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t62.m().
       |[3:10..3:13): use => _empty_/t62.use().
       |[3:14..3:24): x => x - 1 <= local0
       |[3:14..3:15): x <= local2
       |[3:19..3:20): x => local2
       |[3:21..3:22): - => scala/Int#`-`(+3).
       |""".stripMargin,
  )

  symbols(
    "SAM lambda with an untyped parameter",
    """|trait S { def f(a: Int): Int }
       |object t62 {
       |  def use(s: S) = s.f(0)
       |  def m = use(x => x - 1)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t62. => final object t62 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t62.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t62.use(). => method use(s: S): Int
       |  s => _empty_/t62.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t62.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM block lambda",
    """|trait S { def f(a: Int): Int }
       |object t63 {
       |  def use(s: S) = s.f(0)
       |  def m = use { x =>
       |    val y = x - 1
       |    y
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t63 <= _empty_/t63.
       |[2:6..2:9): use <= _empty_/t63.use().
       |[2:10..2:11): s <= _empty_/t63.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t63.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t63.m().
       |[3:10..3:13): use => _empty_/t63.use().
       |[3:16..3:17): x <= local2
       |[3:16..5:5): x =>
       |    val y = x - 1
       |    y <= local0
       |[3:16..5:5): x =>
       |    val y = x - 1
       |    y <= local1
       |[4:8..4:9): y <= local3
       |[4:12..4:13): x => local2
       |[4:14..4:15): - => scala/Int#`-`(+3).
       |[5:4..5:5): y => local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM block lambda",
    """|trait S { def f(a: Int): Int }
       |object t63 {
       |  def use(s: S) = s.f(0)
       |  def m = use { x =>
       |    val y = x - 1
       |    y
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t63 <= _empty_/t63.
       |[2:6..2:9): use <= _empty_/t63.use().
       |[2:10..2:11): s <= _empty_/t63.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t63.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t63.m().
       |[3:10..3:13): use => _empty_/t63.use().
       |[3:16..5:5): x =>
       |    val y = x - 1
       |    y <= local0
       |[3:16..3:17): x <= local2
       |[4:8..4:9): y <= local3
       |[4:12..4:13): x => local2
       |[4:14..4:15): - => scala/Int#`-`(+3).
       |[5:4..5:5): y => local3
       |""".stripMargin,
  )

  symbols(
    "SAM block lambda",
    """|trait S { def f(a: Int): Int }
       |object t63 {
       |  def use(s: S) = s.f(0)
       |  def m = use { x =>
       |    val y = x - 1
       |    y
       |  }
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t63. => final object t63 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t63.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t63.use(). => method use(s: S): Int
       |  s => _empty_/t63.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t63.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |local3 => val local y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value",
    """|trait S { def f(a: Int): Int }
       |object t64 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t64 <= _empty_/t64.
       |[2:6..2:9): use <= _empty_/t64.use().
       |[2:10..2:11): s <= _empty_/t64.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t64.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t64.g().
       |[3:8..3:9): y <= _empty_/t64.g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t64.g().(y)
       |[4:6..4:7): m <= _empty_/t64.m().
       |[4:10..4:13): use => _empty_/t64.use().
       |[4:14..4:15): g => _empty_/t64.g().
       |[4:14..4:15): g <= local0
       |[4:14..4:15): g <= local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value",
    """|trait S { def f(a: Int): Int }
       |object t64 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t64 <= _empty_/t64.
       |[2:6..2:9): use <= _empty_/t64.use().
       |[2:10..2:11): s <= _empty_/t64.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t64.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t64.g().
       |[3:8..3:9): y <= _empty_/t64.g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t64.g().(y)
       |[4:6..4:7): m <= _empty_/t64.m().
       |[4:10..4:13): use => _empty_/t64.use().
       |[4:14..4:15): g => _empty_/t64.g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value",
    """|trait S { def f(a: Int): Int }
       |object t64 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(g)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t64. => final object t64 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t64.g(). => method g(y: Int): Int
       |  y => _empty_/t64.g().(y)
       |  Int => scala/Int#
       |_empty_/t64.g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/t64.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t64.use(). => method use(s: S): Int
       |  s => _empty_/t64.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t64.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM eta expansion",
    """|trait S { def f(a: Int): Int }
       |object t65 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(g _)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t65 <= _empty_/t65.
       |[2:6..2:9): use <= _empty_/t65.use().
       |[2:10..2:11): s <= _empty_/t65.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t65.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t65.g().
       |[3:8..3:9): y <= _empty_/t65.g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t65.g().(y)
       |[4:6..4:7): m <= _empty_/t65.m().
       |[4:10..4:13): use => _empty_/t65.use().
       |[4:14..4:15): g => _empty_/t65.g().
       |[4:14..4:15): g <= local0
       |[4:14..4:15): g <= local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM eta expansion",
    """|trait S { def f(a: Int): Int }
       |object t65 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(g _)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t65 <= _empty_/t65.
       |[2:6..2:9): use <= _empty_/t65.use().
       |[2:10..2:11): s <= _empty_/t65.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t65.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t65.g().
       |[3:8..3:9): y <= _empty_/t65.g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t65.g().(y)
       |[4:6..4:7): m <= _empty_/t65.m().
       |[4:10..4:13): use => _empty_/t65.use().
       |[4:14..4:15): g => _empty_/t65.g().
       |""".stripMargin,
  )

  symbols(
    "SAM eta expansion",
    """|trait S { def f(a: Int): Int }
       |object t65 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(g _)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t65. => final object t65 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t65.g(). => method g(y: Int): Int
       |  y => _empty_/t65.g().(y)
       |  Int => scala/Int#
       |_empty_/t65.g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/t65.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t65.use(). => method use(s: S): Int
       |  s => _empty_/t65.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t65.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of an abstract class",
    """|abstract class A { def f(a: Int): Int }
       |object t66 {
       |  def use(s: A) = s.f(0)
       |  def m = use(x => x - 1)
       |}
       |""".stripMargin,
    """|[0:15..0:16): A <= _empty_/A#
       |[0:17..0:17):  <= _empty_/A#`<init>`().
       |[0:23..0:24): f <= _empty_/A#f().
       |[0:25..0:26): a <= _empty_/A#f().(a)
       |[0:28..0:31): Int => scala/Int#
       |[0:34..0:37): Int => scala/Int#
       |[1:7..1:10): t66 <= _empty_/t66.
       |[2:6..2:9): use <= _empty_/t66.use().
       |[2:10..2:11): s <= _empty_/t66.use().(s)
       |[2:13..2:14): A => _empty_/A#
       |[2:18..2:19): s => _empty_/t66.use().(s)
       |[2:20..2:21): f => _empty_/A#f().
       |[3:6..3:7): m <= _empty_/t66.m().
       |[3:10..3:13): use => _empty_/t66.use().
       |[3:14..3:15): x <= local2
       |[3:14..3:24): x => x - 1 <= local0
       |[3:14..3:24): x => x - 1 <= local1
       |[3:19..3:20): x => local2
       |[3:21..3:22): - => scala/Int#`-`(+3).
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of an abstract class",
    """|abstract class A { def f(a: Int): Int }
       |object t66 {
       |  def use(s: A) = s.f(0)
       |  def m = use(x => x - 1)
       |}
       |""".stripMargin,
    """|[0:15..0:16): A <= _empty_/A#
       |[0:17..0:17):  <= _empty_/A#`<init>`().
       |[0:23..0:24): f <= _empty_/A#f().
       |[0:25..0:26): a <= _empty_/A#f().(a)
       |[0:28..0:31): Int => scala/Int#
       |[0:34..0:37): Int => scala/Int#
       |[1:7..1:10): t66 <= _empty_/t66.
       |[2:6..2:9): use <= _empty_/t66.use().
       |[2:10..2:11): s <= _empty_/t66.use().(s)
       |[2:13..2:14): A => _empty_/A#
       |[2:18..2:19): s => _empty_/t66.use().(s)
       |[2:20..2:21): f => _empty_/A#f().
       |[3:6..3:7): m <= _empty_/t66.m().
       |[3:10..3:13): use => _empty_/t66.use().
       |[3:14..3:24): x => x - 1 <= local0
       |[3:14..3:15): x <= local2
       |[3:19..3:20): x => local2
       |[3:21..3:22): - => scala/Int#`-`(+3).
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of an abstract class",
    """|abstract class A { def f(a: Int): Int }
       |object t66 {
       |  def use(s: A) = s.f(0)
       |  def m = use(x => x - 1)
       |}
       |""".stripMargin,
    """|_empty_/A# => abstract class A extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/A#`<init>`(). => primary ctor <init>()
       |_empty_/A#f(). => abstract method f(a: Int): Int
       |  a => _empty_/A#f().(a)
       |  Int => scala/Int#
       |_empty_/A#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t66. => final object t66 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t66.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t66.use(). => method use(s: A): Int
       |  s => _empty_/t66.use().(s)
       |  A => _empty_/A#
       |  Int => scala/Int#
       |_empty_/t66.use().(s) => param s: A
       |  A => _empty_/A#
       |local0 => synthetic class $anon extends Object with A { +1 decls }
       |  Object => java/lang/Object#
       |  A => _empty_/A#
       |local1 => synthetic method f(x: Int): Int <: _empty_/A#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "nested SAM lambdas",
    """|trait S { def f(a: Int): Int }
       |object t67 {
       |  def use(s: S) = s.f(0)
       |  def m = use(x => use(y => x + y))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t67 <= _empty_/t67.
       |[2:6..2:9): use <= _empty_/t67.use().
       |[2:10..2:11): s <= _empty_/t67.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t67.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t67.m().
       |[3:10..3:13): use => _empty_/t67.use().
       |[3:14..3:15): x <= local2
       |[3:14..3:34): x => use(y => x + y) <= local0
       |[3:14..3:34): x => use(y => x + y) <= local1
       |[3:19..3:22): use => _empty_/t67.use().
       |[3:23..3:24): y <= local5
       |[3:23..3:33): y => x + y <= local3
       |[3:23..3:33): y => x + y <= local4
       |[3:28..3:29): x => local2
       |[3:30..3:31): + => scala/Int#`+`(+4).
       |[3:32..3:33): y => local5
       |""".stripMargin,
  )

  treeSymbols(
    "nested SAM lambdas",
    """|trait S { def f(a: Int): Int }
       |object t67 {
       |  def use(s: S) = s.f(0)
       |  def m = use(x => use(y => x + y))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t67 <= _empty_/t67.
       |[2:6..2:9): use <= _empty_/t67.use().
       |[2:10..2:11): s <= _empty_/t67.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t67.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t67.m().
       |[3:10..3:13): use => _empty_/t67.use().
       |[3:14..3:34): x => use(y => x + y) <= local0
       |[3:14..3:15): x <= local2
       |[3:19..3:22): use => _empty_/t67.use().
       |[3:23..3:33): y => x + y <= local3
       |[3:23..3:24): y <= local5
       |[3:28..3:29): x => local2
       |[3:30..3:31): + => scala/Int#`+`(+4).
       |[3:32..3:33): y => local5
       |""".stripMargin,
  )

  symbols(
    "nested SAM lambdas",
    """|trait S { def f(a: Int): Int }
       |object t67 {
       |  def use(s: S) = s.f(0)
       |  def m = use(x => use(y => x + y))
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t67. => final object t67 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t67.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t67.use(). => method use(s: S): Int
       |  s => _empty_/t67.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t67.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local4 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local5
       |  Int => scala/Int#
       |local5 => param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda in a local class",
    """|trait S { def f(a: Int): Int }
       |object t68 {
       |  def use(s: S) = s.f(0)
       |  def m = { class K { val v = use(x => x) }; new K }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t68 <= _empty_/t68.
       |[2:6..2:9): use <= _empty_/t68.use().
       |[2:10..2:11): s <= _empty_/t68.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t68.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t68.m().
       |[3:18..3:19): K <= local1
       |[3:20..3:20):  <= local2
       |[3:26..3:27): v <= local0
       |[3:30..3:33): use => _empty_/t68.use().
       |[3:34..3:35): x <= local5
       |[3:34..3:40): x => x <= local3
       |[3:34..3:40): x => x <= local4
       |[3:39..3:40): x => local5
       |[3:49..3:50): K => local1
       |[3:50..3:50):  => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda in a local class",
    """|trait S { def f(a: Int): Int }
       |object t68 {
       |  def use(s: S) = s.f(0)
       |  def m = { class K { val v = use(x => x) }; new K }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t68 <= _empty_/t68.
       |[2:6..2:9): use <= _empty_/t68.use().
       |[2:10..2:11): s <= _empty_/t68.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t68.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t68.m().
       |[3:18..3:19): K <= local1
       |[3:20..3:20):  <= local2
       |[3:26..3:27): v <= local0
       |[3:30..3:33): use => _empty_/t68.use().
       |[3:34..3:40): x => x <= local3
       |[3:34..3:35): x <= local5
       |[3:39..3:40): x => local5
       |[3:49..3:50): K => local1
       |[3:50..3:50):  => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda in a local class",
    """|trait S { def f(a: Int): Int }
       |object t68 {
       |  def use(s: S) = s.f(0)
       |  def m = { class K { val v = use(x => x) }; new K }
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t68. => final object t68 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t68.m(). => method m: AnyRef { val def v: Int }
       |  AnyRef => scala/AnyRef#
       |  v => local0
       |  Int => scala/Int#
       |_empty_/t68.use(). => method use(s: S): Int
       |  s => _empty_/t68.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t68.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => val method v: Int
       |  Int => scala/Int#
       |local1 => class K extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local2 => primary ctor <init>()
       |local3 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local4 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local5
       |  Int => scala/Int#
       |local5 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda in a template",
    """|trait S { def f(a: Int): Int }
       |object t69 {
       |  def use(s: S) = s.f(0)
       |  use(x => x)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t69 <= _empty_/t69.
       |[2:6..2:9): use <= _empty_/t69.use().
       |[2:10..2:11): s <= _empty_/t69.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t69.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:2..3:5): use => _empty_/t69.use().
       |[3:6..3:7): x <= local2
       |[3:6..3:12): x => x <= local0
       |[3:6..3:12): x => x <= local1
       |[3:11..3:12): x => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda in a template",
    """|trait S { def f(a: Int): Int }
       |object t69 {
       |  def use(s: S) = s.f(0)
       |  use(x => x)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t69 <= _empty_/t69.
       |[2:6..2:9): use <= _empty_/t69.use().
       |[2:10..2:11): s <= _empty_/t69.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t69.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:2..3:5): use => _empty_/t69.use().
       |[3:6..3:12): x => x <= local0
       |[3:6..3:7): x <= local2
       |[3:11..3:12): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda in a template",
    """|trait S { def f(a: Int): Int }
       |object t69 {
       |  def use(s: S) = s.f(0)
       |  use(x => x)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t69. => final object t69 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t69.use(). => method use(s: S): Int
       |  s => _empty_/t69.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t69.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of a generic trait",
    """|trait G[T] { def f(t: T): T }
       |object t70 {
       |  val g: G[Int] = x => x
       |}
       |""".stripMargin,
    """|[0:6..0:7): G <= _empty_/G#
       |[0:8..0:9): T <= _empty_/G#[T]
       |[0:17..0:18): f <= _empty_/G#f().
       |[0:19..0:20): t <= _empty_/G#f().(t)
       |[0:22..0:23): T => _empty_/G#[T]
       |[0:26..0:27): T => _empty_/G#[T]
       |[1:7..1:10): t70 <= _empty_/t70.
       |[2:6..2:7): g <= _empty_/t70.g.
       |[2:9..2:10): G => _empty_/G#
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): x <= local2
       |[2:18..2:24): x => x <= local0
       |[2:18..2:24): x => x <= local1
       |[2:23..2:24): x => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of a generic trait",
    """|trait G[T] { def f(t: T): T }
       |object t70 {
       |  val g: G[Int] = x => x
       |}
       |""".stripMargin,
    """|[0:6..0:7): G <= _empty_/G#
       |[0:8..0:9): T <= _empty_/G#[T]
       |[0:17..0:18): f <= _empty_/G#f().
       |[0:19..0:20): t <= _empty_/G#f().(t)
       |[0:22..0:23): T => _empty_/G#[T]
       |[0:26..0:27): T => _empty_/G#[T]
       |[1:7..1:10): t70 <= _empty_/t70.
       |[2:6..2:7): g <= _empty_/t70.g.
       |[2:9..2:10): G => _empty_/G#
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:24): x => x <= local0
       |[2:18..2:19): x <= local2
       |[2:23..2:24): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of a generic trait",
    """|trait G[T] { def f(t: T): T }
       |object t70 {
       |  val g: G[Int] = x => x
       |}
       |""".stripMargin,
    """|_empty_/G# => trait G[T] extends AnyRef { +1 decls }
       |  T => _empty_/G#[T]
       |  AnyRef => scala/AnyRef#
       |_empty_/G#[T] => typeparam T
       |_empty_/G#f(). => abstract method f(t: T): T
       |  t => _empty_/G#f().(t)
       |  T => _empty_/G#[T]
       |_empty_/G#f().(t) => param t: T
       |  T => _empty_/G#[T]
       |_empty_/t70. => final object t70 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t70.g. => val method g: G[Int]
       |  G => _empty_/G#
       |  Int => scala/Int#
       |local0 => synthetic class $anon extends Object with G[Int] { +1 decls }
       |  Object => java/lang/Object#
       |  G => _empty_/G#
       |  Int => scala/Int#
       |local1 => synthetic method f(x: Int): Int <: _empty_/G#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of a Java interface",
    """|object t71 {
       |  val r: Runnable = () => ()
       |}
       |""".stripMargin,
    """|[0:7..0:10): t71 <= _empty_/t71.
       |[1:6..1:7): r <= _empty_/t71.r.
       |[1:9..1:17): Runnable => java/lang/Runnable#
       |[1:20..1:28): () => () <= local0
       |[1:20..1:28): () => () <= local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of a Java interface",
    """|object t71 {
       |  val r: Runnable = () => ()
       |}
       |""".stripMargin,
    """|[0:7..0:10): t71 <= _empty_/t71.
       |[1:6..1:7): r <= _empty_/t71.r.
       |[1:9..1:17): Runnable => java/lang/Runnable#
       |[1:20..1:28): () => () <= local0
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of a Java interface",
    """|object t71 {
       |  val r: Runnable = () => ()
       |}
       |""".stripMargin,
    """|_empty_/t71. => final object t71 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t71.r. => val method r: Runnable
       |  Runnable => java/lang/Runnable#
       |local0 => synthetic class $anon extends Object with Runnable { +1 decls }
       |  Object => java/lang/Object#
       |  Runnable => java/lang/Runnable#
       |local1 => synthetic method run(): Unit <: java/lang/Runnable#run().
       |  Unit => scala/Unit#
       |""".stripMargin,
  )

  occurrences(
    "lambda of a function type",
    """|object t72 {
       |  val h: Int => Int = x => x
       |}
       |""".stripMargin,
    """|[0:7..0:10): t72 <= _empty_/t72.
       |[1:6..1:7): h <= _empty_/t72.h.
       |[1:9..1:12): Int => scala/Int#
       |[1:16..1:19): Int => scala/Int#
       |[1:22..1:23): x <= local0
       |[1:27..1:28): x => local0
       |""".stripMargin,
  )

  symbols(
    "lambda of a function type",
    """|object t72 {
       |  val h: Int => Int = x => x
       |}
       |""".stripMargin,
    """|_empty_/t72. => final object t72 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t72.h. => val method h: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a qualifier",
    """|trait S { def f(a: Int): Int }
       |object K { def g(y: Int) = y }
       |object t74 {
       |  def use(s: S) = s.f(0)
       |  def m = use(K.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:8): K <= _empty_/K.
       |[1:15..1:16): g <= _empty_/K.g().
       |[1:17..1:18): y <= _empty_/K.g().(y)
       |[1:20..1:23): Int => scala/Int#
       |[1:27..1:28): y => _empty_/K.g().(y)
       |[2:7..2:10): t74 <= _empty_/t74.
       |[3:6..3:9): use <= _empty_/t74.use().
       |[3:10..3:11): s <= _empty_/t74.use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t74.use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t74.m().
       |[4:10..4:13): use => _empty_/t74.use().
       |[4:14..4:15): K => _empty_/K.
       |[4:14..4:17): K.g <= local0
       |[4:14..4:17): K.g <= local1
       |[4:16..4:17): g => _empty_/K.g().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a qualifier",
    """|trait S { def f(a: Int): Int }
       |object K { def g(y: Int) = y }
       |object t74 {
       |  def use(s: S) = s.f(0)
       |  def m = use(K.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:8): K <= _empty_/K.
       |[1:15..1:16): g <= _empty_/K.g().
       |[1:17..1:18): y <= _empty_/K.g().(y)
       |[1:20..1:23): Int => scala/Int#
       |[1:27..1:28): y => _empty_/K.g().(y)
       |[2:7..2:10): t74 <= _empty_/t74.
       |[3:6..3:9): use <= _empty_/t74.use().
       |[3:10..3:11): s <= _empty_/t74.use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t74.use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t74.m().
       |[4:10..4:13): use => _empty_/t74.use().
       |[4:14..4:17): K.g <= local0
       |[4:14..4:15): K => _empty_/K.
       |[4:16..4:17): g => _empty_/K.g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a qualifier",
    """|trait S { def f(a: Int): Int }
       |object K { def g(y: Int) = y }
       |object t74 {
       |  def use(s: S) = s.f(0)
       |  def m = use(K.g)
       |}
       |""".stripMargin,
    """|_empty_/K. => final object K extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/K.g(). => method g(y: Int): Int
       |  y => _empty_/K.g().(y)
       |  Int => scala/Int#
       |_empty_/K.g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t74. => final object t74 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t74.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t74.use(). => method use(s: S): Int
       |  s => _empty_/t74.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t74.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a type argument",
    """|trait S { def f(a: Int): Int }
       |object t80 {
       |  def use(s: S) = s.f(0)
       |  def id[A](a: A) = a
       |  def m = use(id[Int])
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t80 <= _empty_/t80.
       |[2:6..2:9): use <= _empty_/t80.use().
       |[2:10..2:11): s <= _empty_/t80.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t80.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:8): id <= _empty_/t80.id().
       |[3:9..3:10): A <= _empty_/t80.id().[A]
       |[3:12..3:13): a <= _empty_/t80.id().(a)
       |[3:15..3:16): A => _empty_/t80.id().[A]
       |[3:20..3:21): a => _empty_/t80.id().(a)
       |[4:6..4:7): m <= _empty_/t80.m().
       |[4:10..4:13): use => _empty_/t80.use().
       |[4:14..4:16): id => _empty_/t80.id().
       |[4:14..4:21): id[Int] <= local0
       |[4:14..4:21): id[Int] <= local1
       |[4:17..4:20): Int => scala/Int#
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a type argument",
    """|trait S { def f(a: Int): Int }
       |object t80 {
       |  def use(s: S) = s.f(0)
       |  def id[A](a: A) = a
       |  def m = use(id[Int])
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t80 <= _empty_/t80.
       |[2:6..2:9): use <= _empty_/t80.use().
       |[2:10..2:11): s <= _empty_/t80.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t80.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:8): id <= _empty_/t80.id().
       |[3:9..3:10): A <= _empty_/t80.id().[A]
       |[3:12..3:13): a <= _empty_/t80.id().(a)
       |[3:15..3:16): A => _empty_/t80.id().[A]
       |[3:20..3:21): a => _empty_/t80.id().(a)
       |[4:6..4:7): m <= _empty_/t80.m().
       |[4:10..4:13): use => _empty_/t80.use().
       |[4:14..4:21): id[Int] <= local0
       |[4:14..4:16): id => _empty_/t80.id().
       |[4:17..4:20): Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a type argument",
    """|trait S { def f(a: Int): Int }
       |object t80 {
       |  def use(s: S) = s.f(0)
       |  def id[A](a: A) = a
       |  def m = use(id[Int])
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t80. => final object t80 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t80.id(). => method id[A](a: A): A
       |  A => _empty_/t80.id().[A]
       |  a => _empty_/t80.id().(a)
       |_empty_/t80.id().(a) => param a: A
       |  A => _empty_/t80.id().[A]
       |_empty_/t80.id().[A] => typeparam A
       |_empty_/t80.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t80.use(). => method use(s: S): Int
       |  s => _empty_/t80.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t80.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(a: Int): Int <: _empty_/S#f().
       |  a => local2
       |  Int => scala/Int#
       |local2 => synthetic param a: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda whose body is a method value",
    """|trait S { def f(a: Int): Int }
       |trait R { def r(a: Int): S }
       |object t75 {
       |  def g(y: Int) = y
       |  val v: R = x => g
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): R <= _empty_/R#
       |[1:14..1:15): r <= _empty_/R#r().
       |[1:16..1:17): a <= _empty_/R#r().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): S => _empty_/S#
       |[2:7..2:10): t75 <= _empty_/t75.
       |[3:6..3:7): g <= _empty_/t75.g().
       |[3:8..3:9): y <= _empty_/t75.g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t75.g().(y)
       |[4:6..4:7): v <= _empty_/t75.v.
       |[4:9..4:10): R => _empty_/R#
       |[4:13..4:14): x <= local2
       |[4:13..4:19): x => g <= local0
       |[4:13..4:19): x => g <= local1
       |[4:18..4:19): g => _empty_/t75.g().
       |[4:18..4:19): g <= local3
       |[4:18..4:19): g <= local4
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda whose body is a method value",
    """|trait S { def f(a: Int): Int }
       |trait R { def r(a: Int): S }
       |object t75 {
       |  def g(y: Int) = y
       |  val v: R = x => g
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): R <= _empty_/R#
       |[1:14..1:15): r <= _empty_/R#r().
       |[1:16..1:17): a <= _empty_/R#r().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): S => _empty_/S#
       |[2:7..2:10): t75 <= _empty_/t75.
       |[3:6..3:7): g <= _empty_/t75.g().
       |[3:8..3:9): y <= _empty_/t75.g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t75.g().(y)
       |[4:6..4:7): v <= _empty_/t75.v.
       |[4:9..4:10): R => _empty_/R#
       |[4:13..4:19): x => g <= local0
       |[4:13..4:14): x <= local2
       |[4:18..4:19): g => _empty_/t75.g().
       |""".stripMargin,
  )

  symbols(
    "SAM lambda whose body is a method value",
    """|trait S { def f(a: Int): Int }
       |trait R { def r(a: Int): S }
       |object t75 {
       |  def g(y: Int) = y
       |  val v: R = x => g
       |}
       |""".stripMargin,
    """|_empty_/R# => trait R extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/R#r(). => abstract method r(a: Int): S
       |  a => _empty_/R#r().(a)
       |  Int => scala/Int#
       |  S => _empty_/S#
       |_empty_/R#r().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t75. => final object t75 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t75.g(). => method g(y: Int): Int
       |  y => _empty_/t75.g().(y)
       |  Int => scala/Int#
       |_empty_/t75.g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/t75.v. => val method v: R
       |  R => _empty_/R#
       |local0 => synthetic class $anon extends Object with R { +1 decls }
       |  Object => java/lang/Object#
       |  R => _empty_/R#
       |local1 => synthetic method r(x: Int): S <: _empty_/R#r().
       |  x => local2
       |  Int => scala/Int#
       |  S => _empty_/S#
       |local2 => param x: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local4 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local5
       |  Int => scala/Int#
       |local5 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "two SAM lambdas in one argument list",
    """|trait S { def f(a: Int): Int }
       |object t76 {
       |  def use(s: S, t: S) = s.f(0) + t.f(0)
       |  def m = use(x => x, y => y)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t76 <= _empty_/t76.
       |[2:6..2:9): use <= _empty_/t76.use().
       |[2:10..2:11): s <= _empty_/t76.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:16..2:17): t <= _empty_/t76.use().(t)
       |[2:19..2:20): S => _empty_/S#
       |[2:24..2:25): s => _empty_/t76.use().(s)
       |[2:26..2:27): f => _empty_/S#f().
       |[2:31..2:32): + => scala/Int#`+`(+4).
       |[2:33..2:34): t => _empty_/t76.use().(t)
       |[2:35..2:36): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t76.m().
       |[3:10..3:13): use => _empty_/t76.use().
       |[3:14..3:15): x <= local2
       |[3:14..3:20): x => x <= local0
       |[3:14..3:20): x => x <= local1
       |[3:19..3:20): x => local2
       |[3:22..3:23): y <= local5
       |[3:22..3:28): y => y <= local3
       |[3:22..3:28): y => y <= local4
       |[3:27..3:28): y => local5
       |""".stripMargin,
  )

  treeSymbols(
    "two SAM lambdas in one argument list",
    """|trait S { def f(a: Int): Int }
       |object t76 {
       |  def use(s: S, t: S) = s.f(0) + t.f(0)
       |  def m = use(x => x, y => y)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t76 <= _empty_/t76.
       |[2:6..2:9): use <= _empty_/t76.use().
       |[2:10..2:11): s <= _empty_/t76.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:16..2:17): t <= _empty_/t76.use().(t)
       |[2:19..2:20): S => _empty_/S#
       |[2:24..2:25): s => _empty_/t76.use().(s)
       |[2:26..2:27): f => _empty_/S#f().
       |[2:31..2:32): + => scala/Int#`+`(+4).
       |[2:33..2:34): t => _empty_/t76.use().(t)
       |[2:35..2:36): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t76.m().
       |[3:10..3:13): use => _empty_/t76.use().
       |[3:14..3:20): x => x <= local0
       |[3:14..3:15): x <= local2
       |[3:19..3:20): x => local2
       |[3:22..3:28): y => y <= local3
       |[3:22..3:23): y <= local5
       |[3:27..3:28): y => local5
       |""".stripMargin,
  )

  symbols(
    "two SAM lambdas in one argument list",
    """|trait S { def f(a: Int): Int }
       |object t76 {
       |  def use(s: S, t: S) = s.f(0) + t.f(0)
       |  def m = use(x => x, y => y)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t76. => final object t76 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t76.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t76.use(). => method use(s: S, t: S): Int
       |  s => _empty_/t76.use().(s)
       |  S => _empty_/S#
       |  t => _empty_/t76.use().(t)
       |  Int => scala/Int#
       |_empty_/t76.use().(s) => param s: S
       |  S => _empty_/S#
       |_empty_/t76.use().(t) => param t: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local4 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local5
       |  Int => scala/Int#
       |local5 => param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda with two parameters",
    """|trait P { def f(a: Int, b: String): Int }
       |object t77 {
       |  val p: P = (x, y) => x + y.length
       |}
       |""".stripMargin,
    """|[0:6..0:7): P <= _empty_/P#
       |[0:14..0:15): f <= _empty_/P#f().
       |[0:16..0:17): a <= _empty_/P#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:24..0:25): b <= _empty_/P#f().(b)
       |[0:27..0:33): String => scala/Predef.String#
       |[0:36..0:39): Int => scala/Int#
       |[1:7..1:10): t77 <= _empty_/t77.
       |[2:6..2:7): p <= _empty_/t77.p.
       |[2:9..2:10): P => _empty_/P#
       |[2:13..2:35): (x, y) => x + y.length <= local0
       |[2:13..2:35): (x, y) => x + y.length <= local1
       |[2:14..2:15): x <= local2
       |[2:17..2:18): y <= local3
       |[2:23..2:24): x => local2
       |[2:25..2:26): + => scala/Int#`+`(+4).
       |[2:27..2:28): y => local3
       |[2:29..2:35): length => java/lang/String#length().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda with two parameters",
    """|trait P { def f(a: Int, b: String): Int }
       |object t77 {
       |  val p: P = (x, y) => x + y.length
       |}
       |""".stripMargin,
    """|[0:6..0:7): P <= _empty_/P#
       |[0:14..0:15): f <= _empty_/P#f().
       |[0:16..0:17): a <= _empty_/P#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:24..0:25): b <= _empty_/P#f().(b)
       |[0:27..0:33): String => scala/Predef.String#
       |[0:36..0:39): Int => scala/Int#
       |[1:7..1:10): t77 <= _empty_/t77.
       |[2:6..2:7): p <= _empty_/t77.p.
       |[2:9..2:10): P => _empty_/P#
       |[2:13..2:35): (x, y) => x + y.length <= local0
       |[2:14..2:15): x <= local2
       |[2:17..2:18): y <= local3
       |[2:23..2:24): x => local2
       |[2:25..2:26): + => scala/Int#`+`(+4).
       |[2:27..2:28): y => local3
       |[2:29..2:35): length => java/lang/String#length().
       |""".stripMargin,
  )

  symbols(
    "SAM lambda with two parameters",
    """|trait P { def f(a: Int, b: String): Int }
       |object t77 {
       |  val p: P = (x, y) => x + y.length
       |}
       |""".stripMargin,
    """|_empty_/P# => trait P extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/P#f(). => abstract method f(a: Int, b: String): Int
       |  a => _empty_/P#f().(a)
       |  Int => scala/Int#
       |  b => _empty_/P#f().(b)
       |  String => scala/Predef.String#
       |_empty_/P#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/P#f().(b) => param b: String
       |  String => scala/Predef.String#
       |_empty_/t77. => final object t77 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t77.p. => val method p: P
       |  P => _empty_/P#
       |local0 => synthetic class $anon extends Object with P { +1 decls }
       |  Object => java/lang/Object#
       |  P => _empty_/P#
       |local1 => synthetic method f(x: Int, y: String): Int <: _empty_/P#f().
       |  x => local2
       |  Int => scala/Int#
       |  y => local3
       |  String => scala/Predef.String#
       |local2 => param x: Int
       |  Int => scala/Int#
       |local3 => param y: String
       |  String => scala/Predef.String#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda with a wider parameter type",
    """|trait S { def f(a: Int): Int }
       |object t78 {
       |  val s: S = (x: Any) => 0
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t78 <= _empty_/t78.
       |[2:6..2:7): s <= _empty_/t78.s.
       |[2:9..2:10): S => _empty_/S#
       |[2:13..2:26): (x: Any) => 0 <= local0
       |[2:13..2:26): (x: Any) => 0 <= local1
       |[2:14..2:15): x <= local2
       |[2:17..2:20): Any => scala/Any#
       |""".stripMargin,
  )

  symbols(
    "SAM lambda with a wider parameter type",
    """|trait S { def f(a: Int): Int }
       |object t78 {
       |  val s: S = (x: Any) => 0
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t78. => final object t78 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t78.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Any): Int <: _empty_/S#f().
       |  x => local2
       |  Any => scala/Any#
       |  Int => scala/Int#
       |local2 => param x: Any
       |  Any => scala/Any#
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda with a wider parameter type",
    """|trait S { def f(a: Int): Int }
       |object t78 {
       |  val s: S = (x: Any) => 0
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t78 <= _empty_/t78.
       |[2:6..2:7): s <= _empty_/t78.s.
       |[2:9..2:10): S => _empty_/S#
       |[2:13..2:26): (x: Any) => 0 <= local0
       |[2:14..2:15): x <= local2
       |[2:17..2:20): Any => scala/Any#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value of a generic trait",
    """|trait G[T] { def f(t: T): T }
       |object t79 {
       |  def h(s: String) = s
       |  val g: G[String] = h
       |}
       |""".stripMargin,
    """|[0:6..0:7): G <= _empty_/G#
       |[0:8..0:9): T <= _empty_/G#[T]
       |[0:17..0:18): f <= _empty_/G#f().
       |[0:19..0:20): t <= _empty_/G#f().(t)
       |[0:22..0:23): T => _empty_/G#[T]
       |[0:26..0:27): T => _empty_/G#[T]
       |[1:7..1:10): t79 <= _empty_/t79.
       |[2:6..2:7): h <= _empty_/t79.h().
       |[2:8..2:9): s <= _empty_/t79.h().(s)
       |[2:11..2:17): String => scala/Predef.String#
       |[2:21..2:22): s => _empty_/t79.h().(s)
       |[3:6..3:7): g <= _empty_/t79.g.
       |[3:9..3:10): G => _empty_/G#
       |[3:11..3:17): String => scala/Predef.String#
       |[3:21..3:22): h => _empty_/t79.h().
       |[3:21..3:22): h <= local0
       |[3:21..3:22): h <= local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value of a generic trait",
    """|trait G[T] { def f(t: T): T }
       |object t79 {
       |  def h(s: String) = s
       |  val g: G[String] = h
       |}
       |""".stripMargin,
    """|[0:6..0:7): G <= _empty_/G#
       |[0:8..0:9): T <= _empty_/G#[T]
       |[0:17..0:18): f <= _empty_/G#f().
       |[0:19..0:20): t <= _empty_/G#f().(t)
       |[0:22..0:23): T => _empty_/G#[T]
       |[0:26..0:27): T => _empty_/G#[T]
       |[1:7..1:10): t79 <= _empty_/t79.
       |[2:6..2:7): h <= _empty_/t79.h().
       |[2:8..2:9): s <= _empty_/t79.h().(s)
       |[2:11..2:17): String => scala/Predef.String#
       |[2:21..2:22): s => _empty_/t79.h().(s)
       |[3:6..3:7): g <= _empty_/t79.g.
       |[3:9..3:10): G => _empty_/G#
       |[3:11..3:17): String => scala/Predef.String#
       |[3:21..3:22): h => _empty_/t79.h().
       |""".stripMargin,
  )

  symbols(
    "SAM method value of a generic trait",
    """|trait G[T] { def f(t: T): T }
       |object t79 {
       |  def h(s: String) = s
       |  val g: G[String] = h
       |}
       |""".stripMargin,
    """|_empty_/G# => trait G[T] extends AnyRef { +1 decls }
       |  T => _empty_/G#[T]
       |  AnyRef => scala/AnyRef#
       |_empty_/G#[T] => typeparam T
       |_empty_/G#f(). => abstract method f(t: T): T
       |  t => _empty_/G#f().(t)
       |  T => _empty_/G#[T]
       |_empty_/G#f().(t) => param t: T
       |  T => _empty_/G#[T]
       |_empty_/t79. => final object t79 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t79.g. => val method g: G[String]
       |  G => _empty_/G#
       |  String => scala/Predef.String#
       |_empty_/t79.h(). => method h(s: String): String
       |  s => _empty_/t79.h().(s)
       |  String => scala/Predef.String#
       |_empty_/t79.h().(s) => param s: String
       |  String => scala/Predef.String#
       |local0 => synthetic class $anon extends Object with G[String] { +1 decls }
       |  Object => java/lang/Object#
       |  G => _empty_/G#
       |  String => scala/Predef.String#
       |local1 => synthetic method f(s: String): String <: _empty_/G#f().
       |  s => local2
       |  String => scala/Predef.String#
       |local2 => synthetic param s: String
       |  String => scala/Predef.String#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t81 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(this.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:9): t81 <= _empty_/t81#
       |[1:10..1:10):  <= _empty_/t81#`<init>`().
       |[2:6..2:9): use <= _empty_/t81#use().
       |[2:10..2:11): s <= _empty_/t81#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t81#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t81#g().
       |[3:8..3:9): y <= _empty_/t81#g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t81#g().(y)
       |[4:6..4:7): m <= _empty_/t81#m().
       |[4:10..4:13): use => _empty_/t81#use().
       |[4:14..4:20): this.g <= local0
       |[4:14..4:20): this.g <= local1
       |[4:19..4:20): g => _empty_/t81#g().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t81 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(this.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:9): t81 <= _empty_/t81#
       |[1:10..1:10):  <= _empty_/t81#`<init>`().
       |[2:6..2:9): use <= _empty_/t81#use().
       |[2:10..2:11): s <= _empty_/t81#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t81#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t81#g().
       |[3:8..3:9): y <= _empty_/t81#g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t81#g().(y)
       |[4:6..4:7): m <= _empty_/t81#m().
       |[4:10..4:13): use => _empty_/t81#use().
       |[4:14..4:20): this.g <= local0
       |[4:19..4:20): g => _empty_/t81#g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t81 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(this.g)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t81# => class t81 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t81#`<init>`(). => primary ctor <init>()
       |_empty_/t81#g(). => method g(y: Int): Int
       |  y => _empty_/t81#g().(y)
       |  Int => scala/Int#
       |_empty_/t81#g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/t81#m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t81#use(). => method use(s: S): Int
       |  s => _empty_/t81#use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t81#use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  names(
    "SAM method value with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t81 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(this.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:8..0:8):
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:9): t81 <= _empty_/t81#
       |[1:10..1:10):  <= _empty_/t81#`<init>`().
       |[2:6..2:9): use <= _empty_/t81#use().
       |[2:10..2:11): s <= _empty_/t81#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t81#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t81#g().
       |[3:8..3:9): y <= _empty_/t81#g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t81#g().(y)
       |[4:6..4:7): m <= _empty_/t81#m().
       |[4:10..4:13): use => _empty_/t81#use().
       |[4:14..4:14):
       |[4:19..4:20): g => _empty_/t81#g().
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a qualified this",
    """|trait S { def f(a: Int): Int }
       |class t98 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(t98.this.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:9): t98 <= _empty_/t98#
       |[1:10..1:10):  <= _empty_/t98#`<init>`().
       |[2:6..2:9): use <= _empty_/t98#use().
       |[2:10..2:11): s <= _empty_/t98#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t98#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t98#g().
       |[3:8..3:9): y <= _empty_/t98#g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t98#g().(y)
       |[4:6..4:7): m <= _empty_/t98#m().
       |[4:10..4:13): use => _empty_/t98#use().
       |[4:14..4:17): t98 => _empty_/t98#
       |[4:14..4:24): t98.this.g <= local0
       |[4:14..4:24): t98.this.g <= local1
       |[4:23..4:24): g => _empty_/t98#g().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a qualified this",
    """|trait S { def f(a: Int): Int }
       |class t98 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(t98.this.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:9): t98 <= _empty_/t98#
       |[1:10..1:10):  <= _empty_/t98#`<init>`().
       |[2:6..2:9): use <= _empty_/t98#use().
       |[2:10..2:11): s <= _empty_/t98#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t98#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t98#g().
       |[3:8..3:9): y <= _empty_/t98#g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t98#g().(y)
       |[4:6..4:7): m <= _empty_/t98#m().
       |[4:10..4:13): use => _empty_/t98#use().
       |[4:14..4:24): t98.this.g <= local0
       |[4:14..4:17): t98 => _empty_/t98#
       |[4:23..4:24): g => _empty_/t98#g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a qualified this",
    """|trait S { def f(a: Int): Int }
       |class t98 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(t98.this.g)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t98# => class t98 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t98#`<init>`(). => primary ctor <init>()
       |_empty_/t98#g(). => method g(y: Int): Int
       |  y => _empty_/t98#g().(y)
       |  Int => scala/Int#
       |_empty_/t98#g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/t98#m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t98#use(). => method use(s: S): Int
       |  s => _empty_/t98#use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t98#use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a super qualifier",
    """|trait S { def f(a: Int): Int }
       |class B { def g(y: Int) = y }
       |class t82 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(super.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): B <= _empty_/B#
       |[1:8..1:8):  <= _empty_/B#`<init>`().
       |[1:14..1:15): g <= _empty_/B#g().
       |[1:16..1:17): y <= _empty_/B#g().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/B#g().(y)
       |[2:6..2:9): t82 <= _empty_/t82#
       |[2:10..2:10):  <= _empty_/t82#`<init>`().
       |[2:18..2:19): B => _empty_/B#
       |[2:19..2:19):  => _empty_/B#`<init>`().
       |[3:6..3:9): use <= _empty_/t82#use().
       |[3:10..3:11): s <= _empty_/t82#use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t82#use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t82#m().
       |[4:10..4:13): use => _empty_/t82#use().
       |[4:14..4:14):  => _empty_/t82#
       |[4:14..4:21): super.g <= local0
       |[4:14..4:21): super.g <= local1
       |[4:19..4:19):  => _empty_/B#
       |[4:20..4:21): g => _empty_/B#g().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a super qualifier",
    """|trait S { def f(a: Int): Int }
       |class B { def g(y: Int) = y }
       |class t82 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(super.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): B <= _empty_/B#
       |[1:8..1:8):  <= _empty_/B#`<init>`().
       |[1:14..1:15): g <= _empty_/B#g().
       |[1:16..1:17): y <= _empty_/B#g().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/B#g().(y)
       |[2:6..2:9): t82 <= _empty_/t82#
       |[2:10..2:10):  <= _empty_/t82#`<init>`().
       |[2:18..2:19): B => _empty_/B#
       |[2:19..2:19):  => _empty_/B#`<init>`().
       |[3:6..3:9): use <= _empty_/t82#use().
       |[3:10..3:11): s <= _empty_/t82#use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t82#use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t82#m().
       |[4:10..4:13): use => _empty_/t82#use().
       |[4:14..4:21): super.g <= local0
       |[4:14..4:14):  => _empty_/t82#
       |[4:19..4:19):  => _empty_/B#
       |[4:20..4:21): g => _empty_/B#g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a super qualifier",
    """|trait S { def f(a: Int): Int }
       |class B { def g(y: Int) = y }
       |class t82 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(super.g)
       |}
       |""".stripMargin,
    """|_empty_/B# => class B extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/B#`<init>`(). => primary ctor <init>()
       |_empty_/B#g(). => method g(y: Int): Int
       |  y => _empty_/B#g().(y)
       |  Int => scala/Int#
       |_empty_/B#g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t82# => class t82 extends B { +3 decls }
       |  B => _empty_/B#
       |_empty_/t82#`<init>`(). => primary ctor <init>()
       |_empty_/t82#m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t82#use(). => method use(s: S): Int
       |  s => _empty_/t82#use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t82#use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  names(
    "SAM method value with a super qualifier",
    """|trait S { def f(a: Int): Int }
       |class B { def g(y: Int) = y }
       |class t82 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(super.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:8..0:8):
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): B <= _empty_/B#
       |[1:8..1:8):  <= _empty_/B#`<init>`().
       |[1:14..1:15): g <= _empty_/B#g().
       |[1:16..1:17): y <= _empty_/B#g().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/B#g().(y)
       |[2:6..2:9): t82 <= _empty_/t82#
       |[2:10..2:10):  <= _empty_/t82#`<init>`().
       |[2:18..2:19): B => _empty_/B#
       |[2:19..2:19):  => _empty_/B#`<init>`().
       |[3:6..3:9): use <= _empty_/t82#use().
       |[3:10..3:11): s <= _empty_/t82#use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t82#use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t82#m().
       |[4:10..4:13): use => _empty_/t82#use().
       |[4:14..4:14):  => _empty_/t82#
       |[4:19..4:19):  => _empty_/B#
       |[4:20..4:21): g => _empty_/B#g().
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a qualified super",
    """|trait S { def f(a: Int): Int }
       |class B { def g(y: Int) = y }
       |class t99 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(t99.super.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): B <= _empty_/B#
       |[1:8..1:8):  <= _empty_/B#`<init>`().
       |[1:14..1:15): g <= _empty_/B#g().
       |[1:16..1:17): y <= _empty_/B#g().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/B#g().(y)
       |[2:6..2:9): t99 <= _empty_/t99#
       |[2:10..2:10):  <= _empty_/t99#`<init>`().
       |[2:18..2:19): B => _empty_/B#
       |[2:19..2:19):  => _empty_/B#`<init>`().
       |[3:6..3:9): use <= _empty_/t99#use().
       |[3:10..3:11): s <= _empty_/t99#use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t99#use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t99#m().
       |[4:10..4:13): use => _empty_/t99#use().
       |[4:14..4:17): t99 => _empty_/t99#
       |[4:14..4:25): t99.super.g <= local0
       |[4:14..4:25): t99.super.g <= local1
       |[4:23..4:23):  => _empty_/B#
       |[4:24..4:25): g => _empty_/B#g().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a qualified super",
    """|trait S { def f(a: Int): Int }
       |class B { def g(y: Int) = y }
       |class t99 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(t99.super.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): B <= _empty_/B#
       |[1:8..1:8):  <= _empty_/B#`<init>`().
       |[1:14..1:15): g <= _empty_/B#g().
       |[1:16..1:17): y <= _empty_/B#g().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/B#g().(y)
       |[2:6..2:9): t99 <= _empty_/t99#
       |[2:10..2:10):  <= _empty_/t99#`<init>`().
       |[2:18..2:19): B => _empty_/B#
       |[2:19..2:19):  => _empty_/B#`<init>`().
       |[3:6..3:9): use <= _empty_/t99#use().
       |[3:10..3:11): s <= _empty_/t99#use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t99#use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t99#m().
       |[4:10..4:13): use => _empty_/t99#use().
       |[4:14..4:25): t99.super.g <= local0
       |[4:14..4:17): t99 => _empty_/t99#
       |[4:23..4:23):  => _empty_/B#
       |[4:24..4:25): g => _empty_/B#g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a qualified super",
    """|trait S { def f(a: Int): Int }
       |class B { def g(y: Int) = y }
       |class t99 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(t99.super.g)
       |}
       |""".stripMargin,
    """|_empty_/B# => class B extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/B#`<init>`(). => primary ctor <init>()
       |_empty_/B#g(). => method g(y: Int): Int
       |  y => _empty_/B#g().(y)
       |  Int => scala/Int#
       |_empty_/B#g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t99# => class t99 extends B { +3 decls }
       |  B => _empty_/B#
       |_empty_/t99#`<init>`(). => primary ctor <init>()
       |_empty_/t99#m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t99#use(). => method use(s: S): Int
       |  s => _empty_/t99#use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t99#use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a new qualifier",
    """|trait S { def f(a: Int): Int }
       |class K { def g(y: Int) = y }
       |object t83 {
       |  def use(s: S) = s.f(0)
       |  def m = use(new K().g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): K <= _empty_/K#
       |[1:8..1:8):  <= _empty_/K#`<init>`().
       |[1:14..1:15): g <= _empty_/K#g().
       |[1:16..1:17): y <= _empty_/K#g().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/K#g().(y)
       |[2:7..2:10): t83 <= _empty_/t83.
       |[3:6..3:9): use <= _empty_/t83.use().
       |[3:10..3:11): s <= _empty_/t83.use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t83.use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t83.m().
       |[4:10..4:13): use => _empty_/t83.use().
       |[4:14..4:23): new K().g <= local0
       |[4:14..4:23): new K().g <= local1
       |[4:18..4:19): K => _empty_/K#
       |[4:19..4:19):  => _empty_/K#`<init>`().
       |[4:22..4:23): g => _empty_/K#g().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a new qualifier",
    """|trait S { def f(a: Int): Int }
       |class K { def g(y: Int) = y }
       |object t83 {
       |  def use(s: S) = s.f(0)
       |  def m = use(new K().g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): K <= _empty_/K#
       |[1:8..1:8):  <= _empty_/K#`<init>`().
       |[1:14..1:15): g <= _empty_/K#g().
       |[1:16..1:17): y <= _empty_/K#g().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/K#g().(y)
       |[2:7..2:10): t83 <= _empty_/t83.
       |[3:6..3:9): use <= _empty_/t83.use().
       |[3:10..3:11): s <= _empty_/t83.use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t83.use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t83.m().
       |[4:10..4:13): use => _empty_/t83.use().
       |[4:14..4:23): new K().g <= local0
       |[4:18..4:19): K => _empty_/K#
       |[4:19..4:19):  => _empty_/K#`<init>`().
       |[4:22..4:23): g => _empty_/K#g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a new qualifier",
    """|trait S { def f(a: Int): Int }
       |class K { def g(y: Int) = y }
       |object t83 {
       |  def use(s: S) = s.f(0)
       |  def m = use(new K().g)
       |}
       |""".stripMargin,
    """|_empty_/K# => class K extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/K#`<init>`(). => primary ctor <init>()
       |_empty_/K#g(). => method g(y: Int): Int
       |  y => _empty_/K#g().(y)
       |  Int => scala/Int#
       |_empty_/K#g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t83. => final object t83 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t83.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t83.use(). => method use(s: S): Int
       |  s => _empty_/t83.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t83.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a parenthesized qualifier",
    """|trait S { def f(a: Int): Int }
       |class K { def g(y: Int) = y }
       |object t84 {
       |  def use(s: S) = s.f(0)
       |  val k = new K
       |  def m = use((k).g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): K <= _empty_/K#
       |[1:8..1:8):  <= _empty_/K#`<init>`().
       |[1:14..1:15): g <= _empty_/K#g().
       |[1:16..1:17): y <= _empty_/K#g().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/K#g().(y)
       |[2:7..2:10): t84 <= _empty_/t84.
       |[3:6..3:9): use <= _empty_/t84.use().
       |[3:10..3:11): s <= _empty_/t84.use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t84.use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): k <= _empty_/t84.k.
       |[4:14..4:15): K => _empty_/K#
       |[4:15..4:15):  => _empty_/K#`<init>`().
       |[5:6..5:7): m <= _empty_/t84.m().
       |[5:10..5:13): use => _empty_/t84.use().
       |[5:14..5:19): (k).g <= local0
       |[5:14..5:19): (k).g <= local1
       |[5:15..5:16): k => _empty_/t84.k.
       |[5:18..5:19): g => _empty_/K#g().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a parenthesized qualifier",
    """|trait S { def f(a: Int): Int }
       |class K { def g(y: Int) = y }
       |object t84 {
       |  def use(s: S) = s.f(0)
       |  val k = new K
       |  def m = use((k).g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): K <= _empty_/K#
       |[1:8..1:8):  <= _empty_/K#`<init>`().
       |[1:14..1:15): g <= _empty_/K#g().
       |[1:16..1:17): y <= _empty_/K#g().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/K#g().(y)
       |[2:7..2:10): t84 <= _empty_/t84.
       |[3:6..3:9): use <= _empty_/t84.use().
       |[3:10..3:11): s <= _empty_/t84.use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t84.use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): k <= _empty_/t84.k.
       |[4:14..4:15): K => _empty_/K#
       |[4:15..4:15):  => _empty_/K#`<init>`().
       |[5:6..5:7): m <= _empty_/t84.m().
       |[5:10..5:13): use => _empty_/t84.use().
       |[5:14..5:19): (k).g <= local0
       |[5:15..5:16): k => _empty_/t84.k.
       |[5:18..5:19): g => _empty_/K#g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a parenthesized qualifier",
    """|trait S { def f(a: Int): Int }
       |class K { def g(y: Int) = y }
       |object t84 {
       |  def use(s: S) = s.f(0)
       |  val k = new K
       |  def m = use((k).g)
       |}
       |""".stripMargin,
    """|_empty_/K# => class K extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/K#`<init>`(). => primary ctor <init>()
       |_empty_/K#g(). => method g(y: Int): Int
       |  y => _empty_/K#g().(y)
       |  Int => scala/Int#
       |_empty_/K#g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t84. => final object t84 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t84.k. => val method k: K
       |  K => _empty_/K#
       |_empty_/t84.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t84.use(). => method use(s: S): Int
       |  s => _empty_/t84.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t84.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a literal qualifier",
    """|trait C { def f(a: Int): Char }
       |object t85 {
       |  val c: C = "abc".charAt
       |}
       |""".stripMargin,
    """|[0:6..0:7): C <= _empty_/C#
       |[0:14..0:15): f <= _empty_/C#f().
       |[0:16..0:17): a <= _empty_/C#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:29): Char => scala/Char#
       |[1:7..1:10): t85 <= _empty_/t85.
       |[2:6..2:7): c <= _empty_/t85.c.
       |[2:9..2:10): C => _empty_/C#
       |[2:13..2:25): "abc".charAt <= local0
       |[2:13..2:25): "abc".charAt <= local1
       |[2:19..2:25): charAt => java/lang/String#charAt().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a literal qualifier",
    """|trait C { def f(a: Int): Char }
       |object t85 {
       |  val c: C = "abc".charAt
       |}
       |""".stripMargin,
    """|[0:6..0:7): C <= _empty_/C#
       |[0:14..0:15): f <= _empty_/C#f().
       |[0:16..0:17): a <= _empty_/C#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:29): Char => scala/Char#
       |[1:7..1:10): t85 <= _empty_/t85.
       |[2:6..2:7): c <= _empty_/t85.c.
       |[2:9..2:10): C => _empty_/C#
       |[2:13..2:25): "abc".charAt <= local0
       |[2:19..2:25): charAt => java/lang/String#charAt().
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a literal qualifier",
    """|trait C { def f(a: Int): Char }
       |object t85 {
       |  val c: C = "abc".charAt
       |}
       |""".stripMargin,
    """|_empty_/C# => trait C extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/C#f(). => abstract method f(a: Int): Char
       |  a => _empty_/C#f().(a)
       |  Int => scala/Int#
       |  Char => scala/Char#
       |_empty_/C#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t85. => final object t85 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t85.c. => val method c: C
       |  C => _empty_/C#
       |local0 => synthetic class $anon extends Object with C { +1 decls }
       |  Object => java/lang/Object#
       |  C => _empty_/C#
       |local1 => synthetic method f(x$1: Int): Char <: _empty_/C#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |  Char => scala/Char#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "parenthesized SAM method value",
    """|trait S { def f(a: Int): Int }
       |object t86 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use((g))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t86 <= _empty_/t86.
       |[2:6..2:9): use <= _empty_/t86.use().
       |[2:10..2:11): s <= _empty_/t86.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t86.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t86.g().
       |[3:8..3:9): y <= _empty_/t86.g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t86.g().(y)
       |[4:6..4:7): m <= _empty_/t86.m().
       |[4:10..4:13): use => _empty_/t86.use().
       |[4:15..4:16): g => _empty_/t86.g().
       |[4:15..4:16): g <= local0
       |[4:15..4:16): g <= local1
       |""".stripMargin,
  )

  treeSymbols(
    "parenthesized SAM method value",
    """|trait S { def f(a: Int): Int }
       |object t86 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use((g))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t86 <= _empty_/t86.
       |[2:6..2:9): use <= _empty_/t86.use().
       |[2:10..2:11): s <= _empty_/t86.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t86.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t86.g().
       |[3:8..3:9): y <= _empty_/t86.g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t86.g().(y)
       |[4:6..4:7): m <= _empty_/t86.m().
       |[4:10..4:13): use => _empty_/t86.use().
       |[4:15..4:16): g => _empty_/t86.g().
       |""".stripMargin,
  )

  symbols(
    "parenthesized SAM method value",
    """|trait S { def f(a: Int): Int }
       |object t86 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use((g))
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t86. => final object t86 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t86.g(). => method g(y: Int): Int
       |  y => _empty_/t86.g().(y)
       |  Int => scala/Int#
       |_empty_/t86.g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/t86.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t86.use(). => method use(s: S): Int
       |  s => _empty_/t86.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t86.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM eta expansion with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t87 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(this.g _)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:9): t87 <= _empty_/t87#
       |[1:10..1:10):  <= _empty_/t87#`<init>`().
       |[2:6..2:9): use <= _empty_/t87#use().
       |[2:10..2:11): s <= _empty_/t87#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t87#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t87#g().
       |[3:8..3:9): y <= _empty_/t87#g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t87#g().(y)
       |[4:6..4:7): m <= _empty_/t87#m().
       |[4:10..4:13): use => _empty_/t87#use().
       |[4:14..4:20): this.g <= local0
       |[4:14..4:20): this.g <= local1
       |[4:19..4:20): g => _empty_/t87#g().
       |""".stripMargin,
  )

  symbols(
    "SAM eta expansion with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t87 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(this.g _)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t87# => class t87 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t87#`<init>`(). => primary ctor <init>()
       |_empty_/t87#g(). => method g(y: Int): Int
       |  y => _empty_/t87#g().(y)
       |  Int => scala/Int#
       |_empty_/t87#g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/t87#m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t87#use(). => method use(s: S): Int
       |  s => _empty_/t87#use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t87#use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  names(
    "SAM eta expansion with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t87 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(this.g _)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:8..0:8):
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:9): t87 <= _empty_/t87#
       |[1:10..1:10):  <= _empty_/t87#`<init>`().
       |[2:6..2:9): use <= _empty_/t87#use().
       |[2:10..2:11): s <= _empty_/t87#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t87#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t87#g().
       |[3:8..3:9): y <= _empty_/t87#g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t87#g().(y)
       |[4:6..4:7): m <= _empty_/t87#m().
       |[4:10..4:13): use => _empty_/t87#use().
       |[4:14..4:14):
       |[4:19..4:20): g => _empty_/t87#g().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM eta expansion with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t87 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(this.g _)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:9): t87 <= _empty_/t87#
       |[1:10..1:10):  <= _empty_/t87#`<init>`().
       |[2:6..2:9): use <= _empty_/t87#use().
       |[2:10..2:11): s <= _empty_/t87#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t87#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t87#g().
       |[3:8..3:9): y <= _empty_/t87#g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t87#g().(y)
       |[4:6..4:7): m <= _empty_/t87#m().
       |[4:10..4:13): use => _empty_/t87#use().
       |[4:14..4:20): this.g <= local0
       |[4:19..4:20): g => _empty_/t87#g().
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a wider parameter type",
    """|trait S { def f(a: Int): Int }
       |object t95 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Any) = 0
       |  def m = use(g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t95 <= _empty_/t95.
       |[2:6..2:9): use <= _empty_/t95.use().
       |[2:10..2:11): s <= _empty_/t95.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t95.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t95.g().
       |[3:8..3:9): y <= _empty_/t95.g().(y)
       |[3:11..3:14): Any => scala/Any#
       |[4:6..4:7): m <= _empty_/t95.m().
       |[4:10..4:13): use => _empty_/t95.use().
       |[4:14..4:15): g => _empty_/t95.g().
       |[4:14..4:15): g <= local0
       |[4:14..4:15): g <= local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a wider parameter type",
    """|trait S { def f(a: Int): Int }
       |object t95 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Any) = 0
       |  def m = use(g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t95 <= _empty_/t95.
       |[2:6..2:9): use <= _empty_/t95.use().
       |[2:10..2:11): s <= _empty_/t95.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t95.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t95.g().
       |[3:8..3:9): y <= _empty_/t95.g().(y)
       |[3:11..3:14): Any => scala/Any#
       |[4:6..4:7): m <= _empty_/t95.m().
       |[4:10..4:13): use => _empty_/t95.use().
       |[4:14..4:15): g => _empty_/t95.g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a wider parameter type",
    """|trait S { def f(a: Int): Int }
       |object t95 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Any) = 0
       |  def m = use(g)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t95. => final object t95 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t95.g(). => method g(y: Any): Int
       |  y => _empty_/t95.g().(y)
       |  Any => scala/Any#
       |  Int => scala/Int#
       |_empty_/t95.g().(y) => param y: Any
       |  Any => scala/Any#
       |_empty_/t95.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t95.use(). => method use(s: S): Int
       |  s => _empty_/t95.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t95.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Any): Int <: _empty_/S#f().
       |  y => local2
       |  Any => scala/Any#
       |  Int => scala/Int#
       |local2 => synthetic param y: Any
       |  Any => scala/Any#
       |""".stripMargin,
  )

  occurrences(
    "SAM pattern-matching anonymous function",
    """|trait S { def f(a: Int): Int }
       |object t88 {
       |  val s: S = { case 0 => 1; case x => x }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t88 <= _empty_/t88.
       |[2:6..2:7): s <= _empty_/t88.s.
       |[2:9..2:10): S => _empty_/S#
       |[2:13..2:41): { case 0 => 1; case x => x } <= local0
       |[2:13..2:41): { case 0 => 1; case x => x } <= local1
       |[2:33..2:34): x <= local3
       |[2:38..2:39): x => local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM pattern-matching anonymous function",
    """|trait S { def f(a: Int): Int }
       |object t88 {
       |  val s: S = { case 0 => 1; case x => x }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t88 <= _empty_/t88.
       |[2:6..2:7): s <= _empty_/t88.s.
       |[2:9..2:10): S => _empty_/S#
       |[2:13..2:41): { case 0 => 1; case x => x } <= local0
       |[2:33..2:34): x <= local3
       |[2:38..2:39): x => local3
       |""".stripMargin,
  )

  symbols(
    "SAM pattern-matching anonymous function",
    """|trait S { def f(a: Int): Int }
       |object t88 {
       |  val s: S = { case 0 => 1; case x => x }
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t88. => final object t88 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t88.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x0$1: Int): Int <: _empty_/S#f().
       |  x0$1 => local2
       |  Int => scala/Int#
       |local2 => synthetic param x0$1: Int
       |  Int => scala/Int#
       |local3 => val local x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM pattern-matching anonymous function as an argument",
    """|trait S { def f(a: Int): Int }
       |object t89 {
       |  def use(s: S) = s.f(0)
       |  def m = use { case 0 => 1; case x => x }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t89 <= _empty_/t89.
       |[2:6..2:9): use <= _empty_/t89.use().
       |[2:10..2:11): s <= _empty_/t89.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t89.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t89.m().
       |[3:10..3:13): use => _empty_/t89.use().
       |[3:14..3:42): { case 0 => 1; case x => x } <= local0
       |[3:14..3:42): { case 0 => 1; case x => x } <= local1
       |[3:34..3:35): x <= local3
       |[3:39..3:40): x => local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM pattern-matching anonymous function as an argument",
    """|trait S { def f(a: Int): Int }
       |object t89 {
       |  def use(s: S) = s.f(0)
       |  def m = use { case 0 => 1; case x => x }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t89 <= _empty_/t89.
       |[2:6..2:9): use <= _empty_/t89.use().
       |[2:10..2:11): s <= _empty_/t89.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t89.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t89.m().
       |[3:10..3:13): use => _empty_/t89.use().
       |[3:14..3:42): { case 0 => 1; case x => x } <= local0
       |[3:34..3:35): x <= local3
       |[3:39..3:40): x => local3
       |""".stripMargin,
  )

  symbols(
    "SAM pattern-matching anonymous function as an argument",
    """|trait S { def f(a: Int): Int }
       |object t89 {
       |  def use(s: S) = s.f(0)
       |  def m = use { case 0 => 1; case x => x }
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t89. => final object t89 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t89.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t89.use(). => method use(s: S): Int
       |  s => _empty_/t89.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t89.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x0$1: Int): Int <: _empty_/S#f().
       |  x0$1 => local2
       |  Int => scala/Int#
       |local2 => synthetic param x0$1: Int
       |  Int => scala/Int#
       |local3 => val local x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value of a curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t90 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = h
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:10): t90 <= _empty_/t90.
       |[3:6..3:7): h <= _empty_/t90.h().
       |[3:8..3:9): a <= _empty_/t90.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t90.h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t90.h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t90.h().(b)
       |[4:6..4:7): s <= _empty_/t90.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): h => _empty_/t90.h().
       |[4:13..4:14): h <= local0
       |[4:13..4:14): h <= local1
       |[4:13..4:14): h <= local3
       |[4:13..4:14): h <= local4
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value of a curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t90 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = h
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:10): t90 <= _empty_/t90.
       |[3:6..3:7): h <= _empty_/t90.h().
       |[3:8..3:9): a <= _empty_/t90.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t90.h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t90.h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t90.h().(b)
       |[4:6..4:7): s <= _empty_/t90.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): h => _empty_/t90.h().
       |""".stripMargin,
  )

  symbols(
    "SAM method value of a curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t90 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = h
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t90. => final object t90 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t90.h(). => method h(a: Int)(b: Int): Int
       |  a => _empty_/t90.h().(a)
       |  Int => scala/Int#
       |  b => _empty_/t90.h().(b)
       |_empty_/t90.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t90.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t90.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(a: Int): T <: _empty_/S#f().
       |  a => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param a: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM eta expansion of a curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t91 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = h _
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:10): t91 <= _empty_/t91.
       |[3:6..3:7): h <= _empty_/t91.h().
       |[3:8..3:9): a <= _empty_/t91.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t91.h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t91.h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t91.h().(b)
       |[4:6..4:7): s <= _empty_/t91.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): h => _empty_/t91.h().
       |[4:13..4:14): h <= local0
       |[4:13..4:14): h <= local1
       |[4:13..4:14): h <= local3
       |[4:13..4:14): h <= local4
       |""".stripMargin,
  )

  treeSymbols(
    "SAM eta expansion of a curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t91 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = h _
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:10): t91 <= _empty_/t91.
       |[3:6..3:7): h <= _empty_/t91.h().
       |[3:8..3:9): a <= _empty_/t91.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t91.h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t91.h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t91.h().(b)
       |[4:6..4:7): s <= _empty_/t91.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): h => _empty_/t91.h().
       |""".stripMargin,
  )

  symbols(
    "SAM eta expansion of a curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t91 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = h _
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t91. => final object t91 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t91.h(). => method h(a: Int)(b: Int): Int
       |  a => _empty_/t91.h().(a)
       |  Int => scala/Int#
       |  b => _empty_/t91.h().(b)
       |_empty_/t91.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t91.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t91.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(a: Int): T <: _empty_/S#f().
       |  a => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param a: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda as a default argument",
    """|trait S { def f(a: Int): Int }
       |object t92 {
       |  def m(s: S = x => x) = s
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t92 <= _empty_/t92.
       |[2:6..2:7): m <= _empty_/t92.m().
       |[2:8..2:9): s <= _empty_/t92.m().(s)
       |[2:11..2:12): S => _empty_/S#
       |[2:15..2:16): x <= local2
       |[2:15..2:21): x => x <= local0
       |[2:15..2:21): x => x <= local1
       |[2:20..2:21): x => local2
       |[2:25..2:26): s => _empty_/t92.m().(s)
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda as a default argument",
    """|trait S { def f(a: Int): Int }
       |object t92 {
       |  def m(s: S = x => x) = s
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:10): t92 <= _empty_/t92.
       |[2:6..2:7): m <= _empty_/t92.m().
       |[2:8..2:9): s <= _empty_/t92.m().(s)
       |[2:11..2:12): S => _empty_/S#
       |[2:15..2:21): x => x <= local0
       |[2:15..2:16): x <= local2
       |[2:20..2:21): x => local2
       |[2:25..2:26): s => _empty_/t92.m().(s)
       |""".stripMargin,
  )

  symbols(
    "SAM lambda as a default argument",
    """|trait S { def f(a: Int): Int }
       |object t92 {
       |  def m(s: S = x => x) = s
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t92. => final object t92 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t92.m$default$1(). => synthetic method m$default$1: S @uncheckedVariance
       |  S => _empty_/S#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/t92.m(). => method m(s: S): S
       |  s => _empty_/t92.m().(s)
       |  S => _empty_/S#
       |_empty_/t92.m().(s) => default param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda as a default constructor argument",
    """|trait S { def f(a: Int): Int }
       |class t93(s: S = x => x)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:9): t93 <= _empty_/t93#
       |[1:9..1:9):  <= _empty_/t93#`<init>`().
       |[1:10..1:11): s <= _empty_/t93#`<init>`().(s)
       |[1:10..1:11): s <= _empty_/t93#s.
       |[1:13..1:14): S => _empty_/S#
       |[1:17..1:18): x <= local2
       |[1:17..1:23): x => x <= local0
       |[1:17..1:23): x => x <= local1
       |[1:22..1:23): x => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda as a default constructor argument",
    """|trait S { def f(a: Int): Int }
       |class t93(s: S = x => x)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:9): t93 <= _empty_/t93#
       |[1:9..1:9):  <= _empty_/t93#`<init>`().
       |[1:10..1:11): s <= _empty_/t93#`<init>`().(s)
       |[1:13..1:14): S => _empty_/S#
       |[1:17..1:23): x => x <= local0
       |[1:17..1:18): x <= local2
       |[1:22..1:23): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda as a default constructor argument",
    """|trait S { def f(a: Int): Int }
       |class t93(s: S = x => x)
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t93# => class t93 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t93#`<init>`(). => primary ctor <init>(s: S)
       |  s => _empty_/t93#`<init>`().(s)
       |  S => _empty_/S#
       |_empty_/t93#`<init>`().(s) => default param s: S
       |  S => _empty_/S#
       |_empty_/t93#s. => private[this] val method s: S
       |  S => _empty_/S#
       |_empty_/t93. => synthetic final object t93 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t93.`<init>$default$1`(). => synthetic method <init>$default$1: S @uncheckedVariance
       |  S => _empty_/S#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda as a default argument of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t119(s: S = x => x)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:11..1:15): t119 <= _empty_/t119#
       |[1:15..1:15):  <= _empty_/t119#`<init>`().
       |[1:16..1:17): s <= _empty_/t119#`<init>`().(s)
       |[1:16..1:17): s <= _empty_/t119#s.
       |[1:19..1:20): S => _empty_/S#
       |[1:23..1:24): x <= local2
       |[1:23..1:29): x => x <= local0
       |[1:23..1:29): x => x <= local1
       |[1:28..1:29): x => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda as a default argument of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t119(s: S = x => x)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:11..1:15): t119 <= _empty_/t119#
       |[1:15..1:15):  <= _empty_/t119#`<init>`().
       |[1:16..1:17): s <= _empty_/t119#`<init>`().(s)
       |[1:19..1:20): S => _empty_/S#
       |[1:23..1:29): x => x <= local0
       |[1:23..1:24): x <= local2
       |[1:28..1:29): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda as a default argument of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t119(s: S = x => x)
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t119# => case class t119 extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |_empty_/t119#`<init>`(). => primary ctor <init>(val s: S)
         |  s => _empty_/t119#`<init>`().(s)
         |  S => _empty_/S#
         |_empty_/t119#`<init>`().(s) => val default param s: S
         |  S => _empty_/S#
         |_empty_/t119#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t119#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t119#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t119#copy$default$1(). => synthetic method copy$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t119#copy(). => synthetic method copy(s: S): t119
         |  s => _empty_/t119#copy().(s)
         |  S => _empty_/S#
         |  t119 => _empty_/t119#
         |_empty_/t119#copy().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t119#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t119#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t119#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t119#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t119#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t119#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t119#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t119#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t119#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t119#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t119#s. => val method s: S
         |  S => _empty_/S#
         |_empty_/t119#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t119. => synthetic final object t119 extends AbstractFunction1[S, t119] with Serializable { +6 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  S => _empty_/S#
         |  t119 => _empty_/t119#
         |  Serializable => scala/Serializable#
         |_empty_/t119.`<init>$default$1`(). => synthetic method <init>$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t119.apply$default$1(). => synthetic method apply$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t119.apply(). => synthetic method apply(s: S): t119 <: scala/Function1#apply().
         |  s => _empty_/t119.apply().(s)
         |  S => _empty_/S#
         |  t119 => _empty_/t119#
         |_empty_/t119.apply().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t119.readResolve(). => private synthetic method readResolve(): Object
         |  Object => java/lang/Object#
         |_empty_/t119.toString(). => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t119.unapply(). => synthetic method unapply(x$0: t119): Option[S]
         |  x$0 => _empty_/t119.unapply().(x$0)
         |  t119 => _empty_/t119#
         |  Option => scala/Option#
         |  S => _empty_/S#
         |_empty_/t119.unapply().(x$0) => synthetic param x$0: t119
         |  t119 => _empty_/t119#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
         |  x => local2
         |  Int => scala/Int#
         |local2 => param x: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t119# => case class t119 extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |_empty_/t119#`<init>`(). => primary ctor <init>(val s: S)
         |  s => _empty_/t119#`<init>`().(s)
         |  S => _empty_/S#
         |_empty_/t119#`<init>`().(s) => val default param s: S
         |  S => _empty_/S#
         |_empty_/t119#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t119#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t119#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t119#copy$default$1(). => synthetic method copy$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t119#copy(). => synthetic method copy(s: S): t119
         |  s => _empty_/t119#copy().(s)
         |  S => _empty_/S#
         |  t119 => _empty_/t119#
         |_empty_/t119#copy().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t119#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t119#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t119#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t119#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t119#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t119#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t119#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t119#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t119#productElementName(). => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => _empty_/t119#productElementName().(x$1)
         |  Int => scala/Int#
         |  String => java/lang/String#
         |_empty_/t119#productElementName().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t119#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t119#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t119#s. => val method s: S
         |  S => _empty_/S#
         |_empty_/t119#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t119. => synthetic final object t119 extends AbstractFunction1[S, t119] with Serializable { +6 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  S => _empty_/S#
         |  t119 => _empty_/t119#
         |  Serializable => java/io/Serializable#
         |_empty_/t119.`<init>$default$1`(). => synthetic method <init>$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t119.apply$default$1(). => synthetic method apply$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t119.apply(). => synthetic method apply(s: S): t119 <: scala/Function1#apply().
         |  s => _empty_/t119.apply().(s)
         |  S => _empty_/S#
         |  t119 => _empty_/t119#
         |_empty_/t119.apply().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t119.toString(). => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t119.unapply(). => synthetic method unapply(x$0: t119): Option[S]
         |  x$0 => _empty_/t119.unapply().(x$0)
         |  t119 => _empty_/t119#
         |  Option => scala/Option#
         |  S => _empty_/S#
         |_empty_/t119.unapply().(x$0) => synthetic param x$0: t119
         |  t119 => _empty_/t119#
         |_empty_/t119.writeReplace(). => private synthetic method writeReplace(): Object
         |  Object => java/lang/Object#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
         |  x => local2
         |  Int => scala/Int#
         |local2 => param x: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "SAM method value as a default argument of a case class",
    """|trait S { def f(a: Int): Int }
       |object t120 {
       |  def g(y: Int) = y
       |  case class c(s: S = g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t120 <= _empty_/t120.
       |[2:6..2:7): g <= _empty_/t120.g().
       |[2:8..2:9): y <= _empty_/t120.g().(y)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): y => _empty_/t120.g().(y)
       |[3:13..3:14): c <= _empty_/t120.c#
       |[3:14..3:14):  <= _empty_/t120.c#`<init>`().
       |[3:15..3:16): s <= _empty_/t120.c#`<init>`().(s)
       |[3:15..3:16): s <= _empty_/t120.c#s.
       |[3:18..3:19): S => _empty_/S#
       |[3:22..3:23): g => _empty_/t120.g().
       |[3:22..3:23): g <= local0
       |[3:22..3:23): g <= local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value as a default argument of a case class",
    """|trait S { def f(a: Int): Int }
       |object t120 {
       |  def g(y: Int) = y
       |  case class c(s: S = g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t120 <= _empty_/t120.
       |[2:6..2:7): g <= _empty_/t120.g().
       |[2:8..2:9): y <= _empty_/t120.g().(y)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): y => _empty_/t120.g().(y)
       |[3:13..3:14): c <= _empty_/t120.c#
       |[3:14..3:14):  <= _empty_/t120.c#`<init>`().
       |[3:15..3:16): s <= _empty_/t120.c#`<init>`().(s)
       |[3:18..3:19): S => _empty_/S#
       |[3:22..3:23): g => _empty_/t120.g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value as a default argument of a case class",
    """|trait S { def f(a: Int): Int }
       |object t120 {
       |  def g(y: Int) = y
       |  case class c(s: S = g)
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t120. => final object t120 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t120.c# => case class c extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |_empty_/t120.c#`<init>`(). => primary ctor <init>(val s: S)
         |  s => _empty_/t120.c#`<init>`().(s)
         |  S => _empty_/S#
         |_empty_/t120.c#`<init>`().(s) => val default param s: S
         |  S => _empty_/S#
         |_empty_/t120.c#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t120.c#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t120.c#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t120.c#copy$default$1(). => synthetic method copy$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t120.c#copy(). => synthetic method copy(s: S): c
         |  s => _empty_/t120.c#copy().(s)
         |  S => _empty_/S#
         |  c => _empty_/t120.c#
         |_empty_/t120.c#copy().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t120.c#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t120.c#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t120.c#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t120.c#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t120.c#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t120.c#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t120.c#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t120.c#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t120.c#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t120.c#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t120.c#s. => val method s: S
         |  S => _empty_/S#
         |_empty_/t120.c#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t120.c. => synthetic final object c extends AbstractFunction1[S, c] with Serializable { +6 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  S => _empty_/S#
         |  c => _empty_/t120.c#
         |  Serializable => scala/Serializable#
         |_empty_/t120.c.`<init>$default$1`(). => synthetic method <init>$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t120.c.apply$default$1(). => synthetic method apply$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t120.c.apply(). => synthetic method apply(s: S): c <: scala/Function1#apply().
         |  s => _empty_/t120.c.apply().(s)
         |  S => _empty_/S#
         |  c => _empty_/t120.c#
         |_empty_/t120.c.apply().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t120.c.readResolve(). => private synthetic method readResolve(): Object
         |  Object => java/lang/Object#
         |_empty_/t120.c.toString(). => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t120.c.unapply(). => synthetic method unapply(x$0: c): Option[S]
         |  x$0 => _empty_/t120.c.unapply().(x$0)
         |  c => _empty_/t120.c#
         |  Option => scala/Option#
         |  S => _empty_/S#
         |_empty_/t120.c.unapply().(x$0) => synthetic param x$0: c
         |  c => _empty_/t120.c#
         |_empty_/t120.g(). => method g(y: Int): Int
         |  y => _empty_/t120.g().(y)
         |  Int => scala/Int#
         |_empty_/t120.g().(y) => param y: Int
         |  Int => scala/Int#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
         |  y => local2
         |  Int => scala/Int#
         |local2 => synthetic param y: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t120. => final object t120 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t120.c# => case class c extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |_empty_/t120.c#`<init>`(). => primary ctor <init>(val s: S)
         |  s => _empty_/t120.c#`<init>`().(s)
         |  S => _empty_/S#
         |_empty_/t120.c#`<init>`().(s) => val default param s: S
         |  S => _empty_/S#
         |_empty_/t120.c#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t120.c#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t120.c#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t120.c#copy$default$1(). => synthetic method copy$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t120.c#copy(). => synthetic method copy(s: S): c
         |  s => _empty_/t120.c#copy().(s)
         |  S => _empty_/S#
         |  c => _empty_/t120.c#
         |_empty_/t120.c#copy().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t120.c#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t120.c#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t120.c#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t120.c#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t120.c#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t120.c#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t120.c#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t120.c#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t120.c#productElementName(). => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => _empty_/t120.c#productElementName().(x$1)
         |  Int => scala/Int#
         |  String => java/lang/String#
         |_empty_/t120.c#productElementName().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t120.c#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t120.c#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t120.c#s. => val method s: S
         |  S => _empty_/S#
         |_empty_/t120.c#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t120.c. => synthetic final object c extends AbstractFunction1[S, c] with Serializable { +6 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  S => _empty_/S#
         |  c => _empty_/t120.c#
         |  Serializable => java/io/Serializable#
         |_empty_/t120.c.`<init>$default$1`(). => synthetic method <init>$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t120.c.apply$default$1(). => synthetic method apply$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t120.c.apply(). => synthetic method apply(s: S): c <: scala/Function1#apply().
         |  s => _empty_/t120.c.apply().(s)
         |  S => _empty_/S#
         |  c => _empty_/t120.c#
         |_empty_/t120.c.apply().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t120.c.toString(). => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t120.c.unapply(). => synthetic method unapply(x$0: c): Option[S]
         |  x$0 => _empty_/t120.c.unapply().(x$0)
         |  c => _empty_/t120.c#
         |  Option => scala/Option#
         |  S => _empty_/S#
         |_empty_/t120.c.unapply().(x$0) => synthetic param x$0: c
         |  c => _empty_/t120.c#
         |_empty_/t120.c.writeReplace(). => private synthetic method writeReplace(): Object
         |  Object => java/lang/Object#
         |_empty_/t120.g(). => method g(y: Int): Int
         |  y => _empty_/t120.g().(y)
         |  Int => scala/Int#
         |_empty_/t120.g().(y) => param y: Int
         |  Int => scala/Int#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
         |  y => local2
         |  Int => scala/Int#
         |local2 => synthetic param y: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "SAM lambda as a default argument of a local case class",
    """|trait S { def f(a: Int): Int }
       |object t121 {
       |  def m = {
       |    case class c(s: S = x => x)
       |    c()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:6..0:7): S <= _empty_/S#
         |[0:14..0:15): f <= _empty_/S#f().
         |[0:16..0:17): a <= _empty_/S#f().(a)
         |[0:19..0:22): Int => scala/Int#
         |[0:25..0:28): Int => scala/Int#
         |[1:7..1:11): t121 <= _empty_/t121.
         |[2:6..2:7): m <= _empty_/t121.m().
         |[3:15..3:16): c <= local0
         |[3:16..3:16):  <= local5
         |[3:17..3:18): s <= local1
         |[3:20..3:21): S => _empty_/S#
         |[3:24..3:25): x <= local16
         |[3:24..3:30): x => x <= local14
         |[3:24..3:30): x => x <= local15
         |[3:29..3:30): x => local16
         |[4:4..4:5): c => local21
         |""".stripMargin
    else
      """|[0:6..0:7): S <= _empty_/S#
         |[0:14..0:15): f <= _empty_/S#f().
         |[0:16..0:17): a <= _empty_/S#f().(a)
         |[0:19..0:22): Int => scala/Int#
         |[0:25..0:28): Int => scala/Int#
         |[1:7..1:11): t121 <= _empty_/t121.
         |[2:6..2:7): m <= _empty_/t121.m().
         |[3:15..3:16): c <= local0
         |[3:16..3:16):  <= local5
         |[3:17..3:18): s <= local1
         |[3:20..3:21): S => _empty_/S#
         |[3:24..3:25): x <= local17
         |[3:24..3:30): x => x <= local15
         |[3:24..3:30): x => x <= local16
         |[3:29..3:30): x => local17
         |[4:4..4:5): c => local23
         |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda as a default argument of a local case class",
    """|trait S { def f(a: Int): Int }
       |object t121 {
       |  def m = {
       |    case class c(s: S = x => x)
       |    c()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:6..0:7): S <= _empty_/S#
         |[0:14..0:15): f <= _empty_/S#f().
         |[0:16..0:17): a <= _empty_/S#f().(a)
         |[0:19..0:22): Int => scala/Int#
         |[0:25..0:28): Int => scala/Int#
         |[1:7..1:11): t121 <= _empty_/t121.
         |[2:6..2:7): m <= _empty_/t121.m().
         |[3:15..3:16): c <= local0
         |[3:16..3:16):  <= local5
         |[3:17..3:18): s <= local1
         |[3:20..3:21): S => _empty_/S#
         |[3:24..3:30): x => x <= local14
         |[3:24..3:25): x <= local16
         |[3:29..3:30): x => local16
         |[4:4..4:5): c => local21
         |""".stripMargin
    else
      """|[0:6..0:7): S <= _empty_/S#
         |[0:14..0:15): f <= _empty_/S#f().
         |[0:16..0:17): a <= _empty_/S#f().(a)
         |[0:19..0:22): Int => scala/Int#
         |[0:25..0:28): Int => scala/Int#
         |[1:7..1:11): t121 <= _empty_/t121.
         |[2:6..2:7): m <= _empty_/t121.m().
         |[3:15..3:16): c <= local0
         |[3:16..3:16):  <= local5
         |[3:17..3:18): s <= local1
         |[3:20..3:21): S => _empty_/S#
         |[3:24..3:30): x => x <= local15
         |[3:24..3:25): x <= local17
         |[3:29..3:30): x => local17
         |[4:4..4:5): c => local23
         |""".stripMargin,
  )

  symbols(
    "SAM lambda as a default argument of a local case class",
    """|trait S { def f(a: Int): Int }
       |object t121 {
       |  def m = {
       |    case class c(s: S = x => x)
       |    c()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t121. => final object t121 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t121.m(). => method m: c forSome { case class c extends AnyRef with Product with Serializable { +12 decls } }
         |  c => local0
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local0 => case class c extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val default param s: S
         |  S => _empty_/S#
         |local2 => synthetic method copy(s: S): c
         |  s => local3
         |  S => _empty_/S#
         |  c => local0
         |local3 => default param s: S
         |  S => _empty_/S#
         |local4 => synthetic method copy$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => primary ctor <init>(val s: S)
         |  s => local1
         |  S => _empty_/S#
         |local6 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local7 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local8 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local17
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local9 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local10 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local19
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local15 => synthetic method f(x: Int): Int <: _empty_/S#f().
         |  x => local16
         |  Int => scala/Int#
         |local16 => param x: Int
         |  Int => scala/Int#
         |local17 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local19 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local20 => synthetic method apply(s: S): c <: scala/Function1#apply().
         |  s => local22
         |  S => _empty_/S#
         |  c => local0
         |local21 => synthetic final object c extends AbstractFunction1[S, c] with Serializable { +5 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  S => _empty_/S#
         |  c => local0
         |  Serializable => scala/Serializable#
         |local22 => default param s: S
         |  S => _empty_/S#
         |local23 => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local24 => synthetic method apply$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local25 => synthetic method unapply(x$0: c): Option[S]
         |  x$0 => local27
         |  c => local0
         |  Option => scala/Option#
         |  S => _empty_/S#
         |local26 => synthetic method <init>$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local27 => synthetic param x$0: c
         |  c => local0
         |""".stripMargin
    else
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t121. => final object t121 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t121.m(). => method m: c forSome { case class c extends AnyRef with Product with Serializable { +13 decls } }
         |  c => local0
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local0 => case class c extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val default param s: S
         |  S => _empty_/S#
         |local2 => synthetic method copy(s: S): c
         |  s => local3
         |  S => _empty_/S#
         |  c => local0
         |local3 => default param s: S
         |  S => _empty_/S#
         |local4 => synthetic method copy$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => primary ctor <init>(val s: S)
         |  s => local1
         |  S => _empty_/S#
         |local6 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local7 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local8 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local18
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local9 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local10 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local19
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local11 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local20
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local12 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local13 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local14 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local21
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local15 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local16 => synthetic method f(x: Int): Int <: _empty_/S#f().
         |  x => local17
         |  Int => scala/Int#
         |local17 => param x: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local19 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local20 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local21 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local22 => synthetic method apply(s: S): c <: scala/Function1#apply().
         |  s => local24
         |  S => _empty_/S#
         |  c => local0
         |local23 => synthetic final object c extends AbstractFunction1[S, c] with Serializable { +5 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  S => _empty_/S#
         |  c => local0
         |  Serializable => java/io/Serializable#
         |local24 => default param s: S
         |  S => _empty_/S#
         |local25 => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local26 => synthetic method apply$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local27 => synthetic method unapply(x$0: c): Option[S]
         |  x$0 => local29
         |  c => local0
         |  Option => scala/Option#
         |  S => _empty_/S#
         |local28 => synthetic method <init>$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local29 => synthetic param x$0: c
         |  c => local0
         |""".stripMargin,
  )

  occurrences(
    "SAM lambdas as two default arguments of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t122(s: S = x => x, u: S = y => y)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:11..1:15): t122 <= _empty_/t122#
       |[1:15..1:15):  <= _empty_/t122#`<init>`().
       |[1:16..1:17): s <= _empty_/t122#`<init>`().(s)
       |[1:16..1:17): s <= _empty_/t122#s.
       |[1:19..1:20): S => _empty_/S#
       |[1:23..1:24): x <= local2
       |[1:23..1:29): x => x <= local0
       |[1:23..1:29): x => x <= local1
       |[1:28..1:29): x => local2
       |[1:31..1:32): u <= _empty_/t122#`<init>`().(u)
       |[1:31..1:32): u <= _empty_/t122#u.
       |[1:34..1:35): S => _empty_/S#
       |[1:38..1:39): y <= local5
       |[1:38..1:44): y => y <= local3
       |[1:38..1:44): y => y <= local4
       |[1:43..1:44): y => local5
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambdas as two default arguments of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t122(s: S = x => x, u: S = y => y)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:11..1:15): t122 <= _empty_/t122#
       |[1:15..1:15):  <= _empty_/t122#`<init>`().
       |[1:16..1:17): s <= _empty_/t122#`<init>`().(s)
       |[1:19..1:20): S => _empty_/S#
       |[1:23..1:29): x => x <= local0
       |[1:23..1:24): x <= local2
       |[1:28..1:29): x => local2
       |[1:31..1:32): u <= _empty_/t122#`<init>`().(u)
       |[1:34..1:35): S => _empty_/S#
       |[1:38..1:44): y => y <= local3
       |[1:38..1:39): y <= local5
       |[1:43..1:44): y => local5
       |""".stripMargin,
  )

  symbols(
    "SAM lambdas as two default arguments of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t122(s: S = x => x, u: S = y => y)
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t122# => case class t122 extends AnyRef with Product with Serializable { +14 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |_empty_/t122#`<init>`(). => primary ctor <init>(val s: S, val u: S)
         |  s => _empty_/t122#`<init>`().(s)
         |  S => _empty_/S#
         |  u => _empty_/t122#`<init>`().(u)
         |_empty_/t122#`<init>`().(s) => val default param s: S
         |  S => _empty_/S#
         |_empty_/t122#`<init>`().(u) => val default param u: S
         |  S => _empty_/S#
         |_empty_/t122#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t122#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t122#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t122#copy$default$1(). => synthetic method copy$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122#copy$default$2(). => synthetic method copy$default$2: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122#copy(). => synthetic method copy(s: S, u: S): t122
         |  s => _empty_/t122#copy().(s)
         |  S => _empty_/S#
         |  u => _empty_/t122#copy().(u)
         |  t122 => _empty_/t122#
         |_empty_/t122#copy().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t122#copy().(u) => default param u: S
         |  S => _empty_/S#
         |_empty_/t122#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t122#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t122#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t122#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t122#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t122#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t122#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t122#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t122#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t122#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t122#s. => val method s: S
         |  S => _empty_/S#
         |_empty_/t122#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t122#u. => val method u: S
         |  S => _empty_/S#
         |_empty_/t122. => synthetic final object t122 extends AbstractFunction2[S, S, t122] with Serializable { +8 decls }
         |  AbstractFunction2 => scala/runtime/AbstractFunction2#
         |  S => _empty_/S#
         |  t122 => _empty_/t122#
         |  Serializable => scala/Serializable#
         |_empty_/t122.`<init>$default$1`(). => synthetic method <init>$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122.`<init>$default$2`(). => synthetic method <init>$default$2: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122.apply$default$1(). => synthetic method apply$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122.apply$default$2(). => synthetic method apply$default$2: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122.apply(). => synthetic method apply(s: S, u: S): t122 <: scala/Function2#apply().
         |  s => _empty_/t122.apply().(s)
         |  S => _empty_/S#
         |  u => _empty_/t122.apply().(u)
         |  t122 => _empty_/t122#
         |_empty_/t122.apply().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t122.apply().(u) => default param u: S
         |  S => _empty_/S#
         |_empty_/t122.readResolve(). => private synthetic method readResolve(): Object
         |  Object => java/lang/Object#
         |_empty_/t122.toString(). => synthetic final method toString(): String <: scala/Function2#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t122.unapply(). => synthetic method unapply(x$0: t122): Option[Tuple2[S, S]]
         |  x$0 => _empty_/t122.unapply().(x$0)
         |  t122 => _empty_/t122#
         |  Option => scala/Option#
         |  Tuple2 => scala/Tuple2#
         |  S => _empty_/S#
         |_empty_/t122.unapply().(x$0) => synthetic param x$0: t122
         |  t122 => _empty_/t122#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
         |  x => local2
         |  Int => scala/Int#
         |local2 => param x: Int
         |  Int => scala/Int#
         |local3 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local4 => synthetic method f(y: Int): Int <: _empty_/S#f().
         |  y => local5
         |  Int => scala/Int#
         |local5 => param y: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t122# => case class t122 extends AnyRef with Product with Serializable { +15 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |_empty_/t122#`<init>`(). => primary ctor <init>(val s: S, val u: S)
         |  s => _empty_/t122#`<init>`().(s)
         |  S => _empty_/S#
         |  u => _empty_/t122#`<init>`().(u)
         |_empty_/t122#`<init>`().(s) => val default param s: S
         |  S => _empty_/S#
         |_empty_/t122#`<init>`().(u) => val default param u: S
         |  S => _empty_/S#
         |_empty_/t122#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t122#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t122#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t122#copy$default$1(). => synthetic method copy$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122#copy$default$2(). => synthetic method copy$default$2: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122#copy(). => synthetic method copy(s: S, u: S): t122
         |  s => _empty_/t122#copy().(s)
         |  S => _empty_/S#
         |  u => _empty_/t122#copy().(u)
         |  t122 => _empty_/t122#
         |_empty_/t122#copy().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t122#copy().(u) => default param u: S
         |  S => _empty_/S#
         |_empty_/t122#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t122#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t122#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t122#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t122#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t122#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t122#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t122#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t122#productElementName(). => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => _empty_/t122#productElementName().(x$1)
         |  Int => scala/Int#
         |  String => java/lang/String#
         |_empty_/t122#productElementName().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t122#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t122#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t122#s. => val method s: S
         |  S => _empty_/S#
         |_empty_/t122#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t122#u. => val method u: S
         |  S => _empty_/S#
         |_empty_/t122. => synthetic final object t122 extends AbstractFunction2[S, S, t122] with Serializable { +8 decls }
         |  AbstractFunction2 => scala/runtime/AbstractFunction2#
         |  S => _empty_/S#
         |  t122 => _empty_/t122#
         |  Serializable => java/io/Serializable#
         |_empty_/t122.`<init>$default$1`(). => synthetic method <init>$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122.`<init>$default$2`(). => synthetic method <init>$default$2: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122.apply$default$1(). => synthetic method apply$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122.apply$default$2(). => synthetic method apply$default$2: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t122.apply(). => synthetic method apply(s: S, u: S): t122 <: scala/Function2#apply().
         |  s => _empty_/t122.apply().(s)
         |  S => _empty_/S#
         |  u => _empty_/t122.apply().(u)
         |  t122 => _empty_/t122#
         |_empty_/t122.apply().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t122.apply().(u) => default param u: S
         |  S => _empty_/S#
         |_empty_/t122.toString(). => synthetic final method toString(): String <: scala/Function2#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t122.unapply(). => synthetic method unapply(x$0: t122): Option[Tuple2[S, S]]
         |  x$0 => _empty_/t122.unapply().(x$0)
         |  t122 => _empty_/t122#
         |  Option => scala/Option#
         |  Tuple2 => scala/Tuple2#
         |  S => _empty_/S#
         |_empty_/t122.unapply().(x$0) => synthetic param x$0: t122
         |  t122 => _empty_/t122#
         |_empty_/t122.writeReplace(). => private synthetic method writeReplace(): Object
         |  Object => java/lang/Object#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
         |  x => local2
         |  Int => scala/Int#
         |local2 => param x: Int
         |  Int => scala/Int#
         |local3 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local4 => synthetic method f(y: Int): Int <: _empty_/S#f().
         |  y => local5
         |  Int => scala/Int#
         |local5 => param y: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "SAM lambda as a default argument in a second parameter list of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t123(i: Int)(s: S = x => x)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:11..1:15): t123 <= _empty_/t123#
       |[1:15..1:15):  <= _empty_/t123#`<init>`().
       |[1:16..1:17): i <= _empty_/t123#`<init>`().(i)
       |[1:16..1:17): i <= _empty_/t123#i.
       |[1:19..1:22): Int => scala/Int#
       |[1:24..1:25): s <= _empty_/t123#`<init>`().(s)
       |[1:24..1:25): s <= _empty_/t123#s.
       |[1:27..1:28): S => _empty_/S#
       |[1:31..1:32): x <= local2
       |[1:31..1:37): x => x <= local0
       |[1:31..1:37): x => x <= local1
       |[1:36..1:37): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda as a default argument in a second parameter list of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t123(i: Int)(s: S = x => x)
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t123# => case class t123 extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |_empty_/t123#`<init>`(). => primary ctor <init>(val i: Int)(s: S)
         |  i => _empty_/t123#`<init>`().(i)
         |  Int => scala/Int#
         |  s => _empty_/t123#`<init>`().(s)
         |  S => _empty_/S#
         |_empty_/t123#`<init>`().(i) => val param i: Int
         |  Int => scala/Int#
         |_empty_/t123#`<init>`().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t123#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t123#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t123#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t123#copy$default$1(). => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t123#copy(). => synthetic method copy(i: Int)(s: S): t123
         |  i => _empty_/t123#copy().(i)
         |  Int => scala/Int#
         |  s => _empty_/t123#copy().(s)
         |  S => _empty_/S#
         |  t123 => _empty_/t123#
         |_empty_/t123#copy().(i) => default param i: Int
         |  Int => scala/Int#
         |_empty_/t123#copy().(s) => param s: S
         |  S => _empty_/S#
         |_empty_/t123#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t123#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t123#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t123#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t123#i. => val method i: Int
         |  Int => scala/Int#
         |_empty_/t123#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t123#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t123#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t123#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t123#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t123#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t123#s. => private[this] val method s: S
         |  S => _empty_/S#
         |_empty_/t123#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t123. => synthetic final object t123 extends AnyRef with Serializable { +6 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |_empty_/t123.`<init>$default$2`(). => synthetic method <init>$default$2(i: Int): S @uncheckedVariance
         |  i => _empty_/t123.`<init>$default$2`().(i)
         |  Int => scala/Int#
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t123.`<init>$default$2`().(i) => param i: Int
         |  Int => scala/Int#
         |_empty_/t123.apply$default$2(). => synthetic method apply$default$2(i: Int): S @uncheckedVariance
         |  i => _empty_/t123.apply$default$2().(i)
         |  Int => scala/Int#
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t123.apply$default$2().(i) => param i: Int
         |  Int => scala/Int#
         |_empty_/t123.apply(). => synthetic method apply(i: Int)(s: S): t123
         |  i => _empty_/t123.apply().(i)
         |  Int => scala/Int#
         |  s => _empty_/t123.apply().(s)
         |  S => _empty_/S#
         |  t123 => _empty_/t123#
         |_empty_/t123.apply().(i) => param i: Int
         |  Int => scala/Int#
         |_empty_/t123.apply().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t123.readResolve(). => private synthetic method readResolve(): Object
         |  Object => java/lang/Object#
         |_empty_/t123.toString(). => synthetic final method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t123.unapply(). => synthetic method unapply(x$0: t123): Option[Int]
         |  x$0 => _empty_/t123.unapply().(x$0)
         |  t123 => _empty_/t123#
         |  Option => scala/Option#
         |  Int => scala/Int#
         |_empty_/t123.unapply().(x$0) => synthetic param x$0: t123
         |  t123 => _empty_/t123#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
         |  x => local2
         |  Int => scala/Int#
         |local2 => param x: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t123# => case class t123 extends AnyRef with Product with Serializable { +14 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |_empty_/t123#`<init>`(). => primary ctor <init>(val i: Int)(s: S)
         |  i => _empty_/t123#`<init>`().(i)
         |  Int => scala/Int#
         |  s => _empty_/t123#`<init>`().(s)
         |  S => _empty_/S#
         |_empty_/t123#`<init>`().(i) => val param i: Int
         |  Int => scala/Int#
         |_empty_/t123#`<init>`().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t123#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t123#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t123#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t123#copy$default$1(). => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t123#copy(). => synthetic method copy(i: Int)(s: S): t123
         |  i => _empty_/t123#copy().(i)
         |  Int => scala/Int#
         |  s => _empty_/t123#copy().(s)
         |  S => _empty_/S#
         |  t123 => _empty_/t123#
         |_empty_/t123#copy().(i) => default param i: Int
         |  Int => scala/Int#
         |_empty_/t123#copy().(s) => param s: S
         |  S => _empty_/S#
         |_empty_/t123#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t123#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t123#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t123#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t123#i. => val method i: Int
         |  Int => scala/Int#
         |_empty_/t123#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t123#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t123#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t123#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t123#productElementName(). => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => _empty_/t123#productElementName().(x$1)
         |  Int => scala/Int#
         |  String => java/lang/String#
         |_empty_/t123#productElementName().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t123#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t123#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t123#s. => private[this] val method s: S
         |  S => _empty_/S#
         |_empty_/t123#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t123. => synthetic final object t123 extends AnyRef with Serializable { +6 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => java/io/Serializable#
         |_empty_/t123.`<init>$default$2`(). => synthetic method <init>$default$2(i: Int): S @uncheckedVariance
         |  i => _empty_/t123.`<init>$default$2`().(i)
         |  Int => scala/Int#
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t123.`<init>$default$2`().(i) => param i: Int
         |  Int => scala/Int#
         |_empty_/t123.apply$default$2(). => synthetic method apply$default$2(i: Int): S @uncheckedVariance
         |  i => _empty_/t123.apply$default$2().(i)
         |  Int => scala/Int#
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t123.apply$default$2().(i) => param i: Int
         |  Int => scala/Int#
         |_empty_/t123.apply(). => synthetic method apply(i: Int)(s: S): t123
         |  i => _empty_/t123.apply().(i)
         |  Int => scala/Int#
         |  s => _empty_/t123.apply().(s)
         |  S => _empty_/S#
         |  t123 => _empty_/t123#
         |_empty_/t123.apply().(i) => param i: Int
         |  Int => scala/Int#
         |_empty_/t123.apply().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t123.toString(). => synthetic final method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t123.unapply(). => synthetic method unapply(x$0: t123): Option[Int]
         |  x$0 => _empty_/t123.unapply().(x$0)
         |  t123 => _empty_/t123#
         |  Option => scala/Option#
         |  Int => scala/Int#
         |_empty_/t123.unapply().(x$0) => synthetic param x$0: t123
         |  t123 => _empty_/t123#
         |_empty_/t123.writeReplace(). => private synthetic method writeReplace(): Object
         |  Object => java/lang/Object#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
         |  x => local2
         |  Int => scala/Int#
         |local2 => param x: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda as a default argument in a second parameter list of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t123(i: Int)(s: S = x => x)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:11..1:15): t123 <= _empty_/t123#
       |[1:15..1:15):  <= _empty_/t123#`<init>`().
       |[1:16..1:17): i <= _empty_/t123#`<init>`().(i)
       |[1:19..1:22): Int => scala/Int#
       |[1:24..1:25): s <= _empty_/t123#`<init>`().(s)
       |[1:27..1:28): S => _empty_/S#
       |[1:31..1:37): x => x <= local0
       |[1:31..1:32): x <= local2
       |[1:36..1:37): x => local2
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder as a default argument of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t124(s: S = _ + 1)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:11..1:15): t124 <= _empty_/t124#
       |[1:15..1:15):  <= _empty_/t124#`<init>`().
       |[1:16..1:17): s <= _empty_/t124#`<init>`().(s)
       |[1:16..1:17): s <= _empty_/t124#s.
       |[1:19..1:20): S => _empty_/S#
       |[1:23..1:24): _ <= local2
       |[1:23..1:28): _ + 1 <= local0
       |[1:23..1:28): _ + 1 <= local1
       |[1:25..1:26): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder as a default argument of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t124(s: S = _ + 1)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:11..1:15): t124 <= _empty_/t124#
       |[1:15..1:15):  <= _empty_/t124#`<init>`().
       |[1:16..1:17): s <= _empty_/t124#`<init>`().(s)
       |[1:19..1:20): S => _empty_/S#
       |[1:23..1:28): _ + 1 <= local0
       |[1:23..1:24): _ <= local2
       |[1:25..1:26): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder as a default argument of a case class",
    """|trait S { def f(a: Int): Int }
       |case class t124(s: S = _ + 1)
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t124# => case class t124 extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |_empty_/t124#`<init>`(). => primary ctor <init>(val s: S)
         |  s => _empty_/t124#`<init>`().(s)
         |  S => _empty_/S#
         |_empty_/t124#`<init>`().(s) => val default param s: S
         |  S => _empty_/S#
         |_empty_/t124#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t124#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t124#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t124#copy$default$1(). => synthetic method copy$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t124#copy(). => synthetic method copy(s: S): t124
         |  s => _empty_/t124#copy().(s)
         |  S => _empty_/S#
         |  t124 => _empty_/t124#
         |_empty_/t124#copy().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t124#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t124#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t124#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t124#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t124#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t124#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t124#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t124#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t124#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t124#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t124#s. => val method s: S
         |  S => _empty_/S#
         |_empty_/t124#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t124. => synthetic final object t124 extends AbstractFunction1[S, t124] with Serializable { +6 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  S => _empty_/S#
         |  t124 => _empty_/t124#
         |  Serializable => scala/Serializable#
         |_empty_/t124.`<init>$default$1`(). => synthetic method <init>$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t124.apply$default$1(). => synthetic method apply$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t124.apply(). => synthetic method apply(s: S): t124 <: scala/Function1#apply().
         |  s => _empty_/t124.apply().(s)
         |  S => _empty_/S#
         |  t124 => _empty_/t124#
         |_empty_/t124.apply().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t124.readResolve(). => private synthetic method readResolve(): Object
         |  Object => java/lang/Object#
         |_empty_/t124.toString(). => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t124.unapply(). => synthetic method unapply(x$0: t124): Option[S]
         |  x$0 => _empty_/t124.unapply().(x$0)
         |  t124 => _empty_/t124#
         |  Option => scala/Option#
         |  S => _empty_/S#
         |_empty_/t124.unapply().(x$0) => synthetic param x$0: t124
         |  t124 => _empty_/t124#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local1 => synthetic method f(x$1: Int): Int <: _empty_/S#f().
         |  x$1 => local2
         |  Int => scala/Int#
         |local2 => synthetic param x$1: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/S# => trait S extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/S#f(). => abstract method f(a: Int): Int
         |  a => _empty_/S#f().(a)
         |  Int => scala/Int#
         |_empty_/S#f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t124# => case class t124 extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |_empty_/t124#`<init>`(). => primary ctor <init>(val s: S)
         |  s => _empty_/t124#`<init>`().(s)
         |  S => _empty_/S#
         |_empty_/t124#`<init>`().(s) => val default param s: S
         |  S => _empty_/S#
         |_empty_/t124#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t124#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t124#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t124#copy$default$1(). => synthetic method copy$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t124#copy(). => synthetic method copy(s: S): t124
         |  s => _empty_/t124#copy().(s)
         |  S => _empty_/S#
         |  t124 => _empty_/t124#
         |_empty_/t124#copy().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t124#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t124#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t124#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t124#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t124#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t124#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t124#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t124#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t124#productElementName(). => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => _empty_/t124#productElementName().(x$1)
         |  Int => scala/Int#
         |  String => java/lang/String#
         |_empty_/t124#productElementName().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t124#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t124#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t124#s. => val method s: S
         |  S => _empty_/S#
         |_empty_/t124#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t124. => synthetic final object t124 extends AbstractFunction1[S, t124] with Serializable { +6 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  S => _empty_/S#
         |  t124 => _empty_/t124#
         |  Serializable => java/io/Serializable#
         |_empty_/t124.`<init>$default$1`(). => synthetic method <init>$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t124.apply$default$1(). => synthetic method apply$default$1: S @uncheckedVariance
         |  S => _empty_/S#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t124.apply(). => synthetic method apply(s: S): t124 <: scala/Function1#apply().
         |  s => _empty_/t124.apply().(s)
         |  S => _empty_/S#
         |  t124 => _empty_/t124#
         |_empty_/t124.apply().(s) => default param s: S
         |  S => _empty_/S#
         |_empty_/t124.toString(). => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t124.unapply(). => synthetic method unapply(x$0: t124): Option[S]
         |  x$0 => _empty_/t124.unapply().(x$0)
         |  t124 => _empty_/t124#
         |  Option => scala/Option#
         |  S => _empty_/S#
         |_empty_/t124.unapply().(x$0) => synthetic param x$0: t124
         |  t124 => _empty_/t124#
         |_empty_/t124.writeReplace(). => private synthetic method writeReplace(): Object
         |  Object => java/lang/Object#
         |local0 => synthetic class $anon extends Object with S { +1 decls }
         |  Object => java/lang/Object#
         |  S => _empty_/S#
         |local1 => synthetic method f(x$1: Int): Int <: _empty_/S#f().
         |  x$1 => local2
         |  Int => scala/Int#
         |local2 => synthetic param x$1: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "SAM lambda with a dependent result type",
    """|class K { type T = Int }
       |trait S { def f(k: K): k.T }
       |object t125 { val s: S = (k: K) => 1 }
       |""".stripMargin,
    """|[0:6..0:7): K <= _empty_/K#
       |[0:8..0:8):  <= _empty_/K#`<init>`().
       |[0:15..0:16): T <= _empty_/K#T#
       |[0:19..0:22): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): k <= _empty_/S#f().(k)
       |[1:19..1:20): K => _empty_/K#
       |[1:23..1:24): k => _empty_/S#f().(k)
       |[1:25..1:26): T => _empty_/K#T#
       |[2:7..2:11): t125 <= _empty_/t125.
       |[2:18..2:19): s <= _empty_/t125.s.
       |[2:21..2:22): S => _empty_/S#
       |[2:25..2:36): (k: K) => 1 <= local0
       |[2:25..2:36): (k: K) => 1 <= local1
       |[2:26..2:27): k <= local2
       |[2:29..2:30): K => _empty_/K#
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda with a dependent result type",
    """|class K { type T = Int }
       |trait S { def f(k: K): k.T }
       |object t125 { val s: S = (k: K) => 1 }
       |""".stripMargin,
    """|[0:6..0:7): K <= _empty_/K#
       |[0:8..0:8):  <= _empty_/K#`<init>`().
       |[0:15..0:16): T <= _empty_/K#T#
       |[0:19..0:22): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): k <= _empty_/S#f().(k)
       |[1:19..1:20): K => _empty_/K#
       |[1:23..1:24): k => _empty_/S#f().(k)
       |[1:25..1:26): T => _empty_/K#T#
       |[2:7..2:11): t125 <= _empty_/t125.
       |[2:18..2:19): s <= _empty_/t125.s.
       |[2:21..2:22): S => _empty_/S#
       |[2:25..2:36): (k: K) => 1 <= local0
       |[2:26..2:27): k <= local2
       |[2:29..2:30): K => _empty_/K#
       |""".stripMargin,
  )

  symbols(
    "SAM lambda with a dependent result type",
    """|class K { type T = Int }
       |trait S { def f(k: K): k.T }
       |object t125 { val s: S = (k: K) => 1 }
       |""".stripMargin,
    """|_empty_/K# => class K extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/K#T# => type T = Int
       |  Int => scala/Int#
       |_empty_/K#`<init>`(). => primary ctor <init>()
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(k: K): k.T
       |  k => _empty_/S#f().(k)
       |  K => _empty_/K#
       |  T => _empty_/K#T#
       |_empty_/S#f().(k) => param k: K
       |  K => _empty_/K#
       |_empty_/t125. => final object t125 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t125.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(k: K): k.T <: _empty_/S#f().
       |  k => local2
       |  K => _empty_/K#
       |  T => _empty_/K#T#
       |local2 => param k: K
       |  K => _empty_/K#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a dependent result type",
    """|class K { type T = Int }
       |trait S { def f(k: K): k.T }
       |object t128 {
       |  def g(k: K): k.T = 1
       |  val s: S = g
       |}
       |""".stripMargin,
    """|[0:6..0:7): K <= _empty_/K#
       |[0:8..0:8):  <= _empty_/K#`<init>`().
       |[0:15..0:16): T <= _empty_/K#T#
       |[0:19..0:22): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): k <= _empty_/S#f().(k)
       |[1:19..1:20): K => _empty_/K#
       |[1:23..1:24): k => _empty_/S#f().(k)
       |[1:25..1:26): T => _empty_/K#T#
       |[2:7..2:11): t128 <= _empty_/t128.
       |[3:6..3:7): g <= _empty_/t128.g().
       |[3:8..3:9): k <= _empty_/t128.g().(k)
       |[3:11..3:12): K => _empty_/K#
       |[3:15..3:16): k => _empty_/t128.g().(k)
       |[3:17..3:18): T => _empty_/K#T#
       |[4:6..4:7): s <= _empty_/t128.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): g => _empty_/t128.g().
       |[4:13..4:14): g <= local0
       |[4:13..4:14): g <= local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a dependent result type",
    """|class K { type T = Int }
       |trait S { def f(k: K): k.T }
       |object t128 {
       |  def g(k: K): k.T = 1
       |  val s: S = g
       |}
       |""".stripMargin,
    """|[0:6..0:7): K <= _empty_/K#
       |[0:8..0:8):  <= _empty_/K#`<init>`().
       |[0:15..0:16): T <= _empty_/K#T#
       |[0:19..0:22): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): k <= _empty_/S#f().(k)
       |[1:19..1:20): K => _empty_/K#
       |[1:23..1:24): k => _empty_/S#f().(k)
       |[1:25..1:26): T => _empty_/K#T#
       |[2:7..2:11): t128 <= _empty_/t128.
       |[3:6..3:7): g <= _empty_/t128.g().
       |[3:8..3:9): k <= _empty_/t128.g().(k)
       |[3:11..3:12): K => _empty_/K#
       |[3:15..3:16): k => _empty_/t128.g().(k)
       |[3:17..3:18): T => _empty_/K#T#
       |[4:6..4:7): s <= _empty_/t128.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): g => _empty_/t128.g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a dependent result type",
    """|class K { type T = Int }
       |trait S { def f(k: K): k.T }
       |object t128 {
       |  def g(k: K): k.T = 1
       |  val s: S = g
       |}
       |""".stripMargin,
    """|_empty_/K# => class K extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/K#T# => type T = Int
       |  Int => scala/Int#
       |_empty_/K#`<init>`(). => primary ctor <init>()
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(k: K): k.T
       |  k => _empty_/S#f().(k)
       |  K => _empty_/K#
       |  T => _empty_/K#T#
       |_empty_/S#f().(k) => param k: K
       |  K => _empty_/K#
       |_empty_/t128. => final object t128 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t128.g(). => method g(k: K): k.T
       |  k => _empty_/t128.g().(k)
       |  K => _empty_/K#
       |  T => _empty_/K#T#
       |_empty_/t128.g().(k) => param k: K
       |  K => _empty_/K#
       |_empty_/t128.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(k: K): k.T <: _empty_/S#f().
       |  k => local2
       |  K => _empty_/K#
       |  T => _empty_/K#T#
       |local2 => synthetic param k: K
       |  K => _empty_/K#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder with a dependent result type",
    """|class K { type T = Int }
       |trait S { def f(k: K): k.T }
       |object t129 {
       |  def h(k: K): Int = 1
       |  val s: S = h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): K <= _empty_/K#
       |[0:8..0:8):  <= _empty_/K#`<init>`().
       |[0:15..0:16): T <= _empty_/K#T#
       |[0:19..0:22): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): k <= _empty_/S#f().(k)
       |[1:19..1:20): K => _empty_/K#
       |[1:23..1:24): k => _empty_/S#f().(k)
       |[1:25..1:26): T => _empty_/K#T#
       |[2:7..2:11): t129 <= _empty_/t129.
       |[3:6..3:7): h <= _empty_/t129.h().
       |[3:8..3:9): k <= _empty_/t129.h().(k)
       |[3:11..3:12): K => _empty_/K#
       |[3:15..3:18): Int => scala/Int#
       |[4:6..4:7): s <= _empty_/t129.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): h => _empty_/t129.h().
       |[4:13..4:17): h(_) <= local0
       |[4:13..4:17): h(_) <= local1
       |[4:15..4:16): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder with a dependent result type",
    """|class K { type T = Int }
       |trait S { def f(k: K): k.T }
       |object t129 {
       |  def h(k: K): Int = 1
       |  val s: S = h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): K <= _empty_/K#
       |[0:8..0:8):  <= _empty_/K#`<init>`().
       |[0:15..0:16): T <= _empty_/K#T#
       |[0:19..0:22): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): k <= _empty_/S#f().(k)
       |[1:19..1:20): K => _empty_/K#
       |[1:23..1:24): k => _empty_/S#f().(k)
       |[1:25..1:26): T => _empty_/K#T#
       |[2:7..2:11): t129 <= _empty_/t129.
       |[3:6..3:7): h <= _empty_/t129.h().
       |[3:8..3:9): k <= _empty_/t129.h().(k)
       |[3:11..3:12): K => _empty_/K#
       |[3:15..3:18): Int => scala/Int#
       |[4:6..4:7): s <= _empty_/t129.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:17): h(_) <= local0
       |[4:13..4:14): h => _empty_/t129.h().
       |[4:15..4:16): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder with a dependent result type",
    """|class K { type T = Int }
       |trait S { def f(k: K): k.T }
       |object t129 {
       |  def h(k: K): Int = 1
       |  val s: S = h(_)
       |}
       |""".stripMargin,
    """|_empty_/K# => class K extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/K#T# => type T = Int
       |  Int => scala/Int#
       |_empty_/K#`<init>`(). => primary ctor <init>()
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(k: K): k.T
       |  k => _empty_/S#f().(k)
       |  K => _empty_/K#
       |  T => _empty_/K#T#
       |_empty_/S#f().(k) => param k: K
       |  K => _empty_/K#
       |_empty_/t129. => final object t129 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t129.h(). => method h(k: K): Int
       |  k => _empty_/t129.h().(k)
       |  K => _empty_/K#
       |  Int => scala/Int#
       |_empty_/t129.h().(k) => param k: K
       |  K => _empty_/K#
       |_empty_/t129.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: K): x$1.T <: _empty_/S#f().
       |  x$1 => local2
       |  K => _empty_/K#
       |  T => _empty_/K#T#
       |local2 => synthetic param x$1: K
       |  K => _empty_/K#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of a generic trait with a dependent result type",
    """|class K { type T = Int }
       |trait G[A] { def f(k: K, a: A): k.T }
       |object t130 { val g: G[Int] = (k, a) => 1 }
       |""".stripMargin,
    """|[0:6..0:7): K <= _empty_/K#
       |[0:8..0:8):  <= _empty_/K#`<init>`().
       |[0:15..0:16): T <= _empty_/K#T#
       |[0:19..0:22): Int => scala/Int#
       |[1:6..1:7): G <= _empty_/G#
       |[1:8..1:9): A <= _empty_/G#[A]
       |[1:17..1:18): f <= _empty_/G#f().
       |[1:19..1:20): k <= _empty_/G#f().(k)
       |[1:22..1:23): K => _empty_/K#
       |[1:25..1:26): a <= _empty_/G#f().(a)
       |[1:28..1:29): A => _empty_/G#[A]
       |[1:32..1:33): k => _empty_/G#f().(k)
       |[1:34..1:35): T => _empty_/K#T#
       |[2:7..2:11): t130 <= _empty_/t130.
       |[2:18..2:19): g <= _empty_/t130.g.
       |[2:21..2:22): G => _empty_/G#
       |[2:23..2:26): Int => scala/Int#
       |[2:30..2:41): (k, a) => 1 <= local0
       |[2:30..2:41): (k, a) => 1 <= local1
       |[2:31..2:32): k <= local2
       |[2:34..2:35): a <= local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of a generic trait with a dependent result type",
    """|class K { type T = Int }
       |trait G[A] { def f(k: K, a: A): k.T }
       |object t130 { val g: G[Int] = (k, a) => 1 }
       |""".stripMargin,
    """|[0:6..0:7): K <= _empty_/K#
       |[0:8..0:8):  <= _empty_/K#`<init>`().
       |[0:15..0:16): T <= _empty_/K#T#
       |[0:19..0:22): Int => scala/Int#
       |[1:6..1:7): G <= _empty_/G#
       |[1:8..1:9): A <= _empty_/G#[A]
       |[1:17..1:18): f <= _empty_/G#f().
       |[1:19..1:20): k <= _empty_/G#f().(k)
       |[1:22..1:23): K => _empty_/K#
       |[1:25..1:26): a <= _empty_/G#f().(a)
       |[1:28..1:29): A => _empty_/G#[A]
       |[1:32..1:33): k => _empty_/G#f().(k)
       |[1:34..1:35): T => _empty_/K#T#
       |[2:7..2:11): t130 <= _empty_/t130.
       |[2:18..2:19): g <= _empty_/t130.g.
       |[2:21..2:22): G => _empty_/G#
       |[2:23..2:26): Int => scala/Int#
       |[2:30..2:41): (k, a) => 1 <= local0
       |[2:31..2:32): k <= local2
       |[2:34..2:35): a <= local3
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of a generic trait with a dependent result type",
    """|class K { type T = Int }
       |trait G[A] { def f(k: K, a: A): k.T }
       |object t130 { val g: G[Int] = (k, a) => 1 }
       |""".stripMargin,
    """|_empty_/G# => trait G[A] extends AnyRef { +1 decls }
       |  A => _empty_/G#[A]
       |  AnyRef => scala/AnyRef#
       |_empty_/G#[A] => typeparam A
       |_empty_/G#f(). => abstract method f(k: K, a: A): k.T
       |  k => _empty_/G#f().(k)
       |  K => _empty_/K#
       |  a => _empty_/G#f().(a)
       |  A => _empty_/G#[A]
       |  T => _empty_/K#T#
       |_empty_/G#f().(a) => param a: A
       |  A => _empty_/G#[A]
       |_empty_/G#f().(k) => param k: K
       |  K => _empty_/K#
       |_empty_/K# => class K extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/K#T# => type T = Int
       |  Int => scala/Int#
       |_empty_/K#`<init>`(). => primary ctor <init>()
       |_empty_/t130. => final object t130 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t130.g. => val method g: G[Int]
       |  G => _empty_/G#
       |  Int => scala/Int#
       |local0 => synthetic class $anon extends Object with G[Int] { +1 decls }
       |  Object => java/lang/Object#
       |  G => _empty_/G#
       |  Int => scala/Int#
       |local1 => synthetic method f(k: K, a: Int): k.T <: _empty_/G#f().
       |  k => local2
       |  K => _empty_/K#
       |  a => local3
       |  Int => scala/Int#
       |  T => _empty_/K#T#
       |local2 => param k: K
       |  K => _empty_/K#
       |local3 => param a: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of a bounded wildcard type",
    """|trait G[T] { def f(t: T): T }
       |object t96 {
       |  val g: G[_ <: Int] = (x: Int) => x
       |}
       |""".stripMargin,
    """|[0:6..0:7): G <= _empty_/G#
       |[0:8..0:9): T <= _empty_/G#[T]
       |[0:17..0:18): f <= _empty_/G#f().
       |[0:19..0:20): t <= _empty_/G#f().(t)
       |[0:22..0:23): T => _empty_/G#[T]
       |[0:26..0:27): T => _empty_/G#[T]
       |[1:7..1:10): t96 <= _empty_/t96.
       |[2:6..2:7): g <= _empty_/t96.g.
       |[2:9..2:10): G => _empty_/G#
       |[2:11..2:12): _ <= local3
       |[2:23..2:36): (x: Int) => x <= local0
       |[2:23..2:36): (x: Int) => x <= local1
       |[2:24..2:25): x <= local2
       |[2:27..2:30): Int => scala/Int#
       |[2:35..2:36): x => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of a bounded wildcard type",
    """|trait G[T] { def f(t: T): T }
       |object t96 {
       |  val g: G[_ <: Int] = (x: Int) => x
       |}
       |""".stripMargin,
    """|[0:6..0:7): G <= _empty_/G#
       |[0:8..0:9): T <= _empty_/G#[T]
       |[0:17..0:18): f <= _empty_/G#f().
       |[0:19..0:20): t <= _empty_/G#f().(t)
       |[0:22..0:23): T => _empty_/G#[T]
       |[0:26..0:27): T => _empty_/G#[T]
       |[1:7..1:10): t96 <= _empty_/t96.
       |[2:6..2:7): g <= _empty_/t96.g.
       |[2:9..2:10): G => _empty_/G#
       |[2:23..2:36): (x: Int) => x <= local0
       |[2:24..2:25): x <= local2
       |[2:27..2:30): Int => scala/Int#
       |[2:35..2:36): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of a bounded wildcard type",
    """|trait G[T] { def f(t: T): T }
       |object t96 {
       |  val g: G[_ <: Int] = (x: Int) => x
       |}
       |""".stripMargin,
    """|_empty_/G# => trait G[T] extends AnyRef { +1 decls }
       |  T => _empty_/G#[T]
       |  AnyRef => scala/AnyRef#
       |_empty_/G#[T] => typeparam T
       |_empty_/G#f(). => abstract method f(t: T): T
       |  t => _empty_/G#f().(t)
       |  T => _empty_/G#[T]
       |_empty_/G#f().(t) => param t: T
       |  T => _empty_/G#[T]
       |_empty_/t96. => final object t96 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t96.g. => val method g: G[_] forSome { type _ <: Int }
       |  G => _empty_/G#
       |  _ => local3
       |  Int => scala/Int#
       |local0 => synthetic class $anon extends Object with G[Int] { +1 decls }
       |  Object => java/lang/Object#
       |  G => _empty_/G#
       |  Int => scala/Int#
       |local1 => synthetic method f(x: Int): Int <: _empty_/G#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |local3 => synthetic abstract type _ <: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of a wildcard type",
    """|trait G[T] { def f(t: T): Int }
       |object t97 {
       |  val g: G[_] = (x: Any) => 0
       |}
       |""".stripMargin,
    """|[0:6..0:7): G <= _empty_/G#
       |[0:8..0:9): T <= _empty_/G#[T]
       |[0:17..0:18): f <= _empty_/G#f().
       |[0:19..0:20): t <= _empty_/G#f().(t)
       |[0:22..0:23): T => _empty_/G#[T]
       |[0:26..0:29): Int => scala/Int#
       |[1:7..1:10): t97 <= _empty_/t97.
       |[2:6..2:7): g <= _empty_/t97.g.
       |[2:9..2:10): G => _empty_/G#
       |[2:11..2:12): _ <= local1
       |[2:16..2:29): (x: Any) => 0 <= local0
       |[2:16..2:29): (x: Any) => 0 <= local2
       |[2:17..2:18): x <= local3
       |[2:20..2:23): Any => scala/Any#
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of a wildcard type",
    """|trait G[T] { def f(t: T): Int }
       |object t97 {
       |  val g: G[_] = (x: Any) => 0
       |}
       |""".stripMargin,
    """|[0:6..0:7): G <= _empty_/G#
       |[0:8..0:9): T <= _empty_/G#[T]
       |[0:17..0:18): f <= _empty_/G#f().
       |[0:19..0:20): t <= _empty_/G#f().(t)
       |[0:22..0:23): T => _empty_/G#[T]
       |[0:26..0:29): Int => scala/Int#
       |[1:7..1:10): t97 <= _empty_/t97.
       |[2:6..2:7): g <= _empty_/t97.g.
       |[2:9..2:10): G => _empty_/G#
       |[2:11..2:12): _ <= local1
       |[2:16..2:29): (x: Any) => 0 <= local0
       |[2:17..2:18): x <= local3
       |[2:20..2:23): Any => scala/Any#
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of a wildcard type",
    """|trait G[T] { def f(t: T): Int }
       |object t97 {
       |  val g: G[_] = (x: Any) => 0
       |}
       |""".stripMargin,
    """|_empty_/G# => trait G[T] extends AnyRef { +1 decls }
       |  T => _empty_/G#[T]
       |  AnyRef => scala/AnyRef#
       |_empty_/G#[T] => typeparam T
       |_empty_/G#f(). => abstract method f(t: T): Int
       |  t => _empty_/G#f().(t)
       |  T => _empty_/G#[T]
       |  Int => scala/Int#
       |_empty_/G#f().(t) => param t: T
       |  T => _empty_/G#[T]
       |_empty_/t97. => final object t97 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t97.g. => val method g: G[_] forSome { type _ }
       |  G => _empty_/G#
       |  _ => local1
       |local0 => synthetic class $anon extends Object with G[_] forSome { type _ } { +1 decls }
       |  Object => java/lang/Object#
       |  G => _empty_/G#
       |  _ => local1
       |local1 => synthetic abstract type _
       |local2 => synthetic method f(x: Any): Int <: _empty_/G#f().
       |  x => local3
       |  Any => scala/Any#
       |  Int => scala/Int#
       |local3 => param x: Any
       |  Any => scala/Any#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a this qualifier and a type argument",
    """|trait S { def f(a: Int): Int }
       |class t100 {
       |  def use(s: S) = s.f(0)
       |  def id[A](a: A) = a
       |  def m = use(this.id[Int])
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t100 <= _empty_/t100#
       |[1:11..1:11):  <= _empty_/t100#`<init>`().
       |[2:6..2:9): use <= _empty_/t100#use().
       |[2:10..2:11): s <= _empty_/t100#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t100#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:8): id <= _empty_/t100#id().
       |[3:9..3:10): A <= _empty_/t100#id().[A]
       |[3:12..3:13): a <= _empty_/t100#id().(a)
       |[3:15..3:16): A => _empty_/t100#id().[A]
       |[3:20..3:21): a => _empty_/t100#id().(a)
       |[4:6..4:7): m <= _empty_/t100#m().
       |[4:10..4:13): use => _empty_/t100#use().
       |[4:14..4:26): this.id[Int] <= local0
       |[4:14..4:26): this.id[Int] <= local1
       |[4:19..4:21): id => _empty_/t100#id().
       |[4:22..4:25): Int => scala/Int#
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a this qualifier and a type argument",
    """|trait S { def f(a: Int): Int }
       |class t100 {
       |  def use(s: S) = s.f(0)
       |  def id[A](a: A) = a
       |  def m = use(this.id[Int])
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t100 <= _empty_/t100#
       |[1:11..1:11):  <= _empty_/t100#`<init>`().
       |[2:6..2:9): use <= _empty_/t100#use().
       |[2:10..2:11): s <= _empty_/t100#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t100#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:8): id <= _empty_/t100#id().
       |[3:9..3:10): A <= _empty_/t100#id().[A]
       |[3:12..3:13): a <= _empty_/t100#id().(a)
       |[3:15..3:16): A => _empty_/t100#id().[A]
       |[3:20..3:21): a => _empty_/t100#id().(a)
       |[4:6..4:7): m <= _empty_/t100#m().
       |[4:10..4:13): use => _empty_/t100#use().
       |[4:14..4:26): this.id[Int] <= local0
       |[4:19..4:21): id => _empty_/t100#id().
       |[4:22..4:25): Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a this qualifier and a type argument",
    """|trait S { def f(a: Int): Int }
       |class t100 {
       |  def use(s: S) = s.f(0)
       |  def id[A](a: A) = a
       |  def m = use(this.id[Int])
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t100# => class t100 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t100#`<init>`(). => primary ctor <init>()
       |_empty_/t100#id(). => method id[A](a: A): A
       |  A => _empty_/t100#id().[A]
       |  a => _empty_/t100#id().(a)
       |_empty_/t100#id().(a) => param a: A
       |  A => _empty_/t100#id().[A]
       |_empty_/t100#id().[A] => typeparam A
       |_empty_/t100#m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t100#use(). => method use(s: S): Int
       |  s => _empty_/t100#use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t100#use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(a: Int): Int <: _empty_/S#f().
       |  a => local2
       |  Int => scala/Int#
       |local2 => synthetic param a: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  names(
    "SAM method value with a this qualifier and a type argument",
    """|trait S { def f(a: Int): Int }
       |class t100 {
       |  def use(s: S) = s.f(0)
       |  def id[A](a: A) = a
       |  def m = use(this.id[Int])
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:8..0:8):
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t100 <= _empty_/t100#
       |[1:11..1:11):  <= _empty_/t100#`<init>`().
       |[2:6..2:9): use <= _empty_/t100#use().
       |[2:10..2:11): s <= _empty_/t100#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t100#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:8): id <= _empty_/t100#id().
       |[3:9..3:10): A <= _empty_/t100#id().[A]
       |[3:12..3:13): a <= _empty_/t100#id().(a)
       |[3:15..3:16): A => _empty_/t100#id().[A]
       |[3:20..3:21): a => _empty_/t100#id().(a)
       |[4:6..4:7): m <= _empty_/t100#m().
       |[4:10..4:13): use => _empty_/t100#use().
       |[4:14..4:14):
       |[4:19..4:21): id => _empty_/t100#id().
       |[4:22..4:25): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM partial application of a curried method",
    """|trait S { def f(b: Int): Int }
       |object t101 {
       |  def use(s: S) = s.f(0)
       |  def h(a: Int)(b: Int) = a + b
       |  def m = use(h(1))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): b <= _empty_/S#f().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t101 <= _empty_/t101.
       |[2:6..2:9): use <= _empty_/t101.use().
       |[2:10..2:11): s <= _empty_/t101.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t101.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): h <= _empty_/t101.h().
       |[3:8..3:9): a <= _empty_/t101.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t101.h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t101.h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t101.h().(b)
       |[4:6..4:7): m <= _empty_/t101.m().
       |[4:10..4:13): use => _empty_/t101.use().
       |[4:14..4:15): h => _empty_/t101.h().
       |[4:14..4:18): h(1) <= local0
       |[4:14..4:18): h(1) <= local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM partial application of a curried method",
    """|trait S { def f(b: Int): Int }
       |object t101 {
       |  def use(s: S) = s.f(0)
       |  def h(a: Int)(b: Int) = a + b
       |  def m = use(h(1))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): b <= _empty_/S#f().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t101 <= _empty_/t101.
       |[2:6..2:9): use <= _empty_/t101.use().
       |[2:10..2:11): s <= _empty_/t101.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t101.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): h <= _empty_/t101.h().
       |[3:8..3:9): a <= _empty_/t101.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t101.h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t101.h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t101.h().(b)
       |[4:6..4:7): m <= _empty_/t101.m().
       |[4:10..4:13): use => _empty_/t101.use().
       |[4:14..4:18): h(1) <= local0
       |[4:14..4:15): h => _empty_/t101.h().
       |""".stripMargin,
  )

  symbols(
    "SAM partial application of a curried method",
    """|trait S { def f(b: Int): Int }
       |object t101 {
       |  def use(s: S) = s.f(0)
       |  def h(a: Int)(b: Int) = a + b
       |  def m = use(h(1))
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(b: Int): Int
       |  b => _empty_/S#f().(b)
       |  Int => scala/Int#
       |_empty_/S#f().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t101. => final object t101 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t101.h(). => method h(a: Int)(b: Int): Int
       |  a => _empty_/t101.h().(a)
       |  Int => scala/Int#
       |  b => _empty_/t101.h().(b)
       |_empty_/t101.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t101.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t101.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t101.use(). => method use(s: S): Int
       |  s => _empty_/t101.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t101.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(b: Int): Int <: _empty_/S#f().
       |  b => local2
       |  Int => scala/Int#
       |local2 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of a trait with two abstract methods",
    """|trait Q { def f(a: Int): Int }
       |trait S { def f(a: Int): Int }
       |trait U extends Q with S
       |object t103 {
       |  val u: U = x => x
       |}
       |""".stripMargin,
    """|[0:6..0:7): Q <= _empty_/Q#
       |[0:14..0:15): f <= _empty_/Q#f().
       |[0:16..0:17): a <= _empty_/Q#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:28): Int => scala/Int#
       |[2:6..2:7): U <= _empty_/U#
       |[2:16..2:17): Q => _empty_/Q#
       |[2:23..2:24): S => _empty_/S#
       |[3:7..3:11): t103 <= _empty_/t103.
       |[4:6..4:7): u <= _empty_/t103.u.
       |[4:9..4:10): U => _empty_/U#
       |[4:13..4:14): x <= local2
       |[4:13..4:19): x => x <= local0
       |[4:13..4:19): x => x <= local1
       |[4:18..4:19): x => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of a trait with two abstract methods",
    """|trait Q { def f(a: Int): Int }
       |trait S { def f(a: Int): Int }
       |trait U extends Q with S
       |object t103 {
       |  val u: U = x => x
       |}
       |""".stripMargin,
    """|[0:6..0:7): Q <= _empty_/Q#
       |[0:14..0:15): f <= _empty_/Q#f().
       |[0:16..0:17): a <= _empty_/Q#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:28): Int => scala/Int#
       |[2:6..2:7): U <= _empty_/U#
       |[2:16..2:17): Q => _empty_/Q#
       |[2:23..2:24): S => _empty_/S#
       |[3:7..3:11): t103 <= _empty_/t103.
       |[4:6..4:7): u <= _empty_/t103.u.
       |[4:9..4:10): U => _empty_/U#
       |[4:13..4:19): x => x <= local0
       |[4:13..4:14): x <= local2
       |[4:18..4:19): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of a trait with two abstract methods",
    """|trait Q { def f(a: Int): Int }
       |trait S { def f(a: Int): Int }
       |trait U extends Q with S
       |object t103 {
       |  val u: U = x => x
       |}
       |""".stripMargin,
    """|_empty_/Q# => trait Q extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/Q#f(). => abstract method f(a: Int): Int
       |  a => _empty_/Q#f().(a)
       |  Int => scala/Int#
       |_empty_/Q#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/U# => trait U extends AnyRef with Q with S
       |  AnyRef => scala/AnyRef#
       |  Q => _empty_/Q#
       |  S => _empty_/S#
       |_empty_/t103. => final object t103 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t103.u. => val method u: U
       |  U => _empty_/U#
       |local0 => synthetic class $anon extends Object with U { +1 decls }
       |  Object => java/lang/Object#
       |  U => _empty_/U#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f()., _empty_/Q#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder over a curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t104 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t104 <= _empty_/t104.
       |[3:6..3:7): h <= _empty_/t104.h().
       |[3:8..3:9): a <= _empty_/t104.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t104.h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t104.h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t104.h().(b)
       |[4:6..4:7): s <= _empty_/t104.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): h => _empty_/t104.h().
       |[4:13..4:17): h(_) <= local0
       |[4:13..4:17): h(_) <= local1
       |[4:13..4:17): h(_) <= local3
       |[4:13..4:17): h(_) <= local4
       |[4:15..4:16): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder over a curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t104 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t104 <= _empty_/t104.
       |[3:6..3:7): h <= _empty_/t104.h().
       |[3:8..3:9): a <= _empty_/t104.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t104.h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t104.h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t104.h().(b)
       |[4:6..4:7): s <= _empty_/t104.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:17): h(_) <= local0
       |[4:13..4:14): h => _empty_/t104.h().
       |[4:15..4:16): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder over a curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t104 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = h(_)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t104. => final object t104 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t104.h(). => method h(a: Int)(b: Int): Int
       |  a => _empty_/t104.h().(a)
       |  Int => scala/Int#
       |  b => _empty_/t104.h().(b)
       |_empty_/t104.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t104.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t104.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): T <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "placeholder over a curried method of a function type",
    """|trait T { def g(b: Int): Int }
       |object t105 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: Int => T = h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t105 <= _empty_/t105.
       |[2:6..2:7): h <= _empty_/t105.h().
       |[2:8..2:9): a <= _empty_/t105.h().(a)
       |[2:11..2:14): Int => scala/Int#
       |[2:16..2:17): b <= _empty_/t105.h().(b)
       |[2:19..2:22): Int => scala/Int#
       |[2:26..2:27): a => _empty_/t105.h().(a)
       |[2:28..2:29): + => scala/Int#`+`(+4).
       |[2:30..2:31): b => _empty_/t105.h().(b)
       |[3:6..3:7): s <= _empty_/t105.s.
       |[3:9..3:12): Int => scala/Int#
       |[3:16..3:17): T => _empty_/T#
       |[3:20..3:21): h => _empty_/t105.h().
       |[3:20..3:24): h(_) <= local1
       |[3:20..3:24): h(_) <= local2
       |[3:22..3:23): _ <= local0
       |""".stripMargin,
  )

  symbols(
    "placeholder over a curried method of a function type",
    """|trait T { def g(b: Int): Int }
       |object t105 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: Int => T = h(_)
       |}
       |""".stripMargin,
    """|_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t105. => final object t105 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t105.h(). => method h(a: Int)(b: Int): Int
       |  a => _empty_/t105.h().(a)
       |  Int => scala/Int#
       |  b => _empty_/t105.h().(b)
       |_empty_/t105.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t105.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t105.s. => val method s: Function1[Int, T]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local0 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local1 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local2 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local3
       |  Int => scala/Int#
       |local3 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder over a curried method as an argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t106 {
       |  def use(s: S) = s.f(0).g(1)
       |  def h(a: Int)(b: Int) = a + b
       |  def m = use(h(_))
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t106 <= _empty_/t106.
       |[3:6..3:9): use <= _empty_/t106.use().
       |[3:10..3:11): s <= _empty_/t106.use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t106.use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[3:25..3:26): g => _empty_/T#g().
       |[4:6..4:7): h <= _empty_/t106.h().
       |[4:8..4:9): a <= _empty_/t106.h().(a)
       |[4:11..4:14): Int => scala/Int#
       |[4:16..4:17): b <= _empty_/t106.h().(b)
       |[4:19..4:22): Int => scala/Int#
       |[4:26..4:27): a => _empty_/t106.h().(a)
       |[4:28..4:29): + => scala/Int#`+`(+4).
       |[4:30..4:31): b => _empty_/t106.h().(b)
       |[5:6..5:7): m <= _empty_/t106.m().
       |[5:10..5:13): use => _empty_/t106.use().
       |[5:14..5:15): h => _empty_/t106.h().
       |[5:14..5:18): h(_) <= local0
       |[5:14..5:18): h(_) <= local1
       |[5:14..5:18): h(_) <= local3
       |[5:14..5:18): h(_) <= local4
       |[5:16..5:17): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder over a curried method as an argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t106 {
       |  def use(s: S) = s.f(0).g(1)
       |  def h(a: Int)(b: Int) = a + b
       |  def m = use(h(_))
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t106 <= _empty_/t106.
       |[3:6..3:9): use <= _empty_/t106.use().
       |[3:10..3:11): s <= _empty_/t106.use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t106.use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[3:25..3:26): g => _empty_/T#g().
       |[4:6..4:7): h <= _empty_/t106.h().
       |[4:8..4:9): a <= _empty_/t106.h().(a)
       |[4:11..4:14): Int => scala/Int#
       |[4:16..4:17): b <= _empty_/t106.h().(b)
       |[4:19..4:22): Int => scala/Int#
       |[4:26..4:27): a => _empty_/t106.h().(a)
       |[4:28..4:29): + => scala/Int#`+`(+4).
       |[4:30..4:31): b => _empty_/t106.h().(b)
       |[5:6..5:7): m <= _empty_/t106.m().
       |[5:10..5:13): use => _empty_/t106.use().
       |[5:14..5:18): h(_) <= local0
       |[5:14..5:15): h => _empty_/t106.h().
       |[5:16..5:17): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder over a curried method as an argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t106 {
       |  def use(s: S) = s.f(0).g(1)
       |  def h(a: Int)(b: Int) = a + b
       |  def m = use(h(_))
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t106. => final object t106 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t106.h(). => method h(a: Int)(b: Int): Int
       |  a => _empty_/t106.h().(a)
       |  Int => scala/Int#
       |  b => _empty_/t106.h().(b)
       |_empty_/t106.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t106.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t106.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t106.use(). => method use(s: S): Int
       |  s => _empty_/t106.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t106.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): T <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder over a qualified curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object K { def h(a: Int)(b: Int) = a + b }
       |object t107 {
       |  val s: S = K.h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:8): K <= _empty_/K.
       |[2:15..2:16): h <= _empty_/K.h().
       |[2:17..2:18): a <= _empty_/K.h().(a)
       |[2:20..2:23): Int => scala/Int#
       |[2:25..2:26): b <= _empty_/K.h().(b)
       |[2:28..2:31): Int => scala/Int#
       |[2:35..2:36): a => _empty_/K.h().(a)
       |[2:37..2:38): + => scala/Int#`+`(+4).
       |[2:39..2:40): b => _empty_/K.h().(b)
       |[3:7..3:11): t107 <= _empty_/t107.
       |[4:6..4:7): s <= _empty_/t107.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): K => _empty_/K.
       |[4:13..4:19): K.h(_) <= local0
       |[4:13..4:19): K.h(_) <= local1
       |[4:13..4:19): K.h(_) <= local3
       |[4:13..4:19): K.h(_) <= local4
       |[4:15..4:16): h => _empty_/K.h().
       |[4:17..4:18): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder over a qualified curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object K { def h(a: Int)(b: Int) = a + b }
       |object t107 {
       |  val s: S = K.h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:8): K <= _empty_/K.
       |[2:15..2:16): h <= _empty_/K.h().
       |[2:17..2:18): a <= _empty_/K.h().(a)
       |[2:20..2:23): Int => scala/Int#
       |[2:25..2:26): b <= _empty_/K.h().(b)
       |[2:28..2:31): Int => scala/Int#
       |[2:35..2:36): a => _empty_/K.h().(a)
       |[2:37..2:38): + => scala/Int#`+`(+4).
       |[2:39..2:40): b => _empty_/K.h().(b)
       |[3:7..3:11): t107 <= _empty_/t107.
       |[4:6..4:7): s <= _empty_/t107.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:19): K.h(_) <= local0
       |[4:13..4:14): K => _empty_/K.
       |[4:15..4:16): h => _empty_/K.h().
       |[4:17..4:18): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder over a qualified curried method",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object K { def h(a: Int)(b: Int) = a + b }
       |object t107 {
       |  val s: S = K.h(_)
       |}
       |""".stripMargin,
    """|_empty_/K. => final object K extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/K.h(). => method h(a: Int)(b: Int): Int
       |  a => _empty_/K.h().(a)
       |  Int => scala/Int#
       |  b => _empty_/K.h().(b)
       |_empty_/K.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/K.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t107. => final object t107 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t107.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): T <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder over a curried method with a type argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t108 {
       |  def h[A](a: A)(b: Int) = b
       |  val s: S = h[Int](_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t108 <= _empty_/t108.
       |[3:6..3:7): h <= _empty_/t108.h().
       |[3:8..3:9): A <= _empty_/t108.h().[A]
       |[3:11..3:12): a <= _empty_/t108.h().(a)
       |[3:14..3:15): A => _empty_/t108.h().[A]
       |[3:17..3:18): b <= _empty_/t108.h().(b)
       |[3:20..3:23): Int => scala/Int#
       |[3:27..3:28): b => _empty_/t108.h().(b)
       |[4:6..4:7): s <= _empty_/t108.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): h => _empty_/t108.h().
       |[4:13..4:22): h[Int](_) <= local0
       |[4:13..4:22): h[Int](_) <= local1
       |[4:13..4:22): h[Int](_) <= local3
       |[4:13..4:22): h[Int](_) <= local4
       |[4:15..4:18): Int => scala/Int#
       |[4:20..4:21): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder over a curried method with a type argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t108 {
       |  def h[A](a: A)(b: Int) = b
       |  val s: S = h[Int](_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t108 <= _empty_/t108.
       |[3:6..3:7): h <= _empty_/t108.h().
       |[3:8..3:9): A <= _empty_/t108.h().[A]
       |[3:11..3:12): a <= _empty_/t108.h().(a)
       |[3:14..3:15): A => _empty_/t108.h().[A]
       |[3:17..3:18): b <= _empty_/t108.h().(b)
       |[3:20..3:23): Int => scala/Int#
       |[3:27..3:28): b => _empty_/t108.h().(b)
       |[4:6..4:7): s <= _empty_/t108.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:22): h[Int](_) <= local0
       |[4:13..4:14): h => _empty_/t108.h().
       |[4:15..4:18): Int => scala/Int#
       |[4:20..4:21): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder over a curried method with a type argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t108 {
       |  def h[A](a: A)(b: Int) = b
       |  val s: S = h[Int](_)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t108. => final object t108 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t108.h(). => method h[A](a: A)(b: Int): Int
       |  A => _empty_/t108.h().[A]
       |  a => _empty_/t108.h().(a)
       |  b => _empty_/t108.h().(b)
       |  Int => scala/Int#
       |_empty_/t108.h().(a) => param a: A
       |  A => _empty_/t108.h().[A]
       |_empty_/t108.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t108.h().[A] => typeparam A
       |_empty_/t108.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): T <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder over a curried method with a later argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t109 {
       |  def h(a: Int)(c: Int)(b: Int) = a + b + c
       |  val s: S = h(_)(1)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t109 <= _empty_/t109.
       |[3:6..3:7): h <= _empty_/t109.h().
       |[3:8..3:9): a <= _empty_/t109.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): c <= _empty_/t109.h().(c)
       |[3:19..3:22): Int => scala/Int#
       |[3:24..3:25): b <= _empty_/t109.h().(b)
       |[3:27..3:30): Int => scala/Int#
       |[3:34..3:35): a => _empty_/t109.h().(a)
       |[3:36..3:37): + => scala/Int#`+`(+4).
       |[3:38..3:39): b => _empty_/t109.h().(b)
       |[3:40..3:41): + => scala/Int#`+`(+4).
       |[3:42..3:43): c => _empty_/t109.h().(c)
       |[4:6..4:7): s <= _empty_/t109.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): h => _empty_/t109.h().
       |[4:13..4:20): h(_)(1) <= local0
       |[4:13..4:20): h(_)(1) <= local1
       |[4:13..4:20): h(_)(1) <= local3
       |[4:13..4:20): h(_)(1) <= local4
       |[4:15..4:16): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder over a curried method with a later argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t109 {
       |  def h(a: Int)(c: Int)(b: Int) = a + b + c
       |  val s: S = h(_)(1)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t109 <= _empty_/t109.
       |[3:6..3:7): h <= _empty_/t109.h().
       |[3:8..3:9): a <= _empty_/t109.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): c <= _empty_/t109.h().(c)
       |[3:19..3:22): Int => scala/Int#
       |[3:24..3:25): b <= _empty_/t109.h().(b)
       |[3:27..3:30): Int => scala/Int#
       |[3:34..3:35): a => _empty_/t109.h().(a)
       |[3:36..3:37): + => scala/Int#`+`(+4).
       |[3:38..3:39): b => _empty_/t109.h().(b)
       |[3:40..3:41): + => scala/Int#`+`(+4).
       |[3:42..3:43): c => _empty_/t109.h().(c)
       |[4:6..4:7): s <= _empty_/t109.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:20): h(_)(1) <= local0
       |[4:13..4:14): h => _empty_/t109.h().
       |[4:15..4:16): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder over a curried method with a later argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t109 {
       |  def h(a: Int)(c: Int)(b: Int) = a + b + c
       |  val s: S = h(_)(1)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t109. => final object t109 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t109.h(). => method h(a: Int)(c: Int)(b: Int): Int
       |  a => _empty_/t109.h().(a)
       |  Int => scala/Int#
       |  c => _empty_/t109.h().(c)
       |  b => _empty_/t109.h().(b)
       |_empty_/t109.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t109.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t109.h().(c) => param c: Int
       |  Int => scala/Int#
       |_empty_/t109.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): T <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder over a method with three parameter lists",
    """|trait U { def u(c: Int): Int }
       |trait T { def g(b: Int): U }
       |trait S { def f(a: Int): T }
       |object t110 {
       |  def h(a: Int)(b: Int)(c: Int) = a + b + c
       |  val s: S = h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): U <= _empty_/U#
       |[0:14..0:15): u <= _empty_/U#u().
       |[0:16..0:17): c <= _empty_/U#u().(c)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): T <= _empty_/T#
       |[1:14..1:15): g <= _empty_/T#g().
       |[1:16..1:17): b <= _empty_/T#g().(b)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): U => _empty_/U#
       |[2:6..2:7): S <= _empty_/S#
       |[2:14..2:15): f <= _empty_/S#f().
       |[2:16..2:17): a <= _empty_/S#f().(a)
       |[2:19..2:22): Int => scala/Int#
       |[2:25..2:26): T => _empty_/T#
       |[3:7..3:11): t110 <= _empty_/t110.
       |[4:6..4:7): h <= _empty_/t110.h().
       |[4:8..4:9): a <= _empty_/t110.h().(a)
       |[4:11..4:14): Int => scala/Int#
       |[4:16..4:17): b <= _empty_/t110.h().(b)
       |[4:19..4:22): Int => scala/Int#
       |[4:24..4:25): c <= _empty_/t110.h().(c)
       |[4:27..4:30): Int => scala/Int#
       |[4:34..4:35): a => _empty_/t110.h().(a)
       |[4:36..4:37): + => scala/Int#`+`(+4).
       |[4:38..4:39): b => _empty_/t110.h().(b)
       |[4:40..4:41): + => scala/Int#`+`(+4).
       |[4:42..4:43): c => _empty_/t110.h().(c)
       |[5:6..5:7): s <= _empty_/t110.s.
       |[5:9..5:10): S => _empty_/S#
       |[5:13..5:14): h => _empty_/t110.h().
       |[5:13..5:17): h(_) <= local0
       |[5:13..5:17): h(_) <= local1
       |[5:13..5:17): h(_) <= local3
       |[5:13..5:17): h(_) <= local4
       |[5:13..5:17): h(_) <= local6
       |[5:13..5:17): h(_) <= local7
       |[5:15..5:16): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder over a method with three parameter lists",
    """|trait U { def u(c: Int): Int }
       |trait T { def g(b: Int): U }
       |trait S { def f(a: Int): T }
       |object t110 {
       |  def h(a: Int)(b: Int)(c: Int) = a + b + c
       |  val s: S = h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): U <= _empty_/U#
       |[0:14..0:15): u <= _empty_/U#u().
       |[0:16..0:17): c <= _empty_/U#u().(c)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): T <= _empty_/T#
       |[1:14..1:15): g <= _empty_/T#g().
       |[1:16..1:17): b <= _empty_/T#g().(b)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): U => _empty_/U#
       |[2:6..2:7): S <= _empty_/S#
       |[2:14..2:15): f <= _empty_/S#f().
       |[2:16..2:17): a <= _empty_/S#f().(a)
       |[2:19..2:22): Int => scala/Int#
       |[2:25..2:26): T => _empty_/T#
       |[3:7..3:11): t110 <= _empty_/t110.
       |[4:6..4:7): h <= _empty_/t110.h().
       |[4:8..4:9): a <= _empty_/t110.h().(a)
       |[4:11..4:14): Int => scala/Int#
       |[4:16..4:17): b <= _empty_/t110.h().(b)
       |[4:19..4:22): Int => scala/Int#
       |[4:24..4:25): c <= _empty_/t110.h().(c)
       |[4:27..4:30): Int => scala/Int#
       |[4:34..4:35): a => _empty_/t110.h().(a)
       |[4:36..4:37): + => scala/Int#`+`(+4).
       |[4:38..4:39): b => _empty_/t110.h().(b)
       |[4:40..4:41): + => scala/Int#`+`(+4).
       |[4:42..4:43): c => _empty_/t110.h().(c)
       |[5:6..5:7): s <= _empty_/t110.s.
       |[5:9..5:10): S => _empty_/S#
       |[5:13..5:17): h(_) <= local0
       |[5:13..5:14): h => _empty_/t110.h().
       |[5:15..5:16): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder over a method with three parameter lists",
    """|trait U { def u(c: Int): Int }
       |trait T { def g(b: Int): U }
       |trait S { def f(a: Int): T }
       |object t110 {
       |  def h(a: Int)(b: Int)(c: Int) = a + b + c
       |  val s: S = h(_)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): U
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |  U => _empty_/U#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/U# => trait U extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/U#u(). => abstract method u(c: Int): Int
       |  c => _empty_/U#u().(c)
       |  Int => scala/Int#
       |_empty_/U#u().(c) => param c: Int
       |  Int => scala/Int#
       |_empty_/t110. => final object t110 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t110.h(). => method h(a: Int)(b: Int)(c: Int): Int
       |  a => _empty_/t110.h().(a)
       |  Int => scala/Int#
       |  b => _empty_/t110.h().(b)
       |  c => _empty_/t110.h().(c)
       |_empty_/t110.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t110.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t110.h().(c) => param c: Int
       |  Int => scala/Int#
       |_empty_/t110.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): T <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): U <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |  U => _empty_/U#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |local6 => synthetic class $anon extends Object with U { +1 decls }
       |  Object => java/lang/Object#
       |  U => _empty_/U#
       |local7 => synthetic method u(c: Int): Int <: _empty_/U#u().
       |  c => local8
       |  Int => scala/Int#
       |local8 => synthetic param c: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t111 {
       |  def use(s: S) = s.f(0)
       |  def k(y: Int) = y
       |  def m = use(this.k(_))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t111 <= _empty_/t111#
       |[1:11..1:11):  <= _empty_/t111#`<init>`().
       |[2:6..2:9): use <= _empty_/t111#use().
       |[2:10..2:11): s <= _empty_/t111#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t111#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): k <= _empty_/t111#k().
       |[3:8..3:9): y <= _empty_/t111#k().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t111#k().(y)
       |[4:6..4:7): m <= _empty_/t111#m().
       |[4:10..4:13): use => _empty_/t111#use().
       |[4:14..4:23): this.k(_) <= local0
       |[4:14..4:23): this.k(_) <= local1
       |[4:19..4:20): k => _empty_/t111#k().
       |[4:21..4:22): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t111 {
       |  def use(s: S) = s.f(0)
       |  def k(y: Int) = y
       |  def m = use(this.k(_))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t111 <= _empty_/t111#
       |[1:11..1:11):  <= _empty_/t111#`<init>`().
       |[2:6..2:9): use <= _empty_/t111#use().
       |[2:10..2:11): s <= _empty_/t111#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t111#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): k <= _empty_/t111#k().
       |[3:8..3:9): y <= _empty_/t111#k().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t111#k().(y)
       |[4:6..4:7): m <= _empty_/t111#m().
       |[4:10..4:13): use => _empty_/t111#use().
       |[4:14..4:23): this.k(_) <= local0
       |[4:19..4:20): k => _empty_/t111#k().
       |[4:21..4:22): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t111 {
       |  def use(s: S) = s.f(0)
       |  def k(y: Int) = y
       |  def m = use(this.k(_))
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t111# => class t111 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t111#`<init>`(). => primary ctor <init>()
       |_empty_/t111#k(). => method k(y: Int): Int
       |  y => _empty_/t111#k().(y)
       |  Int => scala/Int#
       |_empty_/t111#k().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/t111#m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t111#use(). => method use(s: S): Int
       |  s => _empty_/t111#use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t111#use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): Int <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  names(
    "SAM placeholder with a this qualifier",
    """|trait S { def f(a: Int): Int }
       |class t111 {
       |  def use(s: S) = s.f(0)
       |  def k(y: Int) = y
       |  def m = use(this.k(_))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:8..0:8):
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t111 <= _empty_/t111#
       |[1:11..1:11):  <= _empty_/t111#`<init>`().
       |[2:6..2:9): use <= _empty_/t111#use().
       |[2:10..2:11): s <= _empty_/t111#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t111#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): k <= _empty_/t111#k().
       |[3:8..3:9): y <= _empty_/t111#k().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t111#k().(y)
       |[4:6..4:7): m <= _empty_/t111#m().
       |[4:10..4:13): use => _empty_/t111#use().
       |[4:14..4:14):
       |[4:19..4:20): k => _empty_/t111#k().
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder over a curried method with a this qualifier",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |class t112 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = this.h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:6..2:10): t112 <= _empty_/t112#
       |[2:11..2:11):  <= _empty_/t112#`<init>`().
       |[3:6..3:7): h <= _empty_/t112#h().
       |[3:8..3:9): a <= _empty_/t112#h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t112#h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t112#h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t112#h().(b)
       |[4:6..4:7): s <= _empty_/t112#s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:22): this.h(_) <= local0
       |[4:13..4:22): this.h(_) <= local1
       |[4:13..4:22): this.h(_) <= local3
       |[4:13..4:22): this.h(_) <= local4
       |[4:18..4:19): h => _empty_/t112#h().
       |[4:20..4:21): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder over a curried method with a this qualifier",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |class t112 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = this.h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:6..2:10): t112 <= _empty_/t112#
       |[2:11..2:11):  <= _empty_/t112#`<init>`().
       |[3:6..3:7): h <= _empty_/t112#h().
       |[3:8..3:9): a <= _empty_/t112#h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t112#h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t112#h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t112#h().(b)
       |[4:6..4:7): s <= _empty_/t112#s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:22): this.h(_) <= local0
       |[4:18..4:19): h => _empty_/t112#h().
       |[4:20..4:21): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder over a curried method with a this qualifier",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |class t112 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = this.h(_)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t112# => class t112 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t112#`<init>`(). => primary ctor <init>()
       |_empty_/t112#h(). => method h(a: Int)(b: Int): Int
       |  a => _empty_/t112#h().(a)
       |  Int => scala/Int#
       |  b => _empty_/t112#h().(b)
       |_empty_/t112#h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t112#h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t112#s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): T <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  names(
    "SAM placeholder over a curried method with a this qualifier",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |class t112 {
       |  def h(a: Int)(b: Int) = a + b
       |  val s: S = this.h(_)
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:8..0:8):
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:8..1:8):
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:6..2:10): t112 <= _empty_/t112#
       |[2:11..2:11):  <= _empty_/t112#`<init>`().
       |[3:6..3:7): h <= _empty_/t112#h().
       |[3:8..3:9): a <= _empty_/t112#h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t112#h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t112#h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t112#h().(b)
       |[4:6..4:7): s <= _empty_/t112#s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:13):
       |[4:18..4:19): h => _empty_/t112#h().
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder whose body is a method value",
    """|trait T { def g(b: Int): Int }
       |trait K { def h(b: Int): Int }
       |trait S { def f(k: K): T }
       |object t113 {
       |  val s: S = _.h
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): K <= _empty_/K#
       |[1:14..1:15): h <= _empty_/K#h().
       |[1:16..1:17): b <= _empty_/K#h().(b)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:28): Int => scala/Int#
       |[2:6..2:7): S <= _empty_/S#
       |[2:14..2:15): f <= _empty_/S#f().
       |[2:16..2:17): k <= _empty_/S#f().(k)
       |[2:19..2:20): K => _empty_/K#
       |[2:23..2:24): T => _empty_/T#
       |[3:7..3:11): t113 <= _empty_/t113.
       |[4:6..4:7): s <= _empty_/t113.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:14): _ <= local2
       |[4:13..4:16): _.h <= local0
       |[4:13..4:16): _.h <= local1
       |[4:13..4:16): _.h <= local3
       |[4:13..4:16): _.h <= local4
       |[4:15..4:16): h => _empty_/K#h().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder whose body is a method value",
    """|trait T { def g(b: Int): Int }
       |trait K { def h(b: Int): Int }
       |trait S { def f(k: K): T }
       |object t113 {
       |  val s: S = _.h
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): K <= _empty_/K#
       |[1:14..1:15): h <= _empty_/K#h().
       |[1:16..1:17): b <= _empty_/K#h().(b)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:28): Int => scala/Int#
       |[2:6..2:7): S <= _empty_/S#
       |[2:14..2:15): f <= _empty_/S#f().
       |[2:16..2:17): k <= _empty_/S#f().(k)
       |[2:19..2:20): K => _empty_/K#
       |[2:23..2:24): T => _empty_/T#
       |[3:7..3:11): t113 <= _empty_/t113.
       |[4:6..4:7): s <= _empty_/t113.s.
       |[4:9..4:10): S => _empty_/S#
       |[4:13..4:16): _.h <= local0
       |[4:13..4:14): _ <= local2
       |[4:15..4:16): h => _empty_/K#h().
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder whose body is a method value",
    """|trait T { def g(b: Int): Int }
       |trait K { def h(b: Int): Int }
       |trait S { def f(k: K): T }
       |object t113 {
       |  val s: S = _.h
       |}
       |""".stripMargin,
    """|_empty_/K# => trait K extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/K#h(). => abstract method h(b: Int): Int
       |  b => _empty_/K#h().(b)
       |  Int => scala/Int#
       |_empty_/K#h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(k: K): T
       |  k => _empty_/S#f().(k)
       |  K => _empty_/K#
       |  T => _empty_/T#
       |_empty_/S#f().(k) => param k: K
       |  K => _empty_/K#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t113. => final object t113 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t113.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: K): T <: _empty_/S#f().
       |  x$1 => local2
       |  K => _empty_/K#
       |  T => _empty_/T#
       |local2 => synthetic param x$1: K
       |  K => _empty_/K#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder over a curried method with a lifted argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t114 {
       |  def k(a: Int) = a
       |  def h(a: Int, c: Int)(b: Int) = a + b + c
       |  val s: S = h(_, k(1))
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t114 <= _empty_/t114.
       |[3:6..3:7): k <= _empty_/t114.k().
       |[3:8..3:9): a <= _empty_/t114.k().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): a => _empty_/t114.k().(a)
       |[4:6..4:7): h <= _empty_/t114.h().
       |[4:8..4:9): a <= _empty_/t114.h().(a)
       |[4:11..4:14): Int => scala/Int#
       |[4:16..4:17): c <= _empty_/t114.h().(c)
       |[4:19..4:22): Int => scala/Int#
       |[4:24..4:25): b <= _empty_/t114.h().(b)
       |[4:27..4:30): Int => scala/Int#
       |[4:34..4:35): a => _empty_/t114.h().(a)
       |[4:36..4:37): + => scala/Int#`+`(+4).
       |[4:38..4:39): b => _empty_/t114.h().(b)
       |[4:40..4:41): + => scala/Int#`+`(+4).
       |[4:42..4:43): c => _empty_/t114.h().(c)
       |[5:6..5:7): s <= _empty_/t114.s.
       |[5:9..5:10): S => _empty_/S#
       |[5:13..5:14): h => _empty_/t114.h().
       |[5:13..5:23): h(_, k(1)) <= local0
       |[5:13..5:23): h(_, k(1)) <= local1
       |[5:13..5:23): h(_, k(1)) <= local3
       |[5:13..5:23): h(_, k(1)) <= local4
       |[5:15..5:16): _ <= local2
       |[5:18..5:19): k => _empty_/t114.k().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder over a curried method with a lifted argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t114 {
       |  def k(a: Int) = a
       |  def h(a: Int, c: Int)(b: Int) = a + b + c
       |  val s: S = h(_, k(1))
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t114 <= _empty_/t114.
       |[3:6..3:7): k <= _empty_/t114.k().
       |[3:8..3:9): a <= _empty_/t114.k().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): a => _empty_/t114.k().(a)
       |[4:6..4:7): h <= _empty_/t114.h().
       |[4:8..4:9): a <= _empty_/t114.h().(a)
       |[4:11..4:14): Int => scala/Int#
       |[4:16..4:17): c <= _empty_/t114.h().(c)
       |[4:19..4:22): Int => scala/Int#
       |[4:24..4:25): b <= _empty_/t114.h().(b)
       |[4:27..4:30): Int => scala/Int#
       |[4:34..4:35): a => _empty_/t114.h().(a)
       |[4:36..4:37): + => scala/Int#`+`(+4).
       |[4:38..4:39): b => _empty_/t114.h().(b)
       |[4:40..4:41): + => scala/Int#`+`(+4).
       |[4:42..4:43): c => _empty_/t114.h().(c)
       |[5:6..5:7): s <= _empty_/t114.s.
       |[5:9..5:10): S => _empty_/S#
       |[5:13..5:23): h(_, k(1)) <= local0
       |[5:13..5:14): h => _empty_/t114.h().
       |[5:15..5:16): _ <= local2
       |[5:18..5:19): k => _empty_/t114.k().
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder over a curried method with a lifted argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t114 {
       |  def k(a: Int) = a
       |  def h(a: Int, c: Int)(b: Int) = a + b + c
       |  val s: S = h(_, k(1))
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t114. => final object t114 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t114.h(). => method h(a: Int, c: Int)(b: Int): Int
       |  a => _empty_/t114.h().(a)
       |  Int => scala/Int#
       |  c => _empty_/t114.h().(c)
       |  b => _empty_/t114.h().(b)
       |_empty_/t114.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t114.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t114.h().(c) => param c: Int
       |  Int => scala/Int#
       |_empty_/t114.k(). => method k(a: Int): Int
       |  a => _empty_/t114.k().(a)
       |  Int => scala/Int#
       |_empty_/t114.k().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t114.s. => val method s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): T <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder over a curried method as a default argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t115 {
       |  def h(a: Int)(b: Int) = a + b
       |  def m(s: S = h(_)) = s
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t115 <= _empty_/t115.
       |[3:6..3:7): h <= _empty_/t115.h().
       |[3:8..3:9): a <= _empty_/t115.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t115.h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t115.h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t115.h().(b)
       |[4:6..4:7): m <= _empty_/t115.m().
       |[4:8..4:9): s <= _empty_/t115.m().(s)
       |[4:11..4:12): S => _empty_/S#
       |[4:15..4:16): h => _empty_/t115.h().
       |[4:15..4:19): h(_) <= local0
       |[4:15..4:19): h(_) <= local1
       |[4:15..4:19): h(_) <= local3
       |[4:15..4:19): h(_) <= local4
       |[4:17..4:18): _ <= local2
       |[4:23..4:24): s => _empty_/t115.m().(s)
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder over a curried method as a default argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t115 {
       |  def h(a: Int)(b: Int) = a + b
       |  def m(s: S = h(_)) = s
       |}
       |""".stripMargin,
    """|[0:6..0:7): T <= _empty_/T#
       |[0:14..0:15): g <= _empty_/T#g().
       |[0:16..0:17): b <= _empty_/T#g().(b)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): S <= _empty_/S#
       |[1:14..1:15): f <= _empty_/S#f().
       |[1:16..1:17): a <= _empty_/S#f().(a)
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): T => _empty_/T#
       |[2:7..2:11): t115 <= _empty_/t115.
       |[3:6..3:7): h <= _empty_/t115.h().
       |[3:8..3:9): a <= _empty_/t115.h().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): b <= _empty_/t115.h().(b)
       |[3:19..3:22): Int => scala/Int#
       |[3:26..3:27): a => _empty_/t115.h().(a)
       |[3:28..3:29): + => scala/Int#`+`(+4).
       |[3:30..3:31): b => _empty_/t115.h().(b)
       |[4:6..4:7): m <= _empty_/t115.m().
       |[4:8..4:9): s <= _empty_/t115.m().(s)
       |[4:11..4:12): S => _empty_/S#
       |[4:15..4:19): h(_) <= local0
       |[4:15..4:16): h => _empty_/t115.h().
       |[4:17..4:18): _ <= local2
       |[4:23..4:24): s => _empty_/t115.m().(s)
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder over a curried method as a default argument",
    """|trait T { def g(b: Int): Int }
       |trait S { def f(a: Int): T }
       |object t115 {
       |  def h(a: Int)(b: Int) = a + b
       |  def m(s: S = h(_)) = s
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): T
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |  T => _empty_/T#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/T# => trait T extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/T#g(). => abstract method g(b: Int): Int
       |  b => _empty_/T#g().(b)
       |  Int => scala/Int#
       |_empty_/T#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t115. => final object t115 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t115.h(). => method h(a: Int)(b: Int): Int
       |  a => _empty_/t115.h().(a)
       |  Int => scala/Int#
       |  b => _empty_/t115.h().(b)
       |_empty_/t115.h().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t115.h().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t115.m$default$1(). => synthetic method m$default$1: S @uncheckedVariance
       |  S => _empty_/S#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/t115.m(). => method m(s: S): S
       |  s => _empty_/t115.m().(s)
       |  S => _empty_/S#
       |_empty_/t115.m().(s) => default param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): T <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |  T => _empty_/T#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with T { +1 decls }
       |  Object => java/lang/Object#
       |  T => _empty_/T#
       |local4 => synthetic method g(b: Int): Int <: _empty_/T#g().
       |  b => local5
       |  Int => scala/Int#
       |local5 => synthetic param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder with a super qualifier",
    """|trait S { def f(a: Int): Int }
       |class B { def k(y: Int) = y }
       |class t116 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(super.k(_))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): B <= _empty_/B#
       |[1:8..1:8):  <= _empty_/B#`<init>`().
       |[1:14..1:15): k <= _empty_/B#k().
       |[1:16..1:17): y <= _empty_/B#k().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/B#k().(y)
       |[2:6..2:10): t116 <= _empty_/t116#
       |[2:11..2:11):  <= _empty_/t116#`<init>`().
       |[2:19..2:20): B => _empty_/B#
       |[2:20..2:20):  => _empty_/B#`<init>`().
       |[3:6..3:9): use <= _empty_/t116#use().
       |[3:10..3:11): s <= _empty_/t116#use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t116#use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t116#m().
       |[4:10..4:13): use => _empty_/t116#use().
       |[4:14..4:14):  => _empty_/t116#
       |[4:14..4:24): super.k(_) <= local0
       |[4:14..4:24): super.k(_) <= local1
       |[4:19..4:19):  => _empty_/B#
       |[4:20..4:21): k => _empty_/B#k().
       |[4:22..4:23): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder with a super qualifier",
    """|trait S { def f(a: Int): Int }
       |class B { def k(y: Int) = y }
       |class t116 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(super.k(_))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): B <= _empty_/B#
       |[1:8..1:8):  <= _empty_/B#`<init>`().
       |[1:14..1:15): k <= _empty_/B#k().
       |[1:16..1:17): y <= _empty_/B#k().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/B#k().(y)
       |[2:6..2:10): t116 <= _empty_/t116#
       |[2:11..2:11):  <= _empty_/t116#`<init>`().
       |[2:19..2:20): B => _empty_/B#
       |[2:20..2:20):  => _empty_/B#`<init>`().
       |[3:6..3:9): use <= _empty_/t116#use().
       |[3:10..3:11): s <= _empty_/t116#use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t116#use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t116#m().
       |[4:10..4:13): use => _empty_/t116#use().
       |[4:14..4:24): super.k(_) <= local0
       |[4:14..4:14):  => _empty_/t116#
       |[4:19..4:19):  => _empty_/B#
       |[4:20..4:21): k => _empty_/B#k().
       |[4:22..4:23): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder with a super qualifier",
    """|trait S { def f(a: Int): Int }
       |class B { def k(y: Int) = y }
       |class t116 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(super.k(_))
       |}
       |""".stripMargin,
    """|_empty_/B# => class B extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/B#`<init>`(). => primary ctor <init>()
       |_empty_/B#k(). => method k(y: Int): Int
       |  y => _empty_/B#k().(y)
       |  Int => scala/Int#
       |_empty_/B#k().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t116# => class t116 extends B { +3 decls }
       |  B => _empty_/B#
       |_empty_/t116#`<init>`(). => primary ctor <init>()
       |_empty_/t116#m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t116#use(). => method use(s: S): Int
       |  s => _empty_/t116#use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t116#use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): Int <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  names(
    "SAM placeholder with a super qualifier",
    """|trait S { def f(a: Int): Int }
       |class B { def k(y: Int) = y }
       |class t116 extends B {
       |  def use(s: S) = s.f(0)
       |  def m = use(super.k(_))
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:8..0:8):
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): B <= _empty_/B#
       |[1:8..1:8):  <= _empty_/B#`<init>`().
       |[1:14..1:15): k <= _empty_/B#k().
       |[1:16..1:17): y <= _empty_/B#k().(y)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): y => _empty_/B#k().(y)
       |[2:6..2:10): t116 <= _empty_/t116#
       |[2:11..2:11):  <= _empty_/t116#`<init>`().
       |[2:19..2:20): B => _empty_/B#
       |[2:20..2:20):  => _empty_/B#`<init>`().
       |[3:6..3:9): use <= _empty_/t116#use().
       |[3:10..3:11): s <= _empty_/t116#use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t116#use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t116#m().
       |[4:10..4:13): use => _empty_/t116#use().
       |[4:14..4:14):  => _empty_/t116#
       |[4:19..4:19):  => _empty_/B#
       |[4:20..4:21): k => _empty_/B#k().
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder with an infix this",
    """|trait S { def f(a: Int): Int }
       |class t117 {
       |  def use(s: S) = s.f(0)
       |  def +(a: Int) = a
       |  def m = use(this + _)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t117 <= _empty_/t117#
       |[1:11..1:11):  <= _empty_/t117#`<init>`().
       |[2:6..2:9): use <= _empty_/t117#use().
       |[2:10..2:11): s <= _empty_/t117#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t117#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): + <= _empty_/t117#`+`().
       |[3:8..3:9): a <= _empty_/t117#`+`().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): a => _empty_/t117#`+`().(a)
       |[4:6..4:7): m <= _empty_/t117#m().
       |[4:10..4:13): use => _empty_/t117#use().
       |[4:14..4:22): this + _ <= local0
       |[4:14..4:22): this + _ <= local1
       |[4:19..4:20): + => _empty_/t117#`+`().
       |[4:21..4:22): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder with an infix this",
    """|trait S { def f(a: Int): Int }
       |class t117 {
       |  def use(s: S) = s.f(0)
       |  def +(a: Int) = a
       |  def m = use(this + _)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t117 <= _empty_/t117#
       |[1:11..1:11):  <= _empty_/t117#`<init>`().
       |[2:6..2:9): use <= _empty_/t117#use().
       |[2:10..2:11): s <= _empty_/t117#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t117#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): + <= _empty_/t117#`+`().
       |[3:8..3:9): a <= _empty_/t117#`+`().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): a => _empty_/t117#`+`().(a)
       |[4:6..4:7): m <= _empty_/t117#m().
       |[4:10..4:13): use => _empty_/t117#use().
       |[4:14..4:22): this + _ <= local0
       |[4:19..4:20): + => _empty_/t117#`+`().
       |[4:21..4:22): _ <= local2
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder with an infix this",
    """|trait S { def f(a: Int): Int }
       |class t117 {
       |  def use(s: S) = s.f(0)
       |  def +(a: Int) = a
       |  def m = use(this + _)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t117# => class t117 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t117#`+`(). => method +(a: Int): Int
       |  a => _empty_/t117#`+`().(a)
       |  Int => scala/Int#
       |_empty_/t117#`+`().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t117#`<init>`(). => primary ctor <init>()
       |_empty_/t117#m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t117#use(). => method use(s: S): Int
       |  s => _empty_/t117#use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t117#use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): Int <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  names(
    "SAM placeholder with an infix this",
    """|trait S { def f(a: Int): Int }
       |class t117 {
       |  def use(s: S) = s.f(0)
       |  def +(a: Int) = a
       |  def m = use(this + _)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:8..0:8):
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t117 <= _empty_/t117#
       |[1:11..1:11):  <= _empty_/t117#`<init>`().
       |[2:6..2:9): use <= _empty_/t117#use().
       |[2:10..2:11): s <= _empty_/t117#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t117#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): + <= _empty_/t117#`+`().
       |[3:8..3:9): a <= _empty_/t117#`+`().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): a => _empty_/t117#`+`().(a)
       |[4:6..4:7): m <= _empty_/t117#m().
       |[4:10..4:13): use => _empty_/t117#use().
       |[4:14..4:14):
       |[4:19..4:20): + => _empty_/t117#`+`().
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder with an infix this and a comment",
    """|trait S { def f(a: Int): Int }
       |class t118 {
       |  def use(s: S) = s.f(0)
       |  def +(a: Int) = a
       |  def m = use(this /* c */ + _)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t118 <= _empty_/t118#
       |[1:11..1:11):  <= _empty_/t118#`<init>`().
       |[2:6..2:9): use <= _empty_/t118#use().
       |[2:10..2:11): s <= _empty_/t118#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t118#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): + <= _empty_/t118#`+`().
       |[3:8..3:9): a <= _empty_/t118#`+`().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): a => _empty_/t118#`+`().(a)
       |[4:6..4:7): m <= _empty_/t118#m().
       |[4:10..4:13): use => _empty_/t118#use().
       |[4:14..4:30): this /* c */ + _ <= local0
       |[4:14..4:30): this /* c */ + _ <= local1
       |[4:27..4:28): + => _empty_/t118#`+`().
       |[4:29..4:30): _ <= local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder with an infix this and a comment",
    """|trait S { def f(a: Int): Int }
       |class t118 {
       |  def use(s: S) = s.f(0)
       |  def +(a: Int) = a
       |  def m = use(this /* c */ + _)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:10): t118 <= _empty_/t118#
       |[1:11..1:11):  <= _empty_/t118#`<init>`().
       |[2:6..2:9): use <= _empty_/t118#use().
       |[2:10..2:11): s <= _empty_/t118#use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t118#use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): + <= _empty_/t118#`+`().
       |[3:8..3:9): a <= _empty_/t118#`+`().(a)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): a => _empty_/t118#`+`().(a)
       |[4:6..4:7): m <= _empty_/t118#m().
       |[4:10..4:13): use => _empty_/t118#use().
       |[4:14..4:30): this /* c */ + _ <= local0
       |[4:27..4:28): + => _empty_/t118#`+`().
       |[4:29..4:30): _ <= local2
       |""".stripMargin,
  )

  targeted(
    """|trait S { def f(a: Int): Int }
       |abstract class A { def f(a: Int): Int }
       |object t73 {
       |  def use(s: S) = s.f(0)
       |  def useA(s: A) = s.f(0)
       |  def g(y: Int) = y
       |  def m = { class K { val v = use(x => x) }; new K }
       |  use(_ + 1)
       |  use((x: Int) => x - 1)
       |  use { x => x }
       |  use(g)
       |  use(g _)
       |  useA(x => x)
       |  use(x => use(y => x + y))
       |  val r: Runnable = () => ()
       |}
       |""".stripMargin,
    (doc: s.TextDocument) => {
      def definitions(sym: String) = doc.occurrences
        .filter(o => o.symbol == sym && o.role.isDefinition).flatMap(_.range)
        .map(r => s"[${r.startLine}:${r.startCharacter}..${r.endLine}:${r.endCharacter})")
      val infos = doc.symbols.map(_.symbol).toSet
      val classes = doc.symbols.filter(_.displayName == "$anon")
      val decls = classes.sortBy(_.symbol.stripPrefix("local").toInt).map { i =>
        val sig = i.signature.asInstanceOf[s.ClassSignature]
        val members = sig.declarations.toList.flatMap(_.symlinks).map { d =>
          val info = if (infos.contains(d)) "with" else "without"
          s"$d $info information at ${definitions(d).mkString(", ")}"
        }
        s"${i.symbol} at ${definitions(i.symbol).mkString(", ")} declares ${members.mkString(", ")}"
      }
      assertNoDiff(
        decls.mkString("\n"),
        """|local3 at [6:34..6:40) declares local4 with information at [6:34..6:40)
           |local6 at [7:6..7:11) declares local7 with information at [7:6..7:11)
           |local9 at [8:6..8:23) declares local10 with information at [8:6..8:23)
           |local12 at [9:8..9:14) declares local13 with information at [9:8..9:14)
           |local15 at [10:6..10:7) declares local16 with information at [10:6..10:7)
           |local18 at [11:6..11:7) declares local19 with information at [11:6..11:7)
           |local21 at [12:7..12:13) declares local22 with information at [12:7..12:13)
           |local24 at [13:6..13:26) declares local25 with information at [13:6..13:26)
           |local27 at [13:15..13:25) declares local28 with information at [13:15..13:25)
           |local30 at [14:20..14:28) declares local31 with information at [14:20..14:28)
           |""".stripMargin,
      )
    },
  )

  occurrences(
    "SAM closures of a for loop with a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t149 {
       |  val m = new M(1)
       |  for (x <- m if x > 0) println(x)
       |}
       |""".stripMargin,
    """|[0:6..0:7): F <= _empty_/F#
       |[0:8..0:9): A <= _empty_/F#[A]
       |[0:11..0:12): B <= _empty_/F#[B]
       |[0:20..0:21): f <= _empty_/F#f().
       |[0:22..0:23): a <= _empty_/F#f().(a)
       |[0:25..0:26): A => _empty_/F#[A]
       |[0:29..0:30): B => _empty_/F#[B]
       |[1:6..1:7): M <= _empty_/M#
       |[1:8..1:9): A <= _empty_/M#[A]
       |[1:10..1:10):  <= _empty_/M#`<init>`().
       |[1:11..1:12): a <= _empty_/M#`<init>`().(a)
       |[1:11..1:12): a <= _empty_/M#a.
       |[1:14..1:15): A => _empty_/M#[A]
       |[2:6..2:9): map <= _empty_/M#map().
       |[2:10..2:11): B <= _empty_/M#map().[B]
       |[2:13..2:14): g <= _empty_/M#map().(g)
       |[2:16..2:17): F => _empty_/F#
       |[2:18..2:19): A => _empty_/M#[A]
       |[2:21..2:22): B => _empty_/M#map().[B]
       |[2:31..2:32): M => _empty_/M#
       |[2:32..2:32):  => _empty_/M#`<init>`().
       |[2:33..2:34): g => _empty_/M#map().(g)
       |[2:35..2:36): f => _empty_/F#f().
       |[2:37..2:38): a => _empty_/M#a.
       |[3:6..3:13): flatMap <= _empty_/M#flatMap().
       |[3:14..3:15): B <= _empty_/M#flatMap().[B]
       |[3:17..3:18): g <= _empty_/M#flatMap().(g)
       |[3:20..3:21): F => _empty_/F#
       |[3:22..3:23): A => _empty_/M#[A]
       |[3:25..3:26): M => _empty_/M#
       |[3:27..3:28): B => _empty_/M#flatMap().[B]
       |[3:34..3:35): g => _empty_/M#flatMap().(g)
       |[3:36..3:37): f => _empty_/F#f().
       |[3:38..3:39): a => _empty_/M#a.
       |[4:6..4:16): withFilter <= _empty_/M#withFilter().
       |[4:17..4:18): g <= _empty_/M#withFilter().(g)
       |[4:20..4:21): F => _empty_/F#
       |[4:22..4:23): A => _empty_/M#[A]
       |[4:25..4:32): Boolean => scala/Boolean#
       |[5:6..5:13): foreach <= _empty_/M#foreach().
       |[5:14..5:15): g <= _empty_/M#foreach().(g)
       |[5:17..5:18): F => _empty_/F#
       |[5:19..5:20): A => _empty_/M#[A]
       |[5:22..5:26): Unit => scala/Unit#
       |[5:31..5:32): g => _empty_/M#foreach().(g)
       |[5:33..5:34): f => _empty_/F#f().
       |[5:35..5:36): a => _empty_/M#a.
       |[7:7..7:11): t149 <= _empty_/t149.
       |[8:6..8:7): m <= _empty_/t149.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:7..9:8): x <= local0
       |[9:7..9:13): x <- m <= local3
       |[9:7..9:13): x <- m <= local4
       |[9:12..9:13): m => _empty_/t149.m.
       |[9:14..9:22): if x > 0 <= local1
       |[9:14..9:22): if x > 0 <= local2
       |[9:17..9:18): x => local0
       |[9:19..9:20): > => scala/Int#`>`(+3).
       |[9:24..9:31): println => scala/Predef.println(+1).
       |[9:32..9:33): x => local0
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for loop with a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t149 {
       |  val m = new M(1)
       |  for (x <- m if x > 0) println(x)
       |}
       |""".stripMargin,
    """|[0:6..0:7): F <= _empty_/F#
       |[0:8..0:9): A <= _empty_/F#[A]
       |[0:11..0:12): B <= _empty_/F#[B]
       |[0:20..0:21): f <= _empty_/F#f().
       |[0:22..0:23): a <= _empty_/F#f().(a)
       |[0:25..0:26): A => _empty_/F#[A]
       |[0:29..0:30): B => _empty_/F#[B]
       |[1:6..1:7): M <= _empty_/M#
       |[1:8..1:9): A <= _empty_/M#[A]
       |[1:10..1:10):  <= _empty_/M#`<init>`().
       |[1:11..1:12): a <= _empty_/M#`<init>`().(a)
       |[1:14..1:15): A => _empty_/M#[A]
       |[2:6..2:9): map <= _empty_/M#map().
       |[2:10..2:11): B <= _empty_/M#map().[B]
       |[2:13..2:14): g <= _empty_/M#map().(g)
       |[2:16..2:17): F => _empty_/F#
       |[2:18..2:19): A => _empty_/M#[A]
       |[2:21..2:22): B => _empty_/M#map().[B]
       |[2:31..2:32): M => _empty_/M#
       |[2:32..2:32):  => _empty_/M#`<init>`().
       |[2:33..2:34): g => _empty_/M#map().(g)
       |[2:35..2:36): f => _empty_/F#f().
       |[2:37..2:38): a => _empty_/M#a.
       |[3:6..3:13): flatMap <= _empty_/M#flatMap().
       |[3:14..3:15): B <= _empty_/M#flatMap().[B]
       |[3:17..3:18): g <= _empty_/M#flatMap().(g)
       |[3:20..3:21): F => _empty_/F#
       |[3:22..3:23): A => _empty_/M#[A]
       |[3:25..3:26): M => _empty_/M#
       |[3:27..3:28): B => _empty_/M#flatMap().[B]
       |[3:34..3:35): g => _empty_/M#flatMap().(g)
       |[3:36..3:37): f => _empty_/F#f().
       |[3:38..3:39): a => _empty_/M#a.
       |[4:6..4:16): withFilter <= _empty_/M#withFilter().
       |[4:17..4:18): g <= _empty_/M#withFilter().(g)
       |[4:20..4:21): F => _empty_/F#
       |[4:22..4:23): A => _empty_/M#[A]
       |[4:25..4:32): Boolean => scala/Boolean#
       |[5:6..5:13): foreach <= _empty_/M#foreach().
       |[5:14..5:15): g <= _empty_/M#foreach().(g)
       |[5:17..5:18): F => _empty_/F#
       |[5:19..5:20): A => _empty_/M#[A]
       |[5:22..5:26): Unit => scala/Unit#
       |[5:31..5:32): g => _empty_/M#foreach().(g)
       |[5:33..5:34): f => _empty_/F#f().
       |[5:35..5:36): a => _empty_/M#a.
       |[7:7..7:11): t149 <= _empty_/t149.
       |[8:6..8:7): m <= _empty_/t149.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:7..9:13): x <- m <= local3
       |[9:7..9:8): x <= local0
       |[9:12..9:13): m => _empty_/t149.m.
       |[9:14..9:22): if x > 0 <= local1
       |[9:17..9:18): x => local0
       |[9:19..9:20): > => scala/Int#`>`(+3).
       |[9:24..9:31): println => scala/Predef.println(+1).
       |[9:32..9:33): x => local0
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for loop with a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t149 {
       |  val m = new M(1)
       |  for (x <- m if x > 0) println(x)
       |}
       |""".stripMargin,
    """|_empty_/F# => trait F[A, B] extends AnyRef { +1 decls }
       |  A => _empty_/F#[A]
       |  B => _empty_/F#[B]
       |  AnyRef => scala/AnyRef#
       |_empty_/F#[A] => typeparam A
       |_empty_/F#[B] => typeparam B
       |_empty_/F#f(). => abstract method f(a: A): B
       |  a => _empty_/F#f().(a)
       |  A => _empty_/F#[A]
       |  B => _empty_/F#[B]
       |_empty_/F#f().(a) => param a: A
       |  A => _empty_/F#[A]
       |_empty_/M# => class M[A] extends AnyRef { +6 decls }
       |  A => _empty_/M#[A]
       |  AnyRef => scala/AnyRef#
       |_empty_/M#[A] => typeparam A
       |_empty_/M#`<init>`(). => primary ctor <init>(a: A)
       |  a => _empty_/M#`<init>`().(a)
       |  A => _empty_/M#[A]
       |_empty_/M#`<init>`().(a) => param a: A
       |  A => _empty_/M#[A]
       |_empty_/M#a. => private[this] val method a: A
       |  A => _empty_/M#[A]
       |_empty_/M#flatMap(). => method flatMap[B](g: F[A, M[B]]): M[B]
       |  B => _empty_/M#flatMap().[B]
       |  g => _empty_/M#flatMap().(g)
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  M => _empty_/M#
       |_empty_/M#flatMap().(g) => param g: F[A, M[B]]
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  M => _empty_/M#
       |  B => _empty_/M#flatMap().[B]
       |_empty_/M#flatMap().[B] => typeparam B
       |_empty_/M#foreach(). => method foreach(g: F[A, Unit]): Unit
       |  g => _empty_/M#foreach().(g)
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  Unit => scala/Unit#
       |_empty_/M#foreach().(g) => param g: F[A, Unit]
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  Unit => scala/Unit#
       |_empty_/M#map(). => method map[B](g: F[A, B]): M[B]
       |  B => _empty_/M#map().[B]
       |  g => _empty_/M#map().(g)
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  M => _empty_/M#
       |_empty_/M#map().(g) => param g: F[A, B]
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  B => _empty_/M#map().[B]
       |_empty_/M#map().[B] => typeparam B
       |_empty_/M#withFilter(). => method withFilter(g: F[A, Boolean]): M[A]
       |  g => _empty_/M#withFilter().(g)
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  Boolean => scala/Boolean#
       |  M => _empty_/M#
       |_empty_/M#withFilter().(g) => param g: F[A, Boolean]
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  Boolean => scala/Boolean#
       |_empty_/t149. => final object t149 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t149.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local2 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic class $anon extends Object with F[Int, Unit] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local4 => synthetic method f(x: Int): Unit <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for loop over Option and a SAM type",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t162 {
       |  val m = new M(1)
       |  for (x <- Option(1); y <- m) println(x + y)
       |}
       |""".stripMargin,
    """|[0:6..0:7): F <= _empty_/F#
       |[0:8..0:9): A <= _empty_/F#[A]
       |[0:11..0:12): B <= _empty_/F#[B]
       |[0:20..0:21): f <= _empty_/F#f().
       |[0:22..0:23): a <= _empty_/F#f().(a)
       |[0:25..0:26): A => _empty_/F#[A]
       |[0:29..0:30): B => _empty_/F#[B]
       |[1:6..1:7): M <= _empty_/M#
       |[1:8..1:9): A <= _empty_/M#[A]
       |[1:10..1:10):  <= _empty_/M#`<init>`().
       |[1:11..1:12): a <= _empty_/M#`<init>`().(a)
       |[1:11..1:12): a <= _empty_/M#a.
       |[1:14..1:15): A => _empty_/M#[A]
       |[2:6..2:9): map <= _empty_/M#map().
       |[2:10..2:11): B <= _empty_/M#map().[B]
       |[2:13..2:14): g <= _empty_/M#map().(g)
       |[2:16..2:17): F => _empty_/F#
       |[2:18..2:19): A => _empty_/M#[A]
       |[2:21..2:22): B => _empty_/M#map().[B]
       |[2:31..2:32): M => _empty_/M#
       |[2:32..2:32):  => _empty_/M#`<init>`().
       |[2:33..2:34): g => _empty_/M#map().(g)
       |[2:35..2:36): f => _empty_/F#f().
       |[2:37..2:38): a => _empty_/M#a.
       |[3:6..3:13): flatMap <= _empty_/M#flatMap().
       |[3:14..3:15): B <= _empty_/M#flatMap().[B]
       |[3:17..3:18): g <= _empty_/M#flatMap().(g)
       |[3:20..3:21): F => _empty_/F#
       |[3:22..3:23): A => _empty_/M#[A]
       |[3:25..3:26): M => _empty_/M#
       |[3:27..3:28): B => _empty_/M#flatMap().[B]
       |[3:34..3:35): g => _empty_/M#flatMap().(g)
       |[3:36..3:37): f => _empty_/F#f().
       |[3:38..3:39): a => _empty_/M#a.
       |[4:6..4:16): withFilter <= _empty_/M#withFilter().
       |[4:17..4:18): g <= _empty_/M#withFilter().(g)
       |[4:20..4:21): F => _empty_/F#
       |[4:22..4:23): A => _empty_/M#[A]
       |[4:25..4:32): Boolean => scala/Boolean#
       |[5:6..5:13): foreach <= _empty_/M#foreach().
       |[5:14..5:15): g <= _empty_/M#foreach().(g)
       |[5:17..5:18): F => _empty_/F#
       |[5:19..5:20): A => _empty_/M#[A]
       |[5:22..5:26): Unit => scala/Unit#
       |[5:31..5:32): g => _empty_/M#foreach().(g)
       |[5:33..5:34): f => _empty_/F#f().
       |[5:35..5:36): a => _empty_/M#a.
       |[7:7..7:11): t162 <= _empty_/t162.
       |[8:6..8:7): m <= _empty_/t162.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:7..9:8): x <= local0
       |[9:12..9:18): Option => scala/Option.
       |[9:23..9:24): y <= local1
       |[9:23..9:29): y <- m <= local2
       |[9:23..9:29): y <- m <= local3
       |[9:28..9:29): m => _empty_/t162.m.
       |[9:31..9:38): println => scala/Predef.println(+1).
       |[9:39..9:40): x => local0
       |[9:41..9:42): + => scala/Int#`+`(+4).
       |[9:43..9:44): y => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for loop over Option and a SAM type",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t162 {
       |  val m = new M(1)
       |  for (x <- Option(1); y <- m) println(x + y)
       |}
       |""".stripMargin,
    """|[0:6..0:7): F <= _empty_/F#
       |[0:8..0:9): A <= _empty_/F#[A]
       |[0:11..0:12): B <= _empty_/F#[B]
       |[0:20..0:21): f <= _empty_/F#f().
       |[0:22..0:23): a <= _empty_/F#f().(a)
       |[0:25..0:26): A => _empty_/F#[A]
       |[0:29..0:30): B => _empty_/F#[B]
       |[1:6..1:7): M <= _empty_/M#
       |[1:8..1:9): A <= _empty_/M#[A]
       |[1:10..1:10):  <= _empty_/M#`<init>`().
       |[1:11..1:12): a <= _empty_/M#`<init>`().(a)
       |[1:14..1:15): A => _empty_/M#[A]
       |[2:6..2:9): map <= _empty_/M#map().
       |[2:10..2:11): B <= _empty_/M#map().[B]
       |[2:13..2:14): g <= _empty_/M#map().(g)
       |[2:16..2:17): F => _empty_/F#
       |[2:18..2:19): A => _empty_/M#[A]
       |[2:21..2:22): B => _empty_/M#map().[B]
       |[2:31..2:32): M => _empty_/M#
       |[2:32..2:32):  => _empty_/M#`<init>`().
       |[2:33..2:34): g => _empty_/M#map().(g)
       |[2:35..2:36): f => _empty_/F#f().
       |[2:37..2:38): a => _empty_/M#a.
       |[3:6..3:13): flatMap <= _empty_/M#flatMap().
       |[3:14..3:15): B <= _empty_/M#flatMap().[B]
       |[3:17..3:18): g <= _empty_/M#flatMap().(g)
       |[3:20..3:21): F => _empty_/F#
       |[3:22..3:23): A => _empty_/M#[A]
       |[3:25..3:26): M => _empty_/M#
       |[3:27..3:28): B => _empty_/M#flatMap().[B]
       |[3:34..3:35): g => _empty_/M#flatMap().(g)
       |[3:36..3:37): f => _empty_/F#f().
       |[3:38..3:39): a => _empty_/M#a.
       |[4:6..4:16): withFilter <= _empty_/M#withFilter().
       |[4:17..4:18): g <= _empty_/M#withFilter().(g)
       |[4:20..4:21): F => _empty_/F#
       |[4:22..4:23): A => _empty_/M#[A]
       |[4:25..4:32): Boolean => scala/Boolean#
       |[5:6..5:13): foreach <= _empty_/M#foreach().
       |[5:14..5:15): g <= _empty_/M#foreach().(g)
       |[5:17..5:18): F => _empty_/F#
       |[5:19..5:20): A => _empty_/M#[A]
       |[5:22..5:26): Unit => scala/Unit#
       |[5:31..5:32): g => _empty_/M#foreach().(g)
       |[5:33..5:34): f => _empty_/F#f().
       |[5:35..5:36): a => _empty_/M#a.
       |[7:7..7:11): t162 <= _empty_/t162.
       |[8:6..8:7): m <= _empty_/t162.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:7..9:8): x <= local0
       |[9:12..9:18): Option => scala/Option.
       |[9:23..9:29): y <- m <= local2
       |[9:23..9:24): y <= local1
       |[9:28..9:29): m => _empty_/t162.m.
       |[9:31..9:38): println => scala/Predef.println(+1).
       |[9:39..9:40): x => local0
       |[9:41..9:42): + => scala/Int#`+`(+4).
       |[9:43..9:44): y => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for loop over Option and a SAM type",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t162 {
       |  val m = new M(1)
       |  for (x <- Option(1); y <- m) println(x + y)
       |}
       |""".stripMargin,
    """|_empty_/F# => trait F[A, B] extends AnyRef { +1 decls }
       |  A => _empty_/F#[A]
       |  B => _empty_/F#[B]
       |  AnyRef => scala/AnyRef#
       |_empty_/F#[A] => typeparam A
       |_empty_/F#[B] => typeparam B
       |_empty_/F#f(). => abstract method f(a: A): B
       |  a => _empty_/F#f().(a)
       |  A => _empty_/F#[A]
       |  B => _empty_/F#[B]
       |_empty_/F#f().(a) => param a: A
       |  A => _empty_/F#[A]
       |_empty_/M# => class M[A] extends AnyRef { +6 decls }
       |  A => _empty_/M#[A]
       |  AnyRef => scala/AnyRef#
       |_empty_/M#[A] => typeparam A
       |_empty_/M#`<init>`(). => primary ctor <init>(a: A)
       |  a => _empty_/M#`<init>`().(a)
       |  A => _empty_/M#[A]
       |_empty_/M#`<init>`().(a) => param a: A
       |  A => _empty_/M#[A]
       |_empty_/M#a. => private[this] val method a: A
       |  A => _empty_/M#[A]
       |_empty_/M#flatMap(). => method flatMap[B](g: F[A, M[B]]): M[B]
       |  B => _empty_/M#flatMap().[B]
       |  g => _empty_/M#flatMap().(g)
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  M => _empty_/M#
       |_empty_/M#flatMap().(g) => param g: F[A, M[B]]
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  M => _empty_/M#
       |  B => _empty_/M#flatMap().[B]
       |_empty_/M#flatMap().[B] => typeparam B
       |_empty_/M#foreach(). => method foreach(g: F[A, Unit]): Unit
       |  g => _empty_/M#foreach().(g)
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  Unit => scala/Unit#
       |_empty_/M#foreach().(g) => param g: F[A, Unit]
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  Unit => scala/Unit#
       |_empty_/M#map(). => method map[B](g: F[A, B]): M[B]
       |  B => _empty_/M#map().[B]
       |  g => _empty_/M#map().(g)
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  M => _empty_/M#
       |_empty_/M#map().(g) => param g: F[A, B]
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  B => _empty_/M#map().[B]
       |_empty_/M#map().[B] => typeparam B
       |_empty_/M#withFilter(). => method withFilter(g: F[A, Boolean]): M[A]
       |  g => _empty_/M#withFilter().(g)
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  Boolean => scala/Boolean#
       |  M => _empty_/M#
       |_empty_/M#withFilter().(g) => param g: F[A, Boolean]
       |  F => _empty_/F#
       |  A => _empty_/M#[A]
       |  Boolean => scala/Boolean#
       |_empty_/t162. => final object t162 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t162.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Unit] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local3 => synthetic method f(y: Int): Unit <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda as a constructor argument",
    """|trait S { def f(a: Int): Int }
       |class C(s: S)
       |object t132 {
       |  val c = new C(x => x)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): C <= _empty_/C#
       |[1:7..1:7):  <= _empty_/C#`<init>`().
       |[1:8..1:9): s <= _empty_/C#`<init>`().(s)
       |[1:8..1:9): s <= _empty_/C#s.
       |[1:11..1:12): S => _empty_/S#
       |[2:7..2:11): t132 <= _empty_/t132.
       |[3:6..3:7): c <= _empty_/t132.c.
       |[3:14..3:15): C => _empty_/C#
       |[3:15..3:15):  => _empty_/C#`<init>`().
       |[3:16..3:17): x <= local2
       |[3:16..3:22): x => x <= local0
       |[3:16..3:22): x => x <= local1
       |[3:21..3:22): x => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda as a constructor argument",
    """|trait S { def f(a: Int): Int }
       |class C(s: S)
       |object t132 {
       |  val c = new C(x => x)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): C <= _empty_/C#
       |[1:7..1:7):  <= _empty_/C#`<init>`().
       |[1:8..1:9): s <= _empty_/C#`<init>`().(s)
       |[1:11..1:12): S => _empty_/S#
       |[2:7..2:11): t132 <= _empty_/t132.
       |[3:6..3:7): c <= _empty_/t132.c.
       |[3:14..3:15): C => _empty_/C#
       |[3:15..3:15):  => _empty_/C#`<init>`().
       |[3:16..3:22): x => x <= local0
       |[3:16..3:17): x <= local2
       |[3:21..3:22): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda as a constructor argument",
    """|trait S { def f(a: Int): Int }
       |class C(s: S)
       |object t132 {
       |  val c = new C(x => x)
       |}
       |""".stripMargin,
    """|_empty_/C# => class C extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/C#`<init>`(). => primary ctor <init>(s: S)
       |  s => _empty_/C#`<init>`().(s)
       |  S => _empty_/S#
       |_empty_/C#`<init>`().(s) => param s: S
       |  S => _empty_/S#
       |_empty_/C#s. => private[this] val method s: S
       |  S => _empty_/S#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t132. => final object t132 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t132.c. => val method c: C
       |  C => _empty_/C#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda as a superclass argument",
    """|trait S { def f(a: Int): Int }
       |class C(s: S)
       |class t133 extends C(x => x)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): C <= _empty_/C#
       |[1:7..1:7):  <= _empty_/C#`<init>`().
       |[1:8..1:9): s <= _empty_/C#`<init>`().(s)
       |[1:8..1:9): s <= _empty_/C#s.
       |[1:11..1:12): S => _empty_/S#
       |[2:6..2:10): t133 <= _empty_/t133#
       |[2:11..2:11):  <= _empty_/t133#`<init>`().
       |[2:19..2:20): C => _empty_/C#
       |[2:20..2:20):  => _empty_/C#`<init>`().
       |[2:21..2:22): x <= local2
       |[2:21..2:27): x => x <= local0
       |[2:21..2:27): x => x <= local1
       |[2:26..2:27): x => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda as a superclass argument",
    """|trait S { def f(a: Int): Int }
       |class C(s: S)
       |class t133 extends C(x => x)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): C <= _empty_/C#
       |[1:7..1:7):  <= _empty_/C#`<init>`().
       |[1:8..1:9): s <= _empty_/C#`<init>`().(s)
       |[1:11..1:12): S => _empty_/S#
       |[2:6..2:10): t133 <= _empty_/t133#
       |[2:11..2:11):  <= _empty_/t133#`<init>`().
       |[2:19..2:20): C => _empty_/C#
       |[2:20..2:20):  => _empty_/C#`<init>`().
       |[2:21..2:27): x => x <= local0
       |[2:21..2:22): x <= local2
       |[2:26..2:27): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda as a superclass argument",
    """|trait S { def f(a: Int): Int }
       |class C(s: S)
       |class t133 extends C(x => x)
       |""".stripMargin,
    """|_empty_/C# => class C extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/C#`<init>`(). => primary ctor <init>(s: S)
       |  s => _empty_/C#`<init>`().(s)
       |  S => _empty_/S#
       |_empty_/C#`<init>`().(s) => param s: S
       |  S => _empty_/S#
       |_empty_/C#s. => private[this] val method s: S
       |  S => _empty_/S#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t133# => class t133 extends C { +1 decls }
       |  C => _empty_/C#
       |_empty_/t133#`<init>`(). => primary ctor <init>()
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM placeholder as a superclass argument",
    """|trait S { def f(a: Int): Int }
       |class C(s: S)
       |class t134 extends C(_ + 1)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): C <= _empty_/C#
       |[1:7..1:7):  <= _empty_/C#`<init>`().
       |[1:8..1:9): s <= _empty_/C#`<init>`().(s)
       |[1:8..1:9): s <= _empty_/C#s.
       |[1:11..1:12): S => _empty_/S#
       |[2:6..2:10): t134 <= _empty_/t134#
       |[2:11..2:11):  <= _empty_/t134#`<init>`().
       |[2:19..2:20): C => _empty_/C#
       |[2:20..2:20):  => _empty_/C#`<init>`().
       |[2:21..2:22): _ <= local2
       |[2:21..2:26): _ + 1 <= local0
       |[2:21..2:26): _ + 1 <= local1
       |[2:23..2:24): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  treeSymbols(
    "SAM placeholder as a superclass argument",
    """|trait S { def f(a: Int): Int }
       |class C(s: S)
       |class t134 extends C(_ + 1)
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:6..1:7): C <= _empty_/C#
       |[1:7..1:7):  <= _empty_/C#`<init>`().
       |[1:8..1:9): s <= _empty_/C#`<init>`().(s)
       |[1:11..1:12): S => _empty_/S#
       |[2:6..2:10): t134 <= _empty_/t134#
       |[2:11..2:11):  <= _empty_/t134#`<init>`().
       |[2:19..2:20): C => _empty_/C#
       |[2:20..2:20):  => _empty_/C#`<init>`().
       |[2:21..2:26): _ + 1 <= local0
       |[2:21..2:22): _ <= local2
       |[2:23..2:24): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

  symbols(
    "SAM placeholder as a superclass argument",
    """|trait S { def f(a: Int): Int }
       |class C(s: S)
       |class t134 extends C(_ + 1)
       |""".stripMargin,
    """|_empty_/C# => class C extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/C#`<init>`(). => primary ctor <init>(s: S)
       |  s => _empty_/C#`<init>`().(s)
       |  S => _empty_/S#
       |_empty_/C#`<init>`().(s) => param s: S
       |  S => _empty_/S#
       |_empty_/C#s. => private[this] val method s: S
       |  S => _empty_/S#
       |_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t134# => class t134 extends C { +1 decls }
       |  C => _empty_/C#
       |_empty_/t134#`<init>`(). => primary ctor <init>()
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x$1: Int): Int <: _empty_/S#f().
       |  x$1 => local2
       |  Int => scala/Int#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of a Java interface with parameters",
    """|object t135 {
       |  val c: java.util.Comparator[Int] = (a, b) => a - b
       |}
       |""".stripMargin,
    """|[0:7..0:11): t135 <= _empty_/t135.
       |[1:6..1:7): c <= _empty_/t135.c.
       |[1:9..1:13): java => java/
       |[1:14..1:18): util => java/util/
       |[1:19..1:29): Comparator => java/util/Comparator#
       |[1:30..1:33): Int => scala/Int#
       |[1:37..1:52): (a, b) => a - b <= local0
       |[1:37..1:52): (a, b) => a - b <= local1
       |[1:38..1:39): a <= local2
       |[1:41..1:42): b <= local3
       |[1:47..1:48): a => local2
       |[1:49..1:50): - => scala/Int#`-`(+3).
       |[1:51..1:52): b => local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of a Java interface with parameters",
    """|object t135 {
       |  val c: java.util.Comparator[Int] = (a, b) => a - b
       |}
       |""".stripMargin,
    """|[0:7..0:11): t135 <= _empty_/t135.
       |[1:6..1:7): c <= _empty_/t135.c.
       |[1:9..1:13): java => java/
       |[1:14..1:18): util => java/util/
       |[1:19..1:29): Comparator => java/util/Comparator#
       |[1:30..1:33): Int => scala/Int#
       |[1:37..1:52): (a, b) => a - b <= local0
       |[1:38..1:39): a <= local2
       |[1:41..1:42): b <= local3
       |[1:47..1:48): a => local2
       |[1:49..1:50): - => scala/Int#`-`(+3).
       |[1:51..1:52): b => local3
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of a Java interface with parameters",
    """|object t135 {
       |  val c: java.util.Comparator[Int] = (a, b) => a - b
       |}
       |""".stripMargin,
    """|_empty_/t135. => final object t135 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t135.c. => val method c: Comparator[Int]
       |  Comparator => java/util/Comparator#
       |  Int => scala/Int#
       |local0 => synthetic class $anon extends Object with Comparator[Int] { +1 decls }
       |  Object => java/lang/Object#
       |  Comparator => java/util/Comparator#
       |  Int => scala/Int#
       |local1 => synthetic method compare(a: Int, b: Int): Int <: java/util/Comparator#compare().
       |  a => local2
       |  Int => scala/Int#
       |  b => local3
       |local2 => param a: Int
       |  Int => scala/Int#
       |local3 => param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value of a Java interface",
    """|object t136 {
       |  def g(y: Int) = y
       |  val c: java.util.function.IntUnaryOperator = g
       |}
       |""".stripMargin,
    """|[0:7..0:11): t136 <= _empty_/t136.
       |[1:6..1:7): g <= _empty_/t136.g().
       |[1:8..1:9): y <= _empty_/t136.g().(y)
       |[1:11..1:14): Int => scala/Int#
       |[1:18..1:19): y => _empty_/t136.g().(y)
       |[2:6..2:7): c <= _empty_/t136.c.
       |[2:9..2:13): java => java/
       |[2:14..2:18): util => java/util/
       |[2:19..2:27): function => java/util/function/
       |[2:28..2:44): IntUnaryOperator => java/util/function/IntUnaryOperator#
       |[2:47..2:48): g => _empty_/t136.g().
       |[2:47..2:48): g <= local0
       |[2:47..2:48): g <= local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value of a Java interface",
    """|object t136 {
       |  def g(y: Int) = y
       |  val c: java.util.function.IntUnaryOperator = g
       |}
       |""".stripMargin,
    """|[0:7..0:11): t136 <= _empty_/t136.
       |[1:6..1:7): g <= _empty_/t136.g().
       |[1:8..1:9): y <= _empty_/t136.g().(y)
       |[1:11..1:14): Int => scala/Int#
       |[1:18..1:19): y => _empty_/t136.g().(y)
       |[2:6..2:7): c <= _empty_/t136.c.
       |[2:9..2:13): java => java/
       |[2:14..2:18): util => java/util/
       |[2:19..2:27): function => java/util/function/
       |[2:28..2:44): IntUnaryOperator => java/util/function/IntUnaryOperator#
       |[2:47..2:48): g => _empty_/t136.g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value of a Java interface",
    """|object t136 {
       |  def g(y: Int) = y
       |  val c: java.util.function.IntUnaryOperator = g
       |}
       |""".stripMargin,
    """|_empty_/t136. => final object t136 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t136.c. => val method c: IntUnaryOperator
       |  IntUnaryOperator => java/util/function/IntUnaryOperator#
       |_empty_/t136.g(). => method g(y: Int): Int
       |  y => _empty_/t136.g().(y)
       |  Int => scala/Int#
       |_empty_/t136.g().(y) => param y: Int
       |  Int => scala/Int#
       |local0 => synthetic class $anon extends Object with IntUnaryOperator { +1 decls }
       |  Object => java/lang/Object#
       |  IntUnaryOperator => java/util/function/IntUnaryOperator#
       |local1 => synthetic method applyAsInt(y: Int): Int <: java/util/function/IntUnaryOperator#applyAsInt().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of a local trait",
    """|object t137 {
       |  def m = {
       |    trait L { def f(a: Int): Int }
       |    val l: L = x => x
       |    l.f(0)
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t137 <= _empty_/t137.
       |[1:6..1:7): m <= _empty_/t137.m().
       |[2:10..2:11): L <= local0
       |[2:18..2:19): f <= local1
       |[2:20..2:21): a <= local2
       |[2:23..2:26): Int => scala/Int#
       |[2:29..2:32): Int => scala/Int#
       |[3:8..3:9): l <= local6
       |[3:11..3:12): L => local0
       |[3:15..3:16): x <= local5
       |[3:15..3:21): x => x <= local3
       |[3:15..3:21): x => x <= local4
       |[3:20..3:21): x => local5
       |[4:4..4:5): l => local6
       |[4:6..4:7): f => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of a local trait",
    """|object t137 {
       |  def m = {
       |    trait L { def f(a: Int): Int }
       |    val l: L = x => x
       |    l.f(0)
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t137 <= _empty_/t137.
       |[1:6..1:7): m <= _empty_/t137.m().
       |[2:10..2:11): L <= local0
       |[2:18..2:19): f <= local1
       |[2:20..2:21): a <= local2
       |[2:23..2:26): Int => scala/Int#
       |[2:29..2:32): Int => scala/Int#
       |[3:8..3:9): l <= local6
       |[3:11..3:12): L => local0
       |[3:15..3:21): x => x <= local3
       |[3:15..3:16): x <= local5
       |[3:20..3:21): x => local5
       |[4:4..4:5): l => local6
       |[4:6..4:7): f => local1
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of a local trait",
    """|object t137 {
       |  def m = {
       |    trait L { def f(a: Int): Int }
       |    val l: L = x => x
       |    l.f(0)
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t137. => final object t137 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t137.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => trait L extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => abstract method f(a: Int): Int
       |  a => local2
       |  Int => scala/Int#
       |local2 => param a: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with L { +1 decls }
       |  Object => java/lang/Object#
       |  L => local0
       |local4 => synthetic method f(x: Int): Int <: local1
       |  x => local5
       |  Int => scala/Int#
       |local5 => param x: Int
       |  Int => scala/Int#
       |local6 => val local l: L
       |  L => local0
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of a local generic trait",
    """|object t138 {
       |  def m = {
       |    trait L[T] { def f(t: T): T }
       |    val l: L[Int] = x => x
       |    l.f(0)
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t138 <= _empty_/t138.
       |[1:6..1:7): m <= _empty_/t138.m().
       |[2:10..2:11): L <= local0
       |[2:12..2:13): T <= local2
       |[2:21..2:22): f <= local1
       |[2:23..2:24): t <= local3
       |[2:26..2:27): T => local2
       |[2:30..2:31): T => local2
       |[3:8..3:9): l <= local7
       |[3:11..3:12): L => local0
       |[3:13..3:16): Int => scala/Int#
       |[3:20..3:21): x <= local6
       |[3:20..3:26): x => x <= local4
       |[3:20..3:26): x => x <= local5
       |[3:25..3:26): x => local6
       |[4:4..4:5): l => local7
       |[4:6..4:7): f => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of a local generic trait",
    """|object t138 {
       |  def m = {
       |    trait L[T] { def f(t: T): T }
       |    val l: L[Int] = x => x
       |    l.f(0)
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t138 <= _empty_/t138.
       |[1:6..1:7): m <= _empty_/t138.m().
       |[2:10..2:11): L <= local0
       |[2:12..2:13): T <= local2
       |[2:21..2:22): f <= local1
       |[2:23..2:24): t <= local3
       |[2:26..2:27): T => local2
       |[2:30..2:31): T => local2
       |[3:8..3:9): l <= local7
       |[3:11..3:12): L => local0
       |[3:13..3:16): Int => scala/Int#
       |[3:20..3:26): x => x <= local4
       |[3:20..3:21): x <= local6
       |[3:25..3:26): x => local6
       |[4:4..4:5): l => local7
       |[4:6..4:7): f => local1
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of a local generic trait",
    """|object t138 {
       |  def m = {
       |    trait L[T] { def f(t: T): T }
       |    val l: L[Int] = x => x
       |    l.f(0)
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t138. => final object t138 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t138.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => trait L[T] extends AnyRef { +1 decls }
       |  T => local2
       |  AnyRef => scala/AnyRef#
       |local1 => abstract method f(t: T): T
       |  t => local3
       |  T => local2
       |local2 => typeparam T
       |local3 => param t: T
       |  T => local2
       |local4 => synthetic class $anon extends Object with L[Int] { +1 decls }
       |  Object => java/lang/Object#
       |  L => local0
       |  Int => scala/Int#
       |local5 => synthetic method f(x: Int): Int <: local1
       |  x => local6
       |  Int => scala/Int#
       |local6 => param x: Int
       |  Int => scala/Int#
       |local7 => val local l: L[Int]
       |  L => local0
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda with an implicit parameter",
    """|trait S { def f(a: Int): Int }
       |object t139 {
       |  def use(s: S) = s.f(0)
       |  def m = use(implicit x => x)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t139 <= _empty_/t139.
       |[2:6..2:9): use <= _empty_/t139.use().
       |[2:10..2:11): s <= _empty_/t139.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t139.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t139.m().
       |[3:10..3:13): use => _empty_/t139.use().
       |[3:14..3:29): implicit x => x <= local0
       |[3:14..3:29): implicit x => x <= local1
       |[3:23..3:24): x <= local2
       |[3:28..3:29): x => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda with an implicit parameter",
    """|trait S { def f(a: Int): Int }
       |object t139 {
       |  def use(s: S) = s.f(0)
       |  def m = use(implicit x => x)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t139 <= _empty_/t139.
       |[2:6..2:9): use <= _empty_/t139.use().
       |[2:10..2:11): s <= _empty_/t139.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t139.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t139.m().
       |[3:10..3:13): use => _empty_/t139.use().
       |[3:14..3:29): implicit x => x <= local0
       |[3:23..3:24): x <= local2
       |[3:28..3:29): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda with an implicit parameter",
    """|trait S { def f(a: Int): Int }
       |object t139 {
       |  def use(s: S) = s.f(0)
       |  def m = use(implicit x => x)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t139. => final object t139 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t139.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t139.use(). => method use(s: S): Int
       |  s => _empty_/t139.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t139.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(implicit x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => implicit param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value with a type ascription",
    """|trait S { def f(a: Int): Int }
       |object t140 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(g: S)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t140 <= _empty_/t140.
       |[2:6..2:9): use <= _empty_/t140.use().
       |[2:10..2:11): s <= _empty_/t140.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t140.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t140.g().
       |[3:8..3:9): y <= _empty_/t140.g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t140.g().(y)
       |[4:6..4:7): m <= _empty_/t140.m().
       |[4:10..4:13): use => _empty_/t140.use().
       |[4:14..4:15): g => _empty_/t140.g().
       |[4:14..4:15): g <= local0
       |[4:14..4:15): g <= local1
       |[4:17..4:18): S => _empty_/S#
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value with a type ascription",
    """|trait S { def f(a: Int): Int }
       |object t140 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(g: S)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t140 <= _empty_/t140.
       |[2:6..2:9): use <= _empty_/t140.use().
       |[2:10..2:11): s <= _empty_/t140.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t140.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): g <= _empty_/t140.g().
       |[3:8..3:9): y <= _empty_/t140.g().(y)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): y => _empty_/t140.g().(y)
       |[4:6..4:7): m <= _empty_/t140.m().
       |[4:10..4:13): use => _empty_/t140.use().
       |[4:14..4:15): g => _empty_/t140.g().
       |[4:17..4:18): S => _empty_/S#
       |""".stripMargin,
  )

  symbols(
    "SAM method value with a type ascription",
    """|trait S { def f(a: Int): Int }
       |object t140 {
       |  def use(s: S) = s.f(0)
       |  def g(y: Int) = y
       |  def m = use(g: S)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t140. => final object t140 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t140.g(). => method g(y: Int): Int
       |  y => _empty_/t140.g().(y)
       |  Int => scala/Int#
       |_empty_/t140.g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/t140.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t140.use(). => method use(s: S): Int
       |  s => _empty_/t140.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t140.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda with a type ascription",
    """|trait S { def f(a: Int): Int }
       |object t141 {
       |  def use(s: S) = s.f(0)
       |  def m = use(((x: Int) => x): S)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t141 <= _empty_/t141.
       |[2:6..2:9): use <= _empty_/t141.use().
       |[2:10..2:11): s <= _empty_/t141.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t141.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t141.m().
       |[3:10..3:13): use => _empty_/t141.use().
       |[3:15..3:28): (x: Int) => x <= local0
       |[3:15..3:28): (x: Int) => x <= local1
       |[3:16..3:17): x <= local2
       |[3:19..3:22): Int => scala/Int#
       |[3:27..3:28): x => local2
       |[3:31..3:32): S => _empty_/S#
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda with a type ascription",
    """|trait S { def f(a: Int): Int }
       |object t141 {
       |  def use(s: S) = s.f(0)
       |  def m = use(((x: Int) => x): S)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t141 <= _empty_/t141.
       |[2:6..2:9): use <= _empty_/t141.use().
       |[2:10..2:11): s <= _empty_/t141.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t141.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t141.m().
       |[3:10..3:13): use => _empty_/t141.use().
       |[3:15..3:28): (x: Int) => x <= local0
       |[3:16..3:17): x <= local2
       |[3:19..3:22): Int => scala/Int#
       |[3:27..3:28): x => local2
       |[3:31..3:32): S => _empty_/S#
       |""".stripMargin,
  )

  symbols(
    "SAM lambda with a type ascription",
    """|trait S { def f(a: Int): Int }
       |object t141 {
       |  def use(s: S) = s.f(0)
       |  def m = use(((x: Int) => x): S)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t141. => final object t141 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t141.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t141.use(). => method use(s: S): Int
       |  s => _empty_/t141.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t141.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x: Int): Int <: _empty_/S#f().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "SAM eta expansion of a method without parameters",
    """|object t142 {
       |  def g(): Unit = ()
       |  val r: Runnable = g _
       |}
       |""".stripMargin,
    """|[0:7..0:11): t142 <= _empty_/t142.
       |[1:6..1:7): g <= _empty_/t142.g().
       |[1:11..1:15): Unit => scala/Unit#
       |[2:6..2:7): r <= _empty_/t142.r.
       |[2:9..2:17): Runnable => java/lang/Runnable#
       |[2:20..2:21): g => _empty_/t142.g().
       |[2:20..2:21): g <= local0
       |[2:20..2:21): g <= local1
       |""".stripMargin,
  )

  if (ScalaVersion.is213) treeSymbols(
    "SAM eta expansion of a method without parameters",
    """|object t142 {
       |  def g(): Unit = ()
       |  val r: Runnable = g _
       |}
       |""".stripMargin,
    """|[0:7..0:11): t142 <= _empty_/t142.
       |[1:6..1:7): g <= _empty_/t142.g().
       |[1:11..1:15): Unit => scala/Unit#
       |[2:6..2:7): r <= _empty_/t142.r.
       |[2:9..2:17): Runnable => java/lang/Runnable#
       |[2:20..2:21): g => _empty_/t142.g().
       |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "SAM eta expansion of a method without parameters",
    """|object t142 {
       |  def g(): Unit = ()
       |  val r: Runnable = g _
       |}
       |""".stripMargin,
    """|_empty_/t142. => final object t142 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t142.g(). => method g(): Unit
       |  Unit => scala/Unit#
       |_empty_/t142.r. => val method r: Runnable
       |  Runnable => java/lang/Runnable#
       |local0 => synthetic class $anon extends Object with Runnable { +1 decls }
       |  Object => java/lang/Object#
       |  Runnable => java/lang/Runnable#
       |local1 => synthetic method run(): Unit <: java/lang/Runnable#run().
       |  Unit => scala/Unit#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value of an implicit class",
    """|trait S { def f(a: Int): Int }
       |object t143 {
       |  implicit class I(i: Int) { def g(y: Int) = y + i }
       |  def use(s: S) = s.f(0)
       |  def m = use(1.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t143 <= _empty_/t143.
       |[2:17..2:18): I <= _empty_/t143.I#
       |[2:18..2:18):  <= _empty_/t143.I#`<init>`().
       |[2:19..2:20): i <= _empty_/t143.I#`<init>`().(i)
       |[2:19..2:20): i <= _empty_/t143.I#i.
       |[2:22..2:25): Int => scala/Int#
       |[2:33..2:34): g <= _empty_/t143.I#g().
       |[2:35..2:36): y <= _empty_/t143.I#g().(y)
       |[2:38..2:41): Int => scala/Int#
       |[2:45..2:46): y => _empty_/t143.I#g().(y)
       |[2:47..2:48): + => scala/Int#`+`(+4).
       |[2:49..2:50): i => _empty_/t143.I#i.
       |[3:6..3:9): use <= _empty_/t143.use().
       |[3:10..3:11): s <= _empty_/t143.use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t143.use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t143.m().
       |[4:10..4:13): use => _empty_/t143.use().
       |[4:14..4:17): 1.g <= local0
       |[4:14..4:17): 1.g <= local1
       |[4:16..4:17): g => _empty_/t143.I#g().
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value of an implicit class",
    """|trait S { def f(a: Int): Int }
       |object t143 {
       |  implicit class I(i: Int) { def g(y: Int) = y + i }
       |  def use(s: S) = s.f(0)
       |  def m = use(1.g)
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t143 <= _empty_/t143.
       |[2:17..2:18): I <= _empty_/t143.I#
       |[2:18..2:18):  <= _empty_/t143.I#`<init>`().
       |[2:19..2:20): i <= _empty_/t143.I#`<init>`().(i)
       |[2:22..2:25): Int => scala/Int#
       |[2:33..2:34): g <= _empty_/t143.I#g().
       |[2:35..2:36): y <= _empty_/t143.I#g().(y)
       |[2:38..2:41): Int => scala/Int#
       |[2:45..2:46): y => _empty_/t143.I#g().(y)
       |[2:47..2:48): + => scala/Int#`+`(+4).
       |[2:49..2:50): i => _empty_/t143.I#i.
       |[3:6..3:9): use <= _empty_/t143.use().
       |[3:10..3:11): s <= _empty_/t143.use().(s)
       |[3:13..3:14): S => _empty_/S#
       |[3:18..3:19): s => _empty_/t143.use().(s)
       |[3:20..3:21): f => _empty_/S#f().
       |[4:6..4:7): m <= _empty_/t143.m().
       |[4:10..4:13): use => _empty_/t143.use().
       |[4:14..4:17): 1.g <= local0
       |[4:16..4:17): g => _empty_/t143.I#g().
       |""".stripMargin,
  )

  symbols(
    "SAM method value of an implicit class",
    """|trait S { def f(a: Int): Int }
       |object t143 {
       |  implicit class I(i: Int) { def g(y: Int) = y + i }
       |  def use(s: S) = s.f(0)
       |  def m = use(1.g)
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t143. => final object t143 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t143.I# => implicit class I extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t143.I#`<init>`(). => primary ctor <init>(i: Int)
       |  i => _empty_/t143.I#`<init>`().(i)
       |  Int => scala/Int#
       |_empty_/t143.I#`<init>`().(i) => param i: Int
       |  Int => scala/Int#
       |_empty_/t143.I#g(). => method g(y: Int): Int
       |  y => _empty_/t143.I#g().(y)
       |  Int => scala/Int#
       |_empty_/t143.I#g().(y) => param y: Int
       |  Int => scala/Int#
       |_empty_/t143.I#i. => private[this] val method i: Int
       |  Int => scala/Int#
       |_empty_/t143.I(). => synthetic implicit method I(i: Int): I
       |  i => _empty_/t143.I().(i)
       |  Int => scala/Int#
       |  I => _empty_/t143.I#
       |_empty_/t143.I().(i) => param i: Int
       |  Int => scala/Int#
       |_empty_/t143.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t143.use(). => method use(s: S): Int
       |  s => _empty_/t143.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t143.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local2
       |  Int => scala/Int#
       |local2 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM method value of a local method",
    """|trait S { def f(a: Int): Int }
       |object t144 {
       |  def use(s: S) = s.f(0)
       |  def m = { def g(y: Int) = y; use(g) }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t144 <= _empty_/t144.
       |[2:6..2:9): use <= _empty_/t144.use().
       |[2:10..2:11): s <= _empty_/t144.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t144.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t144.m().
       |[3:16..3:17): g <= local0
       |[3:18..3:19): y <= local1
       |[3:21..3:24): Int => scala/Int#
       |[3:28..3:29): y => local1
       |[3:31..3:34): use => _empty_/t144.use().
       |[3:35..3:36): g => local0
       |[3:35..3:36): g <= local2
       |[3:35..3:36): g <= local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM method value of a local method",
    """|trait S { def f(a: Int): Int }
       |object t144 {
       |  def use(s: S) = s.f(0)
       |  def m = { def g(y: Int) = y; use(g) }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t144 <= _empty_/t144.
       |[2:6..2:9): use <= _empty_/t144.use().
       |[2:10..2:11): s <= _empty_/t144.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t144.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t144.m().
       |[3:16..3:17): g <= local0
       |[3:18..3:19): y <= local1
       |[3:21..3:24): Int => scala/Int#
       |[3:28..3:29): y => local1
       |[3:31..3:34): use => _empty_/t144.use().
       |[3:35..3:36): g => local0
       |""".stripMargin,
  )

  symbols(
    "SAM method value of a local method",
    """|trait S { def f(a: Int): Int }
       |object t144 {
       |  def use(s: S) = s.f(0)
       |  def m = { def g(y: Int) = y; use(g) }
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t144. => final object t144 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t144.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t144.use(). => method use(s: S): Int
       |  s => _empty_/t144.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t144.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => method g(y: Int): Int
       |  y => local1
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local3 => synthetic method f(y: Int): Int <: _empty_/S#f().
       |  y => local4
       |  Int => scala/Int#
       |local4 => synthetic param y: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM pattern-matching anonymous function with one case",
    """|trait S { def f(a: Int): Int }
       |object t145 {
       |  def use(s: S) = s.f(0)
       |  def m = use { case x => x }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t145 <= _empty_/t145.
       |[2:6..2:9): use <= _empty_/t145.use().
       |[2:10..2:11): s <= _empty_/t145.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t145.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t145.m().
       |[3:10..3:13): use => _empty_/t145.use().
       |[3:14..3:29): { case x => x } <= local0
       |[3:14..3:29): { case x => x } <= local1
       |[3:21..3:22): x <= local3
       |[3:26..3:27): x => local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM pattern-matching anonymous function with one case",
    """|trait S { def f(a: Int): Int }
       |object t145 {
       |  def use(s: S) = s.f(0)
       |  def m = use { case x => x }
       |}
       |""".stripMargin,
    """|[0:6..0:7): S <= _empty_/S#
       |[0:14..0:15): f <= _empty_/S#f().
       |[0:16..0:17): a <= _empty_/S#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:25..0:28): Int => scala/Int#
       |[1:7..1:11): t145 <= _empty_/t145.
       |[2:6..2:9): use <= _empty_/t145.use().
       |[2:10..2:11): s <= _empty_/t145.use().(s)
       |[2:13..2:14): S => _empty_/S#
       |[2:18..2:19): s => _empty_/t145.use().(s)
       |[2:20..2:21): f => _empty_/S#f().
       |[3:6..3:7): m <= _empty_/t145.m().
       |[3:10..3:13): use => _empty_/t145.use().
       |[3:14..3:29): { case x => x } <= local0
       |[3:21..3:22): x <= local3
       |[3:26..3:27): x => local3
       |""".stripMargin,
  )

  symbols(
    "SAM pattern-matching anonymous function with one case",
    """|trait S { def f(a: Int): Int }
       |object t145 {
       |  def use(s: S) = s.f(0)
       |  def m = use { case x => x }
       |}
       |""".stripMargin,
    """|_empty_/S# => trait S extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/S#f(). => abstract method f(a: Int): Int
       |  a => _empty_/S#f().(a)
       |  Int => scala/Int#
       |_empty_/S#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t145. => final object t145 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t145.m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/t145.use(). => method use(s: S): Int
       |  s => _empty_/t145.use().(s)
       |  S => _empty_/S#
       |  Int => scala/Int#
       |_empty_/t145.use().(s) => param s: S
       |  S => _empty_/S#
       |local0 => synthetic class $anon extends Object with S { +1 decls }
       |  Object => java/lang/Object#
       |  S => _empty_/S#
       |local1 => synthetic method f(x0$1: Int): Int <: _empty_/S#f().
       |  x0$1 => local2
       |  Int => scala/Int#
       |local2 => synthetic param x0$1: Int
       |  Int => scala/Int#
       |local3 => val local x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM pattern-matching anonymous function with two parameters",
    """|trait P { def f(a: Int, b: Int): Int }
       |object t146 {
       |  val p: P = { case (a, b) => a }
       |}
       |""".stripMargin,
    """|[0:6..0:7): P <= _empty_/P#
       |[0:14..0:15): f <= _empty_/P#f().
       |[0:16..0:17): a <= _empty_/P#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:24..0:25): b <= _empty_/P#f().(b)
       |[0:27..0:30): Int => scala/Int#
       |[0:33..0:36): Int => scala/Int#
       |[1:7..1:11): t146 <= _empty_/t146.
       |[2:6..2:7): p <= _empty_/t146.p.
       |[2:9..2:10): P => _empty_/P#
       |[2:13..2:33): { case (a, b) => a } <= local0
       |[2:13..2:33): { case (a, b) => a } <= local1
       |[2:21..2:22): a <= local4
       |[2:24..2:25): b <= local5
       |[2:30..2:31): a => local4
       |""".stripMargin,
  )

  treeSymbols(
    "SAM pattern-matching anonymous function with two parameters",
    """|trait P { def f(a: Int, b: Int): Int }
       |object t146 {
       |  val p: P = { case (a, b) => a }
       |}
       |""".stripMargin,
    """|[0:6..0:7): P <= _empty_/P#
       |[0:14..0:15): f <= _empty_/P#f().
       |[0:16..0:17): a <= _empty_/P#f().(a)
       |[0:19..0:22): Int => scala/Int#
       |[0:24..0:25): b <= _empty_/P#f().(b)
       |[0:27..0:30): Int => scala/Int#
       |[0:33..0:36): Int => scala/Int#
       |[1:7..1:11): t146 <= _empty_/t146.
       |[2:6..2:7): p <= _empty_/t146.p.
       |[2:9..2:10): P => _empty_/P#
       |[2:13..2:33): { case (a, b) => a } <= local0
       |[2:21..2:22): a <= local4
       |[2:24..2:25): b <= local5
       |[2:30..2:31): a => local4
       |""".stripMargin,
  )

  symbols(
    "SAM pattern-matching anonymous function with two parameters",
    """|trait P { def f(a: Int, b: Int): Int }
       |object t146 {
       |  val p: P = { case (a, b) => a }
       |}
       |""".stripMargin,
    """|_empty_/P# => trait P extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/P#f(). => abstract method f(a: Int, b: Int): Int
       |  a => _empty_/P#f().(a)
       |  Int => scala/Int#
       |  b => _empty_/P#f().(b)
       |_empty_/P#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/P#f().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t146. => final object t146 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t146.p. => val method p: P
       |  P => _empty_/P#
       |local0 => synthetic class $anon extends Object with P { +1 decls }
       |  Object => java/lang/Object#
       |  P => _empty_/P#
       |local1 => synthetic method f(x0$1: Int, x1$1: Int): Int <: _empty_/P#f().
       |  x0$1 => local2
       |  Int => scala/Int#
       |  x1$1 => local3
       |local2 => synthetic param x0$1: Int
       |  Int => scala/Int#
       |local3 => synthetic param x1$1: Int
       |  Int => scala/Int#
       |local4 => val local a: Int
       |  Int => scala/Int#
       |local5 => val local b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambda of a trait that extends a function type",
    """|trait F extends (Int => Int)
       |object t147 {
       |  val f: F = x => x
       |}
       |""".stripMargin,
    """|[0:6..0:7): F <= _empty_/F#
       |[0:17..0:20): Int => scala/Int#
       |[0:24..0:27): Int => scala/Int#
       |[1:7..1:11): t147 <= _empty_/t147.
       |[2:6..2:7): f <= _empty_/t147.f.
       |[2:9..2:10): F => _empty_/F#
       |[2:13..2:14): x <= local2
       |[2:13..2:19): x => x <= local0
       |[2:13..2:19): x => x <= local1
       |[2:18..2:19): x => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM lambda of a trait that extends a function type",
    """|trait F extends (Int => Int)
       |object t147 {
       |  val f: F = x => x
       |}
       |""".stripMargin,
    """|[0:6..0:7): F <= _empty_/F#
       |[0:17..0:20): Int => scala/Int#
       |[0:24..0:27): Int => scala/Int#
       |[1:7..1:11): t147 <= _empty_/t147.
       |[2:6..2:7): f <= _empty_/t147.f.
       |[2:9..2:10): F => _empty_/F#
       |[2:13..2:19): x => x <= local0
       |[2:13..2:14): x <= local2
       |[2:18..2:19): x => local2
       |""".stripMargin,
  )

  symbols(
    "SAM lambda of a trait that extends a function type",
    """|trait F extends (Int => Int)
       |object t147 {
       |  val f: F = x => x
       |}
       |""".stripMargin,
    """|_empty_/F# => trait F extends AnyRef with Function1[Int, Int]
       |  AnyRef => scala/AnyRef#
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |_empty_/t147. => final object t147 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t147.f. => val method f: F
       |  F => _empty_/F#
       |local0 => synthetic class $anon extends Object with F { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |local1 => synthetic method apply(x: Int): Int <: scala/Function1#apply().
       |  x => local2
       |  Int => scala/Int#
       |local2 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "SAM lambdas with a placeholder in an enumerator of a for",
    """|trait F { def apply(l: List[Int]): List[Int] }
       |trait G { def apply(i: Int): List[Int] }
       |abstract class H { def apply(l: List[Int]): List[Int] }
       |object s1 {
       |  val xs: List[Int] = null
       |  val a: F = for (x <- _) yield x + 1
       |  val b: G = for (x <- xs if x > _) yield x
       |  val c: G = for (x <- xs; y = _) yield x + y
       |  val d: H = for (x <- _) yield x + 1
       |}
       |""".stripMargin,
    """|_empty_/F# => trait F extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/F#apply(). => abstract method apply(l: List[Int]): List[Int]
       |  l => _empty_/F#apply().(l)
       |  List => scala/package.List#
       |  Int => scala/Int#
       |_empty_/F#apply().(l) => param l: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |_empty_/G# => trait G extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/G#apply(). => abstract method apply(i: Int): List[Int]
       |  i => _empty_/G#apply().(i)
       |  Int => scala/Int#
       |  List => scala/package.List#
       |_empty_/G#apply().(i) => param i: Int
       |  Int => scala/Int#
       |_empty_/H# => abstract class H extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/H#`<init>`(). => primary ctor <init>()
       |_empty_/H#apply(). => abstract method apply(l: List[Int]): List[Int]
       |  l => _empty_/H#apply().(l)
       |  List => scala/package.List#
       |  Int => scala/Int#
       |_empty_/H#apply().(l) => param l: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |_empty_/s1. => final object s1 extends AnyRef { +5 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s1.a. => val method a: F
       |  F => _empty_/F#
       |_empty_/s1.b. => val method b: G
       |  G => _empty_/G#
       |_empty_/s1.c. => val method c: G
       |  G => _empty_/G#
       |_empty_/s1.d. => val method d: H
       |  H => _empty_/H#
       |_empty_/s1.xs. => val method xs: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local0 => synthetic class $anon extends Object with F { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |local1 => synthetic method apply(x$1: List[Int]): List[Int] <: _empty_/F#apply().
       |  x$1 => local2
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local2 => synthetic param x$1: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local3 => param x: Int
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with G { +1 decls }
       |  Object => java/lang/Object#
       |  G => _empty_/G#
       |local5 => synthetic method apply(x$2: Int): List[Int] <: _empty_/G#apply().
       |  x$2 => local6
       |  Int => scala/Int#
       |  List => scala/package.List#
       |local6 => synthetic param x$2: Int
       |  Int => scala/Int#
       |local7 => param x: Int
       |  Int => scala/Int#
       |local8 => synthetic class $anon extends Object with G { +1 decls }
       |  Object => java/lang/Object#
       |  G => _empty_/G#
       |local9 => synthetic method apply(x$3: Int): List[Int] <: _empty_/G#apply().
       |  x$3 => local10
       |  Int => scala/Int#
       |  List => scala/package.List#
       |local10 => synthetic param x$3: Int
       |  Int => scala/Int#
       |local11 => val local x: Int
       |  Int => scala/Int#
       |local12 => synthetic param x$4: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local13 => val local y: Int
       |  Int => scala/Int#
       |local14 => synthetic class $anon extends Object with H { +1 decls }
       |  Object => java/lang/Object#
       |  H => _empty_/H#
       |local15 => synthetic method apply(x$5: List[Int]): List[Int] <: _empty_/H#apply().
       |  x$5 => local16
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local16 => synthetic param x$5: List[Int]
       |  List => scala/package.List#
       |  Int => scala/Int#
       |local17 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM lambdas with a placeholder in an enumerator of a for",
    """|trait F { def apply(l: List[Int]): List[Int] }
       |trait G { def apply(i: Int): List[Int] }
       |abstract class H { def apply(l: List[Int]): List[Int] }
       |object s1 {
       |  val xs: List[Int] = null
       |  val a: F = for (x <- _) yield x + 1
       |  val b: G = for (x <- xs if x > _) yield x
       |  val c: G = for (x <- xs; y = _) yield x + y
       |  val d: H = for (x <- _) yield x + 1
       |}
       |""".stripMargin,
    """|[0:6..0:7): F <= _empty_/F#
       |[0:14..0:19): apply <= _empty_/F#apply().
       |[0:20..0:21): l <= _empty_/F#apply().(l)
       |[0:23..0:27): List => scala/package.List#
       |[0:28..0:31): Int => scala/Int#
       |[0:35..0:39): List => scala/package.List#
       |[0:40..0:43): Int => scala/Int#
       |[1:6..1:7): G <= _empty_/G#
       |[1:14..1:19): apply <= _empty_/G#apply().
       |[1:20..1:21): i <= _empty_/G#apply().(i)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:33): List => scala/package.List#
       |[1:34..1:37): Int => scala/Int#
       |[2:15..2:16): H <= _empty_/H#
       |[2:17..2:17):  <= _empty_/H#`<init>`().
       |[2:23..2:28): apply <= _empty_/H#apply().
       |[2:29..2:30): l <= _empty_/H#apply().(l)
       |[2:32..2:36): List => scala/package.List#
       |[2:37..2:40): Int => scala/Int#
       |[2:44..2:48): List => scala/package.List#
       |[2:49..2:52): Int => scala/Int#
       |[3:7..3:9): s1 <= _empty_/s1.
       |[4:6..4:8): xs <= _empty_/s1.xs.
       |[4:10..4:14): List => scala/package.List#
       |[4:15..4:18): Int => scala/Int#
       |[5:6..5:7): a <= _empty_/s1.a.
       |[5:9..5:10): F => _empty_/F#
       |[5:13..5:37): for (x <- _) yield x + 1 <= local0
       |[5:13..5:37): for (x <- _) yield x + 1 <= local1
       |[5:18..5:19): x <= local3
       |[5:23..5:24): _ <= local2
       |[5:32..5:33): x => local3
       |[5:34..5:35): + => scala/Int#`+`(+4).
       |[6:6..6:7): b <= _empty_/s1.b.
       |[6:9..6:10): G => _empty_/G#
       |[6:13..6:43): for (x <- xs if x > _) yield x <= local4
       |[6:13..6:43): for (x <- xs if x > _) yield x <= local5
       |[6:18..6:19): x <= local7
       |[6:23..6:25): xs => _empty_/s1.xs.
       |[6:29..6:30): x => local7
       |[6:31..6:32): > => scala/Int#`>`(+3).
       |[6:33..6:34): _ <= local6
       |[6:42..6:43): x => local7
       |[7:6..7:7): c <= _empty_/s1.c.
       |[7:9..7:10): G => _empty_/G#
       |[7:13..7:45): for (x <- xs; y = _) yield x + y <= local8
       |[7:13..7:45): for (x <- xs; y = _) yield x + y <= local9
       |[7:18..7:19): x <= local11
       |[7:23..7:25): xs => _empty_/s1.xs.
       |[7:27..7:28): y <= local13
       |[7:31..7:32): _ <= local10
       |[7:40..7:41): x => local11
       |[7:42..7:43): + => scala/Int#`+`(+4).
       |[7:44..7:45): y => local13
       |[8:6..8:7): d <= _empty_/s1.d.
       |[8:9..8:10): H => _empty_/H#
       |[8:13..8:37): for (x <- _) yield x + 1 <= local14
       |[8:13..8:37): for (x <- _) yield x + 1 <= local15
       |[8:18..8:19): x <= local17
       |[8:23..8:24): _ <= local16
       |[8:32..8:33): x => local17
       |[8:34..8:35): + => scala/Int#`+`(+4).
       |""".stripMargin,
  )

}
