package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}

import scala.collection.mutable

class SyntheticLocalsSuite extends SemanticdbSuite {

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  private def synthetics(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: synthetics", code, expected, "Synthetics")

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
    "SAM closures of a for comprehension",
    """|import java.util.Optional
       |object t102 {
       |  val o = Optional.of(1)
       |  val p: Optional[Int] = for (x <- o; y <- o) yield x + y
       |}
       |""".stripMargin,
    """|[0:7..0:11): java => java/
       |[0:12..0:16): util => java/util/
       |[0:17..0:25): Optional => java/util/Optional#
       |[1:7..1:11): t102 <= _empty_/t102.
       |[2:6..2:7): o <= _empty_/t102.o.
       |[2:10..2:18): Optional => java/util/Optional#
       |[2:19..2:21): of => java/util/Optional#of().
       |[3:6..3:7): p <= _empty_/t102.p.
       |[3:9..3:17): Optional => java/util/Optional#
       |[3:18..3:21): Int => scala/Int#
       |[3:30..3:31): x <= local0
       |[3:30..3:36): x <- o <= local2
       |[3:30..3:36): x <- o <= local3
       |[3:35..3:36): o => _empty_/t102.o.
       |[3:38..3:39): y <= local1
       |[3:38..3:44): y <- o <= local4
       |[3:38..3:44): y <- o <= local5
       |[3:43..3:44): o => _empty_/t102.o.
       |[3:52..3:53): x => local0
       |[3:54..3:55): + => scala/Int#`+`(+4).
       |[3:56..3:57): y => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension",
    """|import java.util.Optional
       |object t102 {
       |  val o = Optional.of(1)
       |  val p: Optional[Int] = for (x <- o; y <- o) yield x + y
       |}
       |""".stripMargin,
    """|[0:7..0:11): java => java/
       |[0:12..0:16): util => java/util/
       |[0:17..0:25): Optional => java/util/Optional#
       |[1:7..1:11): t102 <= _empty_/t102.
       |[2:6..2:7): o <= _empty_/t102.o.
       |[2:10..2:18): Optional => java/util/Optional#
       |[2:19..2:21): of => java/util/Optional#of().
       |[3:6..3:7): p <= _empty_/t102.p.
       |[3:9..3:17): Optional => java/util/Optional#
       |[3:18..3:21): Int => scala/Int#
       |[3:30..3:36): x <- o <= local2
       |[3:30..3:31): x <= local0
       |[3:35..3:36): o => _empty_/t102.o.
       |[3:38..3:44): y <- o <= local4
       |[3:38..3:39): y <= local1
       |[3:43..3:44): o => _empty_/t102.o.
       |[3:52..3:53): x => local0
       |[3:54..3:55): + => scala/Int#`+`(+4).
       |[3:56..3:57): y => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension",
    """|import java.util.Optional
       |object t102 {
       |  val o = Optional.of(1)
       |  val p: Optional[Int] = for (x <- o; y <- o) yield x + y
       |}
       |""".stripMargin,
    """|_empty_/t102. => final object t102 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t102.o. => val method o: Optional[Int]
       |  Optional => java/util/Optional#
       |  Int => scala/Int#
       |_empty_/t102.p. => val method p: Optional[Int]
       |  Optional => java/util/Optional#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with Function[Int, Optional[Int]] { +1 decls }
       |  Object => java/lang/Object#
       |  Function => java/util/function/Function#
       |  Int => scala/Int#
       |  Optional => java/util/Optional#
       |local3 => synthetic method apply(x: Int): Optional[Int] <: java/util/function/Function#apply().
       |  x => local0
       |  Int => scala/Int#
       |  Optional => java/util/Optional#
       |local4 => synthetic class $anon extends Object with Function[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  Function => java/util/function/Function#
       |  Int => scala/Int#
       |local5 => synthetic method apply(y: Int): Int <: java/util/function/Function#apply().
       |  y => local1
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
          val info = if (infos(d)) "with" else "without"
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
    "SAM closure of a for comprehension with a method call",
    """|import java.util.Optional
       |object t131 {
       |  def g(a: Int) = a
       |  val o = Optional.of(1)
       |  val p: Optional[Int] = for (x <- o) yield g(x)
       |}
       |""".stripMargin,
    """|[0:7..0:11): java => java/
       |[0:12..0:16): util => java/util/
       |[0:17..0:25): Optional => java/util/Optional#
       |[1:7..1:11): t131 <= _empty_/t131.
       |[2:6..2:7): g <= _empty_/t131.g().
       |[2:8..2:9): a <= _empty_/t131.g().(a)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): a => _empty_/t131.g().(a)
       |[3:6..3:7): o <= _empty_/t131.o.
       |[3:10..3:18): Optional => java/util/Optional#
       |[3:19..3:21): of => java/util/Optional#of().
       |[4:6..4:7): p <= _empty_/t131.p.
       |[4:9..4:17): Optional => java/util/Optional#
       |[4:18..4:21): Int => scala/Int#
       |[4:30..4:31): x <= local0
       |[4:30..4:36): x <- o <= local1
       |[4:30..4:36): x <- o <= local2
       |[4:35..4:36): o => _empty_/t131.o.
       |[4:44..4:45): g => _empty_/t131.g().
       |[4:46..4:47): x => local0
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closure of a for comprehension with a method call",
    """|import java.util.Optional
       |object t131 {
       |  def g(a: Int) = a
       |  val o = Optional.of(1)
       |  val p: Optional[Int] = for (x <- o) yield g(x)
       |}
       |""".stripMargin,
    """|[0:7..0:11): java => java/
       |[0:12..0:16): util => java/util/
       |[0:17..0:25): Optional => java/util/Optional#
       |[1:7..1:11): t131 <= _empty_/t131.
       |[2:6..2:7): g <= _empty_/t131.g().
       |[2:8..2:9): a <= _empty_/t131.g().(a)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): a => _empty_/t131.g().(a)
       |[3:6..3:7): o <= _empty_/t131.o.
       |[3:10..3:18): Optional => java/util/Optional#
       |[3:19..3:21): of => java/util/Optional#of().
       |[4:6..4:7): p <= _empty_/t131.p.
       |[4:9..4:17): Optional => java/util/Optional#
       |[4:18..4:21): Int => scala/Int#
       |[4:30..4:36): x <- o <= local1
       |[4:30..4:31): x <= local0
       |[4:35..4:36): o => _empty_/t131.o.
       |[4:44..4:45): g => _empty_/t131.g().
       |[4:46..4:47): x => local0
       |""".stripMargin,
  )

  symbols(
    "SAM closure of a for comprehension with a method call",
    """|import java.util.Optional
       |object t131 {
       |  def g(a: Int) = a
       |  val o = Optional.of(1)
       |  val p: Optional[Int] = for (x <- o) yield g(x)
       |}
       |""".stripMargin,
    """|_empty_/t131. => final object t131 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t131.g(). => method g(a: Int): Int
       |  a => _empty_/t131.g().(a)
       |  Int => scala/Int#
       |_empty_/t131.g().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t131.o. => val method o: Optional[Int]
       |  Optional => java/util/Optional#
       |  Int => scala/Int#
       |_empty_/t131.p. => val method p: Optional[Int]
       |  Optional => java/util/Optional#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => synthetic class $anon extends Object with Function[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  Function => java/util/function/Function#
       |  Int => scala/Int#
       |local2 => synthetic method apply(x: Int): Int <: java/util/function/Function#apply().
       |  x => local0
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t148 {
       |  def g(a: Int) = a
       |  val m = new M(1)
       |  val p = for (x <- m if x > 0) yield g(x)
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
       |[7:7..7:11): t148 <= _empty_/t148.
       |[8:6..8:7): g <= _empty_/t148.g().
       |[8:8..8:9): a <= _empty_/t148.g().(a)
       |[8:11..8:14): Int => scala/Int#
       |[8:18..8:19): a => _empty_/t148.g().(a)
       |[9:6..9:7): m <= _empty_/t148.m.
       |[9:14..9:15): M => _empty_/M#
       |[9:15..9:15):  => _empty_/M#`<init>`().
       |[10:6..10:7): p <= _empty_/t148.p.
       |[10:15..10:16): x <= local0
       |[10:15..10:21): x <- m <= local3
       |[10:15..10:21): x <- m <= local4
       |[10:20..10:21): m => _empty_/t148.m.
       |[10:22..10:30): if x > 0 <= local1
       |[10:22..10:30): if x > 0 <= local2
       |[10:25..10:26): x => local0
       |[10:27..10:28): > => scala/Int#`>`(+3).
       |[10:38..10:39): g => _empty_/t148.g().
       |[10:40..10:41): x => local0
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t148 {
       |  def g(a: Int) = a
       |  val m = new M(1)
       |  val p = for (x <- m if x > 0) yield g(x)
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
       |[7:7..7:11): t148 <= _empty_/t148.
       |[8:6..8:7): g <= _empty_/t148.g().
       |[8:8..8:9): a <= _empty_/t148.g().(a)
       |[8:11..8:14): Int => scala/Int#
       |[8:18..8:19): a => _empty_/t148.g().(a)
       |[9:6..9:7): m <= _empty_/t148.m.
       |[9:14..9:15): M => _empty_/M#
       |[9:15..9:15):  => _empty_/M#`<init>`().
       |[10:6..10:7): p <= _empty_/t148.p.
       |[10:15..10:21): x <- m <= local3
       |[10:15..10:16): x <= local0
       |[10:20..10:21): m => _empty_/t148.m.
       |[10:22..10:30): if x > 0 <= local1
       |[10:25..10:26): x => local0
       |[10:27..10:28): > => scala/Int#`>`(+3).
       |[10:38..10:39): g => _empty_/t148.g().
       |[10:40..10:41): x => local0
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t148 {
       |  def g(a: Int) = a
       |  val m = new M(1)
       |  val p = for (x <- m if x > 0) yield g(x)
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
       |_empty_/t148. => final object t148 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t148.g(). => method g(a: Int): Int
       |  a => _empty_/t148.g().(a)
       |  Int => scala/Int#
       |_empty_/t148.g().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t148.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t148.p. => val method p: M[Int]
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
       |local3 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local4 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |""".stripMargin,
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
    "SAM closures of a for comprehension with two guards",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t150 {
       |  val m = new M(1)
       |  val p = for (x <- m if x > 0 if x < 2) yield x
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
       |[7:7..7:11): t150 <= _empty_/t150.
       |[8:6..8:7): m <= _empty_/t150.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t150.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local5
       |[9:15..9:21): x <- m <= local6
       |[9:20..9:21): m => _empty_/t150.m.
       |[9:22..9:30): if x > 0 <= local1
       |[9:22..9:30): if x > 0 <= local2
       |[9:25..9:26): x => local0
       |[9:27..9:28): > => scala/Int#`>`(+3).
       |[9:31..9:39): if x < 2 <= local3
       |[9:31..9:39): if x < 2 <= local4
       |[9:34..9:35): x => local0
       |[9:36..9:37): < => scala/Int#`<`(+3).
       |[9:47..9:48): x => local0
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with two guards",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t150 {
       |  val m = new M(1)
       |  val p = for (x <- m if x > 0 if x < 2) yield x
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
       |[7:7..7:11): t150 <= _empty_/t150.
       |[8:6..8:7): m <= _empty_/t150.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t150.p.
       |[9:15..9:21): x <- m <= local5
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t150.m.
       |[9:22..9:30): if x > 0 <= local1
       |[9:25..9:26): x => local0
       |[9:27..9:28): > => scala/Int#`>`(+3).
       |[9:31..9:39): if x < 2 <= local3
       |[9:34..9:35): x => local0
       |[9:36..9:37): < => scala/Int#`<`(+3).
       |[9:47..9:48): x => local0
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with two guards",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t150 {
       |  val m = new M(1)
       |  val p = for (x <- m if x > 0 if x < 2) yield x
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
       |_empty_/t150. => final object t150 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t150.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t150.p. => val method p: M[Int]
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
       |local3 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local6 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a value and a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t151 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = x + 1 if y > 0) yield y
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
       |[7:7..7:11): t151 <= _empty_/t151.
       |[8:6..8:7): m <= _empty_/t151.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t151.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local6
       |[9:15..9:21): x <- m <= local7
       |[9:20..9:21): m => _empty_/t151.m.
       |[9:23..9:24): y <= local3
       |[9:27..9:28): x => local0
       |[9:29..9:30): + => scala/Int#`+`(+4).
       |[9:33..9:41): if y > 0 <= local4
       |[9:33..9:41): if y > 0 <= local5
       |[9:36..9:37): y => local3
       |[9:38..9:39): > => scala/Int#`>`(+3).
       |[9:49..9:50): y => local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a value and a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t151 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = x + 1 if y > 0) yield y
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
       |[7:7..7:11): t151 <= _empty_/t151.
       |[8:6..8:7): m <= _empty_/t151.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t151.p.
       |[9:15..9:21): x <- m <= local6
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t151.m.
       |[9:23..9:24): y <= local3
       |[9:27..9:28): x => local0
       |[9:29..9:30): + => scala/Int#`+`(+4).
       |[9:33..9:41): if y > 0 <= local4
       |[9:36..9:37): y => local3
       |[9:38..9:39): > => scala/Int#`>`(+3).
       |[9:49..9:50): y => local3
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a value and a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t151 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = x + 1 if y > 0) yield y
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
       |_empty_/t151. => final object t151 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t151.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t151.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => synthetic param x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local3 => val local y: Int
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Tuple2[Int, Int], Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic method f(x$1: Tuple2[Int, Int]): Boolean <: _empty_/F#f().
       |  x$1 => local1
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local6 => synthetic class $anon extends Object with F[Tuple2[Int, Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local7 => synthetic method f(x$2: Tuple2[Int, Int]): Int <: _empty_/F#f().
       |  x$2 => local2
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with two values",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t152 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = x; z = y) yield z
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
       |[7:7..7:11): t152 <= _empty_/t152.
       |[8:6..8:7): m <= _empty_/t152.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t152.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local4
       |[9:15..9:21): x <- m <= local5
       |[9:20..9:21): m => _empty_/t152.m.
       |[9:23..9:24): y <= local2
       |[9:27..9:28): x => local0
       |[9:30..9:31): z <= local3
       |[9:34..9:35): y => local2
       |[9:43..9:44): z => local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with two values",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t152 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = x; z = y) yield z
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
       |[7:7..7:11): t152 <= _empty_/t152.
       |[8:6..8:7): m <= _empty_/t152.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t152.p.
       |[9:15..9:21): x <- m <= local4
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t152.m.
       |[9:23..9:24): y <= local2
       |[9:27..9:28): x => local0
       |[9:30..9:31): z <= local3
       |[9:34..9:35): y => local2
       |[9:43..9:44): z => local3
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with two values",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t152 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = x; z = y) yield z
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
       |_empty_/t152. => final object t152 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t152.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t152.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple3[Int, Int, Int]
       |  Tuple3 => scala/Tuple3#
       |  Int => scala/Int#
       |local2 => val local y: Int
       |  Int => scala/Int#
       |local3 => val local z: Int
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Tuple3[Int, Int, Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple3 => scala/Tuple3#
       |  Int => scala/Int#
       |local5 => synthetic method f(x$1: Tuple3[Int, Int, Int]): Int <: _empty_/F#f().
       |  x$1 => local1
       |  Tuple3 => scala/Tuple3#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a value after two generators",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t153 {
       |  val m = new M(1)
       |  val p = for (x <- m; y <- m; z = x + y) yield z
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
       |[7:7..7:11): t153 <= _empty_/t153.
       |[8:6..8:7): m <= _empty_/t153.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t153.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local3
       |[9:15..9:21): x <- m <= local4
       |[9:20..9:21): m => _empty_/t153.m.
       |[9:23..9:24): y <= local1
       |[9:23..9:29): y <- m <= local6
       |[9:23..9:29): y <- m <= local7
       |[9:28..9:29): m => _empty_/t153.m.
       |[9:31..9:32): z <= local5
       |[9:35..9:36): x => local0
       |[9:37..9:38): + => scala/Int#`+`(+4).
       |[9:39..9:40): y => local1
       |[9:48..9:49): z => local5
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a value after two generators",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t153 {
       |  val m = new M(1)
       |  val p = for (x <- m; y <- m; z = x + y) yield z
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
       |[7:7..7:11): t153 <= _empty_/t153.
       |[8:6..8:7): m <= _empty_/t153.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t153.p.
       |[9:15..9:21): x <- m <= local3
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t153.m.
       |[9:23..9:29): y <- m <= local6
       |[9:23..9:24): y <= local1
       |[9:28..9:29): m => _empty_/t153.m.
       |[9:31..9:32): z <= local5
       |[9:35..9:36): x => local0
       |[9:37..9:38): + => scala/Int#`+`(+4).
       |[9:39..9:40): y => local1
       |[9:48..9:49): z => local5
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a value after two generators",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t153 {
       |  val m = new M(1)
       |  val p = for (x <- m; y <- m; z = x + y) yield z
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
       |_empty_/t153. => final object t153 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t153.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t153.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => val local y: Int
       |  Int => scala/Int#
       |local2 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with F[Int, M[Int]] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local4 => synthetic method f(x: Int): M[Int] <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local5 => val local z: Int
       |  Int => scala/Int#
       |local6 => synthetic class $anon extends Object with F[Tuple2[Int, Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local7 => synthetic method f(x$1: Tuple2[Int, Int]): Int <: _empty_/F#f().
       |  x$1 => local2
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with parentheses",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t154 {
       |  val m = new M(1)
       |  val p = for (x <- m if (x > 0); y = (x)) yield (y)
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
       |[7:7..7:11): t154 <= _empty_/t154.
       |[8:6..8:7): m <= _empty_/t154.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t154.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local5
       |[9:15..9:21): x <- m <= local6
       |[9:20..9:21): m => _empty_/t154.m.
       |[9:22..9:32): if (x > 0) <= local2
       |[9:22..9:32): if (x > 0) <= local3
       |[9:26..9:27): x => local0
       |[9:28..9:29): > => scala/Int#`>`(+3).
       |[9:34..9:35): y <= local4
       |[9:39..9:40): x => local0
       |[9:50..9:51): y => local4
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with parentheses",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t154 {
       |  val m = new M(1)
       |  val p = for (x <- m if (x > 0); y = (x)) yield (y)
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
       |[7:7..7:11): t154 <= _empty_/t154.
       |[8:6..8:7): m <= _empty_/t154.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t154.p.
       |[9:15..9:21): x <- m <= local5
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t154.m.
       |[9:22..9:32): if (x > 0) <= local2
       |[9:26..9:27): x => local0
       |[9:28..9:29): > => scala/Int#`>`(+3).
       |[9:34..9:35): y <= local4
       |[9:39..9:40): x => local0
       |[9:50..9:51): y => local4
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with parentheses",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t154 {
       |  val m = new M(1)
       |  val p = for (x <- m if (x > 0); y = (x)) yield (y)
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
       |_empty_/t154. => final object t154 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t154.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t154.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic method f(val x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => val local y: Int
       |  Int => scala/Int#
       |local5 => synthetic class $anon extends Object with F[Tuple2[Int, Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local6 => synthetic method f(x$1: Tuple2[Int, Int]): Int <: _empty_/F#f().
       |  x$1 => local1
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a tuple pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t155 {
       |  val m = new M((1, 2))
       |  val p = for ((a, b) <- m) yield a + b
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
       |[7:7..7:11): t155 <= _empty_/t155.
       |[8:6..8:7): m <= _empty_/t155.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t155.p.
       |[9:15..9:26): (a, b) <- m <= local4
       |[9:15..9:26): (a, b) <- m <= local5
       |[9:16..9:17): a <= local2
       |[9:19..9:20): b <= local3
       |[9:25..9:26): m => _empty_/t155.m.
       |[9:34..9:35): a => local2
       |[9:36..9:37): + => scala/Int#`+`(+4).
       |[9:38..9:39): b => local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a tuple pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t155 {
       |  val m = new M((1, 2))
       |  val p = for ((a, b) <- m) yield a + b
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
       |[7:7..7:11): t155 <= _empty_/t155.
       |[8:6..8:7): m <= _empty_/t155.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t155.p.
       |[9:15..9:26): (a, b) <- m <= local4
       |[9:16..9:17): a <= local2
       |[9:19..9:20): b <= local3
       |[9:25..9:26): m => _empty_/t155.m.
       |[9:34..9:35): a => local2
       |[9:36..9:37): + => scala/Int#`+`(+4).
       |[9:38..9:39): b => local3
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a tuple pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t155 {
       |  val m = new M((1, 2))
       |  val p = for ((a, b) <- m) yield a + b
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
       |_empty_/t155. => final object t155 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t155.m. => val method m: M[Tuple2[Int, Int]]
       |  M => _empty_/M#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/t155.p. => val method p: M[Int]
       |  M => _empty_/M#
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
       |local4 => synthetic class $anon extends Object with F[Tuple2[Int, Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local5 => synthetic method f(x$1: Tuple2[Int, Int]): Int <: _empty_/F#f().
       |  x$1 => local1
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a refutable pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t156 {
       |  val m = new M(Option(1))
       |  val p = for (Some(a) <- m) yield a
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
       |[7:7..7:11): t156 <= _empty_/t156.
       |[8:6..8:7): m <= _empty_/t156.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[8:16..8:22): Option => scala/Option.
       |[9:6..9:7): p <= _empty_/t156.p.
       |[9:15..9:19): Some => scala/Some.
       |[9:15..9:27): Some(a) <- m <= local3
       |[9:15..9:27): Some(a) <- m <= local4
       |[9:20..9:21): a <= local2
       |[9:26..9:27): m => _empty_/t156.m.
       |[9:35..9:36): a => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a refutable pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t156 {
       |  val m = new M(Option(1))
       |  val p = for (Some(a) <- m) yield a
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
       |[7:7..7:11): t156 <= _empty_/t156.
       |[8:6..8:7): m <= _empty_/t156.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[8:16..8:22): Option => scala/Option.
       |[9:6..9:7): p <= _empty_/t156.p.
       |[9:15..9:27): Some(a) <- m <= local3
       |[9:15..9:19): Some => scala/Some.
       |[9:20..9:21): a <= local2
       |[9:26..9:27): m => _empty_/t156.m.
       |[9:35..9:36): a => local2
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a refutable pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t156 {
       |  val m = new M(Option(1))
       |  val p = for (Some(a) <- m) yield a
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
       |_empty_/t156. => final object t156 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t156.m. => val method m: M[Option[Int]]
       |  M => _empty_/M#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |_empty_/t156.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => synthetic param check$ifrefutable$1: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with F[Option[Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local4 => synthetic method f(x$1: Option[Int]): Int <: _empty_/F#f().
       |  x$1 => local1
       |  Option => scala/Option#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a typed pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t157 {
       |  val m = new M(1)
       |  val p = for ((a: Int) <- m) yield a
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
       |[7:7..7:11): t157 <= _empty_/t157.
       |[8:6..8:7): m <= _empty_/t157.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t157.p.
       |[9:15..9:28): (a: Int) <- m <= local2
       |[9:15..9:28): (a: Int) <- m <= local3
       |[9:16..9:17): a <= local1
       |[9:19..9:22): Int => scala/Int#
       |[9:27..9:28): m => _empty_/t157.m.
       |[9:36..9:37): a => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a typed pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t157 {
       |  val m = new M(1)
       |  val p = for ((a: Int) <- m) yield a
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
       |[7:7..7:11): t157 <= _empty_/t157.
       |[8:6..8:7): m <= _empty_/t157.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t157.p.
       |[9:15..9:28): (a: Int) <- m <= local2
       |[9:16..9:17): a <= local1
       |[9:19..9:22): Int => scala/Int#
       |[9:27..9:28): m => _empty_/t157.m.
       |[9:36..9:37): a => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a typed pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t157 {
       |  val m = new M(1)
       |  val p = for ((a: Int) <- m) yield a
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
       |_empty_/t157. => final object t157 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t157.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t157.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => synthetic param check$ifrefutable$1: Int
       |  Int => scala/Int#
       |local1 => param a: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(a: Int): Int <: _empty_/F#f().
       |  a => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a wildcard pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t158 {
       |  val m = new M(1)
       |  val p = for (_ <- m) yield 0
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
       |[7:7..7:11): t158 <= _empty_/t158.
       |[8:6..8:7): m <= _empty_/t158.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t158.p.
       |[9:15..9:16): _ <= local0
       |[9:15..9:21): _ <- m <= local1
       |[9:15..9:21): _ <- m <= local2
       |[9:20..9:21): m => _empty_/t158.m.
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a wildcard pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t158 {
       |  val m = new M(1)
       |  val p = for (_ <- m) yield 0
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
       |[7:7..7:11): t158 <= _empty_/t158.
       |[8:6..8:7): m <= _empty_/t158.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t158.p.
       |[9:15..9:21): _ <- m <= local1
       |[9:15..9:16): _ <= local0
       |[9:20..9:21): m => _empty_/t158.m.
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a wildcard pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t158 {
       |  val m = new M(1)
       |  val p = for (_ <- m) yield 0
       |}
       |""".stripMargin,
    ScalaVersion.getExpected(
      List {
        ScalaVersion.Scala212 ->
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
             |_empty_/t158. => final object t158 extends AnyRef { +2 decls }
             |  AnyRef => scala/AnyRef#
             |_empty_/t158.m. => val method m: M[Int]
             |  M => _empty_/M#
             |  Int => scala/Int#
             |_empty_/t158.p. => val method p: M[Int]
             |  M => _empty_/M#
             |  Int => scala/Int#
             |local0 => param _: Int
             |  Int => scala/Int#
             |local1 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
             |  Object => java/lang/Object#
             |  F => _empty_/F#
             |  Int => scala/Int#
             |local2 => synthetic method f(_: Int): Int <: _empty_/F#f().
             |  _ => local0
             |  Int => scala/Int#
             |""".stripMargin
      },
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
         |_empty_/t158. => final object t158 extends AnyRef { +2 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t158.m. => val method m: M[Int]
         |  M => _empty_/M#
         |  Int => scala/Int#
         |_empty_/t158.p. => val method p: M[Int]
         |  M => _empty_/M#
         |  Int => scala/Int#
         |local0 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local1 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
         |  Object => java/lang/Object#
         |  F => _empty_/F#
         |  Int => scala/Int#
         |local2 => synthetic method f(x$1: Int): Int <: _empty_/F#f().
         |  x$1 => local0
         |  Int => scala/Int#
         |""".stripMargin,
    ),
  )

  occurrences(
    "SAM closures of a for comprehension in a yield",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t159 {
       |  val m = new M(1)
       |  val p = for (x <- m) yield for (y <- m) yield x + y
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
       |[7:7..7:11): t159 <= _empty_/t159.
       |[8:6..8:7): m <= _empty_/t159.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t159.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local2
       |[9:15..9:21): x <- m <= local3
       |[9:20..9:21): m => _empty_/t159.m.
       |[9:34..9:35): y <= local1
       |[9:34..9:40): y <- m <= local4
       |[9:34..9:40): y <- m <= local5
       |[9:39..9:40): m => _empty_/t159.m.
       |[9:48..9:49): x => local0
       |[9:50..9:51): + => scala/Int#`+`(+4).
       |[9:52..9:53): y => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension in a yield",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t159 {
       |  val m = new M(1)
       |  val p = for (x <- m) yield for (y <- m) yield x + y
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
       |[7:7..7:11): t159 <= _empty_/t159.
       |[8:6..8:7): m <= _empty_/t159.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t159.p.
       |[9:15..9:21): x <- m <= local2
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t159.m.
       |[9:34..9:40): y <- m <= local4
       |[9:34..9:35): y <= local1
       |[9:39..9:40): m => _empty_/t159.m.
       |[9:48..9:49): x => local0
       |[9:50..9:51): + => scala/Int#`+`(+4).
       |[9:52..9:53): y => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension in a yield",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t159 {
       |  val m = new M(1)
       |  val p = for (x <- m) yield for (y <- m) yield x + y
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
       |_empty_/t159. => final object t159 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t159.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t159.p. => val method p: M[M[Int]]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, M[Int]] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local3 => synthetic method f(x: Int): M[Int] <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local4 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local5 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension in a generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t160 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y) yield x
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
       |[7:7..7:11): t160 <= _empty_/t160.
       |[8:6..8:7): m <= _empty_/t160.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t160.p.
       |[9:15..9:16): x <= local1
       |[9:15..9:40): x <- for (y <- m) yield y <= local4
       |[9:15..9:40): x <- for (y <- m) yield y <= local5
       |[9:25..9:26): y <= local0
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:31): y <- m <= local3
       |[9:30..9:31): m => _empty_/t160.m.
       |[9:39..9:40): y => local0
       |[9:48..9:49): x => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension in a generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t160 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y) yield x
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
       |[7:7..7:11): t160 <= _empty_/t160.
       |[8:6..8:7): m <= _empty_/t160.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t160.p.
       |[9:15..9:40): x <- for (y <- m) yield y <= local4
       |[9:15..9:16): x <= local1
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:26): y <= local0
       |[9:30..9:31): m => _empty_/t160.m.
       |[9:39..9:40): y => local0
       |[9:48..9:49): x => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension in a generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t160 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y) yield x
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
       |_empty_/t160. => final object t160 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t160.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t160.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param y: Int
       |  Int => scala/Int#
       |local1 => param x: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local0
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local5 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension that yields a SAM lambda",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t161 {
       |  val m = new M(1)
       |  val p: M[F[Int, Int]] = for (x <- m if x > 0) yield y => x + y
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
       |[7:7..7:11): t161 <= _empty_/t161.
       |[8:6..8:7): m <= _empty_/t161.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t161.p.
       |[9:9..9:10): M => _empty_/M#
       |[9:11..9:12): F => _empty_/F#
       |[9:13..9:16): Int => scala/Int#
       |[9:18..9:21): Int => scala/Int#
       |[9:31..9:32): x <= local0
       |[9:31..9:37): x <- m <= local3
       |[9:31..9:37): x <- m <= local4
       |[9:36..9:37): m => _empty_/t161.m.
       |[9:38..9:46): if x > 0 <= local1
       |[9:38..9:46): if x > 0 <= local2
       |[9:41..9:42): x => local0
       |[9:43..9:44): > => scala/Int#`>`(+3).
       |[9:54..9:55): y <= local7
       |[9:54..9:64): y => x + y <= local5
       |[9:54..9:64): y => x + y <= local6
       |[9:59..9:60): x => local0
       |[9:61..9:62): + => scala/Int#`+`(+4).
       |[9:63..9:64): y => local7
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension that yields a SAM lambda",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t161 {
       |  val m = new M(1)
       |  val p: M[F[Int, Int]] = for (x <- m if x > 0) yield y => x + y
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
       |[7:7..7:11): t161 <= _empty_/t161.
       |[8:6..8:7): m <= _empty_/t161.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t161.p.
       |[9:9..9:10): M => _empty_/M#
       |[9:11..9:12): F => _empty_/F#
       |[9:13..9:16): Int => scala/Int#
       |[9:18..9:21): Int => scala/Int#
       |[9:31..9:37): x <- m <= local3
       |[9:31..9:32): x <= local0
       |[9:36..9:37): m => _empty_/t161.m.
       |[9:38..9:46): if x > 0 <= local1
       |[9:41..9:42): x => local0
       |[9:43..9:44): > => scala/Int#`>`(+3).
       |[9:54..9:64): y => x + y <= local5
       |[9:54..9:55): y <= local7
       |[9:59..9:60): x => local0
       |[9:61..9:62): + => scala/Int#`+`(+4).
       |[9:63..9:64): y => local7
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension that yields a SAM lambda",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t161 {
       |  val m = new M(1)
       |  val p: M[F[Int, Int]] = for (x <- m if x > 0) yield y => x + y
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
       |_empty_/t161. => final object t161 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t161.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t161.p. => val method p: M[F[Int, Int]]
       |  M => _empty_/M#
       |  F => _empty_/F#
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
       |local3 => synthetic class $anon extends Object with F[Int, F[Int, Int]] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local4 => synthetic method f(x: Int): F[Int, Int] <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  F => _empty_/F#
       |local5 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local6 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local7
       |  Int => scala/Int#
       |local7 => param y: Int
       |  Int => scala/Int#
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
    "SAM closures of a for comprehension with a guard after a nested for",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t163 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y if x > 0) yield x
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
       |[7:7..7:11): t163 <= _empty_/t163.
       |[8:6..8:7): m <= _empty_/t163.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t163.p.
       |[9:15..9:16): x <= local1
       |[9:15..9:40): x <- for (y <- m) yield y <= local6
       |[9:15..9:40): x <- for (y <- m) yield y <= local7
       |[9:25..9:26): y <= local0
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:31): y <- m <= local3
       |[9:30..9:31): m => _empty_/t163.m.
       |[9:39..9:40): y => local0
       |[9:41..9:49): if x > 0 <= local4
       |[9:41..9:49): if x > 0 <= local5
       |[9:44..9:45): x => local1
       |[9:46..9:47): > => scala/Int#`>`(+3).
       |[9:57..9:58): x => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a guard after a nested for",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t163 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y if x > 0) yield x
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
       |[7:7..7:11): t163 <= _empty_/t163.
       |[8:6..8:7): m <= _empty_/t163.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t163.p.
       |[9:15..9:40): x <- for (y <- m) yield y <= local6
       |[9:15..9:16): x <= local1
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:26): y <= local0
       |[9:30..9:31): m => _empty_/t163.m.
       |[9:39..9:40): y => local0
       |[9:41..9:49): if x > 0 <= local4
       |[9:44..9:45): x => local1
       |[9:46..9:47): > => scala/Int#`>`(+3).
       |[9:57..9:58): x => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a guard after a nested for",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t163 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y if x > 0) yield x
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
       |_empty_/t163. => final object t163 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t163.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t163.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param y: Int
       |  Int => scala/Int#
       |local1 => param x: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local0
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local6 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local7 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with two guards after a nested for",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t164 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y if x > 0 if x < 2) yield x
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
       |[7:7..7:11): t164 <= _empty_/t164.
       |[8:6..8:7): m <= _empty_/t164.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t164.p.
       |[9:15..9:16): x <= local1
       |[9:15..9:40): x <- for (y <- m) yield y <= local8
       |[9:15..9:40): x <- for (y <- m) yield y <= local9
       |[9:25..9:26): y <= local0
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:31): y <- m <= local3
       |[9:30..9:31): m => _empty_/t164.m.
       |[9:39..9:40): y => local0
       |[9:41..9:49): if x > 0 <= local4
       |[9:41..9:49): if x > 0 <= local5
       |[9:44..9:45): x => local1
       |[9:46..9:47): > => scala/Int#`>`(+3).
       |[9:50..9:58): if x < 2 <= local6
       |[9:50..9:58): if x < 2 <= local7
       |[9:53..9:54): x => local1
       |[9:55..9:56): < => scala/Int#`<`(+3).
       |[9:66..9:67): x => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with two guards after a nested for",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t164 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y if x > 0 if x < 2) yield x
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
       |[7:7..7:11): t164 <= _empty_/t164.
       |[8:6..8:7): m <= _empty_/t164.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t164.p.
       |[9:15..9:40): x <- for (y <- m) yield y <= local8
       |[9:15..9:16): x <= local1
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:26): y <= local0
       |[9:30..9:31): m => _empty_/t164.m.
       |[9:39..9:40): y => local0
       |[9:41..9:49): if x > 0 <= local4
       |[9:44..9:45): x => local1
       |[9:46..9:47): > => scala/Int#`>`(+3).
       |[9:50..9:58): if x < 2 <= local6
       |[9:53..9:54): x => local1
       |[9:55..9:56): < => scala/Int#`<`(+3).
       |[9:66..9:67): x => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with two guards after a nested for",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t164 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y if x > 0 if x < 2) yield x
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
       |_empty_/t164. => final object t164 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t164.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t164.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param y: Int
       |  Int => scala/Int#
       |local1 => param x: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local0
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local6 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local7 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local8 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local9 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a guard after a nested for in parentheses",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t165 {
       |  val m = new M(1)
       |  val p = for (x <- (for (y <- m) yield y) if x > 0) yield x
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
       |[7:7..7:11): t165 <= _empty_/t165.
       |[8:6..8:7): m <= _empty_/t165.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t165.p.
       |[9:15..9:16): x <= local1
       |[9:15..9:42): x <- (for (y <- m) yield y) <= local6
       |[9:15..9:42): x <- (for (y <- m) yield y) <= local7
       |[9:26..9:27): y <= local0
       |[9:26..9:32): y <- m <= local2
       |[9:26..9:32): y <- m <= local3
       |[9:31..9:32): m => _empty_/t165.m.
       |[9:40..9:41): y => local0
       |[9:43..9:51): if x > 0 <= local4
       |[9:43..9:51): if x > 0 <= local5
       |[9:46..9:47): x => local1
       |[9:48..9:49): > => scala/Int#`>`(+3).
       |[9:59..9:60): x => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a guard after a nested for in parentheses",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t165 {
       |  val m = new M(1)
       |  val p = for (x <- (for (y <- m) yield y) if x > 0) yield x
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
       |[7:7..7:11): t165 <= _empty_/t165.
       |[8:6..8:7): m <= _empty_/t165.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t165.p.
       |[9:15..9:42): x <- (for (y <- m) yield y) <= local6
       |[9:15..9:16): x <= local1
       |[9:26..9:32): y <- m <= local2
       |[9:26..9:27): y <= local0
       |[9:31..9:32): m => _empty_/t165.m.
       |[9:40..9:41): y => local0
       |[9:43..9:51): if x > 0 <= local4
       |[9:46..9:47): x => local1
       |[9:48..9:49): > => scala/Int#`>`(+3).
       |[9:59..9:60): x => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a guard after a nested for in parentheses",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t165 {
       |  val m = new M(1)
       |  val p = for (x <- (for (y <- m) yield y) if x > 0) yield x
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
       |_empty_/t165. => final object t165 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t165.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t165.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param y: Int
       |  Int => scala/Int#
       |local1 => param x: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local0
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local6 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local7 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with 22 values",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t166 {
       |  val m = new M(1)
       |  val p = for {
       |    x <- m
       |    a1 = x
       |    a2 = a1
       |    a3 = a2
       |    a4 = a3
       |    a5 = a4
       |    a6 = a5
       |    a7 = a6
       |    a8 = a7
       |    a9 = a8
       |    a10 = a9
       |    a11 = a10
       |    a12 = a11
       |    a13 = a12
       |    a14 = a13
       |    a15 = a14
       |    a16 = a15
       |    a17 = a16
       |    a18 = a17
       |    a19 = a18
       |    a20 = a19
       |    a21 = a20
       |    a22 = a21
       |  } yield a22
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
       |[7:7..7:11): t166 <= _empty_/t166.
       |[8:6..8:7): m <= _empty_/t166.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t166.p.
       |[10:4..10:5): x <= local0
       |[10:4..10:10): x <- m <= local25
       |[10:4..10:10): x <- m <= local26
       |[10:9..10:10): m => _empty_/t166.m.
       |[11:4..11:6): a1 <= local3
       |[11:9..11:10): x => local0
       |[12:4..12:6): a2 <= local4
       |[12:9..12:11): a1 => local3
       |[13:4..13:6): a3 <= local5
       |[13:9..13:11): a2 => local4
       |[14:4..14:6): a4 <= local6
       |[14:9..14:11): a3 => local5
       |[15:4..15:6): a5 <= local7
       |[15:9..15:11): a4 => local6
       |[16:4..16:6): a6 <= local8
       |[16:9..16:11): a5 => local7
       |[17:4..17:6): a7 <= local9
       |[17:9..17:11): a6 => local8
       |[18:4..18:6): a8 <= local10
       |[18:9..18:11): a7 => local9
       |[19:4..19:6): a9 <= local11
       |[19:9..19:11): a8 => local10
       |[20:4..20:7): a10 <= local12
       |[20:10..20:12): a9 => local11
       |[21:4..21:7): a11 <= local13
       |[21:10..21:13): a10 => local12
       |[22:4..22:7): a12 <= local14
       |[22:10..22:13): a11 => local13
       |[23:4..23:7): a13 <= local15
       |[23:10..23:13): a12 => local14
       |[24:4..24:7): a14 <= local16
       |[24:10..24:13): a13 => local15
       |[25:4..25:7): a15 <= local17
       |[25:10..25:13): a14 => local16
       |[26:4..26:7): a16 <= local18
       |[26:10..26:13): a15 => local17
       |[27:4..27:7): a17 <= local19
       |[27:10..27:13): a16 => local18
       |[28:4..28:7): a18 <= local20
       |[28:10..28:13): a17 => local19
       |[29:4..29:7): a19 <= local21
       |[29:10..29:13): a18 => local20
       |[30:4..30:7): a20 <= local22
       |[30:10..30:13): a19 => local21
       |[31:4..31:7): a21 <= local23
       |[31:10..31:13): a20 => local22
       |[32:4..32:7): a22 <= local24
       |[32:10..32:13): a21 => local23
       |[33:10..33:13): a22 => local24
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with 22 values",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t166 {
       |  val m = new M(1)
       |  val p = for {
       |    x <- m
       |    a1 = x
       |    a2 = a1
       |    a3 = a2
       |    a4 = a3
       |    a5 = a4
       |    a6 = a5
       |    a7 = a6
       |    a8 = a7
       |    a9 = a8
       |    a10 = a9
       |    a11 = a10
       |    a12 = a11
       |    a13 = a12
       |    a14 = a13
       |    a15 = a14
       |    a16 = a15
       |    a17 = a16
       |    a18 = a17
       |    a19 = a18
       |    a20 = a19
       |    a21 = a20
       |    a22 = a21
       |  } yield a22
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
       |[7:7..7:11): t166 <= _empty_/t166.
       |[8:6..8:7): m <= _empty_/t166.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t166.p.
       |[10:4..10:10): x <- m <= local25
       |[10:4..10:5): x <= local0
       |[10:9..10:10): m => _empty_/t166.m.
       |[11:4..11:6): a1 <= local3
       |[11:9..11:10): x => local0
       |[12:4..12:6): a2 <= local4
       |[12:9..12:11): a1 => local3
       |[13:4..13:6): a3 <= local5
       |[13:9..13:11): a2 => local4
       |[14:4..14:6): a4 <= local6
       |[14:9..14:11): a3 => local5
       |[15:4..15:6): a5 <= local7
       |[15:9..15:11): a4 => local6
       |[16:4..16:6): a6 <= local8
       |[16:9..16:11): a5 => local7
       |[17:4..17:6): a7 <= local9
       |[17:9..17:11): a6 => local8
       |[18:4..18:6): a8 <= local10
       |[18:9..18:11): a7 => local9
       |[19:4..19:6): a9 <= local11
       |[19:9..19:11): a8 => local10
       |[20:4..20:7): a10 <= local12
       |[20:10..20:12): a9 => local11
       |[21:4..21:7): a11 <= local13
       |[21:10..21:13): a10 => local12
       |[22:4..22:7): a12 <= local14
       |[22:10..22:13): a11 => local13
       |[23:4..23:7): a13 <= local15
       |[23:10..23:13): a12 => local14
       |[24:4..24:7): a14 <= local16
       |[24:10..24:13): a13 => local15
       |[25:4..25:7): a15 <= local17
       |[25:10..25:13): a14 => local16
       |[26:4..26:7): a16 <= local18
       |[26:10..26:13): a15 => local17
       |[27:4..27:7): a17 <= local19
       |[27:10..27:13): a16 => local18
       |[28:4..28:7): a18 <= local20
       |[28:10..28:13): a17 => local19
       |[29:4..29:7): a19 <= local21
       |[29:10..29:13): a18 => local20
       |[30:4..30:7): a20 <= local22
       |[30:10..30:13): a19 => local21
       |[31:4..31:7): a21 <= local23
       |[31:10..31:13): a20 => local22
       |[32:4..32:7): a22 <= local24
       |[32:10..32:13): a21 => local23
       |[33:10..33:13): a22 => local24
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with 22 values",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t166 {
       |  val m = new M(1)
       |  val p = for {
       |    x <- m
       |    a1 = x
       |    a2 = a1
       |    a3 = a2
       |    a4 = a3
       |    a5 = a4
       |    a6 = a5
       |    a7 = a6
       |    a8 = a7
       |    a9 = a8
       |    a10 = a9
       |    a11 = a10
       |    a12 = a11
       |    a13 = a12
       |    a14 = a13
       |    a15 = a14
       |    a16 = a15
       |    a17 = a16
       |    a18 = a17
       |    a19 = a18
       |    a20 = a19
       |    a21 = a20
       |    a22 = a21
       |  } yield a22
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
       |_empty_/t166. => final object t166 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t166.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t166.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$2: Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
       |  Tuple22 => scala/Tuple22#
       |  Int => scala/Int#
       |local2 => synthetic param x$3: Tuple2[Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int], Int]
       |  Tuple2 => scala/Tuple2#
       |  Tuple22 => scala/Tuple22#
       |  Int => scala/Int#
       |local3 => val local a1: Int
       |  Int => scala/Int#
       |local4 => val local a2: Int
       |  Int => scala/Int#
       |local5 => val local a3: Int
       |  Int => scala/Int#
       |local6 => val local a4: Int
       |  Int => scala/Int#
       |local7 => val local a5: Int
       |  Int => scala/Int#
       |local8 => val local a6: Int
       |  Int => scala/Int#
       |local9 => val local a7: Int
       |  Int => scala/Int#
       |local10 => val local a8: Int
       |  Int => scala/Int#
       |local11 => val local a9: Int
       |  Int => scala/Int#
       |local12 => val local a10: Int
       |  Int => scala/Int#
       |local13 => val local a11: Int
       |  Int => scala/Int#
       |local14 => val local a12: Int
       |  Int => scala/Int#
       |local15 => val local a13: Int
       |  Int => scala/Int#
       |local16 => val local a14: Int
       |  Int => scala/Int#
       |local17 => val local a15: Int
       |  Int => scala/Int#
       |local18 => val local a16: Int
       |  Int => scala/Int#
       |local19 => val local a17: Int
       |  Int => scala/Int#
       |local20 => val local a18: Int
       |  Int => scala/Int#
       |local21 => val local a19: Int
       |  Int => scala/Int#
       |local22 => val local a20: Int
       |  Int => scala/Int#
       |local23 => val local a21: Int
       |  Int => scala/Int#
       |local24 => val local a22: Int
       |  Int => scala/Int#
       |local25 => synthetic class $anon extends Object with F[Tuple2[Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int], Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Tuple22 => scala/Tuple22#
       |  Int => scala/Int#
       |local26 => synthetic method f(x$3: Tuple2[Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int], Int]): Int <: _empty_/F#f().
       |  x$3 => local2
       |  Tuple2 => scala/Tuple2#
       |  Tuple22 => scala/Tuple22#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with 43 values",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t167 {
       |  val m = new M(1)
       |  val p = for {
       |    x <- m
       |    a1 = x
       |    a2 = a1
       |    a3 = a2
       |    a4 = a3
       |    a5 = a4
       |    a6 = a5
       |    a7 = a6
       |    a8 = a7
       |    a9 = a8
       |    a10 = a9
       |    a11 = a10
       |    a12 = a11
       |    a13 = a12
       |    a14 = a13
       |    a15 = a14
       |    a16 = a15
       |    a17 = a16
       |    a18 = a17
       |    a19 = a18
       |    a20 = a19
       |    a21 = a20
       |    a22 = a21
       |    a23 = a22
       |    a24 = a23
       |    a25 = a24
       |    a26 = a25
       |    a27 = a26
       |    a28 = a27
       |    a29 = a28
       |    a30 = a29
       |    a31 = a30
       |    a32 = a31
       |    a33 = a32
       |    a34 = a33
       |    a35 = a34
       |    a36 = a35
       |    a37 = a36
       |    a38 = a37
       |    a39 = a38
       |    a40 = a39
       |    a41 = a40
       |    a42 = a41
       |    a43 = a42
       |  } yield a43
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
       |[7:7..7:11): t167 <= _empty_/t167.
       |[8:6..8:7): m <= _empty_/t167.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t167.p.
       |[10:4..10:5): x <= local0
       |[10:4..10:10): x <- m <= local47
       |[10:4..10:10): x <- m <= local48
       |[10:9..10:10): m => _empty_/t167.m.
       |[11:4..11:6): a1 <= local4
       |[11:9..11:10): x => local0
       |[12:4..12:6): a2 <= local5
       |[12:9..12:11): a1 => local4
       |[13:4..13:6): a3 <= local6
       |[13:9..13:11): a2 => local5
       |[14:4..14:6): a4 <= local7
       |[14:9..14:11): a3 => local6
       |[15:4..15:6): a5 <= local8
       |[15:9..15:11): a4 => local7
       |[16:4..16:6): a6 <= local9
       |[16:9..16:11): a5 => local8
       |[17:4..17:6): a7 <= local10
       |[17:9..17:11): a6 => local9
       |[18:4..18:6): a8 <= local11
       |[18:9..18:11): a7 => local10
       |[19:4..19:6): a9 <= local12
       |[19:9..19:11): a8 => local11
       |[20:4..20:7): a10 <= local13
       |[20:10..20:12): a9 => local12
       |[21:4..21:7): a11 <= local14
       |[21:10..21:13): a10 => local13
       |[22:4..22:7): a12 <= local15
       |[22:10..22:13): a11 => local14
       |[23:4..23:7): a13 <= local16
       |[23:10..23:13): a12 => local15
       |[24:4..24:7): a14 <= local17
       |[24:10..24:13): a13 => local16
       |[25:4..25:7): a15 <= local18
       |[25:10..25:13): a14 => local17
       |[26:4..26:7): a16 <= local19
       |[26:10..26:13): a15 => local18
       |[27:4..27:7): a17 <= local20
       |[27:10..27:13): a16 => local19
       |[28:4..28:7): a18 <= local21
       |[28:10..28:13): a17 => local20
       |[29:4..29:7): a19 <= local22
       |[29:10..29:13): a18 => local21
       |[30:4..30:7): a20 <= local23
       |[30:10..30:13): a19 => local22
       |[31:4..31:7): a21 <= local24
       |[31:10..31:13): a20 => local23
       |[32:4..32:7): a22 <= local25
       |[32:10..32:13): a21 => local24
       |[33:4..33:7): a23 <= local26
       |[33:10..33:13): a22 => local25
       |[34:4..34:7): a24 <= local27
       |[34:10..34:13): a23 => local26
       |[35:4..35:7): a25 <= local28
       |[35:10..35:13): a24 => local27
       |[36:4..36:7): a26 <= local29
       |[36:10..36:13): a25 => local28
       |[37:4..37:7): a27 <= local30
       |[37:10..37:13): a26 => local29
       |[38:4..38:7): a28 <= local31
       |[38:10..38:13): a27 => local30
       |[39:4..39:7): a29 <= local32
       |[39:10..39:13): a28 => local31
       |[40:4..40:7): a30 <= local33
       |[40:10..40:13): a29 => local32
       |[41:4..41:7): a31 <= local34
       |[41:10..41:13): a30 => local33
       |[42:4..42:7): a32 <= local35
       |[42:10..42:13): a31 => local34
       |[43:4..43:7): a33 <= local36
       |[43:10..43:13): a32 => local35
       |[44:4..44:7): a34 <= local37
       |[44:10..44:13): a33 => local36
       |[45:4..45:7): a35 <= local38
       |[45:10..45:13): a34 => local37
       |[46:4..46:7): a36 <= local39
       |[46:10..46:13): a35 => local38
       |[47:4..47:7): a37 <= local40
       |[47:10..47:13): a36 => local39
       |[48:4..48:7): a38 <= local41
       |[48:10..48:13): a37 => local40
       |[49:4..49:7): a39 <= local42
       |[49:10..49:13): a38 => local41
       |[50:4..50:7): a40 <= local43
       |[50:10..50:13): a39 => local42
       |[51:4..51:7): a41 <= local44
       |[51:10..51:13): a40 => local43
       |[52:4..52:7): a42 <= local45
       |[52:10..52:13): a41 => local44
       |[53:4..53:7): a43 <= local46
       |[53:10..53:13): a42 => local45
       |[54:10..54:13): a43 => local46
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with 43 values",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t167 {
       |  val m = new M(1)
       |  val p = for {
       |    x <- m
       |    a1 = x
       |    a2 = a1
       |    a3 = a2
       |    a4 = a3
       |    a5 = a4
       |    a6 = a5
       |    a7 = a6
       |    a8 = a7
       |    a9 = a8
       |    a10 = a9
       |    a11 = a10
       |    a12 = a11
       |    a13 = a12
       |    a14 = a13
       |    a15 = a14
       |    a16 = a15
       |    a17 = a16
       |    a18 = a17
       |    a19 = a18
       |    a20 = a19
       |    a21 = a20
       |    a22 = a21
       |    a23 = a22
       |    a24 = a23
       |    a25 = a24
       |    a26 = a25
       |    a27 = a26
       |    a28 = a27
       |    a29 = a28
       |    a30 = a29
       |    a31 = a30
       |    a32 = a31
       |    a33 = a32
       |    a34 = a33
       |    a35 = a34
       |    a36 = a35
       |    a37 = a36
       |    a38 = a37
       |    a39 = a38
       |    a40 = a39
       |    a41 = a40
       |    a42 = a41
       |    a43 = a42
       |  } yield a43
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
       |[7:7..7:11): t167 <= _empty_/t167.
       |[8:6..8:7): m <= _empty_/t167.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t167.p.
       |[10:4..10:10): x <- m <= local47
       |[10:4..10:5): x <= local0
       |[10:9..10:10): m => _empty_/t167.m.
       |[11:4..11:6): a1 <= local4
       |[11:9..11:10): x => local0
       |[12:4..12:6): a2 <= local5
       |[12:9..12:11): a1 => local4
       |[13:4..13:6): a3 <= local6
       |[13:9..13:11): a2 => local5
       |[14:4..14:6): a4 <= local7
       |[14:9..14:11): a3 => local6
       |[15:4..15:6): a5 <= local8
       |[15:9..15:11): a4 => local7
       |[16:4..16:6): a6 <= local9
       |[16:9..16:11): a5 => local8
       |[17:4..17:6): a7 <= local10
       |[17:9..17:11): a6 => local9
       |[18:4..18:6): a8 <= local11
       |[18:9..18:11): a7 => local10
       |[19:4..19:6): a9 <= local12
       |[19:9..19:11): a8 => local11
       |[20:4..20:7): a10 <= local13
       |[20:10..20:12): a9 => local12
       |[21:4..21:7): a11 <= local14
       |[21:10..21:13): a10 => local13
       |[22:4..22:7): a12 <= local15
       |[22:10..22:13): a11 => local14
       |[23:4..23:7): a13 <= local16
       |[23:10..23:13): a12 => local15
       |[24:4..24:7): a14 <= local17
       |[24:10..24:13): a13 => local16
       |[25:4..25:7): a15 <= local18
       |[25:10..25:13): a14 => local17
       |[26:4..26:7): a16 <= local19
       |[26:10..26:13): a15 => local18
       |[27:4..27:7): a17 <= local20
       |[27:10..27:13): a16 => local19
       |[28:4..28:7): a18 <= local21
       |[28:10..28:13): a17 => local20
       |[29:4..29:7): a19 <= local22
       |[29:10..29:13): a18 => local21
       |[30:4..30:7): a20 <= local23
       |[30:10..30:13): a19 => local22
       |[31:4..31:7): a21 <= local24
       |[31:10..31:13): a20 => local23
       |[32:4..32:7): a22 <= local25
       |[32:10..32:13): a21 => local24
       |[33:4..33:7): a23 <= local26
       |[33:10..33:13): a22 => local25
       |[34:4..34:7): a24 <= local27
       |[34:10..34:13): a23 => local26
       |[35:4..35:7): a25 <= local28
       |[35:10..35:13): a24 => local27
       |[36:4..36:7): a26 <= local29
       |[36:10..36:13): a25 => local28
       |[37:4..37:7): a27 <= local30
       |[37:10..37:13): a26 => local29
       |[38:4..38:7): a28 <= local31
       |[38:10..38:13): a27 => local30
       |[39:4..39:7): a29 <= local32
       |[39:10..39:13): a28 => local31
       |[40:4..40:7): a30 <= local33
       |[40:10..40:13): a29 => local32
       |[41:4..41:7): a31 <= local34
       |[41:10..41:13): a30 => local33
       |[42:4..42:7): a32 <= local35
       |[42:10..42:13): a31 => local34
       |[43:4..43:7): a33 <= local36
       |[43:10..43:13): a32 => local35
       |[44:4..44:7): a34 <= local37
       |[44:10..44:13): a33 => local36
       |[45:4..45:7): a35 <= local38
       |[45:10..45:13): a34 => local37
       |[46:4..46:7): a36 <= local39
       |[46:10..46:13): a35 => local38
       |[47:4..47:7): a37 <= local40
       |[47:10..47:13): a36 => local39
       |[48:4..48:7): a38 <= local41
       |[48:10..48:13): a37 => local40
       |[49:4..49:7): a39 <= local42
       |[49:10..49:13): a38 => local41
       |[50:4..50:7): a40 <= local43
       |[50:10..50:13): a39 => local42
       |[51:4..51:7): a41 <= local44
       |[51:10..51:13): a40 => local43
       |[52:4..52:7): a42 <= local45
       |[52:10..52:13): a41 => local44
       |[53:4..53:7): a43 <= local46
       |[53:10..53:13): a42 => local45
       |[54:10..54:13): a43 => local46
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with 43 values",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t167 {
       |  val m = new M(1)
       |  val p = for {
       |    x <- m
       |    a1 = x
       |    a2 = a1
       |    a3 = a2
       |    a4 = a3
       |    a5 = a4
       |    a6 = a5
       |    a7 = a6
       |    a8 = a7
       |    a9 = a8
       |    a10 = a9
       |    a11 = a10
       |    a12 = a11
       |    a13 = a12
       |    a14 = a13
       |    a15 = a14
       |    a16 = a15
       |    a17 = a16
       |    a18 = a17
       |    a19 = a18
       |    a20 = a19
       |    a21 = a20
       |    a22 = a21
       |    a23 = a22
       |    a24 = a23
       |    a25 = a24
       |    a26 = a25
       |    a27 = a26
       |    a28 = a27
       |    a29 = a28
       |    a30 = a29
       |    a31 = a30
       |    a32 = a31
       |    a33 = a32
       |    a34 = a33
       |    a35 = a34
       |    a36 = a35
       |    a37 = a36
       |    a38 = a37
       |    a39 = a38
       |    a40 = a39
       |    a41 = a40
       |    a42 = a41
       |    a43 = a42
       |  } yield a43
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
       |_empty_/t167. => final object t167 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t167.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t167.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$2: Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
       |  Tuple22 => scala/Tuple22#
       |  Int => scala/Int#
       |local2 => synthetic param x$4: Tuple22[Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int], Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]
       |  Tuple22 => scala/Tuple22#
       |  Int => scala/Int#
       |local3 => synthetic param x$5: Tuple2[Tuple22[Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int], Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int], Int]
       |  Tuple2 => scala/Tuple2#
       |  Tuple22 => scala/Tuple22#
       |  Int => scala/Int#
       |local4 => val local a1: Int
       |  Int => scala/Int#
       |local5 => val local a2: Int
       |  Int => scala/Int#
       |local6 => val local a3: Int
       |  Int => scala/Int#
       |local7 => val local a4: Int
       |  Int => scala/Int#
       |local8 => val local a5: Int
       |  Int => scala/Int#
       |local9 => val local a6: Int
       |  Int => scala/Int#
       |local10 => val local a7: Int
       |  Int => scala/Int#
       |local11 => val local a8: Int
       |  Int => scala/Int#
       |local12 => val local a9: Int
       |  Int => scala/Int#
       |local13 => val local a10: Int
       |  Int => scala/Int#
       |local14 => val local a11: Int
       |  Int => scala/Int#
       |local15 => val local a12: Int
       |  Int => scala/Int#
       |local16 => val local a13: Int
       |  Int => scala/Int#
       |local17 => val local a14: Int
       |  Int => scala/Int#
       |local18 => val local a15: Int
       |  Int => scala/Int#
       |local19 => val local a16: Int
       |  Int => scala/Int#
       |local20 => val local a17: Int
       |  Int => scala/Int#
       |local21 => val local a18: Int
       |  Int => scala/Int#
       |local22 => val local a19: Int
       |  Int => scala/Int#
       |local23 => val local a20: Int
       |  Int => scala/Int#
       |local24 => val local a21: Int
       |  Int => scala/Int#
       |local25 => val local a22: Int
       |  Int => scala/Int#
       |local26 => val local a23: Int
       |  Int => scala/Int#
       |local27 => val local a24: Int
       |  Int => scala/Int#
       |local28 => val local a25: Int
       |  Int => scala/Int#
       |local29 => val local a26: Int
       |  Int => scala/Int#
       |local30 => val local a27: Int
       |  Int => scala/Int#
       |local31 => val local a28: Int
       |  Int => scala/Int#
       |local32 => val local a29: Int
       |  Int => scala/Int#
       |local33 => val local a30: Int
       |  Int => scala/Int#
       |local34 => val local a31: Int
       |  Int => scala/Int#
       |local35 => val local a32: Int
       |  Int => scala/Int#
       |local36 => val local a33: Int
       |  Int => scala/Int#
       |local37 => val local a34: Int
       |  Int => scala/Int#
       |local38 => val local a35: Int
       |  Int => scala/Int#
       |local39 => val local a36: Int
       |  Int => scala/Int#
       |local40 => val local a37: Int
       |  Int => scala/Int#
       |local41 => val local a38: Int
       |  Int => scala/Int#
       |local42 => val local a39: Int
       |  Int => scala/Int#
       |local43 => val local a40: Int
       |  Int => scala/Int#
       |local44 => val local a41: Int
       |  Int => scala/Int#
       |local45 => val local a42: Int
       |  Int => scala/Int#
       |local46 => val local a43: Int
       |  Int => scala/Int#
       |local47 => synthetic class $anon extends Object with F[Tuple2[Tuple22[Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int], Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int], Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Tuple22 => scala/Tuple22#
       |  Int => scala/Int#
       |local48 => synthetic method f(x$5: Tuple2[Tuple22[Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int], Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int], Int]): Int <: _empty_/F#f().
       |  x$5 => local3
       |  Tuple2 => scala/Tuple2#
       |  Tuple22 => scala/Tuple22#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a for in a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t168 {
       |  val m = new M(1)
       |  val p = for (x <- m if (for (y <- m) yield y + x) ne null) yield x
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
       |[7:7..7:11): t168 <= _empty_/t168.
       |[8:6..8:7): m <= _empty_/t168.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t168.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local6
       |[9:15..9:21): x <- m <= local7
       |[9:20..9:21): m => _empty_/t168.m.
       |[9:22..9:59): if (for (y <- m) yield y + x) ne null <= local1
       |[9:22..9:59): if (for (y <- m) yield y + x) ne null <= local2
       |[9:31..9:32): y <= local3
       |[9:31..9:37): y <- m <= local4
       |[9:31..9:37): y <- m <= local5
       |[9:36..9:37): m => _empty_/t168.m.
       |[9:45..9:46): y => local3
       |[9:47..9:48): + => scala/Int#`+`(+4).
       |[9:49..9:50): x => local0
       |[9:52..9:54): ne => java/lang/Object#ne().
       |[9:67..9:68): x => local0
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a for in a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t168 {
       |  val m = new M(1)
       |  val p = for (x <- m if (for (y <- m) yield y + x) ne null) yield x
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
       |[7:7..7:11): t168 <= _empty_/t168.
       |[8:6..8:7): m <= _empty_/t168.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t168.p.
       |[9:15..9:21): x <- m <= local6
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t168.m.
       |[9:22..9:59): if (for (y <- m) yield y + x) ne null <= local1
       |[9:31..9:37): y <- m <= local4
       |[9:31..9:32): y <= local3
       |[9:36..9:37): m => _empty_/t168.m.
       |[9:45..9:46): y => local3
       |[9:47..9:48): + => scala/Int#`+`(+4).
       |[9:49..9:50): x => local0
       |[9:52..9:54): ne => java/lang/Object#ne().
       |[9:67..9:68): x => local0
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a for in a guard",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t168 {
       |  val m = new M(1)
       |  val p = for (x <- m if (for (y <- m) yield y + x) ne null) yield x
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
       |_empty_/t168. => final object t168 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t168.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t168.p. => val method p: M[Int]
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
       |local3 => param y: Int
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local5 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local3
       |  Int => scala/Int#
       |local6 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local7 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a for in a value",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t169 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = for (z <- m) yield z + x) yield y
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
       |[7:7..7:11): t169 <= _empty_/t169.
       |[8:6..8:7): m <= _empty_/t169.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t169.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local6
       |[9:15..9:21): x <- m <= local7
       |[9:20..9:21): m => _empty_/t169.m.
       |[9:23..9:24): y <= local5
       |[9:32..9:33): z <= local2
       |[9:32..9:38): z <- m <= local3
       |[9:32..9:38): z <- m <= local4
       |[9:37..9:38): m => _empty_/t169.m.
       |[9:46..9:47): z => local2
       |[9:48..9:49): + => scala/Int#`+`(+4).
       |[9:50..9:51): x => local0
       |[9:59..9:60): y => local5
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a for in a value",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t169 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = for (z <- m) yield z + x) yield y
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
       |[7:7..7:11): t169 <= _empty_/t169.
       |[8:6..8:7): m <= _empty_/t169.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t169.p.
       |[9:15..9:21): x <- m <= local6
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t169.m.
       |[9:23..9:24): y <= local5
       |[9:32..9:38): z <- m <= local3
       |[9:32..9:33): z <= local2
       |[9:37..9:38): m => _empty_/t169.m.
       |[9:46..9:47): z => local2
       |[9:48..9:49): + => scala/Int#`+`(+4).
       |[9:50..9:51): x => local0
       |[9:59..9:60): y => local5
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a for in a value",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t169 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = for (z <- m) yield z + x) yield y
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
       |_empty_/t169. => final object t169 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t169.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t169.p. => val method p: M[M[Int]]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, M[Int]]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local2 => param z: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local4 => synthetic method f(z: Int): Int <: _empty_/F#f().
       |  z => local2
       |  Int => scala/Int#
       |local5 => val local y: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local6 => synthetic class $anon extends Object with F[Tuple2[Int, M[Int]], M[Int]] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local7 => synthetic method f(x$1: Tuple2[Int, M[Int]]): M[Int] <: _empty_/F#f().
       |  x$1 => local1
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  M => _empty_/M#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a value after a guard after a value",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t170 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = x if y > 0; z = y) yield z
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
       |[7:7..7:11): t170 <= _empty_/t170.
       |[8:6..8:7): m <= _empty_/t170.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t170.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local8
       |[9:15..9:21): x <- m <= local9
       |[9:20..9:21): m => _empty_/t170.m.
       |[9:23..9:24): y <= local4
       |[9:27..9:28): x => local0
       |[9:29..9:37): if y > 0 <= local5
       |[9:29..9:37): if y > 0 <= local6
       |[9:32..9:33): y => local4
       |[9:34..9:35): > => scala/Int#`>`(+3).
       |[9:39..9:40): z <= local7
       |[9:43..9:44): y => local4
       |[9:52..9:53): z => local7
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a value after a guard after a value",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t170 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = x if y > 0; z = y) yield z
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
       |[7:7..7:11): t170 <= _empty_/t170.
       |[8:6..8:7): m <= _empty_/t170.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t170.p.
       |[9:15..9:21): x <- m <= local8
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t170.m.
       |[9:23..9:24): y <= local4
       |[9:27..9:28): x => local0
       |[9:29..9:37): if y > 0 <= local5
       |[9:32..9:33): y => local4
       |[9:34..9:35): > => scala/Int#`>`(+3).
       |[9:39..9:40): z <= local7
       |[9:43..9:44): y => local4
       |[9:52..9:53): z => local7
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a value after a guard after a value",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t170 {
       |  val m = new M(1)
       |  val p = for (x <- m; y = x if y > 0; z = y) yield z
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
       |_empty_/t170. => final object t170 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t170.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t170.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => synthetic param x$3: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local3 => synthetic param x$4: Tuple2[Tuple2[Int, Int], Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local4 => val local y: Int
       |  Int => scala/Int#
       |local5 => synthetic class $anon extends Object with F[Tuple2[Int, Int], Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local6 => synthetic method f(x$1: Tuple2[Int, Int]): Boolean <: _empty_/F#f().
       |  x$1 => local1
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local7 => val local z: Int
       |  Int => scala/Int#
       |local8 => synthetic class $anon extends Object with F[Tuple2[Tuple2[Int, Int], Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local9 => synthetic method f(x$4: Tuple2[Tuple2[Int, Int], Int]): Int <: _empty_/F#f().
       |  x$4 => local3
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a guard after a refutable pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t171 {
       |  val m = new M(Option(1))
       |  val p = for (Some(a) <- m if a > 0) yield a
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
       |[7:7..7:11): t171 <= _empty_/t171.
       |[8:6..8:7): m <= _empty_/t171.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[8:16..8:22): Option => scala/Option.
       |[9:6..9:7): p <= _empty_/t171.p.
       |[9:15..9:19): Some => scala/Some.
       |[9:15..9:27): Some(a) <- m <= local6
       |[9:15..9:27): Some(a) <- m <= local7
       |[9:20..9:21): a <= local3
       |[9:26..9:27): m => _empty_/t171.m.
       |[9:28..9:36): if a > 0 <= local4
       |[9:28..9:36): if a > 0 <= local5
       |[9:31..9:32): a => local3
       |[9:33..9:34): > => scala/Int#`>`(+3).
       |[9:44..9:45): a => local3
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a guard after a refutable pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t171 {
       |  val m = new M(Option(1))
       |  val p = for (Some(a) <- m if a > 0) yield a
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
       |[7:7..7:11): t171 <= _empty_/t171.
       |[8:6..8:7): m <= _empty_/t171.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[8:16..8:22): Option => scala/Option.
       |[9:6..9:7): p <= _empty_/t171.p.
       |[9:15..9:27): Some(a) <- m <= local6
       |[9:15..9:19): Some => scala/Some.
       |[9:20..9:21): a <= local3
       |[9:26..9:27): m => _empty_/t171.m.
       |[9:28..9:36): if a > 0 <= local4
       |[9:31..9:32): a => local3
       |[9:33..9:34): > => scala/Int#`>`(+3).
       |[9:44..9:45): a => local3
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a guard after a refutable pattern",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t171 {
       |  val m = new M(Option(1))
       |  val p = for (Some(a) <- m if a > 0) yield a
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
       |_empty_/t171. => final object t171 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t171.m. => val method m: M[Option[Int]]
       |  M => _empty_/M#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |_empty_/t171.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => synthetic param check$ifrefutable$1: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local2 => synthetic param x$2: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local3 => val local a: Int
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Option[Int], Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic method f(x$1: Option[Int]): Boolean <: _empty_/F#f().
       |  x$1 => local1
       |  Option => scala/Option#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local6 => synthetic class $anon extends Object with F[Option[Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local7 => synthetic method f(x$2: Option[Int]): Int <: _empty_/F#f().
       |  x$2 => local2
       |  Option => scala/Option#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a guard after a nested for in a block",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t172 {
       |  val m = new M(1)
       |  val p = for (x <- { for (y <- m) yield y } if x > 0) yield x
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
       |[7:7..7:11): t172 <= _empty_/t172.
       |[8:6..8:7): m <= _empty_/t172.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t172.p.
       |[9:15..9:16): x <= local1
       |[9:15..9:44): x <- { for (y <- m) yield y } <= local6
       |[9:15..9:44): x <- { for (y <- m) yield y } <= local7
       |[9:27..9:28): y <= local0
       |[9:27..9:33): y <- m <= local2
       |[9:27..9:33): y <- m <= local3
       |[9:32..9:33): m => _empty_/t172.m.
       |[9:41..9:42): y => local0
       |[9:45..9:53): if x > 0 <= local4
       |[9:45..9:53): if x > 0 <= local5
       |[9:48..9:49): x => local1
       |[9:50..9:51): > => scala/Int#`>`(+3).
       |[9:61..9:62): x => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a guard after a nested for in a block",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t172 {
       |  val m = new M(1)
       |  val p = for (x <- { for (y <- m) yield y } if x > 0) yield x
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
       |[7:7..7:11): t172 <= _empty_/t172.
       |[8:6..8:7): m <= _empty_/t172.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t172.p.
       |[9:15..9:44): x <- { for (y <- m) yield y } <= local6
       |[9:15..9:16): x <= local1
       |[9:27..9:33): y <- m <= local2
       |[9:27..9:28): y <= local0
       |[9:32..9:33): m => _empty_/t172.m.
       |[9:41..9:42): y => local0
       |[9:45..9:53): if x > 0 <= local4
       |[9:48..9:49): x => local1
       |[9:50..9:51): > => scala/Int#`>`(+3).
       |[9:61..9:62): x => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a guard after a nested for in a block",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t172 {
       |  val m = new M(1)
       |  val p = for (x <- { for (y <- m) yield y } if x > 0) yield x
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
       |_empty_/t172. => final object t172 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t172.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t172.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param y: Int
       |  Int => scala/Int#
       |local1 => param x: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local0
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local6 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local7 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a guard after a nested for in the second generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t173 {
       |  val m = new M(1)
       |  val p = for (x <- m; y <- for (z <- m) yield z if y > x) yield y
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
       |[7:7..7:11): t173 <= _empty_/t173.
       |[8:6..8:7): m <= _empty_/t173.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t173.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local3
       |[9:15..9:21): x <- m <= local4
       |[9:20..9:21): m => _empty_/t173.m.
       |[9:23..9:24): y <= local2
       |[9:23..9:48): y <- for (z <- m) yield z <= local9
       |[9:23..9:48): y <- for (z <- m) yield z <= local10
       |[9:33..9:34): z <= local1
       |[9:33..9:39): z <- m <= local5
       |[9:33..9:39): z <- m <= local6
       |[9:38..9:39): m => _empty_/t173.m.
       |[9:47..9:48): z => local1
       |[9:49..9:57): if y > x <= local7
       |[9:49..9:57): if y > x <= local8
       |[9:52..9:53): y => local2
       |[9:54..9:55): > => scala/Int#`>`(+3).
       |[9:56..9:57): x => local0
       |[9:65..9:66): y => local2
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a guard after a nested for in the second generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t173 {
       |  val m = new M(1)
       |  val p = for (x <- m; y <- for (z <- m) yield z if y > x) yield y
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
       |[7:7..7:11): t173 <= _empty_/t173.
       |[8:6..8:7): m <= _empty_/t173.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t173.p.
       |[9:15..9:21): x <- m <= local3
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t173.m.
       |[9:23..9:48): y <- for (z <- m) yield z <= local9
       |[9:23..9:24): y <= local2
       |[9:33..9:39): z <- m <= local5
       |[9:33..9:34): z <= local1
       |[9:38..9:39): m => _empty_/t173.m.
       |[9:47..9:48): z => local1
       |[9:49..9:57): if y > x <= local7
       |[9:52..9:53): y => local2
       |[9:54..9:55): > => scala/Int#`>`(+3).
       |[9:56..9:57): x => local0
       |[9:65..9:66): y => local2
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a guard after a nested for in the second generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t173 {
       |  val m = new M(1)
       |  val p = for (x <- m; y <- for (z <- m) yield z if y > x) yield y
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
       |_empty_/t173. => final object t173 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t173.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t173.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param z: Int
       |  Int => scala/Int#
       |local2 => param y: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with F[Int, M[Int]] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local4 => synthetic method f(x: Int): M[Int] <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local5 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local6 => synthetic method f(z: Int): Int <: _empty_/F#f().
       |  z => local1
       |  Int => scala/Int#
       |local7 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local8 => synthetic method f(y: Int): Boolean <: _empty_/F#f().
       |  y => local2
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local9 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local10 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local2
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with guards at two levels of a generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t174 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m if y > 0) yield y if x > 0) yield x
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
       |[7:7..7:11): t174 <= _empty_/t174.
       |[8:6..8:7): m <= _empty_/t174.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t174.p.
       |[9:15..9:16): x <= local1
       |[9:15..9:49): x <- for (y <- m if y > 0) yield y <= local8
       |[9:15..9:49): x <- for (y <- m if y > 0) yield y <= local9
       |[9:25..9:26): y <= local0
       |[9:25..9:31): y <- m <= local4
       |[9:25..9:31): y <- m <= local5
       |[9:30..9:31): m => _empty_/t174.m.
       |[9:32..9:40): if y > 0 <= local2
       |[9:32..9:40): if y > 0 <= local3
       |[9:35..9:36): y => local0
       |[9:37..9:38): > => scala/Int#`>`(+3).
       |[9:48..9:49): y => local0
       |[9:50..9:58): if x > 0 <= local6
       |[9:50..9:58): if x > 0 <= local7
       |[9:53..9:54): x => local1
       |[9:55..9:56): > => scala/Int#`>`(+3).
       |[9:66..9:67): x => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with guards at two levels of a generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t174 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m if y > 0) yield y if x > 0) yield x
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
       |[7:7..7:11): t174 <= _empty_/t174.
       |[8:6..8:7): m <= _empty_/t174.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t174.p.
       |[9:15..9:49): x <- for (y <- m if y > 0) yield y <= local8
       |[9:15..9:16): x <= local1
       |[9:25..9:31): y <- m <= local4
       |[9:25..9:26): y <= local0
       |[9:30..9:31): m => _empty_/t174.m.
       |[9:32..9:40): if y > 0 <= local2
       |[9:35..9:36): y => local0
       |[9:37..9:38): > => scala/Int#`>`(+3).
       |[9:48..9:49): y => local0
       |[9:50..9:58): if x > 0 <= local6
       |[9:53..9:54): x => local1
       |[9:55..9:56): > => scala/Int#`>`(+3).
       |[9:66..9:67): x => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with guards at two levels of a generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t174 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m if y > 0) yield y if x > 0) yield x
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
       |_empty_/t174. => final object t174 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t174.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t174.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param y: Int
       |  Int => scala/Int#
       |local1 => param x: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic method f(y: Int): Boolean <: _empty_/F#f().
       |  y => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local5 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local0
       |  Int => scala/Int#
       |local6 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local7 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local8 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local9 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a value and a guard after a nested for",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t175 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y; v = x if v > 0) yield v
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
       |[7:7..7:11): t175 <= _empty_/t175.
       |[8:6..8:7): m <= _empty_/t175.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t175.p.
       |[9:15..9:16): x <= local1
       |[9:15..9:40): x <- for (y <- m) yield y <= local9
       |[9:15..9:40): x <- for (y <- m) yield y <= local10
       |[9:25..9:26): y <= local0
       |[9:25..9:31): y <- m <= local4
       |[9:25..9:31): y <- m <= local5
       |[9:30..9:31): m => _empty_/t175.m.
       |[9:39..9:40): y => local0
       |[9:42..9:43): v <= local6
       |[9:46..9:47): x => local1
       |[9:48..9:56): if v > 0 <= local7
       |[9:48..9:56): if v > 0 <= local8
       |[9:51..9:52): v => local6
       |[9:53..9:54): > => scala/Int#`>`(+3).
       |[9:64..9:65): v => local6
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a value and a guard after a nested for",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t175 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y; v = x if v > 0) yield v
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
       |[7:7..7:11): t175 <= _empty_/t175.
       |[8:6..8:7): m <= _empty_/t175.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t175.p.
       |[9:15..9:40): x <- for (y <- m) yield y <= local9
       |[9:15..9:16): x <= local1
       |[9:25..9:31): y <- m <= local4
       |[9:25..9:26): y <= local0
       |[9:30..9:31): m => _empty_/t175.m.
       |[9:39..9:40): y => local0
       |[9:42..9:43): v <= local6
       |[9:46..9:47): x => local1
       |[9:48..9:56): if v > 0 <= local7
       |[9:51..9:52): v => local6
       |[9:53..9:54): > => scala/Int#`>`(+3).
       |[9:64..9:65): v => local6
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a value and a guard after a nested for",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t175 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m) yield y; v = x if v > 0) yield v
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
       |_empty_/t175. => final object t175 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t175.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t175.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param y: Int
       |  Int => scala/Int#
       |local1 => val local x: Int
       |  Int => scala/Int#
       |local2 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local3 => synthetic param x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local5 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local0
       |  Int => scala/Int#
       |local6 => val local v: Int
       |  Int => scala/Int#
       |local7 => synthetic class $anon extends Object with F[Tuple2[Int, Int], Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local8 => synthetic method f(x$1: Tuple2[Int, Int]): Boolean <: _empty_/F#f().
       |  x$1 => local2
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local9 => synthetic class $anon extends Object with F[Tuple2[Int, Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local10 => synthetic method f(x$2: Tuple2[Int, Int]): Int <: _empty_/F#f().
       |  x$2 => local3
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with a for in a guard of a for in a generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t176 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m if (for (z <- m) yield z) ne null) yield y) yield x
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
       |[7:7..7:11): t176 <= _empty_/t176.
       |[8:6..8:7): m <= _empty_/t176.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t176.p.
       |[9:15..9:16): x <= local1
       |[9:15..9:74): x <- for (y <- m if (for (z <- m) yield z) ne null) yield y <= local9
       |[9:15..9:74): x <- for (y <- m if (for (z <- m) yield z) ne null) yield y <= local10
       |[9:25..9:26): y <= local0
       |[9:25..9:31): y <- m <= local7
       |[9:25..9:31): y <- m <= local8
       |[9:30..9:31): m => _empty_/t176.m.
       |[9:32..9:65): if (for (z <- m) yield z) ne null <= local2
       |[9:32..9:65): if (for (z <- m) yield z) ne null <= local3
       |[9:41..9:42): z <= local4
       |[9:41..9:47): z <- m <= local5
       |[9:41..9:47): z <- m <= local6
       |[9:46..9:47): m => _empty_/t176.m.
       |[9:55..9:56): z => local4
       |[9:58..9:60): ne => java/lang/Object#ne().
       |[9:73..9:74): y => local0
       |[9:82..9:83): x => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with a for in a guard of a for in a generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t176 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m if (for (z <- m) yield z) ne null) yield y) yield x
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
       |[7:7..7:11): t176 <= _empty_/t176.
       |[8:6..8:7): m <= _empty_/t176.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t176.p.
       |[9:15..9:74): x <- for (y <- m if (for (z <- m) yield z) ne null) yield y <= local9
       |[9:15..9:16): x <= local1
       |[9:25..9:31): y <- m <= local7
       |[9:25..9:26): y <= local0
       |[9:30..9:31): m => _empty_/t176.m.
       |[9:32..9:65): if (for (z <- m) yield z) ne null <= local2
       |[9:41..9:47): z <- m <= local5
       |[9:41..9:42): z <= local4
       |[9:46..9:47): m => _empty_/t176.m.
       |[9:55..9:56): z => local4
       |[9:58..9:60): ne => java/lang/Object#ne().
       |[9:73..9:74): y => local0
       |[9:82..9:83): x => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with a for in a guard of a for in a generator",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t176 {
       |  val m = new M(1)
       |  val p = for (x <- for (y <- m if (for (z <- m) yield z) ne null) yield y) yield x
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
       |_empty_/t176. => final object t176 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t176.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t176.p. => val method p: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param y: Int
       |  Int => scala/Int#
       |local1 => param x: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic method f(y: Int): Boolean <: _empty_/F#f().
       |  y => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => param z: Int
       |  Int => scala/Int#
       |local5 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local6 => synthetic method f(z: Int): Int <: _empty_/F#f().
       |  z => local4
       |  Int => scala/Int#
       |local7 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local8 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local0
       |  Int => scala/Int#
       |local9 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local10 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "SAM closures of a for comprehension with guards in a for and in its yield",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t177 {
       |  val m = new M(1)
       |  val p = for (x <- m if x > 0) yield for (y <- m if y > x) yield y
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
       |[7:7..7:11): t177 <= _empty_/t177.
       |[8:6..8:7): m <= _empty_/t177.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t177.p.
       |[9:15..9:16): x <= local0
       |[9:15..9:21): x <- m <= local4
       |[9:15..9:21): x <- m <= local5
       |[9:20..9:21): m => _empty_/t177.m.
       |[9:22..9:30): if x > 0 <= local2
       |[9:22..9:30): if x > 0 <= local3
       |[9:25..9:26): x => local0
       |[9:27..9:28): > => scala/Int#`>`(+3).
       |[9:43..9:44): y <= local1
       |[9:43..9:49): y <- m <= local8
       |[9:43..9:49): y <- m <= local9
       |[9:48..9:49): m => _empty_/t177.m.
       |[9:50..9:58): if y > x <= local6
       |[9:50..9:58): if y > x <= local7
       |[9:53..9:54): y => local1
       |[9:55..9:56): > => scala/Int#`>`(+3).
       |[9:57..9:58): x => local0
       |[9:66..9:67): y => local1
       |""".stripMargin,
  )

  treeSymbols(
    "SAM closures of a for comprehension with guards in a for and in its yield",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t177 {
       |  val m = new M(1)
       |  val p = for (x <- m if x > 0) yield for (y <- m if y > x) yield y
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
       |[7:7..7:11): t177 <= _empty_/t177.
       |[8:6..8:7): m <= _empty_/t177.m.
       |[8:14..8:15): M => _empty_/M#
       |[8:15..8:15):  => _empty_/M#`<init>`().
       |[9:6..9:7): p <= _empty_/t177.p.
       |[9:15..9:21): x <- m <= local4
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t177.m.
       |[9:22..9:30): if x > 0 <= local2
       |[9:25..9:26): x => local0
       |[9:27..9:28): > => scala/Int#`>`(+3).
       |[9:43..9:49): y <- m <= local8
       |[9:43..9:44): y <= local1
       |[9:48..9:49): m => _empty_/t177.m.
       |[9:50..9:58): if y > x <= local6
       |[9:53..9:54): y => local1
       |[9:55..9:56): > => scala/Int#`>`(+3).
       |[9:57..9:58): x => local0
       |[9:66..9:67): y => local1
       |""".stripMargin,
  )

  symbols(
    "SAM closures of a for comprehension with guards in a for and in its yield",
    """|trait F[A, B] { def f(a: A): B }
       |class M[A](a: A) {
       |  def map[B](g: F[A, B]) = new M(g.f(a))
       |  def flatMap[B](g: F[A, M[B]]) = g.f(a)
       |  def withFilter(g: F[A, Boolean]) = this
       |  def foreach(g: F[A, Unit]) = g.f(a)
       |}
       |object t177 {
       |  val m = new M(1)
       |  val p = for (x <- m if x > 0) yield for (y <- m if y > x) yield y
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
       |_empty_/t177. => final object t177 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t177.m. => val method m: M[Int]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |_empty_/t177.p. => val method p: M[M[Int]]
       |  M => _empty_/M#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic class $anon extends Object with F[Int, M[Int]] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local5 => synthetic method f(x: Int): M[Int] <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local6 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local7 => synthetic method f(y: Int): Boolean <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local8 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local9 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
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
    "extractor with a dependent result",
    """|class D { type T }
       |object Ext { def unapply(d: D): Option[d.T] = None }
       |class t21 {
       |  def m(x: Any) = x match { case Ext(y) => y; case _ => null }
       |}
       |""".stripMargin,
    """|_empty_/D# => class D extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/D#T# => abstract type T
       |_empty_/D#`<init>`(). => primary ctor <init>()
       |_empty_/Ext. => final object Ext extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/Ext.unapply(). => method unapply(d: D): Option[d.T]
       |  d => _empty_/Ext.unapply().(d)
       |  D => _empty_/D#
       |  Option => scala/Option#
       |  T => _empty_/D#T#
       |_empty_/Ext.unapply().(d) => param d: D
       |  D => _empty_/D#
       |_empty_/t21# => class t21 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t21#`<init>`(). => primary ctor <init>()
       |_empty_/t21#m(). => method m(x: Any): Any
       |  x => _empty_/t21#m().(x)
       |  Any => scala/Any#
       |_empty_/t21#m().(x) => param x: Any
       |  Any => scala/Any#
       |local0 => val local y: <unapply-selector>.T
       |  <unapply-selector> => local1
       |  T => _empty_/D#T#
       |local1 => synthetic val local <unapply-selector>: D
       |  D => _empty_/D#
       |""".stripMargin,
  )

  synthetics(
    "implicit arguments that depend on a qualifier or an argument",
    """|trait Ev[X]
       |class B { type T; def g(implicit e: Ev[T]): Int = 1; def h(a: Int, b: Int)(implicit e: Ev[T]): Int = a }
       |class L { def +:(b: B)(implicit e: Ev[b.T]): Int = 1 }
       |object t22 {
       |  implicit def ev[X]: Ev[X] = null
       |  def f(a: B, b: Int)(implicit e: Ev[a.T]): Int = b
       |  def mk: B = new B
       |  def m = { mk.g + mk.h(b = 1, a = 2) + (mk +: new L) + f(b = 1, a = mk) }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[7:12..7:16): mk.g => *(ev[_1.type#T])
         |  ev => _empty_/t22.ev().
         |  _1.type => local0
         |  T => _empty_/B#T#
         |[7:19..7:37): mk.h(b = 1, a = 2) => *(ev[qual$1.T])
         |  ev => _empty_/t22.ev().
         |  qual$1 => local1
         |  T => _empty_/B#T#
         |[7:41..7:52): mk +: new L => *(ev[x$1.T])
         |  ev => _empty_/t22.ev().
         |  x$1 => local2
         |  T => _empty_/B#T#
         |[7:56..7:72): f(b = 1, a = mk) => *(ev[x$4.T])
         |  ev => _empty_/t22.ev().
         |  x$4 => local3
         |  T => _empty_/B#T#
         |""".stripMargin
    else
      """|[7:12..7:16): mk.g => *(ev[stabilizer$1.T])
         |  ev => _empty_/t22.ev().
         |  stabilizer$1 => local0
         |  T => _empty_/B#T#
         |[7:19..7:37): mk.h(b = 1, a = 2) => *(ev[stabilizer$2.T])
         |  ev => _empty_/t22.ev().
         |  stabilizer$2 => local1
         |  T => _empty_/B#T#
         |[7:44..7:52): +: new L => *(ev[rassoc$1.T])
         |  ev => _empty_/t22.ev().
         |  rassoc$1 => local2
         |  T => _empty_/B#T#
         |[7:56..7:72): f(b = 1, a = mk) => *(ev[x$4.T])
         |  ev => _empty_/t22.ev().
         |  x$4 => local3
         |  T => _empty_/B#T#
         |""".stripMargin,
  )

  synthetics(
    "implicit argument of an eta expansion that depends on an argument",
    """|trait Ev[X]
       |class B { type T }
       |object t23 {
       |  implicit def ev[X]: Ev[X] = null
       |  def mk: B = new B
       |  def g(b: B)(x: Int)(implicit e: Ev[b.T]): Int = x
       |  def m = List(1).map(g(mk))
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[6:10..6:14): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[6:10..6:21): List(1).map => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[6:10..6:28): List(1).map(g(mk)) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[6:22..6:27): g(mk) => *(ev[eta$0$1.T])
         |  ev => _empty_/t23.ev().
         |  eta$0$1 => local0
         |  T => _empty_/B#T#
         |""".stripMargin
    else
      """|[6:10..6:14): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[6:10..6:21): List(1).map => *[Int]
         |  Int => scala/Int#
         |[6:22..6:27): g(mk) => *(ev[eta$0$1.T])
         |  ev => _empty_/t23.ev().
         |  eta$0$1 => local0
         |  T => _empty_/B#T#
         |""".stripMargin,
  )

  test("implicit arguments that depend on a qualifier or an argument: locals") {
    def load() = computeDatabaseSectionFromSnippet(
      """|trait Ev[X]
         |class B { type T; def g(implicit e: Ev[T]): Int = 1; def h(a: Int, b: Int)(implicit e: Ev[T]): Int = a }
         |class L { def +:(b: B)(implicit e: Ev[b.T]): Int = 1 }
         |object t22 {
         |  implicit def ev[X]: Ev[X] = null
         |  def f(a: B, b: Int)(implicit e: Ev[a.T]): Int = b
         |  def mk: B = new B
         |  def m = { mk.g + mk.h(b = 1, a = 2) + (mk +: new L) + f(b = 1, a = mk) }
         |}
         |""".stripMargin,
      "Symbols",
    )
    assertNoDiff(
      load().linesIterator.filter(_.startsWith("local")).mkString("\n"),
      if (ScalaVersion.is212)
        """|local0 => abstract type _1.type <: B with Singleton {}
           |local1 => val local qual$1: B
           |local2 => synthetic val local x$1: B
           |local3 => val local x$4: B
           |""".stripMargin
      else
        """|local0 => synthetic val local stabilizer$1: B
           |local1 => synthetic val local stabilizer$2: B
           |local2 => synthetic final val local rassoc$1: B
           |local3 => val local x$4: B
           |""".stripMargin,
    )
  }

  test("named arguments with a dependent implicit in local definitions: locals") {
    def load() = computeDatabaseSectionFromSnippet(
      """|trait Ev[X]
         |class B { type T }
         |object t24 {
         |  implicit def ev[X]: Ev[X] = null
         |  def mk: B = new B
         |  def f(a: B, b: Int)(implicit e: Ev[a.T]): Int = b
         |  def m = {
         |    class K { val k = f(b = 1, a = mk) }
         |    val o = new { val n = f(b = 2, a = mk) }
         |    object Q { val q = f(b = 3, a = mk) }
         |    List(1).map(x => f(b = x, a = mk))
         |  }
         |}
         |""".stripMargin,
      "Symbols",
    )
    assertNoDiff(
      load().linesIterator.filter(_.startsWith("local")).mkString("\n"),
      """|local0 => class K extends AnyRef { +2 decls }
         |local1 => primary ctor <init>()
         |local2 => val method k: Int
         |local3 => val local x$2: B
         |local4 => final class $anon extends AnyRef { +1 decls }
         |local5 => val method n: Int
         |local6 => val local x$2: B
         |local7 => val local o: AnyRef { val def n: Int }
         |local8 => final object Q extends AnyRef { +1 decls }
         |local9 => val method q: Int
         |local10 => val local x$2: B
         |local11 => param x: Int
         |local12 => val local x$2: B
         |""".stripMargin,
    )
  }

  symbols(
    "var in a local refinement",
    """|abstract class s1 {
       |  def m = {
       |    type S = { var vs: Int }
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|_empty_/s1# => abstract class s1 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s1#`<init>`(). => primary ctor <init>()
       |_empty_/s1#m(). => method m: Unit
       |  Unit => scala/Unit#
       |local0 => type S = AnyRef { var def vs: Int; var def vs_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vs => local1
       |  Int => scala/Int#
       |  vs_= => local2
       |  x$1 => local3
       |  Unit => scala/Unit#
       |local1 => abstract var method vs: Int
       |  Int => scala/Int#
       |local2 => abstract var method vs_=(x$1: Int): Unit
       |  x$1 => local3
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local3 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "vars in member refinements",
    """|class s2 {
       |  type S = { var vs: Int }
       |  val n: { var vn: Int } = ???
       |  def rs(r: { var vr: Int }) = r.vr
       |}
       |""".stripMargin,
    """|_empty_/s2# => class s2 extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s2#S# => type S = AnyRef { var def vs: Int; var def vs_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vs => local0
       |  Int => scala/Int#
       |  vs_= => local1
       |  x$1 => local2
       |  Unit => scala/Unit#
       |_empty_/s2#`<init>`(). => primary ctor <init>()
       |_empty_/s2#n. => val method n: AnyRef { var def vn: Int; var def vn_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vn => local3
       |  Int => scala/Int#
       |  vn_= => local4
       |  x$1 => local5
       |  Unit => scala/Unit#
       |_empty_/s2#rs(). => method rs(r: AnyRef { var def vr: Int; var def vr_=(x$1: Int): Unit }): Int
       |  r => _empty_/s2#rs().(r)
       |  AnyRef => scala/AnyRef#
       |  vr => local6
       |  Int => scala/Int#
       |  vr_= => local7
       |  x$1 => local8
       |  Unit => scala/Unit#
       |_empty_/s2#rs().(r) => param r: AnyRef { var def vr: Int; var def vr_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vr => local6
       |  Int => scala/Int#
       |  vr_= => local7
       |  x$1 => local8
       |  Unit => scala/Unit#
       |local0 => abstract var method vs: Int
       |  Int => scala/Int#
       |local1 => abstract var method vs_=(x$1: Int): Unit
       |  x$1 => local2
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => abstract var method vn: Int
       |  Int => scala/Int#
       |local4 => abstract var method vn_=(x$1: Int): Unit
       |  x$1 => local5
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local5 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local6 => abstract var method vr: Int
       |  Int => scala/Int#
       |local7 => abstract var method vr_=(x$1: Int): Unit
       |  x$1 => local8
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local8 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "vars in an anonymous and a local class",
    """|class s3 {
       |  def anon = new { var va = 1 }
       |  def m = {
       |    class c { var v = 1 }
       |    new c
       |  }
       |}
       |""".stripMargin,
    """|_empty_/s3# => class s3 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s3#`<init>`(). => primary ctor <init>()
       |_empty_/s3#anon(). => method anon: AnyRef { var def va: Int; var def va_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  va => local0
       |  Int => scala/Int#
       |  va_= => local1
       |  x$1 => local2
       |  Unit => scala/Unit#
       |_empty_/s3#m(). => method m: AnyRef { var def v: Int; var def v_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  v => local4
       |  Int => scala/Int#
       |  v_= => local5
       |  x$1 => local6
       |  Unit => scala/Unit#
       |local0 => var method va: Int
       |  Int => scala/Int#
       |local1 => var method va_=(x$1: Int): Unit
       |  x$1 => local2
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local3 => final class $anon extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local4 => var method v: Int
       |  Int => scala/Int#
       |local5 => var method v_=(x$1: Int): Unit
       |  x$1 => local6
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local6 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local7 => class c extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local8 => primary ctor <init>()
       |""".stripMargin,
  )

  test("parameter of a local case class with a companion that defines apply") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object t31 {
         |  def m: Any = {
         |    case class k(i: Int)
         |    object k { def apply(i: Int): k = new k(i) }
         |    k(1)
         |  }
         |}
         |""".stripMargin,
      "Symbols",
    )
    assertNoDiff(
      load().linesIterator.filter(_.startsWith("local1 =>")).mkString("\n"),
      "local1 => val param i: Int",
    )
  }

  occurrences(
    "companion object before its case class",
    """|object t126
       |case class t126(a: Int)
       |""".stripMargin,
    """|[0:7..0:11): t126 <= _empty_/t126.
       |[1:11..1:15): t126 <= _empty_/t126#
       |[1:15..1:15):  <= _empty_/t126#`<init>`().
       |[1:16..1:17): a <= _empty_/t126#`<init>`().(a)
       |[1:16..1:17): a <= _empty_/t126#a.
       |[1:19..1:22): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "local companion object before its case class",
    """|object t127 {
       |  def m = {
       |    object C
       |    case class C(a: Int)
       |    C(1)
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:11): t127 <= _empty_/t127.
       |[1:6..1:7): m <= _empty_/t127.m().
       |[2:11..2:12): C <= local5
       |[3:15..3:16): C <= local0
       |[3:16..3:16):  <= local10
       |[3:17..3:18): a <= local1
       |[3:20..3:23): Int => scala/Int#
       |[4:4..4:5): C => local5
       |""".stripMargin,
  )

  symbols(
    "companion object before its case class",
    """|object t126
       |case class t126(a: Int)
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t126# => case class t126 extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |_empty_/t126#`<init>`(). => primary ctor <init>(val a: Int)
         |  a => _empty_/t126#`<init>`().(a)
         |  Int => scala/Int#
         |_empty_/t126#`<init>`().(a) => val param a: Int
         |  Int => scala/Int#
         |_empty_/t126#a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/t126#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t126#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t126#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t126#copy$default$1(). => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t126#copy(). => synthetic method copy(a: Int): t126
         |  a => _empty_/t126#copy().(a)
         |  Int => scala/Int#
         |  t126 => _empty_/t126#
         |_empty_/t126#copy().(a) => default param a: Int
         |  Int => scala/Int#
         |_empty_/t126#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t126#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t126#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t126#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t126#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t126#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t126#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t126#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t126#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t126#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t126#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t126. => final object t126 extends AnyRef with Serializable { +3 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |_empty_/t126.apply(). => synthetic method apply(a: Int): t126
         |  a => _empty_/t126.apply().(a)
         |  Int => scala/Int#
         |  t126 => _empty_/t126#
         |_empty_/t126.apply().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t126.readResolve(). => private synthetic method readResolve(): Object
         |  Object => java/lang/Object#
         |_empty_/t126.unapply(). => synthetic method unapply(x$0: t126): Option[Int]
         |  x$0 => _empty_/t126.unapply().(x$0)
         |  t126 => _empty_/t126#
         |  Option => scala/Option#
         |  Int => scala/Int#
         |_empty_/t126.unapply().(x$0) => synthetic param x$0: t126
         |  t126 => _empty_/t126#
         |""".stripMargin
    else
      """|_empty_/t126# => case class t126 extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |_empty_/t126#`<init>`(). => primary ctor <init>(val a: Int)
         |  a => _empty_/t126#`<init>`().(a)
         |  Int => scala/Int#
         |_empty_/t126#`<init>`().(a) => val param a: Int
         |  Int => scala/Int#
         |_empty_/t126#a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/t126#canEqual(). => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => _empty_/t126#canEqual().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t126#canEqual().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t126#copy$default$1(). => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t126#copy(). => synthetic method copy(a: Int): t126
         |  a => _empty_/t126#copy().(a)
         |  Int => scala/Int#
         |  t126 => _empty_/t126#
         |_empty_/t126#copy().(a) => default param a: Int
         |  Int => scala/Int#
         |_empty_/t126#equals(). => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => _empty_/t126#equals().(x$1)
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |_empty_/t126#equals().(x$1) => synthetic param x$1: Any
         |  Any => scala/Any#
         |_empty_/t126#hashCode(). => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |_empty_/t126#productArity(). => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |_empty_/t126#productElement(). => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => _empty_/t126#productElement().(x$1)
         |  Int => scala/Int#
         |  Any => scala/Any#
         |_empty_/t126#productElement().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t126#productElementName(). => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => _empty_/t126#productElementName().(x$1)
         |  Int => scala/Int#
         |  String => java/lang/String#
         |_empty_/t126#productElementName().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t126#productIterator(). => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |_empty_/t126#productPrefix(). => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |_empty_/t126#toString(). => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |_empty_/t126. => final object t126 extends AnyRef with Serializable { +3 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => java/io/Serializable#
         |_empty_/t126.apply(). => synthetic method apply(a: Int): t126
         |  a => _empty_/t126.apply().(a)
         |  Int => scala/Int#
         |  t126 => _empty_/t126#
         |_empty_/t126.apply().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t126.unapply(). => synthetic method unapply(x$0: t126): Option[Int]
         |  x$0 => _empty_/t126.unapply().(x$0)
         |  t126 => _empty_/t126#
         |  Option => scala/Option#
         |  Int => scala/Int#
         |_empty_/t126.unapply().(x$0) => synthetic param x$0: t126
         |  t126 => _empty_/t126#
         |_empty_/t126.writeReplace(). => private synthetic method writeReplace(): Object
         |  Object => java/lang/Object#
         |""".stripMargin,
  )

  symbols(
    "local companion object before its case class",
    """|object t127 {
       |  def m = {
       |    object C
       |    case class C(a: Int)
       |    C(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t127. => final object t127 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t127.m(). => method m: C forSome { case class C extends AnyRef with Product with Serializable { +12 decls } }
         |  C => local0
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local0 => case class C extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val param a: Int
         |  Int => scala/Int#
         |local2 => synthetic method copy(a: Int): C
         |  a => local3
         |  Int => scala/Int#
         |  C => local0
         |local3 => default param a: Int
         |  Int => scala/Int#
         |local4 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => final object C extends AnyRef with Serializable { +2 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |local6 => synthetic method apply(a: Int): C
         |  a => local8
         |  Int => scala/Int#
         |  C => local0
         |local7 => synthetic method unapply(x$0: C): Option[Int]
         |  x$0 => local9
         |  C => local0
         |  Option => scala/Option#
         |  Int => scala/Int#
         |local8 => param a: Int
         |  Int => scala/Int#
         |local9 => synthetic param x$0: C
         |  C => local0
         |local10 => primary ctor <init>(val a: Int)
         |  a => local1
         |  Int => scala/Int#
         |local11 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local12 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local13 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local19
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local14 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local15 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local20
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local16 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local17 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local18 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local21
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local19 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local20 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local21 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin
    else
      """|_empty_/t127. => final object t127 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t127.m(). => method m: C forSome { case class C extends AnyRef with Product with Serializable { +13 decls } }
         |  C => local0
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local0 => case class C extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val param a: Int
         |  Int => scala/Int#
         |local2 => synthetic method copy(a: Int): C
         |  a => local3
         |  Int => scala/Int#
         |  C => local0
         |local3 => default param a: Int
         |  Int => scala/Int#
         |local4 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => final object C extends AnyRef with Serializable { +2 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => java/io/Serializable#
         |local6 => synthetic method apply(a: Int): C
         |  a => local8
         |  Int => scala/Int#
         |  C => local0
         |local7 => synthetic method unapply(x$0: C): Option[Int]
         |  x$0 => local9
         |  C => local0
         |  Option => scala/Option#
         |  Int => scala/Int#
         |local8 => param a: Int
         |  Int => scala/Int#
         |local9 => synthetic param x$0: C
         |  C => local0
         |local10 => primary ctor <init>(val a: Int)
         |  a => local1
         |  Int => scala/Int#
         |local11 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local12 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local13 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local20
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local14 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local15 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local21
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local16 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local22
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local17 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local18 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local19 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local23
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local20 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local21 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local22 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local23 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin,
  )

  symbols(
    "local definitions with synthetic parameters",
    """|object s4 {
       |  def m: Any = {
       |    def g[T: Ordering](t: T) = t
       |    class c[T: Ordering](t: T)
       |    case class k(i: Int)
       |    val f: Int => Int = _ + 1
       |    (g(1), new c(1), k(1), f)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/s4. => final object s4 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/s4.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => method g[T](t: T)(implicit evidence$1: Ordering[T]): T
         |  T => local3
         |  t => local1
         |  evidence$1 => local2
         |  Ordering => scala/package.Ordering#
         |local1 => param t: T
         |  T => local3
         |local2 => synthetic implicit param evidence$1: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local3
         |local3 => typeparam T
         |local4 => class c[T] extends AnyRef { +3 decls }
         |  T => local8
         |  AnyRef => scala/AnyRef#
         |local5 => param t: T
         |  T => local8
         |local6 => implicit param evidence$2: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local8
         |local7 => primary ctor <init>(t: T)(implicit evidence$2: Ordering[T])
         |  t => local5
         |  T => local8
         |  evidence$2 => local6
         |  Ordering => scala/package.Ordering#
         |local8 => typeparam T
         |local9 => case class k extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local10 => val param i: Int
         |  Int => scala/Int#
         |local11 => primary ctor <init>(val i: Int)
         |  i => local10
         |  Int => scala/Int#
         |local12 => synthetic method copy(i: Int): k
         |  i => local22
         |  Int => scala/Int#
         |  k => local9
         |local13 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local14 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local15 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local16 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local23
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local17 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local18 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local24
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local19 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local20 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local21 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local25
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local22 => default param i: Int
         |  Int => scala/Int#
         |local23 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local24 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local25 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local26 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local27 => val local f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |local28 => synthetic method apply(i: Int): k <: scala/Function1#apply().
         |  i => local30
         |  Int => scala/Int#
         |  k => local9
         |local29 => synthetic final object k extends AbstractFunction1[Int, k] with Serializable { +3 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  Int => scala/Int#
         |  k => local9
         |  Serializable => scala/Serializable#
         |local30 => param i: Int
         |  Int => scala/Int#
         |local31 => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local32 => synthetic method unapply(x$0: k): Option[Int]
         |  x$0 => local33
         |  k => local9
         |  Option => scala/Option#
         |  Int => scala/Int#
         |local33 => synthetic param x$0: k
         |  k => local9
         |""".stripMargin
    else
      """|_empty_/s4. => final object s4 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/s4.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => method g[T](t: T)(implicit evidence$1: Ordering[T]): T
         |  T => local3
         |  t => local1
         |  evidence$1 => local2
         |  Ordering => scala/package.Ordering#
         |local1 => param t: T
         |  T => local3
         |local2 => synthetic implicit param evidence$1: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local3
         |local3 => typeparam T
         |local4 => class c[T] extends AnyRef { +3 decls }
         |  T => local8
         |  AnyRef => scala/AnyRef#
         |local5 => param t: T
         |  T => local8
         |local6 => implicit param evidence$2: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local8
         |local7 => primary ctor <init>(t: T)(implicit evidence$2: Ordering[T])
         |  t => local5
         |  T => local8
         |  evidence$2 => local6
         |  Ordering => scala/package.Ordering#
         |local8 => typeparam T
         |local9 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local10 => val param i: Int
         |  Int => scala/Int#
         |local11 => primary ctor <init>(val i: Int)
         |  i => local10
         |  Int => scala/Int#
         |local12 => synthetic method copy(i: Int): k
         |  i => local23
         |  Int => scala/Int#
         |  k => local9
         |local13 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local14 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local15 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local16 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local24
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local17 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local18 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local25
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local19 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local26
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local20 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local21 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local22 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local27
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local23 => default param i: Int
         |  Int => scala/Int#
         |local24 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local25 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local26 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local27 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local28 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local29 => val local f: Function1[Int, Int]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |local30 => synthetic method apply(i: Int): k <: scala/Function1#apply().
         |  i => local32
         |  Int => scala/Int#
         |  k => local9
         |local31 => synthetic final object k extends AbstractFunction1[Int, k] with Serializable { +3 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  Int => scala/Int#
         |  k => local9
         |  Serializable => java/io/Serializable#
         |local32 => param i: Int
         |  Int => scala/Int#
         |local33 => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local34 => synthetic method unapply(x$0: k): Option[Int]
         |  x$0 => local35
         |  k => local9
         |  Option => scala/Option#
         |  Int => scala/Int#
         |local35 => synthetic param x$0: k
         |  k => local9
         |""".stripMargin,
  )

  symbols(
    "class parameters beside context bounds",
    """|object s4b {
       |  class t[T: Ordering](t: T) { def o = implicitly[Ordering[T]] }
       |  def m: Any = {
       |    class c[T: Ordering](t: T) { def o = implicitly[Ordering[T]] }
       |    class d[T: Ordering: Numeric](val t: T)
       |    class e(implicit val o: Ordering[Int])
       |    val a = new AnyRef { def h[T: Ordering](t: T) = t }
       |    (new c(1), new d(1), new e, a)
       |  }
       |}
       |""".stripMargin,
    """|_empty_/s4b. => final object s4b extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s4b.m(). => method m: Any
       |  Any => scala/Any#
       |_empty_/s4b.t# => class t[T] extends AnyRef { +4 decls }
       |  T => _empty_/s4b.t#[T]
       |  AnyRef => scala/AnyRef#
       |_empty_/s4b.t#[T] => typeparam T
       |_empty_/s4b.t#`<init>`(). => primary ctor <init>(t: T)(implicit evidence$1: Ordering[T])
       |  t => _empty_/s4b.t#`<init>`().(t)
       |  T => _empty_/s4b.t#[T]
       |  evidence$1 => _empty_/s4b.t#`<init>`().(evidence$1)
       |  Ordering => scala/package.Ordering#
       |_empty_/s4b.t#`<init>`().(evidence$1) => implicit param evidence$1: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => _empty_/s4b.t#[T]
       |_empty_/s4b.t#`<init>`().(t) => param t: T
       |  T => _empty_/s4b.t#[T]
       |_empty_/s4b.t#evidence$1. => private[this] synthetic implicit val method evidence$1: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => _empty_/s4b.t#[T]
       |_empty_/s4b.t#o(). => method o: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => _empty_/s4b.t#[T]
       |_empty_/s4b.t#t. => private[this] val method t: T
       |  T => _empty_/s4b.t#[T]
       |local0 => class c[T] extends AnyRef { +4 decls }
       |  T => local5
       |  AnyRef => scala/AnyRef#
       |local1 => param t: T
       |  T => local5
       |local2 => implicit param evidence$2: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local5
       |local3 => primary ctor <init>(t: T)(implicit evidence$2: Ordering[T])
       |  t => local1
       |  T => local5
       |  evidence$2 => local2
       |  Ordering => scala/package.Ordering#
       |local4 => method o: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local5
       |local5 => typeparam T
       |local6 => class d[T] extends AnyRef { +4 decls }
       |  T => local11
       |  AnyRef => scala/AnyRef#
       |local7 => val param t: T
       |  T => local11
       |local8 => implicit param evidence$3: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local11
       |local9 => implicit param evidence$4: Numeric[T]
       |  Numeric => scala/package.Numeric#
       |  T => local11
       |local10 => primary ctor <init>(val t: T)(implicit evidence$3: Ordering[T], implicit evidence$4: Numeric[T])
       |  t => local7
       |  T => local11
       |  evidence$3 => local8
       |  Ordering => scala/package.Ordering#
       |  evidence$4 => local9
       |  Numeric => scala/package.Numeric#
       |local11 => typeparam T
       |local12 => class e extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local13 => implicit val param o: Ordering[Int]
       |  Ordering => scala/package.Ordering#
       |  Int => scala/Int#
       |local14 => primary ctor <init>()(implicit val o: Ordering[Int])
       |  o => local13
       |  Ordering => scala/package.Ordering#
       |  Int => scala/Int#
       |local15 => final class $anon extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |local16 => method h[T](t: T)(implicit evidence$5: Ordering[T]): T
       |  T => local19
       |  t => local17
       |  evidence$5 => local18
       |  Ordering => scala/package.Ordering#
       |local17 => param t: T
       |  T => local19
       |local18 => synthetic implicit param evidence$5: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local19
       |local19 => typeparam T
       |local20 => val local a: AnyRef { def h[T](t: T)(implicit evidence$5: Ordering[T]): T }
       |  AnyRef => scala/AnyRef#
       |  h => local16
       |  T => local19
       |  t => local17
       |  evidence$5 => local18
       |  Ordering => scala/package.Ordering#
       |""".stripMargin,
  )

  symbols(
    "classes with context bounds nested in a local class and in an anonymous class",
    """|object s4c {
       |  def m: Any = {
       |    class o { class c[T: Ordering](t: T) { def x = implicitly[Ordering[T]] } }
       |    val a = new AnyRef { class c[T: Ordering](t: T) { def x = implicitly[Ordering[T]] } }
       |    (new o, a)
       |  }
       |}
       |""".stripMargin,
    """|_empty_/s4c. => final object s4c extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s4c.m(). => method m: Any
       |  Any => scala/Any#
       |local0 => class o extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => primary ctor <init>()
       |local2 => class c[T] extends AnyRef { +4 decls }
       |  T => local7
       |  AnyRef => scala/AnyRef#
       |local3 => param t: T
       |  T => local7
       |local4 => implicit param evidence$1: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local7
       |local5 => primary ctor <init>(t: T)(implicit evidence$1: Ordering[T])
       |  t => local3
       |  T => local7
       |  evidence$1 => local4
       |  Ordering => scala/package.Ordering#
       |local6 => method x: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local7
       |local7 => typeparam T
       |local8 => final class $anon extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |local9 => class c[T] extends AnyRef { +4 decls }
       |  T => local14
       |  AnyRef => scala/AnyRef#
       |local10 => param t: T
       |  T => local14
       |local11 => implicit param evidence$2: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local14
       |local12 => primary ctor <init>(t: T)(implicit evidence$2: Ordering[T])
       |  t => local10
       |  T => local14
       |  evidence$2 => local11
       |  Ordering => scala/package.Ordering#
       |local13 => method x: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local14
       |local14 => typeparam T
       |local15 => val local a: AnyRef { class c[T] extends AnyRef { +4 decls } }
       |  AnyRef => scala/AnyRef#
       |  c => local9
       |  T => local14
       |""".stripMargin,
  )

  symbols(
    "local implicit class with a context bound",
    """|object s4d {
       |  def m: Any = {
       |    implicit class i[T: Ordering](t: T) { def x = implicitly[Ordering[T]] }
       |    1.x
       |  }
       |}
       |""".stripMargin,
    """|_empty_/s4d. => final object s4d extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s4d.m(). => method m: Any
       |  Any => scala/Any#
       |local0 => implicit class i[T] extends AnyRef { +4 decls }
       |  T => local5
       |  AnyRef => scala/AnyRef#
       |local1 => param t: T
       |  T => local5
       |local2 => implicit param evidence$1: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local5
       |local3 => primary ctor <init>(t: T)(implicit evidence$1: Ordering[T])
       |  t => local1
       |  T => local5
       |  evidence$1 => local2
       |  Ordering => scala/package.Ordering#
       |local4 => method x: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local5
       |local5 => typeparam T
       |local6 => synthetic implicit method i[T](t: T)(implicit evidence$1: Ordering[T]): i[T]
       |  T => local5
       |  t => local7
       |  evidence$1 => local8
       |  Ordering => scala/package.Ordering#
       |  i => local0
       |local7 => param t: T
       |  T => local5
       |local8 => implicit param evidence$1: Ordering[T]
       |  Ordering => scala/package.Ordering#
       |  T => local5
       |""".stripMargin,
  )

  symbols(
    "local case class with a context bound",
    """|object s4e {
       |  def m: Any = {
       |    case class k[T: Ordering](t: T) { def x = implicitly[Ordering[T]] }
       |    k(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/s4e. => final object s4e extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/s4e.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k[T] extends AnyRef with Product with Serializable { +14 decls }
         |  T => local15
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val param t: T
         |  T => local15
         |local2 => implicit param evidence$1: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local15
         |local3 => primary ctor <init>(val t: T)(implicit evidence$1: Ordering[T])
         |  t => local1
         |  T => local15
         |  evidence$1 => local2
         |  Ordering => scala/package.Ordering#
         |local4 => method x: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local15
         |local5 => synthetic method copy[T](t: T)(implicit evidence$1: Ordering[T]): k[T]
         |  T => local15
         |  t => local16
         |  evidence$1 => local17
         |  Ordering => scala/package.Ordering#
         |  k => local0
         |local6 => synthetic method copy$default$1[T]: T @uncheckedVariance
         |  T => local15
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local7 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local8 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local9 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local18
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local10 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local11 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local19
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local12 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local13 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local14 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local20
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local15 => typeparam T
         |local16 => default param t: T
         |  T => local15
         |local17 => implicit param evidence$1: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local15
         |local18 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local19 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local20 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local21 => synthetic method apply[T](t: T)(implicit evidence$1: Ordering[T]): k[T]
         |  T => local15
         |  t => local23
         |  evidence$1 => local24
         |  Ordering => scala/package.Ordering#
         |  k => local0
         |local22 => synthetic final object k extends AnyRef with Serializable { +3 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => scala/Serializable#
         |local23 => param t: T
         |  T => local15
         |local24 => implicit param evidence$1: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local15
         |local25 => synthetic final method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local26 => synthetic method unapply[T](x$0: k[T]): Option[T]
         |  T => local15
         |  x$0 => local27
         |  k => local0
         |  Option => scala/Option#
         |local27 => synthetic param x$0: k[T]
         |  k => local0
         |  T => local15
         |""".stripMargin
    else
      """|_empty_/s4e. => final object s4e extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/s4e.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k[T] extends AnyRef with Product with Serializable { +15 decls }
         |  T => local16
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val param t: T
         |  T => local16
         |local2 => implicit param evidence$1: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local16
         |local3 => primary ctor <init>(val t: T)(implicit evidence$1: Ordering[T])
         |  t => local1
         |  T => local16
         |  evidence$1 => local2
         |  Ordering => scala/package.Ordering#
         |local4 => method x: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local16
         |local5 => synthetic method copy[T](t: T)(implicit evidence$1: Ordering[T]): k[T]
         |  T => local16
         |  t => local17
         |  evidence$1 => local18
         |  Ordering => scala/package.Ordering#
         |  k => local0
         |local6 => synthetic method copy$default$1[T]: T @uncheckedVariance
         |  T => local16
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local7 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local8 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local9 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local19
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local10 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local11 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local20
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local12 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local21
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local13 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local14 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local15 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local22
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local16 => typeparam T
         |local17 => default param t: T
         |  T => local16
         |local18 => implicit param evidence$1: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local16
         |local19 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local20 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local21 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local22 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local23 => synthetic method apply[T](t: T)(implicit evidence$1: Ordering[T]): k[T]
         |  T => local16
         |  t => local25
         |  evidence$1 => local26
         |  Ordering => scala/package.Ordering#
         |  k => local0
         |local24 => synthetic final object k extends AnyRef with Serializable { +3 decls }
         |  AnyRef => scala/AnyRef#
         |  Serializable => java/io/Serializable#
         |local25 => param t: T
         |  T => local16
         |local26 => implicit param evidence$1: Ordering[T]
         |  Ordering => scala/package.Ordering#
         |  T => local16
         |local27 => synthetic final method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local28 => synthetic method unapply[T](x$0: k[T]): Option[T]
         |  T => local16
         |  x$0 => local29
         |  k => local0
         |  Option => scala/Option#
         |local29 => synthetic param x$0: k[T]
         |  k => local0
         |  T => local16
         |""".stripMargin,
  )

  symbols(
    "default argument that refers to a parameter",
    """|object s7 {
       |  def m = { def f(a: Int = 1)(b: Int = a) = b; f()() }
       |}
       |""".stripMargin,
    """|_empty_/s7. => final object s7 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s7.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => method f(a: Int)(b: Int): Int
       |  a => local1
       |  Int => scala/Int#
       |  b => local2
       |local1 => default param a: Int
       |  Int => scala/Int#
       |local2 => default param b: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "local implicit class",
    """|object s8 {
       |  def m = { implicit class R(val x: Int) { def d = x }; 1.d + new R(x = 2).x }
       |}
       |""".stripMargin,
    """|_empty_/s8. => final object s8 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s8.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => implicit class R extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => val param x: Int
       |  Int => scala/Int#
       |local2 => primary ctor <init>(val x: Int)
       |  x => local1
       |  Int => scala/Int#
       |local3 => method d: Int
       |  Int => scala/Int#
       |local4 => synthetic implicit method R(x: Int): R
       |  x => local5
       |  Int => scala/Int#
       |  R => local0
       |local5 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "unused local implicit class",
    """|object ic1 {
       |  def m = { implicit class R(val x: Int); 1 }
       |}
       |""".stripMargin,
    """|_empty_/ic1. => final object ic1 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ic1.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => implicit class R extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => val param x: Int
       |  Int => scala/Int#
       |local2 => primary ctor <init>(val x: Int)
       |  x => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "unused local implicit class",
    """|object ic1 {
       |  def m = { implicit class R(val x: Int); 1 }
       |}
       |""".stripMargin,
    """|[0:7..0:10): ic1 <= _empty_/ic1.
       |[1:6..1:7): m <= _empty_/ic1.m().
       |[1:27..1:28): R <= local0
       |[1:28..1:28):  <= local2
       |[1:33..1:34): x <= local1
       |[1:36..1:39): Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "local implicit class used as a type",
    """|object ic2 {
       |  def m = { implicit class R(val x: Int); val r: R = null; r.x }
       |}
       |""".stripMargin,
    """|_empty_/ic2. => final object ic2 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ic2.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => implicit class R extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => val param x: Int
       |  Int => scala/Int#
       |local2 => primary ctor <init>(val x: Int)
       |  x => local1
       |  Int => scala/Int#
       |local3 => val local r: R
       |  R => local0
       |""".stripMargin,
  )

  symbols(
    "unused local implicit class in a block of a template val",
    """|class ic3 {
       |  val v = { implicit class R(val x: Int); 1 }
       |}
       |""".stripMargin,
    """|_empty_/ic3# => class ic3 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ic3#`<init>`(). => primary ctor <init>()
       |_empty_/ic3#v. => val method v: Int
       |  Int => scala/Int#
       |local0 => implicit class R extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => val param x: Int
       |  Int => scala/Int#
       |local2 => primary ctor <init>(val x: Int)
       |  x => local1
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "unused implicit class in a local class",
    """|object ic4 {
       |  def m: Any = { class L { implicit class R(val x: Int) }; new L }
       |}
       |""".stripMargin,
    """|_empty_/ic4. => final object ic4 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ic4.m(). => method m: Any
       |  Any => scala/Any#
       |local0 => class L extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => primary ctor <init>()
       |local2 => implicit class R extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local3 => private synthetic implicit method R(x: Int): R
       |  x => local6
       |  Int => scala/Int#
       |  R => local2
       |local4 => val param x: Int
       |  Int => scala/Int#
       |local5 => primary ctor <init>(val x: Int)
       |  x => local4
       |  Int => scala/Int#
       |local6 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "unused local implicit class with a type parameter",
    """|object ic5 {
       |  def m = { implicit class R[T](val x: T); 1 }
       |}
       |""".stripMargin,
    """|_empty_/ic5. => final object ic5 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ic5.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => implicit class R[T] extends AnyRef { +2 decls }
       |  T => local3
       |  AnyRef => scala/AnyRef#
       |local1 => val param x: T
       |  T => local3
       |local2 => primary ctor <init>(val x: T)
       |  x => local1
       |  T => local3
       |local3 => typeparam T
       |""".stripMargin,
  )

  symbols(
    "unused local implicit class with an implicit parameter list",
    """|object ic6 {
       |  def m = { implicit class R(val x: Int)(implicit o: Ordering[Int]); 1 }
       |}
       |""".stripMargin,
    """|_empty_/ic6. => final object ic6 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ic6.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => implicit class R extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => val param x: Int
       |  Int => scala/Int#
       |local2 => implicit param o: Ordering[Int]
       |  Ordering => scala/package.Ordering#
       |  Int => scala/Int#
       |local3 => primary ctor <init>(val x: Int)(implicit o: Ordering[Int])
       |  x => local1
       |  Int => scala/Int#
       |  o => local2
       |  Ordering => scala/package.Ordering#
       |""".stripMargin,
  )

  symbols(
    "local implicit class used as a conversion",
    """|object ic7 {
       |  def m = { implicit class R(val x: Int); val r: R = 1; r.x }
       |}
       |""".stripMargin,
    """|_empty_/ic7. => final object ic7 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ic7.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => implicit class R extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => val param x: Int
       |  Int => scala/Int#
       |local2 => primary ctor <init>(val x: Int)
       |  x => local1
       |  Int => scala/Int#
       |local3 => synthetic implicit method R(x: Int): R
       |  x => local5
       |  Int => scala/Int#
       |  R => local0
       |local4 => val local r: R
       |  R => local0
       |local5 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "local implicit class used as a conversion",
    """|object ic7 {
       |  def m = { implicit class R(val x: Int); val r: R = 1; r.x }
       |}
       |""".stripMargin,
    """|[0:7..0:10): ic7 <= _empty_/ic7.
       |[1:6..1:7): m <= _empty_/ic7.m().
       |[1:27..1:28): R <= local0
       |[1:28..1:28):  <= local2
       |[1:33..1:34): x <= local1
       |[1:36..1:39): Int => scala/Int#
       |[1:46..1:47): r <= local4
       |[1:49..1:50): R => local0
       |[1:56..1:57): r => local4
       |[1:58..1:59): x => local1
       |""".stripMargin,
  )

  symbols(
    "local implicit class called explicitly",
    """|object ic8 {
       |  def m = { implicit class R(val x: Int); R(1).x }
       |}
       |""".stripMargin,
    """|_empty_/ic8. => final object ic8 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ic8.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => implicit class R extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => val param x: Int
       |  Int => scala/Int#
       |local2 => primary ctor <init>(val x: Int)
       |  x => local1
       |  Int => scala/Int#
       |local3 => synthetic implicit method R(x: Int): R
       |  x => local4
       |  Int => scala/Int#
       |  R => local0
       |local4 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "local implicit class called explicitly",
    """|object ic8 {
       |  def m = { implicit class R(val x: Int); R(1).x }
       |}
       |""".stripMargin,
    """|[0:7..0:10): ic8 <= _empty_/ic8.
       |[1:6..1:7): m <= _empty_/ic8.m().
       |[1:27..1:28): R <= local0
       |[1:28..1:28):  <= local2
       |[1:33..1:34): x <= local1
       |[1:36..1:39): Int => scala/Int#
       |[1:42..1:43): R => local3
       |[1:47..1:48): x => local1
       |""".stripMargin,
  )

  symbols(
    "unused local implicit class in a lambda",
    """|object ic9 {
       |  val f = (i: Int) => { implicit class R(val x: Int); i }
       |}
       |""".stripMargin,
    """|_empty_/ic9. => final object ic9 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ic9.f. => val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |local0 => param i: Int
       |  Int => scala/Int#
       |local1 => implicit class R extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local2 => val param x: Int
       |  Int => scala/Int#
       |local3 => primary ctor <init>(val x: Int)
       |  x => local2
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "unused implicit class in an anonymous class",
    """|object ic10 {
       |  def m: Any = new AnyRef { implicit class R(val x: Int) }
       |}
       |""".stripMargin,
    """|_empty_/ic10. => final object ic10 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/ic10.m(). => method m: Any
       |  Any => scala/Any#
       |local0 => final class $anon extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => implicit class R extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local2 => private synthetic implicit method R(x: Int): R
       |  x => local5
       |  Int => scala/Int#
       |  R => local1
       |local3 => val param x: Int
       |  Int => scala/Int#
       |local4 => primary ctor <init>(val x: Int)
       |  x => local3
       |  Int => scala/Int#
       |local5 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  test("named arguments of a local case class") {
    def load() = computeDatabaseSectionFromSnippet(
      """|object s9 {
         |  def m: Any = { case class k(i: Int); val x = k(i = 1); x.copy(i = 3).i }
         |}
         |""".stripMargin,
      "Occurrences",
    )
    assertNoDiff(
      load().linesIterator.filter(_.contains("): i ")).mkString("\n"),
      if (ScalaVersion.is212)
        """|[1:30..1:31): i <= local1
           |[1:49..1:50): i => local18
           |[1:64..1:65): i => local13
           |[1:71..1:72): i => local1
           |""".stripMargin
      else
        """|[1:30..1:31): i <= local1
           |[1:49..1:50): i => local20
           |[1:64..1:65): i => local14
           |[1:71..1:72): i => local1
           |""".stripMargin,
    )
  }

  symbols(
    "var in a refinement of a val parameter",
    """|class s5(val n: { var vn: Int })
       |""".stripMargin,
    """|_empty_/s5# => class s5 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s5#`<init>`(). => primary ctor <init>(val n: AnyRef { var def vn: Int; var def vn_=(x$1: Int): Unit })
       |  n => _empty_/s5#`<init>`().(n)
       |  AnyRef => scala/AnyRef#
       |  vn => local0
       |  Int => scala/Int#
       |  vn_= => local1
       |  x$1 => local2
       |  Unit => scala/Unit#
       |_empty_/s5#`<init>`().(n) => val param n: AnyRef { var def vn: Int; var def vn_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vn => local0
       |  Int => scala/Int#
       |  vn_= => local1
       |  x$1 => local2
       |  Unit => scala/Unit#
       |_empty_/s5#n. => val method n: AnyRef { var def vn: Int; var def vn_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vn => local0
       |  Int => scala/Int#
       |  vn_= => local1
       |  x$1 => local2
       |  Unit => scala/Unit#
       |local0 => abstract var method vn: Int
       |  Int => scala/Int#
       |local1 => abstract var method vn_=(x$1: Int): Unit
       |  x$1 => local2
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local2 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "var in a refinement of a local val",
    """|class s6 {
       |  def m = {
       |    val n: { var vn: Int } = ???
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|_empty_/s6# => class s6 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/s6#`<init>`(). => primary ctor <init>()
       |_empty_/s6#m(). => method m: Unit
       |  Unit => scala/Unit#
       |local0 => val local n: AnyRef { var def vn: Int; var def vn_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  vn => local1
       |  Int => scala/Int#
       |  vn_= => local2
       |  x$1 => local3
       |  Unit => scala/Unit#
       |local1 => abstract var method vn: Int
       |  Int => scala/Int#
       |local2 => abstract var method vn_=(x$1: Int): Unit
       |  x$1 => local3
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local3 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "pattern definitions in a method",
    """|object t1 {
       |  def m = {
       |    val (a, b) = (1, 2)
       |    var (c, d) = (1, 2)
       |    lazy val (e, f) = (1, 2)
       |    val _ = 1
       |    a + b + c + d + e + f
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t1. => final object t1 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t1.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => val local a: Int
         |  Int => scala/Int#
         |local1 => val local b: Int
         |  Int => scala/Int#
         |local2 => var local c: Int
         |  Int => scala/Int#
         |local3 => var local d: Int
         |  Int => scala/Int#
         |local4 => lazy val local e: Int
         |  Int => scala/Int#
         |local5 => lazy val local f: Int
         |  Int => scala/Int#
         |local6 => val local _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t1. => final object t1 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t1.m(). => method m: Int
         |  Int => scala/Int#
         |local0 => val local a: Int
         |  Int => scala/Int#
         |local1 => val local b: Int
         |  Int => scala/Int#
         |local2 => var local c: Int
         |  Int => scala/Int#
         |local3 => var local d: Int
         |  Int => scala/Int#
         |local4 => lazy val local e: Int
         |  Int => scala/Int#
         |local5 => lazy val local f: Int
         |  Int => scala/Int#
         |local6 => synthetic val local x$4: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  occurrences(
    "pattern definitions in a method",
    """|object t1 {
       |  def m = {
       |    val (a, b) = (1, 2)
       |    var (c, d) = (1, 2)
       |    lazy val (e, f) = (1, 2)
       |    val _ = 1
       |    a + b + c + d + e + f
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): t1 <= _empty_/t1.
       |[1:6..1:7): m <= _empty_/t1.m().
       |[2:9..2:10): a <= local0
       |[2:12..2:13): b <= local1
       |[3:9..3:10): c <= local2
       |[3:12..3:13): d <= local3
       |[4:14..4:15): e <= local4
       |[4:17..4:18): f <= local5
       |[5:8..5:9): _ <= local6
       |[6:4..6:5): a => local0
       |[6:6..6:7): + => scala/Int#`+`(+4).
       |[6:8..6:9): b => local1
       |[6:10..6:11): + => scala/Int#`+`(+4).
       |[6:12..6:13): c => local2
       |[6:14..6:15): + => scala/Int#`+`(+4).
       |[6:16..6:17): d => local3
       |[6:18..6:19): + => scala/Int#`+`(+4).
       |[6:20..6:21): e => local4
       |[6:22..6:23): + => scala/Int#`+`(+4).
       |[6:24..6:25): f => local5
       |""".stripMargin,
  )

  synthetics(
    "pattern definitions in a method",
    """|object t1 {
       |  def m = {
       |    val (a, b) = (1, 2)
       |    var (c, d) = (1, 2)
       |    lazy val (e, f) = (1, 2)
       |    val _ = 1
       |    a + b + c + d + e + f
       |  }
       |}
       |""".stripMargin,
    """|[2:8..2:14): (a, b) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[3:8..3:14): (c, d) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[4:13..4:19): (e, f) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "pattern definitions in a class body",
    """|class t2 {
       |  val (a, b) = (1, 2)
       |  var (c, d) = (1, 2)
       |  lazy val (e, f) = (1, 2)
       |  private val (g, h) = (1, 2)
       |  val _ = 1
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t2# => class t2 extends AnyRef { +16 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t2#_. => val method _: Int
         |  Int => scala/Int#
         |_empty_/t2#`<init>`(). => primary ctor <init>()
         |_empty_/t2#`c_=`(). => var method c_=(x$1: Int): Unit
         |  x$1 => _empty_/t2#`c_=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/t2#`c_=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t2#`d_=`(). => var method d_=(x$1: Int): Unit
         |  x$1 => _empty_/t2#`d_=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/t2#`d_=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t2#a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/t2#b. => val method b: Int
         |  Int => scala/Int#
         |_empty_/t2#c(). => var method c: Int
         |  Int => scala/Int#
         |_empty_/t2#d(). => var method d: Int
         |  Int => scala/Int#
         |_empty_/t2#e. => lazy val method e: Int
         |  Int => scala/Int#
         |_empty_/t2#f. => lazy val method f: Int
         |  Int => scala/Int#
         |_empty_/t2#g. => private val method g: Int
         |  Int => scala/Int#
         |_empty_/t2#h. => private val method h: Int
         |  Int => scala/Int#
         |_empty_/t2#x$1. => private[this] synthetic val method x$1: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$2. => private[this] synthetic val method x$2: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$3. => private[this] synthetic lazy val method x$3: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$4. => private[this] synthetic val method x$4: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t2# => class t2 extends AnyRef { +16 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t2#`<init>`(). => primary ctor <init>()
         |_empty_/t2#`c_=`(). => var method c_=(x$1: Int): Unit
         |  x$1 => _empty_/t2#`c_=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/t2#`c_=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t2#`d_=`(). => var method d_=(x$1: Int): Unit
         |  x$1 => _empty_/t2#`d_=`().(x$1)
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |_empty_/t2#`d_=`().(x$1) => synthetic param x$1: Int
         |  Int => scala/Int#
         |_empty_/t2#a. => val method a: Int
         |  Int => scala/Int#
         |_empty_/t2#b. => val method b: Int
         |  Int => scala/Int#
         |_empty_/t2#c(). => var method c: Int
         |  Int => scala/Int#
         |_empty_/t2#d(). => var method d: Int
         |  Int => scala/Int#
         |_empty_/t2#e. => lazy val method e: Int
         |  Int => scala/Int#
         |_empty_/t2#f. => lazy val method f: Int
         |  Int => scala/Int#
         |_empty_/t2#g. => private val method g: Int
         |  Int => scala/Int#
         |_empty_/t2#h. => private val method h: Int
         |  Int => scala/Int#
         |_empty_/t2#x$1. => private[this] synthetic val method x$1: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$2. => private[this] synthetic val method x$2: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$3. => private[this] synthetic lazy val method x$3: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$4. => private[this] synthetic val method x$4: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |_empty_/t2#x$5. => private[this] synthetic val method x$5: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  occurrences(
    "pattern definitions in a class body",
    """|class t2 {
       |  val (a, b) = (1, 2)
       |  var (c, d) = (1, 2)
       |  lazy val (e, f) = (1, 2)
       |  private val (g, h) = (1, 2)
       |  val _ = 1
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:6..0:8): t2 <= _empty_/t2#
         |[0:9..0:9):  <= _empty_/t2#`<init>`().
         |[1:7..1:8): a <= _empty_/t2#a.
         |[1:10..1:11): b <= _empty_/t2#b.
         |[2:7..2:8): c <= _empty_/t2#c().
         |[2:10..2:11): d <= _empty_/t2#d().
         |[3:12..3:13): e <= _empty_/t2#e.
         |[3:15..3:16): f <= _empty_/t2#f.
         |[4:15..4:16): g <= _empty_/t2#g.
         |[4:18..4:19): h <= _empty_/t2#h.
         |[5:6..5:7): _ <= _empty_/t2#_.
         |""".stripMargin
    else
      """|[0:6..0:8): t2 <= _empty_/t2#
         |[0:9..0:9):  <= _empty_/t2#`<init>`().
         |[1:7..1:8): a <= _empty_/t2#a.
         |[1:10..1:11): b <= _empty_/t2#b.
         |[2:7..2:8): c <= _empty_/t2#c().
         |[2:10..2:11): d <= _empty_/t2#d().
         |[3:12..3:13): e <= _empty_/t2#e.
         |[3:15..3:16): f <= _empty_/t2#f.
         |[4:15..4:16): g <= _empty_/t2#g.
         |[4:18..4:19): h <= _empty_/t2#h.
         |[5:6..5:7): _ <= _empty_/t2#x$5.
         |""".stripMargin,
  )

  synthetics(
    "pattern definitions in a class body",
    """|class t2 {
       |  val (a, b) = (1, 2)
       |  var (c, d) = (1, 2)
       |  lazy val (e, f) = (1, 2)
       |  private val (g, h) = (1, 2)
       |  val _ = 1
       |}
       |""".stripMargin,
    """|[1:6..1:12): (a, b) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[2:6..2:12): (c, d) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[3:11..3:17): (e, f) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[4:14..4:20): (g, h) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "pattern definitions in a local class",
    """|object t3 {
       |  def m = {
       |    class c { val (a, b) = (1, 2); var (p, q) = (1, 2) }
       |    new c
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t3. => final object t3 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t3.m(). => method m: AnyRef { val def a: Int; val def b: Int; var def p: Int; var def p_=(x$1: Int): Unit; var def q: Int; var def q_=(x$1: Int): Unit }
       |  AnyRef => scala/AnyRef#
       |  a => local0
       |  Int => scala/Int#
       |  b => local1
       |  p => local2
       |  p_= => local3
       |  x$1 => local4
       |  Unit => scala/Unit#
       |  q => local5
       |  q_= => local6
       |  x$1 => local7
       |local0 => val method a: Int
       |  Int => scala/Int#
       |local1 => val method b: Int
       |  Int => scala/Int#
       |local2 => var method p: Int
       |  Int => scala/Int#
       |local3 => var method p_=(x$1: Int): Unit
       |  x$1 => local4
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local4 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local5 => var method q: Int
       |  Int => scala/Int#
       |local6 => var method q_=(x$1: Int): Unit
       |  x$1 => local7
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local7 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local8 => class c extends AnyRef { +9 decls }
       |  AnyRef => scala/AnyRef#
       |local9 => primary ctor <init>()
       |local10 => private[this] synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local11 => private[this] synthetic val method x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "pattern definitions in a local class",
    """|object t3 {
       |  def m = {
       |    class c { val (a, b) = (1, 2); var (p, q) = (1, 2) }
       |    new c
       |  }
       |}
       |""".stripMargin,
    """|[0:7..0:9): t3 <= _empty_/t3.
       |[1:6..1:7): m <= _empty_/t3.m().
       |[2:10..2:11): c <= local8
       |[2:12..2:12):  <= local9
       |[2:19..2:20): a <= local0
       |[2:22..2:23): b <= local1
       |[2:40..2:41): p <= local2
       |[2:43..2:44): q <= local5
       |[3:8..3:9): c => local8
       |[3:9..3:9):  => local9
       |""".stripMargin,
  )

  synthetics(
    "pattern definitions in a local class",
    """|object t3 {
       |  def m = {
       |    class c { val (a, b) = (1, 2); var (p, q) = (1, 2) }
       |    new c
       |  }
       |}
       |""".stripMargin,
    """|[2:18..2:24): (a, b) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |[2:39..2:45): (p, q) => *.apply[Int, Int]
       |  apply => scala/Tuple2.apply().
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "binders named defaultCase$",
    """|class t14 {
       |  def pf: PartialFunction[Int, Int] = { case 1 => 1; case defaultCase$ => defaultCase$ }
       |  def c = List(1).collect { case defaultCase$ if defaultCase$ > 0 => defaultCase$ }
       |  def m(x: Int) = x match { case defaultCase$ => defaultCase$ }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t14# => class t14 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t14#`<init>`(). => primary ctor <init>()
         |_empty_/t14#c(). => method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t14#m(). => method m(x: Int): Int
         |  x => _empty_/t14#m().(x)
         |  Int => scala/Int#
         |_empty_/t14#m().(x) => param x: Int
         |  Int => scala/Int#
         |_empty_/t14#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local8 => synthetic param x1: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x1 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x1: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local17 => synthetic param x1: Int
         |  Int => scala/Int#
         |local18 => val local defaultCase$: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t14# => class t14 extends AnyRef { +4 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t14#`<init>`(). => primary ctor <init>()
         |_empty_/t14#c(). => method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t14#m(). => method m(x: Int): Int
         |  x => _empty_/t14#m().(x)
         |  Int => scala/Int#
         |_empty_/t14#m().(x) => param x: Int
         |  Int => scala/Int#
         |_empty_/t14#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local8 => synthetic param x1: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x1 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x1: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => val local defaultCase$: Int
         |  Int => scala/Int#
         |local17 => synthetic param x1: Int
         |  Int => scala/Int#
         |local18 => val local defaultCase$: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    "binders named defaultCase$",
    """|class t14 {
       |  def pf: PartialFunction[Int, Int] = { case 1 => 1; case defaultCase$ => defaultCase$ }
       |  def c = List(1).collect { case defaultCase$ if defaultCase$ > 0 => defaultCase$ }
       |  def m(x: Int) = x match { case defaultCase$ => defaultCase$ }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:6..0:9): t14 <= _empty_/t14#
         |[0:10..0:10):  <= _empty_/t14#`<init>`().
         |[1:6..1:8): pf <= _empty_/t14#pf().
         |[1:10..1:25): PartialFunction => scala/PartialFunction#
         |[1:26..1:29): Int => scala/Int#
         |[1:31..1:34): Int => scala/Int#
         |[1:38..1:38):  <= local0
         |[1:58..1:70): defaultCase$ <= local7
         |[1:74..1:86): defaultCase$ => local7
         |[2:6..2:7): c <= _empty_/t14#c().
         |[2:10..2:14): List => scala/collection/immutable/List.
         |[2:18..2:25): collect => scala/collection/immutable/List#collect().
         |[2:26..2:26):  <= local9
         |[2:33..2:45): defaultCase$ <= local16
         |[2:49..2:61): defaultCase$ => local16
         |[2:62..2:63): > => scala/Int#`>`(+3).
         |[2:69..2:81): defaultCase$ => local16
         |[3:6..3:7): m <= _empty_/t14#m().
         |[3:8..3:9): x <= _empty_/t14#m().(x)
         |[3:11..3:14): Int => scala/Int#
         |[3:18..3:19): x => _empty_/t14#m().(x)
         |[3:33..3:45): defaultCase$ <= local18
         |[3:49..3:61): defaultCase$ => local18
         |""".stripMargin
    else
      """|[0:6..0:9): t14 <= _empty_/t14#
         |[0:10..0:10):  <= _empty_/t14#`<init>`().
         |[1:6..1:8): pf <= _empty_/t14#pf().
         |[1:10..1:25): PartialFunction => scala/PartialFunction#
         |[1:26..1:29): Int => scala/Int#
         |[1:31..1:34): Int => scala/Int#
         |[1:38..1:38):  <= local0
         |[1:58..1:70): defaultCase$ <= local7
         |[1:74..1:86): defaultCase$ => local7
         |[2:6..2:7): c <= _empty_/t14#c().
         |[2:10..2:14): List => scala/package.List.
         |[2:18..2:25): collect => scala/collection/immutable/List#collect().
         |[2:26..2:26):  <= local9
         |[2:33..2:45): defaultCase$ <= local16
         |[2:49..2:61): defaultCase$ => local16
         |[2:62..2:63): > => scala/Int#`>`(+3).
         |[2:69..2:81): defaultCase$ => local16
         |[3:6..3:7): m <= _empty_/t14#m().
         |[3:8..3:9): x <= _empty_/t14#m().(x)
         |[3:11..3:14): Int => scala/Int#
         |[3:18..3:19): x => _empty_/t14#m().(x)
         |[3:33..3:45): defaultCase$ <= local18
         |[3:49..3:61): defaultCase$ => local18
         |""".stripMargin,
  )

  symbols(
    "partial functions with binders",
    """|class t11 {
       |  def pf: PartialFunction[Int, Int] = { case i if i > 0 => i }
       |  def c = List(1).collect { case i: Int => i }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t11# => class t11 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t11#`<init>`(). => primary ctor <init>()
         |_empty_/t11#c(). => method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t11#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => val local i: Int
         |  Int => scala/Int#
         |local8 => synthetic param x1: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x1 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x1: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => val local i: Int
         |  Int => scala/Int#
         |local17 => synthetic param x1: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t11# => class t11 extends AnyRef { +3 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t11#`<init>`(). => primary ctor <init>()
         |_empty_/t11#c(). => method c: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t11#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local6
         |  Int => scala/Int#
         |  B1 => local5
         |  x1 => local3
         |  default => local4
         |  Function1 => scala/Function1#
         |local2 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local8
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local3 => synthetic param x1: A1
         |  A1 => local6
         |local4 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local6
         |  B1 => local5
         |local5 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local6 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local7 => val local i: Int
         |  Int => scala/Int#
         |local8 => synthetic param x1: Int
         |  Int => scala/Int#
         |local9 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local10 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local15
         |  Int => scala/Int#
         |  B1 => local14
         |  x1 => local12
         |  default => local13
         |  Function1 => scala/Function1#
         |local11 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local17
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local12 => synthetic param x1: A1
         |  A1 => local15
         |local13 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local15
         |  B1 => local14
         |local14 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local15 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local16 => val local i: Int
         |  Int => scala/Int#
         |local17 => synthetic param x1: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  if (ScalaVersion.is213) symbols(
    "function literal as a partial function",
    """|class t12 {
       |  def pf: PartialFunction[Int, Int] = x => x + 1
       |}
       |""".stripMargin,
    """|_empty_/t12# => class t12 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t12#`<init>`(). => primary ctor <init>()
       |_empty_/t12#pf(). => method pf: PartialFunction[Int, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Int => scala/Int#
       |local0 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
       |  SerialVersionUID => scala/SerialVersionUID#
       |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
       |  Int => scala/Int#
       |  Serializable => java/io/Serializable#
       |local1 => final method applyOrElse[A1 <: Int, B1 >: Int](x: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
       |  A1 => local6
       |  Int => scala/Int#
       |  B1 => local5
       |  x => local3
       |  default => local4
       |  Function1 => scala/Function1#
       |local2 => final method isDefinedAt(x: Int): Boolean <: scala/PartialFunction#isDefinedAt().
       |  x => local7
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic param x: A1
       |  A1 => local6
       |local4 => synthetic param default: Function1[A1, B1]
       |  Function1 => scala/Function1#
       |  A1 => local6
       |  B1 => local5
       |local5 => typeparam B1 >: Int
       |  Int => scala/Int#
       |local6 => typeparam A1 <: Int
       |  Int => scala/Int#
       |local7 => synthetic param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "function literal as a partial function",
    """|class t12 {
       |  def pf: PartialFunction[Int, Int] = x => x + 1
       |}
       |""".stripMargin,
    """|[0:6..0:9): t12 <= _empty_/t12#
       |[0:10..0:10):  <= _empty_/t12#`<init>`().
       |[1:6..1:8): pf <= _empty_/t12#pf().
       |[1:10..1:25): PartialFunction => scala/PartialFunction#
       |[1:26..1:29): Int => scala/Int#
       |[1:31..1:34): Int => scala/Int#
       |[1:43..1:44): x => scala/Any#asInstanceOf().
       |[1:45..1:45):  <= local0
       |[1:45..1:46): + => local1
       |""".stripMargin,
  )

  if (ScalaVersion.is213) occurrences(
    "function literal as a partial function in a block",
    """|object t38 {
       |  def m = { val f: PartialFunction[Int, Int] = x => x; f }
       |}
       |""".stripMargin,
    """|[0:7..0:10): t38 <= _empty_/t38.
       |[1:6..1:7): m <= _empty_/t38.m().
       |[1:16..1:17): f <= local9
       |[1:19..1:34): PartialFunction => scala/PartialFunction#
       |[1:35..1:38): Int => scala/Int#
       |[1:40..1:43): Int => scala/Int#
       |[1:52..1:52):  <= local0
       |[1:52..1:53): x => local3
       |[1:55..1:56): f => local9
       |""".stripMargin,
  )

  symbols(
    "patterns, eta expansion and updates",
    """|class t13 {
       |  def f = for ((a, b) <- List((1, 2))) yield a
       |  def g(a: Int)(b: Int) = a + b
       |  def e = List(1).map(g(1 + 1))
       |  def h = { val arr = Array(1); var i = 0; arr(i + 0) += 1; i }
       |  def m(x: Any) = x match { case (a: Int, b) => a; case l @ List(_) => l; case _ => 0 }
       |}
       |""".stripMargin,
    """|_empty_/t13# => class t13 extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t13#`<init>`(). => primary ctor <init>()
       |_empty_/t13#e(). => method e: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t13#f(). => method f: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |_empty_/t13#g(). => method g(a: Int)(b: Int): Int
       |  a => _empty_/t13#g().(a)
       |  Int => scala/Int#
       |  b => _empty_/t13#g().(b)
       |_empty_/t13#g().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t13#g().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t13#h(). => method h: Int
       |  Int => scala/Int#
       |_empty_/t13#m(). => method m(x: Any): Any
       |  x => _empty_/t13#m().(x)
       |  Any => scala/Any#
       |_empty_/t13#m().(x) => param x: Any
       |  Any => scala/Any#
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
       |local4 => val local arr: Array[Int]
       |  Array => scala/Array#
       |  Int => scala/Int#
       |local5 => var local i: Int
       |  Int => scala/Int#
       |local6 => val local a: Int
       |  Int => scala/Int#
       |local7 => val local b: Any
       |  Any => scala/Any#
       |local8 => val local l: List[A]
       |  List => scala/collection/immutable/List#
       |  A => local9
       |local9 => abstract type A
       |""".stripMargin,
  )

  symbols(
    "value definitions in a for comprehension",
    """|object t4 {
       |  def m = for { x <- List(1); (y, z) = (x, x); w :: Nil = List(y) } yield (y, z, w)
       |}
       |""".stripMargin,
    """|_empty_/t4. => final object t4 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t4.m(). => method m: List[Tuple3[Int, Int, Int]]
       |  List => scala/collection/immutable/List#
       |  Tuple3 => scala/Tuple3#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$5: Tuple3[Int, Tuple2[Int, Int], ::[Int]]
       |  Tuple3 => scala/Tuple3#
       |  Int => scala/Int#
       |  Tuple2 => scala/Tuple2#
       |  :: => scala/collection/immutable/`::`#
       |local2 => val local y: Int
       |  Int => scala/Int#
       |local3 => val local z: Int
       |  Int => scala/Int#
       |local4 => val local w: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "value definitions in a for comprehension",
    """|object t4 {
       |  def m = for { x <- List(1); (y, z) = (x, x); w :: Nil = List(y) } yield (y, z, w)
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[0:7..0:9): t4 <= _empty_/t4.
         |[1:6..1:7): m <= _empty_/t4.m().
         |[1:16..1:17): x <= local0
         |[1:21..1:25): List => scala/collection/immutable/List.
         |[1:31..1:32): y <= local2
         |[1:34..1:35): z <= local3
         |[1:40..1:41): x => local0
         |[1:43..1:44): x => local0
         |[1:47..1:48): w <= local4
         |[1:49..1:51): :: => scala/package.`::`.
         |[1:52..1:55): Nil => scala/collection/immutable/Nil.
         |[1:58..1:62): List => scala/collection/immutable/List.
         |[1:63..1:64): y => local2
         |[1:75..1:76): y => local2
         |[1:78..1:79): z => local3
         |[1:81..1:82): w => local4
         |""".stripMargin
    else
      """|[0:7..0:9): t4 <= _empty_/t4.
         |[1:6..1:7): m <= _empty_/t4.m().
         |[1:16..1:17): x <= local0
         |[1:21..1:25): List => scala/package.List.
         |[1:31..1:32): y <= local2
         |[1:34..1:35): z <= local3
         |[1:40..1:41): x => local0
         |[1:43..1:44): x => local0
         |[1:47..1:48): w <= local4
         |[1:49..1:51): :: => scala/package.`::`.
         |[1:52..1:55): Nil => scala/package.Nil.
         |[1:58..1:62): List => scala/package.List.
         |[1:63..1:64): y => local2
         |[1:75..1:76): y => local2
         |[1:78..1:79): z => local3
         |[1:81..1:82): w => local4
         |""".stripMargin,
  )

  synthetics(
    "value definitions in a for comprehension",
    """|object t4 {
       |  def m = for { x <- List(1); (y, z) = (x, x); w :: Nil = List(y) } yield (y, z, w)
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|[1:10..1:83): for { x <- List(1); (y, z) = (x, x); w :: Nil = List(y) } yield (y, z, w) => orig(List(1)).map[Tuple3[Int, Tuple2[Int, Int], ::[Int]], List[Tuple3[Int, Tuple2[Int, Int], ::[Int]]]]({(x) => orig((y, z) = (x, x); w :: Nil = List(y))})(List.canBuildFrom[Tuple3[Int, Tuple2[Int, Int], ::[Int]]]).map[Tuple3[Int, Int, Int], List[Tuple3[Int, Int, Int]]]({(x$5) => orig((y, z, w))})(List.canBuildFrom[Tuple3[Int, Int, Int]])
         |  map => scala/collection/immutable/List#map().
         |  Tuple3 => scala/Tuple3#
         |  Int => scala/Int#
         |  Tuple2 => scala/Tuple2#
         |  :: => scala/collection/immutable/`::`#
         |  List => scala/collection/immutable/List#
         |  x => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  x$5 => local1
         |[1:21..1:25): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[1:30..1:36): (y, z) => *.apply[Tuple2[Int, Int], Int, Int]
         |  apply => scala/Tuple3.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[1:47..1:55): w :: Nil => *.apply[::[Int], Int]
         |  apply => scala/Tuple2.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |[1:58..1:62): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|[1:10..1:83): for { x <- List(1); (y, z) = (x, x); w :: Nil = List(y) } yield (y, z, w) => orig(List(1)).map[Tuple3[Int, Tuple2[Int, Int], ::[Int]]]({(x) => orig((y, z) = (x, x); w :: Nil = List(y))}).map[Tuple3[Int, Int, Int]]({(x$5) => orig((y, z, w))})
         |  map => scala/collection/immutable/List#map().
         |  Tuple3 => scala/Tuple3#
         |  Int => scala/Int#
         |  Tuple2 => scala/Tuple2#
         |  :: => scala/collection/immutable/`::`#
         |  x => local0
         |  x$5 => local1
         |[1:21..1:25): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |[1:30..1:36): (y, z) => *.apply[Tuple2[Int, Int], Int, Int]
         |  apply => scala/Tuple3.apply().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |[1:47..1:55): w :: Nil => *.apply[::[Int], Int]
         |  apply => scala/Tuple2.apply().
         |  :: => scala/collection/immutable/`::`#
         |  Int => scala/Int#
         |[1:58..1:62): List => *.apply[Int]
         |  apply => scala/collection/IterableFactory#apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  symbols(
    "pattern definitions in an anonymous class",
    """|object t5 {
       |  def m = new AnyRef {
       |    val (a, b) = (1, 2)
       |    lazy val (c, d) = (1, 2)
       |    val _ = 1
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t5. => final object t5 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t5.m(). => method m: AnyRef { val def a: Int; val def b: Int; lazy val def c: Int; lazy val def d: Int; val def _: Int }
         |  AnyRef => scala/AnyRef#
         |  a => local0
         |  Int => scala/Int#
         |  b => local1
         |  c => local2
         |  d => local3
         |  _ => local4
         |local0 => val method a: Int
         |  Int => scala/Int#
         |local1 => val method b: Int
         |  Int => scala/Int#
         |local2 => lazy val method c: Int
         |  Int => scala/Int#
         |local3 => lazy val method d: Int
         |  Int => scala/Int#
         |local4 => val method _: Int
         |  Int => scala/Int#
         |local5 => final class $anon extends AnyRef { +7 decls }
         |  AnyRef => scala/AnyRef#
         |local6 => private[this] synthetic val method x$1: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local7 => private[this] synthetic lazy val method x$2: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t5. => final object t5 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t5.m(). => method m: AnyRef { val def a: Int; val def b: Int; lazy val def c: Int; lazy val def d: Int }
         |  AnyRef => scala/AnyRef#
         |  a => local0
         |  Int => scala/Int#
         |  b => local1
         |  c => local2
         |  d => local3
         |local0 => val method a: Int
         |  Int => scala/Int#
         |local1 => val method b: Int
         |  Int => scala/Int#
         |local2 => lazy val method c: Int
         |  Int => scala/Int#
         |local3 => lazy val method d: Int
         |  Int => scala/Int#
         |local4 => final class $anon extends AnyRef { +7 decls }
         |  AnyRef => scala/AnyRef#
         |local5 => private[this] synthetic val method x$1: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local6 => private[this] synthetic lazy val method x$2: Tuple2[Int, Int]
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |local7 => private[this] synthetic val method x$3: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  symbols(
    "pattern definitions in a local object",
    """|object t6 {
       |  def m = {
       |    object o { val (a, b) = (1, 2); var (c, d) = (1, 2) }
       |    o.a
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t6. => final object t6 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t6.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => final object o extends AnyRef { +8 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => private[this] synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => val method a: Int
       |  Int => scala/Int#
       |local3 => val method b: Int
       |  Int => scala/Int#
       |local4 => private[this] synthetic val method x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local5 => var method c: Int
       |  Int => scala/Int#
       |local6 => var method c_=(x$1: Int): Unit
       |  x$1 => local9
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local7 => var method d: Int
       |  Int => scala/Int#
       |local8 => var method d_=(x$1: Int): Unit
       |  x$1 => local10
       |  Int => scala/Int#
       |  Unit => scala/Unit#
       |local9 => synthetic param x$1: Int
       |  Int => scala/Int#
       |local10 => synthetic param x$1: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "pattern definitions in a block in a class body",
    """|class t7 {
       |  val v = { val (a, b) = (1, 2); a + b }
       |  locally { val (c, d) = (1, 2) }
       |}
       |""".stripMargin,
    """|_empty_/t7# => class t7 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t7#`<init>`(). => primary ctor <init>()
       |_empty_/t7#v. => val method v: Int
       |  Int => scala/Int#
       |local0 => val local a: Int
       |  Int => scala/Int#
       |local1 => val local b: Int
       |  Int => scala/Int#
       |local2 => val local c: Int
       |  Int => scala/Int#
       |local3 => val local d: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "right-associative operators",
    """|class t8 {
       |  val v = 1 :: Nil
       |  def m(x: Int) = x :: (x + 1) :: Nil
       |}
       |""".stripMargin,
    """|_empty_/t8# => class t8 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t8#`<init>`(). => primary ctor <init>()
       |_empty_/t8#m(). => method m(x: Int): List[Int]
       |  x => _empty_/t8#m().(x)
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
       |_empty_/t8#m().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/t8#v. => val method v: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "named arguments",
    """|class t9 {
       |  def f(a: Int, b: Int) = a
       |  def m = f(b = 1 + 1, a = 2 + 2)
       |}
       |""".stripMargin,
    """|_empty_/t9# => class t9 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t9#`<init>`(). => primary ctor <init>()
       |_empty_/t9#f(). => method f(a: Int, b: Int): Int
       |  a => _empty_/t9#f().(a)
       |  Int => scala/Int#
       |  b => _empty_/t9#f().(b)
       |_empty_/t9#f().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t9#f().(b) => param b: Int
       |  Int => scala/Int#
       |_empty_/t9#m(). => method m: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "matches and partial functions",
    """|class t10 {
       |  def m(x: Any) = x match { case i: Int => i; case _ => 0 }
       |  def t = try 1 catch { case e: Exception => 2 } finally ()
       |  def pf: PartialFunction[Int, Int] = { case 1 => 1 }
       |  def eta = List(1).map(m)
       |  def bn(x: => Int) = x
       |  def u = { var v = 1; v += 1; bn(v) }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t10# => class t10 extends AnyRef { +7 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t10#`<init>`(). => primary ctor <init>()
         |_empty_/t10#bn(). => method bn(x: => Int): Int
         |  x => _empty_/t10#bn().(x)
         |  Int => scala/Int#
         |_empty_/t10#bn().(x) => param x: => Int
         |  Int => scala/Int#
         |_empty_/t10#eta(). => method eta: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t10#m(). => method m(x: Any): Int
         |  x => _empty_/t10#m().(x)
         |  Any => scala/Any#
         |  Int => scala/Int#
         |_empty_/t10#m().(x) => param x: Any
         |  Any => scala/Any#
         |_empty_/t10#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t10#t(). => method t: Int
         |  Int => scala/Int#
         |_empty_/t10#u(). => method u: Int
         |  Int => scala/Int#
         |local0 => val local i: Int
         |  Int => scala/Int#
         |local1 => val local e: Exception
         |  Exception => scala/package.Exception#
         |local2 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => scala/Serializable#
         |local3 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local8
         |  Int => scala/Int#
         |  B1 => local7
         |  x1 => local5
         |  default => local6
         |  Function1 => scala/Function1#
         |local4 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local9
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local5 => synthetic param x1: A1
         |  A1 => local8
         |local6 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local8
         |  B1 => local7
         |local7 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local8 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local9 => synthetic param x1: Int
         |  Int => scala/Int#
         |local10 => var local v: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t10# => class t10 extends AnyRef { +7 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t10#`<init>`(). => primary ctor <init>()
         |_empty_/t10#bn(). => method bn(x: => Int): Int
         |  x => _empty_/t10#bn().(x)
         |  Int => scala/Int#
         |_empty_/t10#bn().(x) => param x: => Int
         |  Int => scala/Int#
         |_empty_/t10#eta(). => method eta: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t10#m(). => method m(x: Any): Int
         |  x => _empty_/t10#m().(x)
         |  Any => scala/Any#
         |  Int => scala/Int#
         |_empty_/t10#m().(x) => param x: Any
         |  Any => scala/Any#
         |_empty_/t10#pf(). => method pf: PartialFunction[Int, Int]
         |  PartialFunction => scala/PartialFunction#
         |  Int => scala/Int#
         |_empty_/t10#t(). => method t: Int
         |  Int => scala/Int#
         |_empty_/t10#u(). => method u: Int
         |  Int => scala/Int#
         |local0 => val local i: Int
         |  Int => scala/Int#
         |local1 => val local e: Exception
         |  Exception => scala/package.Exception#
         |local2 => @SerialVersionUID synthetic final class $anonfun extends AbstractPartialFunction[Int, Int] with Serializable { +2 decls }
         |  SerialVersionUID => scala/SerialVersionUID#
         |  AbstractPartialFunction => scala/runtime/AbstractPartialFunction#
         |  Int => scala/Int#
         |  Serializable => java/io/Serializable#
         |local3 => final method applyOrElse[A1 <: Int, B1 >: Int](x1: A1, default: Function1[A1, B1]): B1 <: scala/PartialFunction#applyOrElse().
         |  A1 => local8
         |  Int => scala/Int#
         |  B1 => local7
         |  x1 => local5
         |  default => local6
         |  Function1 => scala/Function1#
         |local4 => final method isDefinedAt(x1: Int): Boolean <: scala/PartialFunction#isDefinedAt().
         |  x1 => local9
         |  Int => scala/Int#
         |  Boolean => scala/Boolean#
         |local5 => synthetic param x1: A1
         |  A1 => local8
         |local6 => synthetic param default: Function1[A1, B1]
         |  Function1 => scala/Function1#
         |  A1 => local8
         |  B1 => local7
         |local7 => typeparam B1 >: Int
         |  Int => scala/Int#
         |local8 => typeparam A1 <: Int
         |  Int => scala/Int#
         |local9 => synthetic param x1: Int
         |  Int => scala/Int#
         |local10 => var local v: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  symbols(
    "named and default arguments in a block",
    """|object t15 {
       |  def f(a: Int, b: Int = 2) = a + b
       |  def mk = t15
       |  def m = { f(b = 1 + 1, a = 2) + f(1) + mk.f(b = 3, a = 4) + mk.f(5) }
       |  def l = List(1).map(x => f(b = x + 1, a = x))
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t15. => final object t15 extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t15.f$default$2(). => synthetic method f$default$2: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t15.f(). => method f(a: Int, b: Int): Int
         |  a => _empty_/t15.f().(a)
         |  Int => scala/Int#
         |  b => _empty_/t15.f().(b)
         |_empty_/t15.f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t15.f().(b) => default param b: Int
         |  Int => scala/Int#
         |_empty_/t15.l(). => method l: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t15.m(). => method m: Int
         |  Int => scala/Int#
         |_empty_/t15.mk(). => method mk: t15.type
         |  t15 => _empty_/t15.
         |local0 => param x: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/t15. => final object t15 extends AnyRef { +5 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t15.f$default$2(). => synthetic method f$default$2: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |_empty_/t15.f(). => method f(a: Int, b: Int): Int
         |  a => _empty_/t15.f().(a)
         |  Int => scala/Int#
         |  b => _empty_/t15.f().(b)
         |_empty_/t15.f().(a) => param a: Int
         |  Int => scala/Int#
         |_empty_/t15.f().(b) => default param b: Int
         |  Int => scala/Int#
         |_empty_/t15.l(). => method l: List[Int]
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |_empty_/t15.m(). => method m: Int
         |  Int => scala/Int#
         |_empty_/t15.mk(). => method mk: t15
         |  t15 => _empty_/t15.
         |local0 => param x: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  symbols(
    "named and default arguments in constructor calls",
    """|class t16(a: Int, b: Int = 2)
       |class t17 extends t16(b = 1 + 1, a = 2) {
       |  val v = new t16(b = 3, a = 4)
       |  def m = { class c extends t16(b = 5, a = 6); new c }
       |}
       |""".stripMargin,
    """|_empty_/t16# => class t16 extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t16#`<init>`(). => primary ctor <init>(a: Int, b: Int)
       |  a => _empty_/t16#`<init>`().(a)
       |  Int => scala/Int#
       |  b => _empty_/t16#`<init>`().(b)
       |_empty_/t16#`<init>`().(a) => param a: Int
       |  Int => scala/Int#
       |_empty_/t16#`<init>`().(b) => default param b: Int
       |  Int => scala/Int#
       |_empty_/t16#a. => private[this] val method a: Int
       |  Int => scala/Int#
       |_empty_/t16#b. => private[this] val method b: Int
       |  Int => scala/Int#
       |_empty_/t16. => synthetic final object t16 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t16.`<init>$default$2`(). => synthetic method <init>$default$2: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |_empty_/t17# => class t17 extends t16 { +3 decls }
       |  t16 => _empty_/t16#
       |_empty_/t17#`<init>`(). => primary ctor <init>()
       |_empty_/t17#m(). => method m: t16
       |  t16 => _empty_/t16#
       |_empty_/t17#v. => val method v: t16
       |  t16 => _empty_/t16#
       |local0 => class c extends t16 { +1 decls }
       |  t16 => _empty_/t16#
       |local1 => primary ctor <init>()
       |""".stripMargin,
  )

  symbols(
    "named and default arguments of local definitions",
    """|object t18 {
       |  def m = {
       |    def g(i: Int = 1, j: String = "") = j
       |    class k { def h(i: Int, j: Int = 1) = i }
       |    g(j = "b" * 2) + new k().h(j = 2, i = 3)
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t18. => final object t18 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t18.m(). => method m: String
       |  String => java/lang/String#
       |local0 => method g(i: Int, j: String): String
       |  i => local1
       |  Int => scala/Int#
       |  j => local2
       |  String => scala/Predef.String#
       |local1 => default param i: Int
       |  Int => scala/Int#
       |local2 => default param j: String
       |  String => scala/Predef.String#
       |local3 => class k extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local4 => primary ctor <init>()
       |local5 => method h(i: Int, j: Int): Int
       |  i => local7
       |  Int => scala/Int#
       |  j => local8
       |local6 => synthetic method h$default$2: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |local7 => param i: Int
       |  Int => scala/Int#
       |local8 => default param j: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "default arguments of a polymorphic local method",
    """|object t25 {
       |  def m = {
       |    def f[T](t: T)(u: T = t) = u
       |    f(1)()
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t25. => final object t25 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t25.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => method f[T](t: T)(u: T): T
       |  T => local3
       |  t => local1
       |  u => local2
       |local1 => param t: T
       |  T => local3
       |local2 => default param u: T
       |  T => local3
       |local3 => typeparam T
       |""".stripMargin,
  )

  symbols(
    "catch expression and unstable qualifier",
    """|class t19 {
       |  def h: PartialFunction[Throwable, Int] = ???
       |  def t = try 1 catch h
       |  class C { type T; def f(t: T): T = t }
       |  def mk = new C { type T = Int }
       |  def r = { mk.f(1); () }
       |}
       |""".stripMargin,
    """|_empty_/t19# => class t19 extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t19#C# => class C extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t19#C#T# => abstract type T
       |_empty_/t19#C#`<init>`(). => primary ctor <init>()
       |_empty_/t19#C#f(). => method f(t: T): T
       |  t => _empty_/t19#C#f().(t)
       |  T => _empty_/t19#C#T#
       |_empty_/t19#C#f().(t) => param t: T
       |  T => _empty_/t19#C#T#
       |_empty_/t19#`<init>`(). => primary ctor <init>()
       |_empty_/t19#h(). => method h: PartialFunction[Throwable, Int]
       |  PartialFunction => scala/PartialFunction#
       |  Throwable => scala/package.Throwable#
       |  Int => scala/Int#
       |_empty_/t19#mk(). => method mk: C { type T = Int }
       |  C => _empty_/t19#C#
       |  T => local0
       |  Int => scala/Int#
       |_empty_/t19#r(). => method r: Unit
       |  Unit => scala/Unit#
       |_empty_/t19#t(). => method t: Int
       |  Int => scala/Int#
       |local0 => type T = Int <: _empty_/t19#C#T#
       |  Int => scala/Int#
       |local1 => final class $anon extends C { +1 decls }
       |  C => _empty_/t19#C#
       |""".stripMargin,
  )

  symbols(
    "default arguments of a local class constructor",
    """|object t26 {
       |  def m = {
       |    class k(i: Int = 1) { def v = i }
       |    new k().v
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t26. => final object t26 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t26.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => class k extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => default param i: Int
       |  Int => scala/Int#
       |local2 => primary ctor <init>(i: Int)
       |  i => local1
       |  Int => scala/Int#
       |local3 => method v: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "default arguments of a method of a local object",
    """|object t27 {
       |  def m = {
       |    object o { def h(i: Int = 1) = i }
       |    o.h()
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t27. => final object t27 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t27.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => final object o extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => method h(i: Int): Int
       |  i => local3
       |  Int => scala/Int#
       |local2 => synthetic method h$default$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |local3 => default param i: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "default argument in an annotation of a local value",
    """|class an(x: Int) extends scala.annotation.StaticAnnotation
       |object t28 {
       |  def m = {
       |    def f(a: Int = 1) = a
       |    @an(f()) val v = 1
       |    v
       |  }
       |}
       |""".stripMargin,
    """|_empty_/an# => class an extends Annotation with StaticAnnotation { +2 decls }
       |  Annotation => scala/annotation/Annotation#
       |  StaticAnnotation => scala/annotation/StaticAnnotation#
       |_empty_/an#`<init>`(). => primary ctor <init>(x: Int)
       |  x => _empty_/an#`<init>`().(x)
       |  Int => scala/Int#
       |_empty_/an#`<init>`().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/an#x. => private[this] val method x: Int
       |  Int => scala/Int#
       |_empty_/t28. => final object t28 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t28.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => method f(a: Int): Int
       |  a => local1
       |  Int => scala/Int#
       |local1 => default param a: Int
       |  Int => scala/Int#
       |local2 => @an val local v: Int
       |  an => _empty_/an#
       |  Int => scala/Int#
       |local3 => synthetic method f$default$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |""".stripMargin,
  )

  symbols(
    "default arguments in a lambda and a block of a class body",
    """|class t29 {
       |  val l = (x: Int) => { def g(i: Int = x) = i; g() }
       |  { def h(i: Int = 2) = i; h() }
       |}
       |""".stripMargin,
    """|_empty_/t29# => class t29 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t29#`<init>`(). => primary ctor <init>()
       |_empty_/t29#l. => val method l: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => method g(i: Int): Int
       |  i => local2
       |  Int => scala/Int#
       |local2 => default param i: Int
       |  Int => scala/Int#
       |local3 => method h(i: Int): Int
       |  i => local4
       |  Int => scala/Int#
       |local4 => default param i: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    "default argument in a type annotation of a local value",
    """|class an(x: Int) extends scala.annotation.StaticAnnotation
       |object t30 {
       |  def m = {
       |    def f(a: Int = 1) = a
       |    val v: Int @an(f()) = 1
       |    v
       |  }
       |}
       |""".stripMargin,
    """|_empty_/an# => class an extends Annotation with StaticAnnotation { +2 decls }
       |  Annotation => scala/annotation/Annotation#
       |  StaticAnnotation => scala/annotation/StaticAnnotation#
       |_empty_/an#`<init>`(). => primary ctor <init>(x: Int)
       |  x => _empty_/an#`<init>`().(x)
       |  Int => scala/Int#
       |_empty_/an#`<init>`().(x) => param x: Int
       |  Int => scala/Int#
       |_empty_/an#x. => private[this] val method x: Int
       |  Int => scala/Int#
       |_empty_/t30. => final object t30 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t30.m(). => method m: Int @an
       |  Int => scala/Int#
       |  an => _empty_/an#
       |local0 => method f(a: Int): Int
       |  a => local2
       |  Int => scala/Int#
       |local1 => synthetic method f$default$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |local2 => default param a: Int
       |  Int => scala/Int#
       |local3 => val local v: Int @an
       |  Int => scala/Int#
       |  an => _empty_/an#
       |""".stripMargin,
  )

  symbols(
    "local case class created with new",
    """|object t32 {
       |  def m: Any = {
       |    case class k(i: Int)
       |    new k(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t32. => final object t32 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t32.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => primary ctor <init>(val i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local3 => private synthetic method copy(i: Int): k
         |  i => local13
         |  Int => scala/Int#
         |  k => local0
         |local4 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local14
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local15
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local11 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local12 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local13 => default param i: Int
         |  Int => scala/Int#
         |local14 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin
    else
      """|_empty_/t32. => final object t32 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t32.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => primary ctor <init>(val i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local3 => private synthetic method copy(i: Int): k
         |  i => local14
         |  Int => scala/Int#
         |  k => local0
         |local4 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local15
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local17
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => default param i: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local17 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin,
  )

  symbols(
    "default argument of a local case class",
    """|object t33 {
       |  def m: Any = {
       |    case class k(i: Int = 1)
       |    k()
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t33. => final object t33 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t33.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val default param i: Int
         |  Int => scala/Int#
         |local2 => primary ctor <init>(val i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local3 => synthetic method copy(i: Int): k
         |  i => local13
         |  Int => scala/Int#
         |  k => local0
         |local4 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local14
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local15
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local11 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local12 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local13 => default param i: Int
         |  Int => scala/Int#
         |local14 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local17 => synthetic method apply(i: Int): k <: scala/Function1#apply().
         |  i => local19
         |  Int => scala/Int#
         |  k => local0
         |local18 => synthetic final object k extends AbstractFunction1[Int, k] with Serializable { +5 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  Int => scala/Int#
         |  k => local0
         |  Serializable => scala/Serializable#
         |local19 => default param i: Int
         |  Int => scala/Int#
         |local20 => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local21 => synthetic method apply$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local22 => synthetic method unapply(x$0: k): Option[Int]
         |  x$0 => local24
         |  k => local0
         |  Option => scala/Option#
         |  Int => scala/Int#
         |local23 => synthetic method <init>$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local24 => synthetic param x$0: k
         |  k => local0
         |""".stripMargin
    else
      """|_empty_/t33. => final object t33 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t33.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val default param i: Int
         |  Int => scala/Int#
         |local2 => primary ctor <init>(val i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local3 => synthetic method copy(i: Int): k
         |  i => local14
         |  Int => scala/Int#
         |  k => local0
         |local4 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local15
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local17
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => default param i: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local17 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local19 => synthetic method apply(i: Int): k <: scala/Function1#apply().
         |  i => local21
         |  Int => scala/Int#
         |  k => local0
         |local20 => synthetic final object k extends AbstractFunction1[Int, k] with Serializable { +5 decls }
         |  AbstractFunction1 => scala/runtime/AbstractFunction1#
         |  Int => scala/Int#
         |  k => local0
         |  Serializable => java/io/Serializable#
         |local21 => default param i: Int
         |  Int => scala/Int#
         |local22 => synthetic final method toString(): String <: scala/Function1#toString()., java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local23 => synthetic method apply$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local24 => synthetic method unapply(x$0: k): Option[Int]
         |  x$0 => local26
         |  k => local0
         |  Option => scala/Option#
         |  Int => scala/Int#
         |local25 => synthetic method <init>$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local26 => synthetic param x$0: k
         |  k => local0
         |""".stripMargin,
  )

  symbols(
    "default arguments of a local class with a companion",
    """|object t34 {
       |  def m = {
       |    class k(i: Int = 1) { def v = i }
       |    object k { def z = 3 }
       |    new k().v + k.z
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t34. => final object t34 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t34.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => class k extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => default param i: Int
       |  Int => scala/Int#
       |local2 => primary ctor <init>(i: Int)
       |  i => local1
       |  Int => scala/Int#
       |local3 => method v: Int
       |  Int => scala/Int#
       |local4 => final object k extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local5 => method z: Int
       |  Int => scala/Int#
       |local6 => synthetic method <init>$default$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |""".stripMargin,
  )

  symbols(
    "default arguments of a class in a local object",
    """|object t35 {
       |  def m = {
       |    object o { class n(x: Int = 2) }
       |    new o.n().hashCode
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t35. => final object t35 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t35.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => final object o extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => class n extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local2 => synthetic final object n extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |local3 => default param x: Int
       |  Int => scala/Int#
       |local4 => primary ctor <init>(x: Int)
       |  x => local3
       |  Int => scala/Int#
       |local5 => synthetic method <init>$default$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |""".stripMargin,
  )

  symbols(
    "default arguments of a method of a local trait",
    """|object t36 {
       |  def m = {
       |    trait t { def h(i: Int = 1) = i }
       |    new t {}.h()
       |  }
       |}
       |""".stripMargin,
    """|_empty_/t36. => final object t36 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/t36.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => trait t extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => primary ctor <init>()
       |local2 => method h(i: Int): Int
       |  i => local4
       |  Int => scala/Int#
       |local3 => synthetic method h$default$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |local4 => default param i: Int
       |  Int => scala/Int#
       |local5 => final class $anon extends AnyRef with t
       |  AnyRef => scala/AnyRef#
       |  t => local0
       |""".stripMargin,
  )

  symbols(
    "local polymorphic case class created with new",
    """|object t37 {
       |  def m: Any = {
       |    case class k[T](t: T)
       |    new k(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t37. => final object t37 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t37.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k[T] extends AnyRef with Product with Serializable { +12 decls }
         |  T => local13
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val param t: T
         |  T => local13
         |local2 => primary ctor <init>(val t: T)
         |  t => local1
         |  T => local13
         |local3 => private synthetic method copy[T](t: T): k[T]
         |  T => local13
         |  t => local14
         |  k => local0
         |local4 => private synthetic method copy$default$1[T]: T @uncheckedVariance
         |  T => local13
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local15
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local11 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local12 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local17
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local13 => typeparam T
         |local14 => default param t: T
         |  T => local13
         |local15 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local17 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin
    else
      """|_empty_/t37. => final object t37 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t37.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k[T] extends AnyRef with Product with Serializable { +13 decls }
         |  T => local14
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val param t: T
         |  T => local14
         |local2 => primary ctor <init>(val t: T)
         |  t => local1
         |  T => local14
         |local3 => private synthetic method copy[T](t: T): k[T]
         |  T => local14
         |  t => local15
         |  k => local0
         |local4 => private synthetic method copy$default$1[T]: T @uncheckedVariance
         |  T => local14
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local16
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local17
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local18
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local19
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => typeparam T
         |local15 => default param t: T
         |  T => local14
         |local16 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local17 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local18 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local19 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin,
  )

  symbols(
    "local case class with a default created with new",
    """|object t39 {
       |  def m: Any = {
       |    case class k(i: Int = 1)
       |    new k(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t39. => final object t39 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t39.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val default param i: Int
         |  Int => scala/Int#
         |local2 => primary ctor <init>(val i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local3 => private synthetic method copy(i: Int): k
         |  i => local13
         |  Int => scala/Int#
         |  k => local0
         |local4 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local14
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local15
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local11 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local12 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local13 => default param i: Int
         |  Int => scala/Int#
         |local14 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin
    else
      """|_empty_/t39. => final object t39 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t39.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val default param i: Int
         |  Int => scala/Int#
         |local2 => primary ctor <init>(val i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local3 => private synthetic method copy(i: Int): k
         |  i => local14
         |  Int => scala/Int#
         |  k => local0
         |local4 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local15
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local17
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => default param i: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local17 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin,
  )

  symbols(
    "local case class with two parameters created with new",
    """|object t40 {
       |  def m: Any = {
       |    case class k(i: Int, j: Int)
       |    new k(1, 2)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t40. => final object t40 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t40.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +14 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => val param j: Int
         |  Int => scala/Int#
         |local3 => primary ctor <init>(val i: Int, val j: Int)
         |  i => local1
         |  Int => scala/Int#
         |  j => local2
         |local4 => private synthetic method copy(i: Int, j: Int): k
         |  i => local15
         |  Int => scala/Int#
         |  j => local16
         |  k => local0
         |local5 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local6 => private synthetic method copy$default$2: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local7 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local8 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local9 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local17
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local10 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local11 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local12 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local13 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local14 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local19
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local15 => default param i: Int
         |  Int => scala/Int#
         |local16 => default param j: Int
         |  Int => scala/Int#
         |local17 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local19 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin
    else
      """|_empty_/t40. => final object t40 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t40.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +15 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => val param j: Int
         |  Int => scala/Int#
         |local3 => primary ctor <init>(val i: Int, val j: Int)
         |  i => local1
         |  Int => scala/Int#
         |  j => local2
         |local4 => private synthetic method copy(i: Int, j: Int): k
         |  i => local16
         |  Int => scala/Int#
         |  j => local17
         |  k => local0
         |local5 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local6 => private synthetic method copy$default$2: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local7 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local8 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local9 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local18
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local10 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local11 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local19
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local12 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local20
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local13 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local14 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local15 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local21
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local16 => default param i: Int
         |  Int => scala/Int#
         |local17 => default param j: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local19 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local20 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local21 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin,
  )

  symbols(
    "local case class with a var parameter created with new",
    """|object t41 {
       |  def m: Any = {
       |    case class k(var i: Int)
       |    new k(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t41. => final object t41 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t41.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => var param i: Int
         |  Int => scala/Int#
         |local2 => private var method i_=(x$1: Int): Unit
         |  x$1 => local14
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |local3 => primary ctor <init>(var i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local4 => private synthetic method copy(i: Int): k
         |  i => local15
         |  Int => scala/Int#
         |  k => local0
         |local5 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local6 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local7 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local8 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local16
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local9 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local10 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local17
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local15 => default param i: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local17 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin
    else
      """|_empty_/t41. => final object t41 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t41.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +14 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => var param i: Int
         |  Int => scala/Int#
         |local2 => private var method i_=(x$1: Int): Unit
         |  x$1 => local15
         |  Int => scala/Int#
         |  Unit => scala/Unit#
         |local3 => primary ctor <init>(var i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local4 => private synthetic method copy(i: Int): k
         |  i => local16
         |  Int => scala/Int#
         |  k => local0
         |local5 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
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
         |local11 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local19
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local12 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local13 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local14 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local20
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local15 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local16 => default param i: Int
         |  Int => scala/Int#
         |local17 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local19 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local20 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin,
  )

  symbols(
    "local case class with an implicit parameter created with new",
    """|object t42 {
       |  def m: Any = {
       |    case class k(i: Int)(implicit s: String)
       |    implicit val s: String = ""
       |    new k(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t42. => final object t42 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t42.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => implicit param s: String
         |  String => scala/Predef.String#
         |local3 => primary ctor <init>(val i: Int)(implicit s: String)
         |  i => local1
         |  Int => scala/Int#
         |  s => local2
         |  String => scala/Predef.String#
         |local4 => synthetic method copy(i: Int)(implicit s: String): k
         |  i => local14
         |  Int => scala/Int#
         |  s => local15
         |  String => scala/Predef.String#
         |  k => local0
         |local5 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local6 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local7 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local8 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local16
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local9 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local10 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local17
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => default param i: Int
         |  Int => scala/Int#
         |local15 => implicit param s: String
         |  String => scala/Predef.String#
         |local16 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local17 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local19 => implicit val local s: String
         |  String => scala/Predef.String#
         |""".stripMargin
    else
      """|_empty_/t42. => final object t42 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t42.m(). => method m: Any
         |  Any => scala/Any#
         |local0 => case class k extends AnyRef with Product with Serializable { +14 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => implicit param s: String
         |  String => scala/Predef.String#
         |local3 => primary ctor <init>(val i: Int)(implicit s: String)
         |  i => local1
         |  Int => scala/Int#
         |  s => local2
         |  String => scala/Predef.String#
         |local4 => synthetic method copy(i: Int)(implicit s: String): k
         |  i => local15
         |  Int => scala/Int#
         |  s => local16
         |  String => scala/Predef.String#
         |  k => local0
         |local5 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
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
         |local11 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local19
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local12 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local13 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local14 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local20
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local15 => default param i: Int
         |  Int => scala/Int#
         |local16 => implicit param s: String
         |  String => scala/Predef.String#
         |local17 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local19 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local20 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local21 => implicit val local s: String
         |  String => scala/Predef.String#
         |""".stripMargin,
  )

  symbols(
    "local case class in a lambda created with new",
    """|object t43 {
       |  def m: Int => Any = { x =>
       |    case class k(i: Int)
       |    new k(x)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t43. => final object t43 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t43.m(). => method m: Function1[Int, Any]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local0 => param x: Int
         |  Int => scala/Int#
         |local1 => case class k extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local2 => val param i: Int
         |  Int => scala/Int#
         |local3 => primary ctor <init>(val i: Int)
         |  i => local2
         |  Int => scala/Int#
         |local4 => private synthetic method copy(i: Int): k
         |  i => local14
         |  Int => scala/Int#
         |  k => local1
         |local5 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local6 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local7 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local8 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local15
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local9 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local10 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local17
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => default param i: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local17 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin
    else
      """|_empty_/t43. => final object t43 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t43.m(). => method m: Function1[Int, Any]
         |  Function1 => scala/Function1#
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local0 => param x: Int
         |  Int => scala/Int#
         |local1 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local2 => val param i: Int
         |  Int => scala/Int#
         |local3 => primary ctor <init>(val i: Int)
         |  i => local2
         |  Int => scala/Int#
         |local4 => private synthetic method copy(i: Int): k
         |  i => local15
         |  Int => scala/Int#
         |  k => local1
         |local5 => private synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local6 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local7 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local8 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local16
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local9 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local10 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local17
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local11 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local18
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local12 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local13 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local14 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local19
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local15 => default param i: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local17 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local18 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local19 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin,
  )

  symbols(
    "local case class in a block in a class body created with new",
    """|class t44 {
       |  {
       |    case class k(i: Int)
       |    new k(1)
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/t44# => class t44 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t44#`<init>`(). => primary ctor <init>()
         |local0 => case class k extends AnyRef with Product with Serializable { +12 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => primary ctor <init>(val i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local3 => synthetic method copy(i: Int): k
         |  i => local13
         |  Int => scala/Int#
         |  k => local0
         |local4 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local14
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local15
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local11 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local12 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local13 => default param i: Int
         |  Int => scala/Int#
         |local14 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin
    else
      """|_empty_/t44# => class t44 extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/t44#`<init>`(). => primary ctor <init>()
         |local0 => case class k extends AnyRef with Product with Serializable { +13 decls }
         |  AnyRef => scala/AnyRef#
         |  Product => scala/Product#
         |  Serializable => scala/package.Serializable#
         |local1 => val param i: Int
         |  Int => scala/Int#
         |local2 => primary ctor <init>(val i: Int)
         |  i => local1
         |  Int => scala/Int#
         |local3 => synthetic method copy(i: Int): k
         |  i => local14
         |  Int => scala/Int#
         |  k => local0
         |local4 => synthetic method copy$default$1: Int @uncheckedVariance
         |  Int => scala/Int#
         |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
         |local5 => synthetic method productPrefix: String <: scala/Product#productPrefix().
         |  String => java/lang/String#
         |local6 => synthetic method productArity: Int <: scala/Product#productArity().
         |  Int => scala/Int#
         |local7 => synthetic method productElement(x$1: Int): Any <: scala/Product#productElement().
         |  x$1 => local15
         |  Int => scala/Int#
         |  Any => scala/Any#
         |local8 => synthetic method productIterator: Iterator[Any] <: scala/Product#productIterator().
         |  Iterator => scala/collection/Iterator#
         |  Any => scala/Any#
         |local9 => synthetic method canEqual(x$1: Any): Boolean <: scala/Equals#canEqual().
         |  x$1 => local16
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local10 => synthetic method productElementName(x$1: Int): String <: scala/Product#productElementName().
         |  x$1 => local17
         |  Int => scala/Int#
         |  String => java/lang/String#
         |local11 => synthetic method hashCode(): Int <: java/lang/Object#hashCode()., scala/Any#hashCode().
         |  Int => scala/Int#
         |local12 => synthetic method toString(): String <: java/lang/Object#toString()., scala/Any#toString().
         |  String => java/lang/String#
         |local13 => synthetic method equals(x$1: Any): Boolean <: scala/Equals#equals()., java/lang/Object#equals()., scala/Any#equals().
         |  x$1 => local18
         |  Any => scala/Any#
         |  Boolean => scala/Boolean#
         |local14 => default param i: Int
         |  Int => scala/Int#
         |local15 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local16 => synthetic param x$1: Any
         |  Any => scala/Any#
         |local17 => synthetic param x$1: Int
         |  Int => scala/Int#
         |local18 => synthetic param x$1: Any
         |  Any => scala/Any#
         |""".stripMargin,
  )

  symbols(
    "early definitions",
    """|trait B
       |class A extends { val a = 1; val (b, c) = (2, 3) } with B
       |class C {
       |  def m = {
       |    class L extends { val d = 4; val (e, f) = (5, 6) } with B
       |    new L().d + new L().e
       |  }
       |}
       |""".stripMargin,
    """|_empty_/A# => class A extends AnyRef with B { +5 decls }
       |  AnyRef => scala/AnyRef#
       |  B => _empty_/B#
       |_empty_/A#`<init>`(). => primary ctor <init>()
       |_empty_/A#a. => val method a: Int
       |  Int => scala/Int#
       |_empty_/A#b. => val method b: Int
       |  Int => scala/Int#
       |_empty_/A#c. => val method c: Int
       |  Int => scala/Int#
       |_empty_/A#x$1. => private[this] synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/B# => trait B extends AnyRef
       |  AnyRef => scala/AnyRef#
       |_empty_/C# => class C extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/C#`<init>`(). => primary ctor <init>()
       |_empty_/C#m(). => method m: Int
       |  Int => scala/Int#
       |local0 => class L extends AnyRef with B { +5 decls }
       |  AnyRef => scala/AnyRef#
       |  B => _empty_/B#
       |local1 => val method d: Int
       |  Int => scala/Int#
       |local2 => private[this] synthetic val method x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local3 => val method e: Int
       |  Int => scala/Int#
       |local4 => val method f: Int
       |  Int => scala/Int#
       |local5 => primary ctor <init>()
       |""".stripMargin,
  )

  occurrences(
    "early definitions",
    """|trait B
       |class A extends { val a = 1; val (b, c) = (2, 3) } with B
       |class C {
       |  def m = {
       |    class L extends { val d = 4; val (e, f) = (5, 6) } with B
       |    new L().d + new L().e
       |  }
       |}
       |""".stripMargin,
    """|[0:6..0:7): B <= _empty_/B#
       |[1:6..1:7): A <= _empty_/A#
       |[1:8..1:8):  <= _empty_/A#`<init>`().
       |[1:22..1:23): a <= _empty_/A#a.
       |[1:34..1:35): b <= _empty_/A#b.
       |[1:37..1:38): c <= _empty_/A#c.
       |[1:56..1:57): B => _empty_/B#
       |[1:57..1:57):  => java/lang/Object#`<init>`().
       |[2:6..2:7): C <= _empty_/C#
       |[2:8..2:8):  <= _empty_/C#`<init>`().
       |[3:6..3:7): m <= _empty_/C#m().
       |[4:10..4:11): L <= local0
       |[4:12..4:12):  <= local5
       |[4:26..4:27): d <= local1
       |[4:38..4:39): e <= local3
       |[4:41..4:42): f <= local4
       |[4:60..4:61): B => _empty_/B#
       |[4:61..4:61):  => java/lang/Object#`<init>`().
       |[5:8..5:9): L => local0
       |[5:9..5:9):  => local5
       |[5:12..5:13): d => local1
       |[5:14..5:15): + => scala/Int#`+`(+4).
       |[5:20..5:21): L => local0
       |[5:21..5:21):  => local5
       |[5:24..5:25): e => local3
       |""".stripMargin,
  )

}
