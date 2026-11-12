package scala.meta.tests.semanticdb

// NOTE(olafur): when this test suite is merged with TargetedSuite you get the following warning:
//   package ap  @org.scalameta.data.data class A(a: In... *** FAILED ***
//   <input>:3:2: error macro implementation not found: macroTransform
//   (the most common reason for that is that you cannot use macro implementations in the same compilation run that defines them)
//   @org.scalameta.data.data
//    ^ (SemanticdbSuite.scala:70)
// I am unable to reproduce the error when running this test individually with `-- -z "package ap"`
// or by moving the test to a separate suite.
class MacroAnnotationSuite extends SemanticdbSuite {

  occurrences(
    "#1789: class and companion",
    """|package a
       |@example.MacroAnnotation
       |class A
       |object A
       |""".stripMargin,
    """|[0:8..0:9): a <= a/
       |[1:1..1:8): example => example/
       |[1:9..1:24): MacroAnnotation => example/MacroAnnotation#
       |[2:6..2:7): A <= a/A#
       |[2:7..2:7):  <= a/A#`<init>`().
       |[3:7..3:8): A <= a/A.
       |""".stripMargin,
  )

  occurrences(
    "#1789: imported annotation with empty arguments",
    """|package b
       |import example.MacroAnnotation
       |@MacroAnnotation() class B[T](x: T)
       |object B
       |""".stripMargin,
    """|[0:8..0:9): b <= b/
       |[1:7..1:14): example => example/
       |[1:15..1:30): MacroAnnotation => example/MacroAnnotation#
       |[1:15..1:30): MacroAnnotation => example/MacroAnnotation.
       |[2:1..2:16): MacroAnnotation => example/MacroAnnotation#
       |[2:25..2:26): B <= b/B#
       |[2:27..2:28): T <= b/B#[T]
       |[2:29..2:29):  <= b/B#`<init>`().
       |[2:30..2:31): x <= b/B#`<init>`().(x)
       |[2:30..2:31): x <= b/B#x.
       |[2:33..2:34): T => b/B#[T]
       |[3:7..3:8): B <= b/B.
       |""".stripMargin,
  )

  occurrences(
    "#1789: @data class",
    """|package c
       |@org.scalameta.data.data
       |class C(a: Int)
       |""".stripMargin,
    """|[0:8..0:9): c <= c/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:19): data => org/scalameta/data/
       |[1:20..1:24): data => org/scalameta/data/data#
       |[2:6..2:7): C <= c/C#
       |[2:7..2:7):  <= c/C#`<init>`().
       |[2:11..2:14): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "#1789: after another annotation",
    """|package d
       |@deprecated @example.MacroAnnotation class D
       |object D
       |""".stripMargin,
    """|[0:8..0:9): d <= d/
       |[1:1..1:11): deprecated => scala/deprecated#
       |[1:11..1:11):  => scala/deprecated#`<init>`().
       |[1:13..1:20): example => example/
       |[1:21..1:36): MacroAnnotation => example/MacroAnnotation#
       |[1:43..1:44): D <= d/D#
       |[1:44..1:44):  <= d/D#`<init>`().
       |[2:7..2:8): D <= d/D.
       |""".stripMargin,
  )

  occurrences(
    "#1789: an annotation that is not a macro",
    """|package e
       |@deprecated class E
       |object E
       |""".stripMargin,
    """|[0:8..0:9): e <= e/
       |[1:1..1:11): deprecated => scala/deprecated#
       |[1:11..1:11):  => scala/deprecated#`<init>`().
       |[1:18..1:19): E <= e/E#
       |[1:19..1:19):  <= e/E#`<init>`().
       |[2:7..2:8): E <= e/E.
       |""".stripMargin,
  )

  occurrences(
    "#1789: member class and companion",
    """|package f
       |object F {
       |  @example.MacroAnnotation class G
       |  object G
       |  val g = new G().classNumber + G.objectNumber
       |}
       |""".stripMargin,
    """|[0:8..0:9): f <= f/
       |[1:7..1:8): F <= f/F.
       |[2:3..2:10): example => example/
       |[2:11..2:26): MacroAnnotation => example/MacroAnnotation#
       |[2:33..2:34): G <= f/F.G#
       |[2:34..2:34):  <= f/F.G#`<init>`().
       |[3:9..3:10): G <= f/F.G.
       |[4:6..4:7): g <= f/F.g.
       |[4:14..4:15): G => f/F.G#
       |[4:15..4:15):  => f/F.G#`<init>`().
       |[4:18..4:29): classNumber => f/F.G#classNumber().
       |[4:30..4:31): + => scala/Int#`+`(+4).
       |[4:32..4:33): G => f/F.G.
       |[4:34..4:46): objectNumber => f/F.G.objectNumber().
       |""".stripMargin,
  )

  occurrences(
    "#1789: @root trait",
    """|package s
       |@org.scalameta.adt.root trait R
       |""".stripMargin,
    """|[0:8..0:9): s <= s/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:18): adt => org/scalameta/adt/
       |[1:19..1:23): root => org/scalameta/adt/root#
       |[1:30..1:31): R <= s/R#
       |""".stripMargin,
  )

  occurrences(
    "#1815: class and companion",
    """|package a
       |@example.MacroAnnotation
       |class A
       |object A
       |""".stripMargin,
    """|[0:8..0:9): a <= a/
       |[1:1..1:8): example => example/
       |[1:9..1:24): MacroAnnotation => example/MacroAnnotation#
       |[2:6..2:7): A <= a/A#
       |[2:7..2:7):  <= a/A#`<init>`().
       |[3:7..3:8): A <= a/A.
       |""".stripMargin,
  )

  occurrences(
    "#1815: _root_ prefix",
    """|package b
       |@_root_.example.MacroAnnotation class B
       |object B
       |""".stripMargin,
    """|[0:8..0:9): b <= b/
       |[1:1..1:7): _root_ => _root_/
       |[1:8..1:15): example => example/
       |[1:16..1:31): MacroAnnotation => example/MacroAnnotation#
       |[1:38..1:39): B <= b/B#
       |[1:39..1:39):  <= b/B#`<init>`().
       |[2:7..2:8): B <= b/B.
       |""".stripMargin,
  )

  occurrences(
    "#1815: quasiquote with a type argument",
    """|package scala.meta
       |package c
       |object C {
       |  @scala.meta.internal.trees.quasiquote[Stat]("q")
       |  implicit class XtensionQuasiquoteC(ctx: StringContext)
       |}
       |""".stripMargin,
    """|[0:8..0:13): scala => scala/
       |[0:14..0:18): meta => scala/meta/
       |[1:8..1:9): c <= scala/meta/c/
       |[2:7..2:8): C <= scala/meta/c/C.
       |[3:3..3:8): scala => scala/
       |[3:9..3:13): meta => scala/meta/
       |[3:14..3:22): internal => scala/meta/internal/
       |[3:23..3:28): trees => scala/meta/internal/trees/
       |[3:29..3:39): quasiquote => scala/meta/internal/trees/quasiquote#
       |[3:40..3:44): Stat => scala/meta/Stat#
       |[4:17..4:36): XtensionQuasiquoteC <= scala/meta/c/C.XtensionQuasiquoteC#
       |[4:36..4:36):  <= scala/meta/c/C.XtensionQuasiquoteC#`<init>`().
       |[4:37..4:40): ctx <= scala/meta/c/C.XtensionQuasiquoteC#`<init>`().(ctx)
       |[4:37..4:40): ctx <= scala/meta/c/C.XtensionQuasiquoteC#ctx.
       |[4:42..4:55): StringContext => scala/StringContext#
       |""".stripMargin,
  )

  occurrences(
    "#1815: before another annotation",
    """|package d
       |@example.MacroAnnotation() @deprecated class D
       |object D
       |""".stripMargin,
    """|[0:8..0:9): d <= d/
       |[1:1..1:8): example => example/
       |[1:9..1:24): MacroAnnotation => example/MacroAnnotation#
       |[1:28..1:38): deprecated => scala/deprecated#
       |[1:38..1:38):  => scala/deprecated#`<init>`().
       |[1:45..1:46): D <= d/D#
       |[1:46..1:46):  <= d/D#`<init>`().
       |[2:7..2:8): D <= d/D.
       |""".stripMargin,
  )

  occurrences(
    "#1815: imported annotation",
    """|package e
       |import example.MacroAnnotation
       |@MacroAnnotation class E
       |object E
       |""".stripMargin,
    """|[0:8..0:9): e <= e/
       |[1:7..1:14): example => example/
       |[1:15..1:30): MacroAnnotation => example/MacroAnnotation#
       |[1:15..1:30): MacroAnnotation => example/MacroAnnotation.
       |[2:1..2:16): MacroAnnotation => example/MacroAnnotation#
       |[2:23..2:24): E <= e/E#
       |[2:24..2:24):  <= e/E#`<init>`().
       |[3:7..3:8): E <= e/E.
       |""".stripMargin,
  )

  occurrences(
    "#1815: @data from an enclosing package",
    """|package org.scalameta
       |package f
       |@data.data class F(a: Int)
       |""".stripMargin,
    """|[0:8..0:11): org => org/
       |[0:12..0:21): scalameta => org/scalameta/
       |[1:8..1:9): f <= org/scalameta/f/
       |[2:1..2:5): data => org/scalameta/data/
       |[2:6..2:10): data => org/scalameta/data/data#
       |[2:17..2:18): F <= org/scalameta/f/F#
       |[2:18..2:18):  <= org/scalameta/f/F#`<init>`().
       |[2:22..2:25): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "#1815: @data from an imported package",
    """|package g
       |import org.scalameta.data
       |@data.data class G(a: Int)
       |""".stripMargin,
    """|[0:8..0:9): g <= g/
       |[1:7..1:10): org => org/
       |[1:11..1:20): scalameta => org/scalameta/
       |[1:21..1:25): data => org/scalameta/data/
       |[2:1..2:5): data => org/scalameta/data/
       |[2:6..2:10): data => org/scalameta/data/data#
       |[2:17..2:18): G <= g/G#
       |[2:18..2:18):  <= g/G#`<init>`().
       |[2:22..2:25): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    "#1815: prefix shadowed by an object",
    """|package h
       |object example
       |@example.MacroAnnotation class H
       |object H
       |""".stripMargin,
    """|[0:8..0:9): h <= h/
       |[1:7..1:14): example <= h/example.
       |[2:31..2:32): H <= h/H#
       |[2:32..2:32):  <= h/H#`<init>`().
       |[3:7..3:8): H <= h/H.
       |""".stripMargin,
  )

  occurrences(
    "#1815: prefix shadowed by a member object",
    """|package i
       |object I {
       |  object example
       |  @example.MacroAnnotation class J
       |  object J
       |}
       |""".stripMargin,
    """|[0:8..0:9): i <= i/
       |[1:7..1:8): I <= i/I.
       |[2:9..2:16): example <= i/I.example.
       |[3:33..3:34): J <= i/I.J#
       |[3:34..3:34):  <= i/I.J#`<init>`().
       |[4:9..4:10): J <= i/I.J.
       |""".stripMargin,
  )

  occurrences(
    "#1815: prefix shadowed by a wildcard import",
    """|package j
       |object Ex {
       |  object example
       |}
       |import Ex._
       |@example.MacroAnnotation class J
       |object J
       |""".stripMargin,
    """|[0:8..0:9): j <= j/
       |[1:7..1:9): Ex <= j/Ex.
       |[2:9..2:16): example <= j/Ex.example.
       |[4:7..4:9): Ex => j/Ex.
       |[5:31..5:32): J <= j/J#
       |[5:32..5:32):  <= j/J#`<init>`().
       |[6:7..6:8): J <= j/J.
       |""".stripMargin,
  )

  occurrences(
    "#1815: prefix shadowed by an import",
    """|package k
       |object Ex {
       |  object example
       |}
       |import Ex.example
       |@example.MacroAnnotation class K
       |object K
       |""".stripMargin,
    """|[0:8..0:9): k <= k/
       |[1:7..1:9): Ex <= k/Ex.
       |[2:9..2:16): example <= k/Ex.example.
       |[4:7..4:9): Ex => k/Ex.
       |[4:10..4:17): example => k/Ex.example.
       |[5:31..5:32): K <= k/K#
       |[5:32..5:32):  <= k/K#`<init>`().
       |[6:7..6:8): K <= k/K.
       |""".stripMargin,
  )

  occurrences(
    "#1815: local class and companion",
    """|package l
       |object L {
       |  def f = {
       |    @example.MacroAnnotation class M
       |    object M
       |    new M
       |  }
       |}
       |""".stripMargin,
    """|[0:8..0:9): l <= l/
       |[1:7..1:8): L <= l/L.
       |[2:6..2:7): f <= l/L.f().
       |[3:5..3:12): example => example/
       |[3:13..3:28): MacroAnnotation => example/MacroAnnotation#
       |[3:35..3:36): M <= local1
       |[3:36..3:36):  <= local2
       |[4:11..4:12): M <= local3
       |[5:8..5:9): M => local1
       |[5:9..5:9):  => local2
       |""".stripMargin,
  )

  occurrences(
    "#1815: annotations that are not macros",
    """|package n
       |@_root_.scala.deprecated class N
       |@scala.deprecated class O
       |object P
       |""".stripMargin,
    """|[0:8..0:9): n <= n/
       |[1:1..1:7): _root_ => _root_/
       |[1:8..1:13): scala => scala/
       |[1:14..1:24): deprecated => scala/deprecated#
       |[1:24..1:24):  => scala/deprecated#`<init>`().
       |[1:31..1:32): N <= n/N#
       |[1:32..1:32):  <= n/N#`<init>`().
       |[2:1..2:6): scala => scala/
       |[2:7..2:17): deprecated => scala/deprecated#
       |[2:17..2:17):  => scala/deprecated#`<init>`().
       |[2:24..2:25): O <= n/O#
       |[2:25..2:25):  <= n/O#`<init>`().
       |[3:7..3:8): P <= n/P.
       |""".stripMargin,
  )

  // an unqualified name from the enclosing package, without a term of that name
  occurrences(
    "#1815: unqualified annotation",
    """|package org.scalameta.adt
       |@root trait R
       |""".stripMargin,
    """|[0:8..0:11): org => org/
       |[0:12..0:21): scalameta => org/scalameta/
       |[0:22..0:25): adt => org/scalameta/adt/
       |[1:1..1:5): root => org/scalameta/adt/root#
       |[1:12..1:13): R <= org/scalameta/adt/R#
       |""".stripMargin,
  )

  // a renamed import
  occurrences(
    "#1815: renamed import",
    """|package p
       |import example.{MacroAnnotation => MA}
       |@MA class P
       |object P
       |""".stripMargin,
    """|[0:8..0:9): p <= p/
       |[1:7..1:14): example => example/
       |[1:16..1:31): MacroAnnotation => example/MacroAnnotation#
       |[1:16..1:31): MacroAnnotation => example/MacroAnnotation.
       |[2:1..2:3): MA => example/MacroAnnotation#
       |[2:10..2:11): P <= p/P#
       |[2:11..2:11):  <= p/P#`<init>`().
       |[3:7..3:8): P <= p/P.
       |""".stripMargin,
  )

  // a wildcard import
  occurrences(
    "#1815: wildcard import",
    """|package q
       |import example._
       |@MacroAnnotation class Q
       |object Q
       |""".stripMargin,
    """|[0:8..0:9): q <= q/
       |[1:7..1:14): example => example/
       |[2:1..2:16): MacroAnnotation => example/MacroAnnotation#
       |[2:23..2:24): Q <= q/Q#
       |[2:24..2:24):  <= q/Q#`<init>`().
       |[3:7..3:8): Q <= q/Q.
       |""".stripMargin,
  )

  // an import in an enclosing object
  occurrences(
    "#1815: import in an enclosing object",
    """|package r
       |object R {
       |  import example.MacroAnnotation
       |  @MacroAnnotation class S
       |  object S
       |}
       |""".stripMargin,
    """|[0:8..0:9): r <= r/
       |[1:7..1:8): R <= r/R.
       |[2:9..2:16): example => example/
       |[2:17..2:32): MacroAnnotation => example/MacroAnnotation#
       |[2:17..2:32): MacroAnnotation => example/MacroAnnotation.
       |[3:3..3:18): MacroAnnotation => example/MacroAnnotation#
       |[3:25..3:26): S <= r/R.S#
       |[3:26..3:26):  <= r/R.S#`<init>`().
       |[4:9..4:10): S <= r/R.S.
       |""".stripMargin,
  )

  targeted(
    """|package a
       |
       |@org.scalameta.data.data
       |class A(a: Int)
       |""".stripMargin,
    { doc =>
      val symbols = doc.symbols.map(_.symbol).sorted
      assert(symbols.contains("a/A#"))
      assert(symbols.contains("a/A#productElement()."))
      assert(symbols.contains("a/A.unapply()."))
      assert(symbols.contains("a/A.apply()."))
    },
  )

  occurrences(
    """|package ma1
       |@org.scalameta.data.data
       |class A(a: Int)
       |object o { val x = A(1); x.a; val A(y) = x }
       |""".stripMargin,
    """|[0:8..0:11): ma1 <= ma1/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:19): data => org/scalameta/data/
       |[1:20..1:24): data => org/scalameta/data/data#
       |[2:6..2:7): A <= ma1/A#
       |[2:7..2:7):  <= ma1/A#`<init>`().
       |[2:11..2:14): Int => scala/Int#
       |[3:7..3:8): o <= ma1/o.
       |[3:15..3:16): x <= ma1/o.x.
       |[3:19..3:20): A => ma1/A.
       |[3:25..3:26): x => ma1/o.x.
       |[3:27..3:28): a => ma1/A#a.
       |[3:34..3:35): A => ma1/A.
       |[3:36..3:37): y <= ma1/o.y.
       |[3:41..3:42): x => ma1/o.x.
       |""".stripMargin,
  )

}
