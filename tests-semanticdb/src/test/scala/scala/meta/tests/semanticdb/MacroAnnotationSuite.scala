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
       |[2:1..2:8): example => example/
       |[2:9..2:24): MacroAnnotation => example/MacroAnnotation#
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
       |[3:3..3:10): example => example/
       |[3:11..3:26): MacroAnnotation => example/MacroAnnotation#
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
       |[5:1..5:8): example => example/
       |[5:9..5:24): MacroAnnotation => example/MacroAnnotation#
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
       |[5:1..5:8): example => example/
       |[5:9..5:24): MacroAnnotation => example/MacroAnnotation#
       |[5:31..5:32): K <= k/K#
       |[5:32..5:32):  <= k/K#`<init>`().
       |[6:7..6:8): K <= k/K.
       |""".stripMargin,
  )

  occurrences(
    "#1815: prefix shadowed by an object with a class of that name",
    """|package t
       |object example { class MacroAnnotation extends scala.annotation.StaticAnnotation }
       |@example.MacroAnnotation class T
       |object T
       |object W { new T().classNumber }
       |""".stripMargin,
    """|[0:8..0:9): t <= t/
       |[1:7..1:14): example <= t/example.
       |[1:23..1:38): MacroAnnotation <= t/example.MacroAnnotation#
       |[1:39..1:39):  <= t/example.MacroAnnotation#`<init>`().
       |[1:47..1:52): scala => scala/
       |[1:53..1:63): annotation => scala/annotation/
       |[1:64..1:80): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[1:80..1:80):  => scala/annotation/Annotation#`<init>`().
       |[2:1..2:8): example => example/
       |[2:9..2:24): MacroAnnotation => example/MacroAnnotation#
       |[2:31..2:32): T <= t/T#
       |[2:32..2:32):  <= t/T#`<init>`().
       |[3:7..3:8): T <= t/T.
       |[4:7..4:8): W <= t/W.
       |[4:15..4:16): T => t/T#
       |[4:16..4:16):  => t/T#`<init>`().
       |[4:19..4:30): classNumber => t/T#classNumber().
       |""".stripMargin,
  )

  occurrences(
    "#1815: prefix shadowed by a member object, on a method",
    """|package u
       |object U {
       |  object example
       |  @example.RebuildDef def f(i: Int) = i
       |}
       |""".stripMargin,
    """|[0:8..0:9): u <= u/
       |[1:7..1:8): U <= u/U.
       |[2:9..2:16): example <= u/U.example.
       |[3:28..3:29): i <= u/U.f().(i)
       |[3:31..3:34): Int => scala/Int#
       |[3:38..3:39): i => u/U.f().(i)
       |""".stripMargin,
  )

  occurrences(
    "#1815: prefix beside an object of that name",
    """|package v
       |object Ex { object example }
       |@example.MacroAnnotation class V
       |object V
       |""".stripMargin,
    """|[0:8..0:9): v <= v/
       |[1:7..1:9): Ex <= v/Ex.
       |[1:19..1:26): example <= v/Ex.example.
       |[2:1..2:8): example => example/
       |[2:9..2:24): MacroAnnotation => example/MacroAnnotation#
       |[2:31..2:32): V <= v/V#
       |[2:32..2:32):  <= v/V#`<init>`().
       |[3:7..3:8): V <= v/V.
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

  occurrences(
    """|package ma2
       |@org.scalameta.data.data
       |class A[T](a: Int, val b: T)(c: String) {
       |  val v = a
       |  def d(x: Int) = b
       |}
       |object A { val w = 1 }
       |""".stripMargin,
    """|[0:8..0:11): ma2 <= ma2/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:19): data => org/scalameta/data/
       |[1:20..1:24): data => org/scalameta/data/data#
       |[2:6..2:7): A <= ma2/A#
       |[2:8..2:9): T <= ma2/A#[T]
       |[2:10..2:10):  <= ma2/A#`<init>`().
       |[2:14..2:17): Int => scala/Int#
       |[2:26..2:27): T => ma2/A#[T]
       |[2:32..2:38): String => scala/Predef.String#
       |[3:6..3:7): v <= ma2/A#v.
       |[3:10..3:11): a => ma2/A#a.
       |[4:6..4:7): d <= ma2/A#d().
       |[4:8..4:9): x <= ma2/A#d().(x)
       |[4:11..4:14): Int => scala/Int#
       |[4:18..4:19): b => ma2/A#b.
       |[6:7..6:8): A <= ma2/A.
       |[6:15..6:16): w <= ma2/A.w.
       |""".stripMargin,
  )

  // the class of ma2 without the annotation
  occurrences(
    """|package ma3
       |class A[T](a: Int, val b: T)(c: String) {
       |  val v = a
       |  def d(x: Int) = b
       |}
       |object A { val w = 1 }
       |""".stripMargin,
    """|[0:8..0:11): ma3 <= ma3/
       |[1:6..1:7): A <= ma3/A#
       |[1:8..1:9): T <= ma3/A#[T]
       |[1:10..1:10):  <= ma3/A#`<init>`().
       |[1:11..1:12): a <= ma3/A#`<init>`().(a)
       |[1:11..1:12): a <= ma3/A#a.
       |[1:14..1:17): Int => scala/Int#
       |[1:23..1:24): b <= ma3/A#`<init>`().(b)
       |[1:23..1:24): b <= ma3/A#b.
       |[1:26..1:27): T => ma3/A#[T]
       |[1:29..1:30): c <= ma3/A#`<init>`().(c)
       |[1:29..1:30): c <= ma3/A#c.
       |[1:32..1:38): String => scala/Predef.String#
       |[2:6..2:7): v <= ma3/A#v.
       |[2:10..2:11): a => ma3/A#a.
       |[3:6..3:7): d <= ma3/A#d().
       |[3:8..3:9): x <= ma3/A#d().(x)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): b => ma3/A#b.
       |[5:7..5:8): A <= ma3/A.
       |[5:15..5:16): w <= ma3/A.w.
       |""".stripMargin,
  )

  // the macro replaces the members of the companion with those of the class
  occurrences(
    """|package ma4
       |@example.MacroAnnotation
       |class A[T](a: Int, val b: T)(c: String)
       |object A { val w = 1 }
       |""".stripMargin,
    """|[0:8..0:11): ma4 <= ma4/
       |[1:1..1:8): example => example/
       |[1:9..1:24): MacroAnnotation => example/MacroAnnotation#
       |[2:6..2:7): A <= ma4/A#
       |[2:8..2:9): T <= ma4/A#[T]
       |[2:10..2:10):  <= ma4/A#`<init>`().
       |[2:11..2:12): a <= ma4/A#`<init>`().(a)
       |[2:11..2:12): a <= ma4/A#a.
       |[2:14..2:17): Int => scala/Int#
       |[2:23..2:24): b <= ma4/A#`<init>`().(b)
       |[2:23..2:24): b <= ma4/A#b.
       |[2:26..2:27): T => ma4/A#[T]
       |[2:29..2:30): c <= ma4/A#`<init>`().(c)
       |[2:29..2:30): c <= ma4/A#c.
       |[2:32..2:38): String => scala/Predef.String#
       |[3:7..3:8): A <= ma4/A.
       |""".stripMargin,
  )

  occurrences(
    """|package ma5
       |@org.scalameta.data.data class A(private val a: Int = 1, b: String = "")
       |class U { def u = A(b = "b").b }
       |""".stripMargin,
    """|[0:8..0:11): ma5 <= ma5/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:19): data => org/scalameta/data/
       |[1:20..1:24): data => org/scalameta/data/data#
       |[1:31..1:32): A <= ma5/A#
       |[1:32..1:32):  <= ma5/A#`<init>`().
       |[1:48..1:51): Int => scala/Int#
       |[1:60..1:66): String => scala/Predef.String#
       |[2:6..2:7): U <= ma5/U#
       |[2:8..2:8):  <= ma5/U#`<init>`().
       |[2:14..2:15): u <= ma5/U#u().
       |[2:18..2:19): A => ma5/A.
       |[2:20..2:21): b => ma5/A.apply().(b)
       |[2:29..2:30): b => ma5/A#b.
       |""".stripMargin,
  )

  occurrences(
    """|package ma6
       |object N {
       |  @org.scalameta.data.data class A(a: Int)
       |  def f = { @org.scalameta.data.data class B(b: Int); B(1).b }
       |}
       |""".stripMargin,
    """|[0:8..0:11): ma6 <= ma6/
       |[1:7..1:8): N <= ma6/N.
       |[2:3..2:6): org => org/
       |[2:7..2:16): scalameta => org/scalameta/
       |[2:17..2:21): data => org/scalameta/data/
       |[2:22..2:26): data => org/scalameta/data/data#
       |[2:33..2:34): A <= ma6/N.A#
       |[2:34..2:34):  <= ma6/N.A#`<init>`().
       |[2:38..2:41): Int => scala/Int#
       |[3:6..3:7): f <= ma6/N.f().
       |[3:13..3:16): org => org/
       |[3:17..3:26): scalameta => org/scalameta/
       |[3:27..3:31): data => org/scalameta/data/
       |[3:32..3:36): data => org/scalameta/data/data#
       |[3:43..3:44): B <= local1
       |[3:44..3:44):  <= local3
       |[3:48..3:51): Int => scala/Int#
       |[3:54..3:55): B => local19
       |[3:59..3:60): b => local2
       |""".stripMargin,
  )

  // `@byNeed` renames the parameter to `_a` and adds a method `a`
  occurrences(
    """|package ma7
       |@org.scalameta.data.data
       |class A(a: Int @byNeed, b: Int)
       |""".stripMargin,
    """|[0:8..0:11): ma7 <= ma7/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:19): data => org/scalameta/data/
       |[1:20..1:24): data => org/scalameta/data/data#
       |[2:6..2:7): A <= ma7/A#
       |[2:7..2:7):  <= ma7/A#`<init>`().
       |[2:11..2:14): Int => scala/Int#
       |[2:27..2:30): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    """|package ma8
       |@org.scalameta.adt.root trait R[T] { def r(t: T): Int; val q: Int }
       |object R { def s = 1 }
       |@org.scalameta.adt.leaf class L(a: Int) extends R[Int] { def r(t: Int) = a; val q = 1 }
       |@org.scalameta.adt.leaf object O extends R[Int] { def r(t: Int) = 1; val q = 2 }
       |""".stripMargin,
    """|[0:8..0:11): ma8 <= ma8/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:18): adt => org/scalameta/adt/
       |[1:19..1:23): root => org/scalameta/adt/root#
       |[1:30..1:31): R <= ma8/R#
       |[1:32..1:33): T <= ma8/R#[T]
       |[1:41..1:42): r <= ma8/R#r().
       |[1:43..1:44): t <= ma8/R#r().(t)
       |[1:46..1:47): T => ma8/R#[T]
       |[1:50..1:53): Int => scala/Int#
       |[1:59..1:60): q <= ma8/R#q.
       |[1:62..1:65): Int => scala/Int#
       |[2:7..2:8): R <= ma8/R.
       |[2:15..2:16): s <= ma8/R.s().
       |[3:1..3:4): org => org/
       |[3:5..3:14): scalameta => org/scalameta/
       |[3:15..3:18): adt => org/scalameta/adt/
       |[3:19..3:23): leaf => org/scalameta/adt/leaf#
       |[3:30..3:31): L <= ma8/L#
       |[3:31..3:31):  <= ma8/L#`<init>`().
       |[3:35..3:38): Int => scala/Int#
       |[3:48..3:49): R => ma8/R#
       |[3:50..3:53): Int => scala/Int#
       |[3:54..3:54):  => java/lang/Object#`<init>`().
       |[3:61..3:62): r <= ma8/L#r().
       |[3:63..3:64): t <= ma8/L#r().(t)
       |[3:66..3:69): Int => scala/Int#
       |[3:73..3:74): a => ma8/L#a.
       |[3:80..3:81): q <= ma8/L#q.
       |[4:1..4:4): org => org/
       |[4:5..4:14): scalameta => org/scalameta/
       |[4:15..4:18): adt => org/scalameta/adt/
       |[4:19..4:23): leaf => org/scalameta/adt/leaf#
       |[4:31..4:32): O <= ma8/O.
       |[4:41..4:42): R => ma8/R#
       |[4:43..4:46): Int => scala/Int#
       |[4:47..4:47):  => java/lang/Object#`<init>`().
       |[4:54..4:55): r <= ma8/O.r().
       |[4:56..4:57): t <= ma8/O.r().(t)
       |[4:59..4:62): Int => scala/Int#
       |[4:73..4:74): q <= ma8/O.q.
       |""".stripMargin,
  )

  occurrences(
    """|package ma9
       |@example.KeepClass case class A[T](a: Int, b: T) { def d(x: Int) = a }
       |@example.KeepClass case class B(c: Int)
       |object B { val w = 1 }
       |object U { A(1, "").copy(a = 2).a + B(3).c + B.w + A.lens + B.lens }
       |""".stripMargin,
    """|[0:8..0:11): ma9 <= ma9/
       |[1:1..1:8): example => example/
       |[1:9..1:18): KeepClass => example/KeepClass#
       |[1:30..1:31): A <= ma9/A#
       |[1:32..1:33): T <= ma9/A#[T]
       |[1:34..1:34):  <= ma9/A#`<init>`().
       |[1:35..1:36): a <= ma9/A#`<init>`().(a)
       |[1:35..1:36): a <= ma9/A#a.
       |[1:38..1:41): Int => scala/Int#
       |[1:43..1:44): b <= ma9/A#`<init>`().(b)
       |[1:43..1:44): b <= ma9/A#b.
       |[1:46..1:47): T => ma9/A#[T]
       |[1:55..1:56): d <= ma9/A#d().
       |[1:57..1:58): x <= ma9/A#d().(x)
       |[1:60..1:63): Int => scala/Int#
       |[1:67..1:68): a => ma9/A#a.
       |[2:1..2:8): example => example/
       |[2:9..2:18): KeepClass => example/KeepClass#
       |[2:30..2:31): B <= ma9/B#
       |[2:31..2:31):  <= ma9/B#`<init>`().
       |[2:32..2:33): c <= ma9/B#`<init>`().(c)
       |[2:32..2:33): c <= ma9/B#c.
       |[2:35..2:38): Int => scala/Int#
       |[3:7..3:8): B <= ma9/B.
       |[3:15..3:16): w <= ma9/B.w.
       |[4:7..4:8): U <= ma9/U.
       |[4:11..4:12): A => ma9/A.
       |[4:20..4:24): copy => ma9/A#copy().
       |[4:25..4:26): a => ma9/A#copy().(a)
       |[4:32..4:33): a => ma9/A#a.
       |[4:34..4:35): + => scala/Int#`+`(+4).
       |[4:36..4:37): B => ma9/B.
       |[4:41..4:42): c => ma9/B#c.
       |[4:43..4:44): + => scala/Int#`+`(+4).
       |[4:45..4:46): B => ma9/B.
       |[4:47..4:48): w => ma9/B.w.
       |[4:49..4:50): + => scala/Int#`+`(+4).
       |[4:51..4:52): A => ma9/A.
       |[4:53..4:57): lens => ma9/A.lens().
       |[4:58..4:59): + => scala/Int#`+`(+4).
       |[4:60..4:61): B => ma9/B.
       |[4:62..4:66): lens => ma9/B.lens().
       |""".stripMargin,
  )

  // the classes of ma9 without the annotation
  occurrences(
    """|package ma10
       |case class A[T](a: Int, b: T) { def d(x: Int) = a }
       |case class B(c: Int)
       |object B { val w = 1 }
       |object U { A(1, "").copy(a = 2).a + B(3).c + B.w }
       |""".stripMargin,
    """|[0:8..0:12): ma10 <= ma10/
       |[1:11..1:12): A <= ma10/A#
       |[1:13..1:14): T <= ma10/A#[T]
       |[1:15..1:15):  <= ma10/A#`<init>`().
       |[1:16..1:17): a <= ma10/A#`<init>`().(a)
       |[1:16..1:17): a <= ma10/A#a.
       |[1:19..1:22): Int => scala/Int#
       |[1:24..1:25): b <= ma10/A#`<init>`().(b)
       |[1:24..1:25): b <= ma10/A#b.
       |[1:27..1:28): T => ma10/A#[T]
       |[1:36..1:37): d <= ma10/A#d().
       |[1:38..1:39): x <= ma10/A#d().(x)
       |[1:41..1:44): Int => scala/Int#
       |[1:48..1:49): a => ma10/A#a.
       |[2:11..2:12): B <= ma10/B#
       |[2:12..2:12):  <= ma10/B#`<init>`().
       |[2:13..2:14): c <= ma10/B#`<init>`().(c)
       |[2:13..2:14): c <= ma10/B#c.
       |[2:16..2:19): Int => scala/Int#
       |[3:7..3:8): B <= ma10/B.
       |[3:15..3:16): w <= ma10/B.w.
       |[4:7..4:8): U <= ma10/U.
       |[4:11..4:12): A => ma10/A.
       |[4:20..4:24): copy => ma10/A#copy().
       |[4:25..4:26): a => ma10/A#copy().(a)
       |[4:32..4:33): a => ma10/A#a.
       |[4:34..4:35): + => scala/Int#`+`(+4).
       |[4:36..4:37): B => ma10/B.
       |[4:41..4:42): c => ma10/B#c.
       |[4:43..4:44): + => scala/Int#`+`(+4).
       |[4:45..4:46): B => ma10/B.
       |[4:47..4:48): w => ma10/B.w.
       |""".stripMargin,
  )

  occurrences(
    """|package ma11
       |object O {
       |  @example.RebuildDef
       |  def f[T](x: Int)(y: T): Int = x
       |  def f(s: String): Int = 1
       |  def g = f(1)("") + f("")
       |}
       |""".stripMargin,
    """|[0:8..0:12): ma11 <= ma11/
       |[1:7..1:8): O <= ma11/O.
       |[3:8..3:9): T <= ma11/O.f(+1).[T]
       |[3:11..3:12): x <= ma11/O.f(+1).(x)
       |[3:14..3:17): Int => scala/Int#
       |[3:19..3:20): y <= ma11/O.f(+1).(y)
       |[3:22..3:23): T => ma11/O.f(+1).[T]
       |[3:26..3:29): Int => scala/Int#
       |[3:32..3:33): x => ma11/O.f(+1).(x)
       |[4:6..4:7): f <= ma11/O.f().
       |[4:8..4:9): s <= ma11/O.f().(s)
       |[4:11..4:17): String => scala/Predef.String#
       |[4:20..4:23): Int => scala/Int#
       |[5:6..5:7): g <= ma11/O.g().
       |[5:10..5:11): f => ma11/O.f(+1).
       |[5:19..5:20): + => scala/Int#`+`(+4).
       |[5:21..5:22): f => ma11/O.f().
       |""".stripMargin,
  )

  // the methods of ma11 without the annotation
  occurrences(
    """|package ma12
       |object O {
       |  def f[T](x: Int)(y: T): Int = x
       |  def f(s: String): Int = 1
       |  def g = f(1)("") + f("")
       |}
       |""".stripMargin,
    """|[0:8..0:12): ma12 <= ma12/
       |[1:7..1:8): O <= ma12/O.
       |[2:6..2:7): f <= ma12/O.f().
       |[2:8..2:9): T <= ma12/O.f().[T]
       |[2:11..2:12): x <= ma12/O.f().(x)
       |[2:14..2:17): Int => scala/Int#
       |[2:19..2:20): y <= ma12/O.f().(y)
       |[2:22..2:23): T => ma12/O.f().[T]
       |[2:26..2:29): Int => scala/Int#
       |[2:32..2:33): x => ma12/O.f().(x)
       |[3:6..3:7): f <= ma12/O.f(+1).
       |[3:8..3:9): s <= ma12/O.f(+1).(s)
       |[3:11..3:17): String => scala/Predef.String#
       |[3:20..3:23): Int => scala/Int#
       |[4:6..4:7): g <= ma12/O.g().
       |[4:10..4:11): f => ma12/O.f().
       |[4:19..4:20): + => scala/Int#`+`(+4).
       |[4:21..4:22): f => ma12/O.f(+1).
       |""".stripMargin,
  )

  occurrences(
    """|package ma13
       |@example.RebuildDefs object O { def f(x: Int) = x }
       |@example.RebuildDefs class C { def f(x: Int) = x }
       |""".stripMargin,
    """|[0:8..0:12): ma13 <= ma13/
       |[1:1..1:8): example => example/
       |[1:9..1:20): RebuildDefs => example/RebuildDefs#
       |[1:28..1:29): O <= ma13/O.
       |[1:38..1:39): x <= ma13/O.f().(x)
       |[1:41..1:44): Int => scala/Int#
       |[1:48..1:49): x => ma13/O.f().(x)
       |[2:1..2:8): example => example/
       |[2:9..2:20): RebuildDefs => example/RebuildDefs#
       |[2:27..2:28): C <= ma13/C#
       |[2:29..2:29):  <= ma13/C#`<init>`().
       |[2:37..2:38): x <= ma13/C#f().(x)
       |[2:40..2:43): Int => scala/Int#
       |[2:47..2:48): x => ma13/C#f().(x)
       |""".stripMargin,
  )

  occurrences(
    """|package ma14
       |@example.PrependOverloads class A { def f(s: String) = 1 }
       |object U { new A().f("") + new A().f(1) }
       |""".stripMargin,
    """|[0:8..0:12): ma14 <= ma14/
       |[1:1..1:8): example => example/
       |[1:9..1:25): PrependOverloads => example/PrependOverloads#
       |[1:32..1:33): A <= ma14/A#
       |[1:34..1:34):  <= ma14/A#`<init>`().
       |[1:40..1:41): f <= ma14/A#f(+1).
       |[1:42..1:43): s <= ma14/A#f(+1).(s)
       |[1:45..1:51): String => scala/Predef.String#
       |[2:7..2:8): U <= ma14/U.
       |[2:15..2:16): A => ma14/A#
       |[2:16..2:16):  => ma14/A#`<init>`().
       |[2:19..2:20): f => ma14/A#f(+1).
       |[2:25..2:26): + => scala/Int#`+`(+4).
       |[2:31..2:32): A => ma14/A#
       |[2:32..2:32):  => ma14/A#`<init>`().
       |[2:35..2:36): f => ma14/A#f().
       |""".stripMargin,
  )

  occurrences(
    """|package ma15
       |@org.scalameta.data.data
       |class O(o: Int) { @org.scalameta.data.data class I(i: Int) }
       |""".stripMargin,
    """|[0:8..0:12): ma15 <= ma15/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:19): data => org/scalameta/data/
       |[1:20..1:24): data => org/scalameta/data/data#
       |[2:6..2:7): O <= ma15/O#
       |[2:7..2:7):  <= ma15/O#`<init>`().
       |[2:11..2:14): Int => scala/Int#
       |[2:19..2:22): org => org/
       |[2:23..2:32): scalameta => org/scalameta/
       |[2:33..2:37): data => org/scalameta/data/
       |[2:38..2:42): data => org/scalameta/data/data#
       |[2:49..2:50): I <= ma15/O#I#
       |[2:50..2:50):  <= ma15/O#I#`<init>`().
       |[2:54..2:57): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    """|package ma16
       |@org.scalameta.adt.root trait T
       |object T { @org.scalameta.data.data class I(i: Int) }
       |""".stripMargin,
    """|[0:8..0:12): ma16 <= ma16/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:18): adt => org/scalameta/adt/
       |[1:19..1:23): root => org/scalameta/adt/root#
       |[1:30..1:31): T <= ma16/T#
       |[2:7..2:8): T <= ma16/T.
       |[2:12..2:15): org => org/
       |[2:16..2:25): scalameta => org/scalameta/
       |[2:26..2:30): data => org/scalameta/data/
       |[2:31..2:35): data => org/scalameta/data/data#
       |[2:42..2:43): I <= ma16/T.I#
       |[2:43..2:43):  <= ma16/T.I#`<init>`().
       |[2:47..2:50): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    """|package scala.meta
       |package ma17
       |@scala.meta.internal.trees.branch trait B extends Tree
       |object B { @scala.meta.internal.trees.ast class N(value: String) extends B }
       |object U { def u(n: B.N) = n.value }
       |""".stripMargin,
    """|[0:8..0:13): scala => scala/
       |[0:14..0:18): meta => scala/meta/
       |[1:8..1:12): ma17 <= scala/meta/ma17/
       |[2:1..2:1):  <= local65
       |[2:1..2:6): scala => scala/
       |[2:7..2:11): meta => scala/meta/
       |[2:12..2:20): internal => scala/meta/internal/
       |[2:21..2:26): trees => scala/meta/internal/trees/
       |[2:27..2:33): branch => scala/meta/internal/trees/branch#
       |[2:40..2:41): B <= scala/meta/ma17/B#
       |[2:50..2:54): Tree => scala/meta/Tree#
       |[3:7..3:8): B <= scala/meta/ma17/B.
       |[3:12..3:12):  <= local24
       |[3:12..3:17): scala => scala/
       |[3:18..3:22): meta => scala/meta/
       |[3:23..3:31): internal => scala/meta/internal/
       |[3:32..3:37): trees => scala/meta/internal/trees/
       |[3:38..3:41): ast => scala/meta/internal/trees/ast#
       |[3:48..3:49): N <= scala/meta/ma17/B.N#
       |[3:49..3:49):  <= scala/meta/ma17/B.N#`<init>`().
       |[3:57..3:63): String => scala/Predef.String#
       |[3:73..3:74): B => scala/meta/ma17/B#
       |[4:7..4:8): U <= scala/meta/ma17/U.
       |[4:15..4:16): u <= scala/meta/ma17/U.u().
       |[4:17..4:18): n <= scala/meta/ma17/U.u().(n)
       |[4:20..4:21): B => scala/meta/ma17/B.
       |[4:22..4:23): N => scala/meta/ma17/B.N#
       |[4:27..4:28): n => scala/meta/ma17/U.u().(n)
       |[4:29..4:34): value => scala/meta/ma17/B.N#value().
       |""".stripMargin,
  )

  occurrences(
    """|package ma18
       |object O {
       |  @example.RebuildVal val v: Int = 1
       |  def g = v
       |}
       |""".stripMargin,
    """|[0:8..0:12): ma18 <= ma18/
       |[1:7..1:8): O <= ma18/O.
       |[2:29..2:32): Int => scala/Int#
       |[3:6..3:7): g <= ma18/O.g().
       |[3:10..3:11): v => ma18/O.v.
       |""".stripMargin,
  )

  occurrences(
    """|package ma19
       |@example.RebuildDefs class C { def f(x: Int) = x; def f(s: String) = 1 }
       |""".stripMargin,
    """|[0:8..0:12): ma19 <= ma19/
       |[1:1..1:8): example => example/
       |[1:9..1:20): RebuildDefs => example/RebuildDefs#
       |[1:27..1:28): C <= ma19/C#
       |[1:29..1:29):  <= ma19/C#`<init>`().
       |[1:37..1:38): x <= ma19/C#f().(x)
       |[1:40..1:43): Int => scala/Int#
       |[1:47..1:48): x => ma19/C#f().(x)
       |[1:56..1:57): s <= ma19/C#f(+1).(s)
       |[1:59..1:65): String => scala/Predef.String#
       |""".stripMargin,
  )

  occurrences(
    """|package ma20
       |@example.RebuildDefs class C { def f(x: Int) = x }
       |object C { def g = 1 }
       |""".stripMargin,
    """|[0:8..0:12): ma20 <= ma20/
       |[1:1..1:8): example => example/
       |[1:9..1:20): RebuildDefs => example/RebuildDefs#
       |[1:27..1:28): C <= ma20/C#
       |[1:29..1:29):  <= ma20/C#`<init>`().
       |[1:37..1:38): x <= ma20/C#f().(x)
       |[1:40..1:43): Int => scala/Int#
       |[1:47..1:48): x => ma20/C#f().(x)
       |[2:7..2:8): C <= ma20/C.
       |[2:15..2:16): g <= ma20/C.g().
       |""".stripMargin,
  )

  occurrences(
    """|package ma21
       |@example.KeepClass class A { val x = new Object {} }
       |""".stripMargin,
    """|[0:8..0:12): ma21 <= ma21/
       |[1:1..1:8): example => example/
       |[1:9..1:18): KeepClass => example/KeepClass#
       |[1:25..1:26): A <= ma21/A#
       |[1:27..1:27):  <= ma21/A#`<init>`().
       |[1:33..1:34): x <= ma21/A#x.
       |[1:41..1:41):  <= local0
       |[1:41..1:47): Object => java/lang/Object#
       |[1:47..1:47):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    """|package ma22
       |object O { def f = { @example.RebuildDef def g(x: Int) = x; g(1) } }
       |""".stripMargin,
    """|[0:8..0:12): ma22 <= ma22/
       |[1:7..1:8): O <= ma22/O.
       |[1:15..1:16): f <= ma22/O.f().
       |[1:47..1:48): x <= local1
       |[1:50..1:53): Int => scala/Int#
       |[1:57..1:58): x => local1
       |[1:60..1:61): g => local0
       |""".stripMargin,
  )

  occurrences(
    """|package ma23
       |class O
       |@example.RebuildDefs object O { def f(x: Int) = x }
       |""".stripMargin,
    """|[0:8..0:12): ma23 <= ma23/
       |[1:6..1:7): O <= ma23/O#
       |[1:7..1:7):  <= ma23/O#`<init>`().
       |[2:1..2:8): example => example/
       |[2:9..2:20): RebuildDefs => example/RebuildDefs#
       |[2:28..2:29): O <= ma23/O.
       |[2:38..2:39): x <= ma23/O.f().(x)
       |[2:41..2:44): Int => scala/Int#
       |[2:48..2:49): x => ma23/O.f().(x)
       |""".stripMargin,
  )

  occurrences(
    """|package ma24
       |object O { val v = { @org.scalameta.data.data class B(b: Int); B(1).b } }
       |""".stripMargin,
    """|[0:8..0:12): ma24 <= ma24/
       |[1:7..1:8): O <= ma24/O.
       |[1:15..1:16): v <= ma24/O.v.
       |[1:22..1:25): org => org/
       |[1:26..1:35): scalameta => org/scalameta/
       |[1:36..1:40): data => org/scalameta/data/
       |[1:41..1:45): data => org/scalameta/data/data#
       |[1:52..1:53): B <= local0
       |[1:53..1:53):  <= local2
       |[1:57..1:60): Int => scala/Int#
       |[1:63..1:64): B => local18
       |[1:68..1:69): b => local1
       |""".stripMargin,
  )

  occurrences(
    """|package ma25
       |object N { def f = { class B(b: Int); class C(val c: Int); new C(1).c } }
       |""".stripMargin,
    """|[0:8..0:12): ma25 <= ma25/
       |[1:7..1:8): N <= ma25/N.
       |[1:15..1:16): f <= ma25/N.f().
       |[1:27..1:28): B <= local0
       |[1:28..1:28):  <= local2
       |[1:29..1:30): b <= local1
       |[1:32..1:35): Int => scala/Int#
       |[1:44..1:45): C <= local3
       |[1:45..1:45):  <= local5
       |[1:50..1:51): c <= local4
       |[1:53..1:56): Int => scala/Int#
       |[1:63..1:64): C => local3
       |[1:64..1:64):  => local5
       |[1:68..1:69): c => local4
       |""".stripMargin,
  )

  occurrences(
    """|package ma26
       |object O { @example.PrependMethodOverload def f(s: String) = 1 }
       |object U { O.f("") + O.f(1) }
       |""".stripMargin,
    """|[0:8..0:12): ma26 <= ma26/
       |[1:7..1:8): O <= ma26/O.
       |[1:48..1:49): s <= ma26/O.f(+1).(s)
       |[1:51..1:57): String => scala/Predef.String#
       |[2:7..2:8): U <= ma26/U.
       |[2:11..2:12): O => ma26/O.
       |[2:13..2:14): f => ma26/O.f(+1).
       |[2:19..2:20): + => scala/Int#`+`(+4).
       |[2:21..2:22): O => ma26/O.
       |[2:23..2:24): f => ma26/O.f().
       |""".stripMargin,
  )

  occurrences(
    """|package ma27
       |object O { @example.AppendMethodOverload def f(s: String) = 1 }
       |object U { O.f("") + O.f(1) }
       |""".stripMargin,
    """|[0:8..0:12): ma27 <= ma27/
       |[1:7..1:8): O <= ma27/O.
       |[1:47..1:48): s <= ma27/O.f().(s)
       |[1:50..1:56): String => scala/Predef.String#
       |[2:7..2:8): U <= ma27/U.
       |[2:11..2:12): O => ma27/O.
       |[2:13..2:14): f => ma27/O.f().
       |[2:19..2:20): + => scala/Int#`+`(+4).
       |[2:21..2:22): O => ma27/O.
       |[2:23..2:24): f => ma27/O.f(+1).
       |""".stripMargin,
  )

  occurrences(
    """|package ma28
       |object O { @example.PrependMethodOverload def f = 1 }
       |""".stripMargin,
    """|[0:8..0:12): ma28 <= ma28/
       |[1:7..1:8): O <= ma28/O.
       |""".stripMargin,
  )

  occurrences(
    """|package ma29
       |@example.RebuildDefs class C { def f = 1; def f(x: Int) = x }
       |""".stripMargin,
    """|[0:8..0:12): ma29 <= ma29/
       |[1:1..1:8): example => example/
       |[1:9..1:20): RebuildDefs => example/RebuildDefs#
       |[1:27..1:28): C <= ma29/C#
       |[1:29..1:29):  <= ma29/C#`<init>`().
       |[1:48..1:49): x <= ma29/C#f(+1).(x)
       |[1:51..1:54): Int => scala/Int#
       |[1:58..1:59): x => ma29/C#f(+1).(x)
       |""".stripMargin,
  )

  occurrences(
    """|package ma30
       |object O { @example.PrependMethodOverload def f[T](t: T) = 1 }
       |""".stripMargin,
    """|[0:8..0:12): ma30 <= ma30/
       |[1:7..1:8): O <= ma30/O.
       |[1:48..1:49): T <= ma30/O.f(+1).[T]
       |[1:51..1:52): t <= ma30/O.f(+1).(t)
       |[1:54..1:55): T => ma30/O.f(+1).[T]
       |""".stripMargin,
  )

  occurrences(
    """|package ma31
       |@deprecated(since = "1") class N
       |object N
       |""".stripMargin,
    """|[0:8..0:12): ma31 <= ma31/
       |[1:1..1:11): deprecated => scala/deprecated#
       |[1:11..1:11):  => scala/deprecated#`<init>`().
       |[1:12..1:17): since => scala/deprecated#`<init>`().(since)
       |[1:31..1:32): N <= ma31/N#
       |[1:32..1:32):  <= ma31/N#`<init>`().
       |[2:7..2:8): N <= ma31/N.
       |""".stripMargin,
  )

  occurrences(
    """|package ma32
       |object O { @deprecated("m", "1") def g = 1; @deprecated(since = "1") val v = 1 }
       |""".stripMargin,
    """|[0:8..0:12): ma32 <= ma32/
       |[1:7..1:8): O <= ma32/O.
       |[1:12..1:22): deprecated => scala/deprecated#
       |[1:22..1:22):  => scala/deprecated#`<init>`().
       |[1:37..1:38): g <= ma32/O.g().
       |[1:45..1:55): deprecated => scala/deprecated#
       |[1:55..1:55):  => scala/deprecated#`<init>`().
       |[1:56..1:61): since => scala/deprecated#`<init>`().(since)
       |[1:73..1:74): v <= ma32/O.v.
       |""".stripMargin,
  )

  occurrences(
    """|package ma33
       |object O { type dep = scala.deprecated; @dep(since = "1") class N; object N }
       |""".stripMargin,
    """|[0:8..0:12): ma33 <= ma33/
       |[1:7..1:8): O <= ma33/O.
       |[1:16..1:19): dep <= ma33/O.dep#
       |[1:22..1:27): scala => scala/
       |[1:28..1:38): deprecated => scala/deprecated#
       |[1:41..1:44): dep => scala/deprecated#
       |[1:44..1:44):  => scala/deprecated#`<init>`().
       |[1:45..1:50): since => scala/deprecated#`<init>`().(since)
       |[1:64..1:65): N <= ma33/O.N#
       |[1:65..1:65):  <= ma33/O.N#`<init>`().
       |[1:74..1:75): N <= ma33/O.N.
       |""".stripMargin,
  )

  occurrences(
    """|package ma34
       |class ann(a: Int = 1, b: Int = 2) extends scala.annotation.StaticAnnotation
       |@ann(b = 3) class N
       |object N
       |""".stripMargin,
    """|[0:8..0:12): ma34 <= ma34/
       |[1:6..1:9): ann <= ma34/ann#
       |[1:9..1:9):  <= ma34/ann#`<init>`().
       |[1:10..1:11): a <= ma34/ann#`<init>`().(a)
       |[1:10..1:11): a <= ma34/ann#a.
       |[1:13..1:16): Int => scala/Int#
       |[1:22..1:23): b <= ma34/ann#`<init>`().(b)
       |[1:22..1:23): b <= ma34/ann#b.
       |[1:25..1:28): Int => scala/Int#
       |[1:42..1:47): scala => scala/
       |[1:48..1:58): annotation => scala/annotation/
       |[1:59..1:75): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[1:75..1:75):  => scala/annotation/Annotation#`<init>`().
       |[2:1..2:4): ann => ma34/ann#
       |[2:4..2:4):  => ma34/ann#`<init>`().
       |[2:5..2:6): b => ma34/ann#`<init>`().(b)
       |[2:18..2:19): N <= ma34/N#
       |[2:19..2:19):  <= ma34/N#`<init>`().
       |[3:7..3:8): N <= ma34/N.
       |""".stripMargin,
  )

  occurrences(
    """|package ma35
       |class A(@example.KeepParamOwner a: Int)
       |""".stripMargin,
    """|[0:8..0:12): ma35 <= ma35/
       |""".stripMargin + (if (ScalaVersion.is212) "" else "[1:7..1:7):  <= ma35/A#`<init>`().\n") +
      """|[1:9..1:16): example => example/
         |[1:17..1:31): KeepParamOwner => example/KeepParamOwner#
         |[1:35..1:38): Int => scala/Int#
         |""".stripMargin,
  )

  occurrences(
    """|package ma36
       |class B[@example.KeepParamOwner T]
       |""".stripMargin,
    """|[0:8..0:12): ma36 <= ma36/
       |[1:9..1:16): example => example/
       |[1:17..1:31): KeepParamOwner => example/KeepParamOwner#
       |[1:34..1:34):  <= ma36/B#`<init>`().
       |""".stripMargin,
  )

  occurrences(
    """|package ma37
       |object O { def f(@example.KeepParamOwner s: String) = s }
       |""".stripMargin,
    """|[0:8..0:12): ma37 <= ma37/
       |[1:7..1:8): O <= ma37/O.
       |[1:44..1:50): String => scala/Predef.String#
       |[1:54..1:55): s => ma37/O.f().(s)
       |""".stripMargin,
  )

  occurrences(
    """|package ma38
       |trait T[@example.KeepParamOwner X]
       |""".stripMargin,
    """|[0:8..0:12): ma38 <= ma38/
       |[1:9..1:16): example => example/
       |[1:17..1:31): KeepParamOwner => example/KeepParamOwner#
       |""".stripMargin,
  )

  occurrences(
    """|package ma39
       |object O { def f[@example.KeepParamOwner X](x: X) = x }
       |""".stripMargin,
    """|[0:8..0:12): ma39 <= ma39/
       |[1:7..1:8): O <= ma39/O.
       |[1:44..1:45): x <= ma39/O.f().(x)
       |[1:47..1:48): X => ma39/O.f().[X]
       |[1:52..1:53): x => ma39/O.f().(x)
       |""".stripMargin,
  )

  occurrences(
    """|package ma40
       |object O { type dep = scala.deprecated; class N(@dep(since = "1") a: Int) }
       |""".stripMargin,
    """|[0:8..0:12): ma40 <= ma40/
       |[1:7..1:8): O <= ma40/O.
       |[1:16..1:19): dep <= ma40/O.dep#
       |[1:22..1:27): scala => scala/
       |[1:28..1:38): deprecated => scala/deprecated#
       |[1:46..1:47): N <= ma40/O.N#
       |[1:47..1:47):  <= ma40/O.N#`<init>`().
       |[1:49..1:52): dep => scala/deprecated#
       |[1:52..1:52):  => scala/deprecated#`<init>`().
       |[1:53..1:58): since => scala/deprecated#`<init>`().(since)
       |[1:66..1:67): a <= ma40/O.N#`<init>`().(a)
       |[1:66..1:67): a <= ma40/O.N#a.
       |[1:69..1:72): Int => scala/Int#
       |""".stripMargin,
  )

  // another file of the run declares the package first, at an offset within the annottee
  occurrences(
    Seq(
      """|/* Copyright 2026 */
         |package ma41
         |class Other
         |""".stripMargin,
    ),
    """|package ma41
       |@org.scalameta.data.data
       |class A(a: Int)
       |object A { val w = 1 }
       |""".stripMargin,
    """|[0:8..0:12): ma41 <= ma41/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:19): data => org/scalameta/data/
       |[1:20..1:24): data => org/scalameta/data/data#
       |[2:6..2:7): A <= ma41/A#
       |[2:7..2:7):  <= ma41/A#`<init>`().
       |[2:11..2:14): Int => scala/Int#
       |[3:7..3:8): A <= ma41/A.
       |[3:15..3:16): w <= ma41/A.w.
       |""".stripMargin,
  )

  occurrences(
    Seq(
      """|/* Copyright 2026 */
         |package ma42
         |class Other
         |""".stripMargin,
    ),
    """|package ma42
       |@example.RebuildDefs object O { def f(x: Int) = x }
       |""".stripMargin,
    """|[0:8..0:12): ma42 <= ma42/
       |[1:1..1:8): example => example/
       |[1:9..1:20): RebuildDefs => example/RebuildDefs#
       |[1:28..1:29): O <= ma42/O.
       |[1:38..1:39): x <= ma42/O.f().(x)
       |[1:41..1:44): Int => scala/Int#
       |[1:48..1:49): x => ma42/O.f().(x)
       |""".stripMargin,
  )

  // the file of ma41 without the other file
  occurrences(
    """|package ma43
       |@org.scalameta.data.data
       |class A(a: Int)
       |object A { val w = 1 }
       |""".stripMargin,
    """|[0:8..0:12): ma43 <= ma43/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:19): data => org/scalameta/data/
       |[1:20..1:24): data => org/scalameta/data/data#
       |[2:6..2:7): A <= ma43/A#
       |[2:7..2:7):  <= ma43/A#`<init>`().
       |[2:11..2:14): Int => scala/Int#
       |[3:7..3:8): A <= ma43/A.
       |[3:15..3:16): w <= ma43/A.w.
       |""".stripMargin,
  )

  // paradise rejects a case class annottee with a context bound
  if (ScalaVersion.is213) occurrences(
    """|package ma44
       |@example.KeepClass case class B[T: Ordering](t: T)
       |""".stripMargin,
    """|[0:8..0:12): ma44 <= ma44/
       |[1:1..1:8): example => example/
       |[1:9..1:18): KeepClass => example/KeepClass#
       |[1:30..1:31): B <= ma44/B#
       |[1:32..1:33): T <= ma44/B#[T]
       |[1:35..1:43): Ordering => scala/package.Ordering#
       |[1:44..1:44):  <= ma44/B#`<init>`().
       |[1:45..1:46): t <= ma44/B#`<init>`().(t)
       |[1:45..1:46): t <= ma44/B#t.
       |[1:48..1:49): T => ma44/B#[T]
       |""".stripMargin,
  )

  // the class of ma44 without the annotation
  occurrences(
    """|package ma45
       |case class B[T: Ordering](t: T)
       |""".stripMargin,
    """|[0:8..0:12): ma45 <= ma45/
       |[1:11..1:12): B <= ma45/B#
       |[1:13..1:14): T <= ma45/B#[T]
       |[1:16..1:16):  <= ma45/B#`<init>`().(evidence$1)
       |[1:16..1:16):  <= ma45/B#evidence$1.
       |[1:16..1:24): Ordering => scala/package.Ordering#
       |[1:26..1:27): t <= ma45/B#`<init>`().(t)
       |[1:26..1:27): t <= ma45/B#t.
       |[1:29..1:30): T => ma45/B#[T]
       |""".stripMargin,
  )

  occurrences(
    """|package ma46
       |object O { @example.RebuildDef def f[T: Ordering](x: T) = x }
       |""".stripMargin,
    """|[0:8..0:12): ma46 <= ma46/
       |[1:7..1:8): O <= ma46/O.
       |[1:37..1:38): T <= ma46/O.f().[T]
       |[1:40..1:48): Ordering => scala/package.Ordering#
       |[1:50..1:51): x <= ma46/O.f().(x)
       |[1:53..1:54): T => ma46/O.f().[T]
       |[1:58..1:59): x => ma46/O.f().(x)
       |""".stripMargin,
  )

  // the method of ma46 without the annotation
  occurrences(
    """|package ma47
       |object O { def f[T: Ordering](x: T) = x }
       |""".stripMargin,
    """|[0:8..0:12): ma47 <= ma47/
       |[1:7..1:8): O <= ma47/O.
       |[1:15..1:16): f <= ma47/O.f().
       |[1:17..1:18): T <= ma47/O.f().[T]
       |[1:20..1:20):  <= ma47/O.f().(evidence$1)
       |[1:20..1:28): Ordering => scala/package.Ordering#
       |[1:30..1:31): x <= ma47/O.f().(x)
       |[1:33..1:34): T => ma47/O.f().[T]
       |[1:38..1:39): x => ma47/O.f().(x)
       |""".stripMargin,
  )

}
