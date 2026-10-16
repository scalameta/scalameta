package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1815
class Issue1815Suite extends SemanticdbSuite {

  occurrences(
    """|package a
       |@example.MacroAnnotation
       |class A
       |object A
       |""".stripMargin,
    """|[0:8..0:9): a <= a/
       |[2:6..2:7): A <= a/A#
       |[2:7..2:7):  <= a/A#`<init>`().
       |[3:7..3:8): A <= a/A.
       |""".stripMargin,
  )

  occurrences(
    """|package b
       |@_root_.example.MacroAnnotation class B
       |object B
       |""".stripMargin,
    """|[0:8..0:9): b <= b/
       |[1:38..1:39): B <= b/B#
       |[1:39..1:39):  <= b/B#`<init>`().
       |[2:7..2:8): B <= b/B.
       |""".stripMargin,
  )

  occurrences(
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
       |[3:40..3:44): Stat => scala/meta/Stat#
       |[4:17..4:36): XtensionQuasiquoteC <= scala/meta/c/C.XtensionQuasiquoteC#
       |[4:36..4:36):  <= scala/meta/c/C.XtensionQuasiquoteC#`<init>`().
       |[4:37..4:40): ctx <= scala/meta/c/C.XtensionQuasiquoteC#`<init>`().(ctx)
       |[4:37..4:40): ctx <= scala/meta/c/C.XtensionQuasiquoteC#ctx.
       |[4:42..4:55): StringContext => scala/StringContext#
       |""".stripMargin,
  )

  occurrences(
    """|package d
       |@example.MacroAnnotation() @deprecated class D
       |object D
       |""".stripMargin,
    """|[0:8..0:9): d <= d/
       |[1:28..1:38): deprecated => scala/deprecated#
       |[1:38..1:38):  => scala/deprecated#`<init>`().
       |[1:45..1:46): D <= d/D#
       |[1:46..1:46):  <= d/D#`<init>`().
       |[2:7..2:8): D <= d/D.
       |""".stripMargin,
  )

  occurrences(
    """|package e
       |import example.MacroAnnotation
       |@MacroAnnotation class E
       |object E
       |""".stripMargin,
    """|[0:8..0:9): e <= e/
       |[1:7..1:14): example => example/
       |[1:15..1:30): MacroAnnotation => example/MacroAnnotation#
       |[1:15..1:30): MacroAnnotation => example/MacroAnnotation.
       |[2:23..2:24): E <= e/E#
       |[2:24..2:24):  <= e/E#`<init>`().
       |[3:7..3:8): E <= e/E.
       |""".stripMargin,
  )

  occurrences(
    """|package org.scalameta
       |package f
       |@data.data class F(a: Int)
       |""".stripMargin,
    """|[0:8..0:11): org => org/
       |[0:12..0:21): scalameta => org/scalameta/
       |[1:8..1:9): f <= org/scalameta/f/
       |[2:17..2:18): F <= org/scalameta/f/F#
       |[2:18..2:18):  <= org/scalameta/f/F#`<init>`().
       |[2:22..2:25): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
    """|package g
       |import org.scalameta.data
       |@data.data class G(a: Int)
       |""".stripMargin,
    """|[0:8..0:9): g <= g/
       |[1:7..1:10): org => org/
       |[1:11..1:20): scalameta => org/scalameta/
       |[1:21..1:25): data => org/scalameta/data/
       |[2:17..2:18): G <= g/G#
       |[2:18..2:18):  <= g/G#`<init>`().
       |[2:22..2:25): Int => scala/Int#
       |""".stripMargin,
  )

  occurrences(
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
       |[3:35..3:36): M <= local1
       |[3:36..3:36):  <= local2
       |[4:11..4:12): M <= local3
       |[5:8..5:9): M => local1
       |[5:9..5:9):  => local2
       |""".stripMargin,
  )

  occurrences(
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
    """|package org.scalameta.adt
       |@root trait R
       |""".stripMargin,
    """|[0:8..0:11): org => org/
       |[0:12..0:21): scalameta => org/scalameta/
       |[0:22..0:25): adt => org/scalameta/adt/
       |[1:12..1:13): R <= org/scalameta/adt/R#
       |""".stripMargin,
  )

  // a renamed import
  occurrences(
    """|package p
       |import example.{MacroAnnotation => MA}
       |@MA class P
       |object P
       |""".stripMargin,
    """|[0:8..0:9): p <= p/
       |[1:7..1:14): example => example/
       |[1:16..1:31): MacroAnnotation => example/MacroAnnotation#
       |[1:16..1:31): MacroAnnotation => example/MacroAnnotation.
       |[2:10..2:11): P <= p/P#
       |[2:11..2:11):  <= p/P#`<init>`().
       |[3:7..3:8): P <= p/P.
       |""".stripMargin,
  )

  // a wildcard import
  occurrences(
    """|package q
       |import example._
       |@MacroAnnotation class Q
       |object Q
       |""".stripMargin,
    """|[0:8..0:9): q <= q/
       |[1:7..1:14): example => example/
       |[2:23..2:24): Q <= q/Q#
       |[2:24..2:24):  <= q/Q#`<init>`().
       |[3:7..3:8): Q <= q/Q.
       |""".stripMargin,
  )

  // an import in an enclosing object
  occurrences(
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
       |[3:25..3:26): S <= r/R.S#
       |[3:26..3:26):  <= r/R.S#`<init>`().
       |[4:9..4:10): S <= r/R.S.
       |""".stripMargin,
  )

}
