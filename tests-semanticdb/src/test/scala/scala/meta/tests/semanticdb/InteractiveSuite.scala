package scala.meta.tests
package semanticdb

import org.scalameta.logger
import scala.meta.interactive.InteractiveSemanticdb._
import scala.meta.internal.semanticdb.Print

import scala.tools.nsc.interactive.Global

import munit.FunSuite

class InteractiveSuite extends FunSuite {

  val option = "-Ywarn-unused:imports"
  val compiler: Global = newCompiler(scalacOptions = option :: Nil)
  def check(
      original: String,
      expected: String,
      compat: List[(ScalaVersion.Version, String)] = List.empty,
  )(implicit loc: munit.Location): Unit = test(logger.revealWhitespace(original)) {
    val options = List("-P:semanticdb:synthetics:on", "-P:semanticdb:text:on")
    val document = toTextDocument(compiler, original, options)
    val format = scala.meta.metap.Format.Detailed
    val syntax = Print.document(format, document)
    val expectedCompat = ScalaVersion.getExpected(compat, expected)
    assertNoDiff(syntax, expectedCompat)
  }

  def expected(occurrences: Int, listOccurences: String, applySynth: String) =
    s"""|interactive.scala
        |-----------------
        |
        |Summary:
        |Schema => SemanticDB v4
        |Uri => interactive.scala
        |Text => non-empty
        |Language => Scala
        |Symbols => 2 entries
        |Occurrences => $occurrences entries
        |Diagnostics => 1 entries
        |Synthetics => 2 entries
        |
        |Symbols:
        |b/a. => final object a extends AnyRef { +1 decls }
        |  AnyRef => scala/AnyRef#
        |b/a.x. => val method x: List[Nothing]
        |  List => scala/collection/immutable/List#
        |  Nothing => scala/Nothing#
        |
        |Occurrences:
        |[0:8..0:9): b <= b/
        |[1:7..1:12): scala => scala/
        |[1:13..1:23): concurrent => scala/concurrent/
        |[1:24..1:30): Future => scala/concurrent/Future#
        |[1:24..1:30): Future => scala/concurrent/Future.
        |[2:7..2:8): a <= b/a.
        |[3:6..3:7): x <= b/a.x.
        |$listOccurences
        |[4:2..4:3): x => b/a.x.
        |[4:4..4:5): + => scala/Predef.any2stringadd#`+`().
        |
        |Diagnostics:
        |[1:24..1:30) [warning] Unused import
        |
        |Synthetics:
        |[3:10..3:27): _root_.scala.List => *.apply[Nothing]
        |  $applySynth
        |  Nothing => scala/Nothing#
        |[4:2..4:3): x => Predef.any2stringadd[List[Nothing]](*)
        |  Predef => scala/Predef.
        |  any2stringadd => scala/Predef.any2stringadd().
        |  List => scala/collection/immutable/List#
        |  Nothing => scala/Nothing#
    """.stripMargin

  val expectedLatest = expected(
    11,
    """|[3:17..3:22): scala => scala/
       |[3:23..3:27): List => scala/package.List.""".stripMargin,
    "apply => scala/collection/IterableFactory#apply().",
  )

  val expectedPrevious = expected(
    11,
    """|[3:17..3:22): scala => scala/
       |[3:23..3:27): List => scala/collection/immutable/List.""".stripMargin,
    "apply => scala/collection/immutable/List.apply().",
  )

  check(
    """|package b
       |import scala.concurrent.Future
       |object a {
       |  val x = _root_.scala.List()
       |  x + "string"
       |}
       |""".stripMargin,
    expectedPrevious,
    compat = List(ScalaVersion.Scala213 -> expectedLatest),
  )

  // This tests a case where SymbolOps.toSemantic crashes
  check(
    """|
       |object b {
       |  def add(a: In) = 1
       |}""".stripMargin,
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 3 entries
       |Occurrences => 4 entries
       |Diagnostics => 1 entries
       |
       |Symbols:
       |_empty_/b. => final object b extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/b.add(). => method add(a): Int
       |  a => _empty_/b.add().(a)
       |  Int => scala/Int#
       |_empty_/b.add().(a) => param a
       |
       |Occurrences:
       |[1:7..1:8): b <= _empty_/b.
       |[2:6..2:9): add <= _empty_/b.add().
       |[2:10..2:11): a <= _empty_/b.add().(a)
       |[2:13..2:15): In => local0
       |
       |Diagnostics:
       |[2:13..2:15) [error] not found: type In
       |""".stripMargin,
  )

  check(
    """|object n61 {
       |  final val K = 1
       |  val a = K :: Nil
       |}
       |""".stripMargin,
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 3 entries
       |Occurrences => 6 entries
       |Synthetics => 1 entries
       |
       |Symbols:
       |_empty_/n61. => final object n61 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/n61.K. => final val method K: 1
       |_empty_/n61.a. => val method a: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |
       |Occurrences:
       |[0:7..0:10): n61 <= _empty_/n61.
       |[1:12..1:13): K <= _empty_/n61.K.
       |[2:6..2:7): a <= _empty_/n61.a.
       |[2:10..2:11): K => _empty_/n61.K.
       |[2:12..2:14): :: => scala/collection/immutable/List#`::`().
       |[2:15..2:18): Nil => scala/collection/immutable/Nil.
       |
       |Synthetics:
       |[2:12..2:18): :: Nil => *[Int]
       |  Int => scala/Int#
       |""".stripMargin,
    compat = List {
      ScalaVersion.Scala213 ->
        """|interactive.scala
           |-----------------
           |
           |Summary:
           |Schema => SemanticDB v4
           |Uri => interactive.scala
           |Text => non-empty
           |Language => Scala
           |Symbols => 3 entries
           |Occurrences => 6 entries
           |Synthetics => 1 entries
           |
           |Symbols:
           |_empty_/n61. => final object n61 extends AnyRef { +2 decls }
           |  AnyRef => scala/AnyRef#
           |_empty_/n61.K. => final val method K: 1
           |_empty_/n61.a. => val method a: List[Int]
           |  List => scala/collection/immutable/List#
           |  Int => scala/Int#
           |
           |Occurrences:
           |[0:7..0:10): n61 <= _empty_/n61.
           |[1:12..1:13): K <= _empty_/n61.K.
           |[2:6..2:7): a <= _empty_/n61.a.
           |[2:10..2:11): K => _empty_/n61.K.
           |[2:12..2:14): :: => scala/collection/immutable/List#`::`().
           |[2:15..2:18): Nil => scala/package.Nil.
           |
           |Synthetics:
           |[2:12..2:18): :: Nil => *[Int]
           |  Int => scala/Int#
           |""".stripMargin
    },
  )

  check(
    """|object n77 {
       |  class C
       |  implicit val c: C = new C
       |  class R { def +:(s: String)(implicit c: C) = this }
       |  implicit def v(i: Int): String = i.toString
       |  val r = new R
       |  val x = 2
       |  val a = x +: r
       |}
       |""".stripMargin,
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 14 entries
       |Occurrences => 28 entries
       |Synthetics => 2 entries
       |
       |Symbols:
       |_empty_/n77. => final object n77 extends AnyRef { +7 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/n77.C# => class C extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/n77.C#`<init>`(). => primary ctor <init>()
       |_empty_/n77.R# => class R extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/n77.R#`+:`(). => method +:(s: String)(implicit c: C): R
       |  s => _empty_/n77.R#`+:`().(s)
       |  String => scala/Predef.String#
       |  c => _empty_/n77.R#`+:`().(c)
       |  C => _empty_/n77.C#
       |  R => _empty_/n77.R#
       |_empty_/n77.R#`+:`().(c) => implicit param c: C
       |  C => _empty_/n77.C#
       |_empty_/n77.R#`+:`().(s) => param s: String
       |  String => scala/Predef.String#
       |_empty_/n77.R#`<init>`(). => primary ctor <init>()
       |_empty_/n77.a. => val method a: R
       |  R => _empty_/n77.R#
       |_empty_/n77.c. => implicit val method c: C
       |  C => _empty_/n77.C#
       |_empty_/n77.r. => val method r: R
       |  R => _empty_/n77.R#
       |_empty_/n77.v(). => implicit method v(i: Int): String
       |  i => _empty_/n77.v().(i)
       |  Int => scala/Int#
       |  String => scala/Predef.String#
       |_empty_/n77.v().(i) => param i: Int
       |  Int => scala/Int#
       |_empty_/n77.x. => val method x: Int
       |  Int => scala/Int#
       |
       |Occurrences:
       |[0:7..0:10): n77 <= _empty_/n77.
       |[1:8..1:9): C <= _empty_/n77.C#
       |[1:9..1:9):  <= _empty_/n77.C#`<init>`().
       |[2:15..2:16): c <= _empty_/n77.c.
       |[2:18..2:19): C => _empty_/n77.C#
       |[2:26..2:27): C => _empty_/n77.C#
       |[2:27..2:27):  => _empty_/n77.C#`<init>`().
       |[3:8..3:9): R <= _empty_/n77.R#
       |[3:10..3:10):  <= _empty_/n77.R#`<init>`().
       |[3:16..3:18): +: <= _empty_/n77.R#`+:`().
       |[3:19..3:20): s <= _empty_/n77.R#`+:`().(s)
       |[3:22..3:28): String => scala/Predef.String#
       |[3:39..3:40): c <= _empty_/n77.R#`+:`().(c)
       |[3:42..3:43): C => _empty_/n77.C#
       |[4:15..4:16): v <= _empty_/n77.v().
       |[4:17..4:18): i <= _empty_/n77.v().(i)
       |[4:20..4:23): Int => scala/Int#
       |[4:26..4:32): String => scala/Predef.String#
       |[4:35..4:36): i => _empty_/n77.v().(i)
       |[4:37..4:45): toString => scala/Any#toString().
       |[5:6..5:7): r <= _empty_/n77.r.
       |[5:14..5:15): R => _empty_/n77.R#
       |[5:15..5:15):  => _empty_/n77.R#`<init>`().
       |[6:6..6:7): x <= _empty_/n77.x.
       |[7:6..7:7): a <= _empty_/n77.a.
       |[7:10..7:11): x => _empty_/n77.x.
       |[7:12..7:14): +: => _empty_/n77.R#`+:`().
       |[7:15..7:16): r => _empty_/n77.r.
       |
       |Synthetics:
       |[7:10..7:11): x => v(*)
       |  v => _empty_/n77.v().
       |[7:10..7:16): x +: r => *(c)
       |  c => _empty_/n77.c.
       |""".stripMargin,
  )

  check(
    """|object n87 {
       |  final val S = "a"
       |  @SuppressWarnings(Array(S)) def k = 1
       |}
       |""".stripMargin,
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 3 entries
       |Occurrences => 6 entries
       |
       |Symbols:
       |_empty_/n87. => final object n87 extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/n87.S. => final val method S: "a"
       |_empty_/n87.k(). => @SuppressWarnings method k: Int
       |  SuppressWarnings => java/lang/SuppressWarnings#
       |  Int => scala/Int#
       |
       |Occurrences:
       |[0:7..0:10): n87 <= _empty_/n87.
       |[1:12..1:13): S <= _empty_/n87.S.
       |[2:3..2:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[2:20..2:25): Array => scala/Array.
       |[2:26..2:27): S => _empty_/n87.S.
       |[2:34..2:35): k <= _empty_/n87.k().
       |""".stripMargin,
  )

  val expectedIa1 =
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 11 entries
       |Occurrences => 18 entries
       |Diagnostics => 1 entries
       |
       |Symbols:
       |ia1/N# => @ann class N extends AnyRef { +1 decls }
       |  ann => ia1/ann#
       |  AnyRef => scala/AnyRef#
       |ia1/N#`<init>`(). => primary ctor <init>()
       |ia1/ann# => class ann extends Annotation with StaticAnnotation { +3 decls }
       |  Annotation => scala/annotation/Annotation#
       |  StaticAnnotation => scala/annotation/StaticAnnotation#
       |ia1/ann#`<init>`(). => primary ctor <init>(a: Int, b: Int)
       |  a => ia1/ann#`<init>`().(a)
       |  Int => scala/Int#
       |  b => ia1/ann#`<init>`().(b)
       |ia1/ann#`<init>`().(a) => default param a: Int
       |  Int => scala/Int#
       |ia1/ann#`<init>`().(b) => default param b: Int
       |  Int => scala/Int#
       |ia1/ann#a. => private[this] val method a: Int
       |  Int => scala/Int#
       |ia1/ann#b. => private[this] val method b: Int
       |  Int => scala/Int#
       |ia1/ann. => synthetic final object ann extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |ia1/ann.`<init>$default$1`(). => synthetic method <init>$default$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |ia1/ann.`<init>$default$2`(). => synthetic method <init>$default$2: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |
       |Occurrences:
       |[0:8..0:11): ia1 <= ia1/
       |[1:6..1:9): ann <= ia1/ann#
       |[1:9..1:9):  <= ia1/ann#`<init>`().
       |[1:10..1:11): a <= ia1/ann#`<init>`().(a)
       |[1:10..1:11): a <= ia1/ann#a.
       |[1:13..1:16): Int => scala/Int#
       |[1:22..1:23): b <= ia1/ann#`<init>`().(b)
       |[1:22..1:23): b <= ia1/ann#b.
       |[1:25..1:28): Int => scala/Int#
       |[1:42..1:47): scala => scala/
       |[1:48..1:58): annotation => scala/annotation/
       |[1:59..1:75): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[1:75..1:75):  => scala/annotation/Annotation#`<init>`().
       |[2:1..2:4): ann => ia1/ann#
       |[2:4..2:4):  => ia1/ann#`<init>`().
       |[2:5..2:6): b => ia1/ann#`<init>`().(b)
       |[2:18..2:19): N <= ia1/N#
       |[2:19..2:19):  <= ia1/N#`<init>`().
       |
       |Diagnostics:
       |[2:1..2:4) [warning] Usage of named or default arguments transformed this annotation
       |constructor call into a block. The corresponding AnnotationInfo
       |will contain references to local values and default getters instead
       |of the actual argument trees
       |""".stripMargin

  check(
    """|package ia1
       |class ann(a: Int = 1, b: Int = 2) extends scala.annotation.StaticAnnotation
       |@ann(b = 3) class N
       |""".stripMargin,
    """|interactive.scala
       |-----------------
       |
       |Summary:
       |Schema => SemanticDB v4
       |Uri => interactive.scala
       |Text => non-empty
       |Language => Scala
       |Symbols => 11 entries
       |Occurrences => 18 entries
       |
       |Symbols:
       |ia1/N# => @ann class N extends AnyRef { +1 decls }
       |  ann => ia1/ann#
       |  AnyRef => scala/AnyRef#
       |ia1/N#`<init>`(). => primary ctor <init>()
       |ia1/ann# => class ann extends Annotation with StaticAnnotation { +3 decls }
       |  Annotation => scala/annotation/Annotation#
       |  StaticAnnotation => scala/annotation/StaticAnnotation#
       |ia1/ann#`<init>`(). => primary ctor <init>(@defaultArg a: Int, @defaultArg b: Int)
       |  a => ia1/ann#`<init>`().(a)
       |  defaultArg => scala/annotation/meta/defaultArg#
       |  Int => scala/Int#
       |  b => ia1/ann#`<init>`().(b)
       |ia1/ann#`<init>`().(a) => @defaultArg default param a: Int
       |  defaultArg => scala/annotation/meta/defaultArg#
       |  Int => scala/Int#
       |ia1/ann#`<init>`().(b) => @defaultArg default param b: Int
       |  defaultArg => scala/annotation/meta/defaultArg#
       |  Int => scala/Int#
       |ia1/ann#a. => private[this] val method a: Int
       |  Int => scala/Int#
       |ia1/ann#b. => private[this] val method b: Int
       |  Int => scala/Int#
       |ia1/ann. => synthetic final object ann extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |ia1/ann.`<init>$default$1`(). => synthetic method <init>$default$1: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |ia1/ann.`<init>$default$2`(). => synthetic method <init>$default$2: Int @uncheckedVariance
       |  Int => scala/Int#
       |  uncheckedVariance => scala/annotation/unchecked/uncheckedVariance#
       |
       |Occurrences:
       |[0:8..0:11): ia1 <= ia1/
       |[1:6..1:9): ann <= ia1/ann#
       |[1:9..1:9):  <= ia1/ann#`<init>`().
       |[1:10..1:11): a <= ia1/ann#`<init>`().(a)
       |[1:10..1:11): a <= ia1/ann#a.
       |[1:13..1:16): Int => scala/Int#
       |[1:22..1:23): b <= ia1/ann#`<init>`().(b)
       |[1:22..1:23): b <= ia1/ann#b.
       |[1:25..1:28): Int => scala/Int#
       |[1:42..1:47): scala => scala/
       |[1:48..1:58): annotation => scala/annotation/
       |[1:59..1:75): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[1:75..1:75):  => scala/annotation/Annotation#`<init>`().
       |[2:1..2:4): ann => ia1/ann#
       |[2:4..2:4):  => ia1/ann#`<init>`().
       |[2:5..2:6): b => ia1/ann#`<init>`().(b)
       |[2:18..2:19): N <= ia1/N#
       |[2:19..2:19):  <= ia1/N#`<init>`().
       |""".stripMargin,
    compat = List(ScalaVersion.Scala212 -> expectedIa1, ScalaVersion.AtMost213(15) -> expectedIa1),
  )

}
