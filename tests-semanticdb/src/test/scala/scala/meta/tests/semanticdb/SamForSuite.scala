package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}

import scala.collection.mutable

class SamForSuite extends SemanticdbSuite {

  private def symbols(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: symbols", code, expected, "Symbols")

  private def occurrences(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: occurrences", code, expected, "Occurrences")

  private def synthetics(name: String, code: String, expected: String)(implicit
      loc: munit.Location,
  ): Unit = checkSection(s"$name: synthetics", code, expected, "Synthetics")

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
       |[9:15..9:26): (a, b) <- m <= local3
       |[9:15..9:26): (a, b) <- m <= local4
       |[9:16..9:17): a <= local1
       |[9:19..9:20): b <= local2
       |[9:25..9:26): m => _empty_/t155.m.
       |[9:34..9:35): a => local1
       |[9:36..9:37): + => scala/Int#`+`(+4).
       |[9:38..9:39): b => local2
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
       |[9:15..9:26): (a, b) <- m <= local3
       |[9:16..9:17): a <= local1
       |[9:19..9:20): b <= local2
       |[9:25..9:26): m => _empty_/t155.m.
       |[9:34..9:35): a => local1
       |[9:36..9:37): + => scala/Int#`+`(+4).
       |[9:38..9:39): b => local2
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
       |local0 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local1 => val local a: Int
       |  Int => scala/Int#
       |local2 => val local b: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with F[Tuple2[Int, Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local4 => synthetic method f(x$1: Tuple2[Int, Int]): Int <: _empty_/F#f().
       |  x$1 => local0
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
       |[9:15..9:27): Some(a) <- m <= local2
       |[9:15..9:27): Some(a) <- m <= local3
       |[9:20..9:21): a <= local1
       |[9:26..9:27): m => _empty_/t156.m.
       |[9:35..9:36): a => local1
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
       |[9:15..9:27): Some(a) <- m <= local2
       |[9:15..9:19): Some => scala/Some.
       |[9:20..9:21): a <= local1
       |[9:26..9:27): m => _empty_/t156.m.
       |[9:35..9:36): a => local1
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
       |local0 => synthetic param x$1: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local1 => val local a: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Option[Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local3 => synthetic method f(x$1: Option[Int]): Int <: _empty_/F#f().
       |  x$1 => local0
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
       |[9:15..9:28): (a: Int) <- m <= local1
       |[9:15..9:28): (a: Int) <- m <= local2
       |[9:16..9:17): a <= local0
       |[9:19..9:22): Int => scala/Int#
       |[9:27..9:28): m => _empty_/t157.m.
       |[9:36..9:37): a => local0
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
       |[9:15..9:28): (a: Int) <- m <= local1
       |[9:16..9:17): a <= local0
       |[9:19..9:22): Int => scala/Int#
       |[9:27..9:28): m => _empty_/t157.m.
       |[9:36..9:37): a => local0
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
       |local0 => param a: Int
       |  Int => scala/Int#
       |local1 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local2 => synthetic method f(a: Int): Int <: _empty_/F#f().
       |  a => local0
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
       |[9:15..9:21): x <- m <= local1
       |[9:15..9:21): x <- m <= local2
       |[9:20..9:21): m => _empty_/t159.m.
       |[9:34..9:35): y <= local3
       |[9:34..9:40): y <- m <= local4
       |[9:34..9:40): y <- m <= local5
       |[9:39..9:40): m => _empty_/t159.m.
       |[9:48..9:49): x => local0
       |[9:50..9:51): + => scala/Int#`+`(+4).
       |[9:52..9:53): y => local3
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
       |[9:15..9:21): x <- m <= local1
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t159.m.
       |[9:34..9:40): y <- m <= local4
       |[9:34..9:35): y <= local3
       |[9:39..9:40): m => _empty_/t159.m.
       |[9:48..9:49): x => local0
       |[9:50..9:51): + => scala/Int#`+`(+4).
       |[9:52..9:53): y => local3
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
       |local1 => synthetic class $anon extends Object with F[Int, M[Int]] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local2 => synthetic method f(x: Int): M[Int] <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local3 => param y: Int
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local5 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local3
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
       |[9:15..9:16): x <= local0
       |[9:15..9:40): x <- for (y <- m) yield y <= local4
       |[9:15..9:40): x <- for (y <- m) yield y <= local5
       |[9:25..9:26): y <= local1
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:31): y <- m <= local3
       |[9:30..9:31): m => _empty_/t160.m.
       |[9:39..9:40): y => local1
       |[9:48..9:49): x => local0
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
       |[9:15..9:16): x <= local0
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:26): y <= local1
       |[9:30..9:31): m => _empty_/t160.m.
       |[9:39..9:40): y => local1
       |[9:48..9:49): x => local0
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
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local5 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local0
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
       |[9:15..9:16): x <= local0
       |[9:15..9:40): x <- for (y <- m) yield y <= local6
       |[9:15..9:40): x <- for (y <- m) yield y <= local7
       |[9:25..9:26): y <= local1
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:31): y <- m <= local3
       |[9:30..9:31): m => _empty_/t163.m.
       |[9:39..9:40): y => local1
       |[9:41..9:49): if x > 0 <= local4
       |[9:41..9:49): if x > 0 <= local5
       |[9:44..9:45): x => local0
       |[9:46..9:47): > => scala/Int#`>`(+3).
       |[9:57..9:58): x => local0
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
       |[9:15..9:16): x <= local0
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:26): y <= local1
       |[9:30..9:31): m => _empty_/t163.m.
       |[9:39..9:40): y => local1
       |[9:41..9:49): if x > 0 <= local4
       |[9:44..9:45): x => local0
       |[9:46..9:47): > => scala/Int#`>`(+3).
       |[9:57..9:58): x => local0
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
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
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
       |[9:15..9:16): x <= local0
       |[9:15..9:40): x <- for (y <- m) yield y <= local8
       |[9:15..9:40): x <- for (y <- m) yield y <= local9
       |[9:25..9:26): y <= local1
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:31): y <- m <= local3
       |[9:30..9:31): m => _empty_/t164.m.
       |[9:39..9:40): y => local1
       |[9:41..9:49): if x > 0 <= local4
       |[9:41..9:49): if x > 0 <= local5
       |[9:44..9:45): x => local0
       |[9:46..9:47): > => scala/Int#`>`(+3).
       |[9:50..9:58): if x < 2 <= local6
       |[9:50..9:58): if x < 2 <= local7
       |[9:53..9:54): x => local0
       |[9:55..9:56): < => scala/Int#`<`(+3).
       |[9:66..9:67): x => local0
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
       |[9:15..9:16): x <= local0
       |[9:25..9:31): y <- m <= local2
       |[9:25..9:26): y <= local1
       |[9:30..9:31): m => _empty_/t164.m.
       |[9:39..9:40): y => local1
       |[9:41..9:49): if x > 0 <= local4
       |[9:44..9:45): x => local0
       |[9:46..9:47): > => scala/Int#`>`(+3).
       |[9:50..9:58): if x < 2 <= local6
       |[9:53..9:54): x => local0
       |[9:55..9:56): < => scala/Int#`<`(+3).
       |[9:66..9:67): x => local0
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
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local6 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local7 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local8 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local9 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local0
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
       |[9:15..9:16): x <= local0
       |[9:15..9:42): x <- (for (y <- m) yield y) <= local6
       |[9:15..9:42): x <- (for (y <- m) yield y) <= local7
       |[9:26..9:27): y <= local1
       |[9:26..9:32): y <- m <= local2
       |[9:26..9:32): y <- m <= local3
       |[9:31..9:32): m => _empty_/t165.m.
       |[9:40..9:41): y => local1
       |[9:43..9:51): if x > 0 <= local4
       |[9:43..9:51): if x > 0 <= local5
       |[9:46..9:47): x => local0
       |[9:48..9:49): > => scala/Int#`>`(+3).
       |[9:59..9:60): x => local0
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
       |[9:15..9:16): x <= local0
       |[9:26..9:32): y <- m <= local2
       |[9:26..9:27): y <= local1
       |[9:31..9:32): m => _empty_/t165.m.
       |[9:40..9:41): y => local1
       |[9:43..9:51): if x > 0 <= local4
       |[9:46..9:47): x => local0
       |[9:48..9:49): > => scala/Int#`>`(+3).
       |[9:59..9:60): x => local0
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
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
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
       |[9:15..9:27): Some(a) <- m <= local5
       |[9:15..9:27): Some(a) <- m <= local6
       |[9:20..9:21): a <= local2
       |[9:26..9:27): m => _empty_/t171.m.
       |[9:28..9:36): if a > 0 <= local3
       |[9:28..9:36): if a > 0 <= local4
       |[9:31..9:32): a => local2
       |[9:33..9:34): > => scala/Int#`>`(+3).
       |[9:44..9:45): a => local2
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
       |[9:15..9:27): Some(a) <- m <= local5
       |[9:15..9:19): Some => scala/Some.
       |[9:20..9:21): a <= local2
       |[9:26..9:27): m => _empty_/t171.m.
       |[9:28..9:36): if a > 0 <= local3
       |[9:31..9:32): a => local2
       |[9:33..9:34): > => scala/Int#`>`(+3).
       |[9:44..9:45): a => local2
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
       |local0 => synthetic param x$1: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local1 => synthetic param x$2: Option[Int]
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => synthetic class $anon extends Object with F[Option[Int], Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic method f(x$1: Option[Int]): Boolean <: _empty_/F#f().
       |  x$1 => local0
       |  Option => scala/Option#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic class $anon extends Object with F[Option[Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Option => scala/Option#
       |  Int => scala/Int#
       |local6 => synthetic method f(x$2: Option[Int]): Int <: _empty_/F#f().
       |  x$2 => local1
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
       |[9:15..9:16): x <= local0
       |[9:15..9:44): x <- { for (y <- m) yield y } <= local6
       |[9:15..9:44): x <- { for (y <- m) yield y } <= local7
       |[9:27..9:28): y <= local1
       |[9:27..9:33): y <- m <= local2
       |[9:27..9:33): y <- m <= local3
       |[9:32..9:33): m => _empty_/t172.m.
       |[9:41..9:42): y => local1
       |[9:45..9:53): if x > 0 <= local4
       |[9:45..9:53): if x > 0 <= local5
       |[9:48..9:49): x => local0
       |[9:50..9:51): > => scala/Int#`>`(+3).
       |[9:61..9:62): x => local0
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
       |[9:15..9:16): x <= local0
       |[9:27..9:33): y <- m <= local2
       |[9:27..9:28): y <= local1
       |[9:32..9:33): m => _empty_/t172.m.
       |[9:41..9:42): y => local1
       |[9:45..9:53): if x > 0 <= local4
       |[9:48..9:49): x => local0
       |[9:50..9:51): > => scala/Int#`>`(+3).
       |[9:61..9:62): x => local0
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
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local3 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local5 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
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
       |[9:15..9:21): x <- m <= local2
       |[9:15..9:21): x <- m <= local3
       |[9:20..9:21): m => _empty_/t173.m.
       |[9:23..9:24): y <= local1
       |[9:23..9:48): y <- for (z <- m) yield z <= local9
       |[9:23..9:48): y <- for (z <- m) yield z <= local10
       |[9:33..9:34): z <= local4
       |[9:33..9:39): z <- m <= local5
       |[9:33..9:39): z <- m <= local6
       |[9:38..9:39): m => _empty_/t173.m.
       |[9:47..9:48): z => local4
       |[9:49..9:57): if y > x <= local7
       |[9:49..9:57): if y > x <= local8
       |[9:52..9:53): y => local1
       |[9:54..9:55): > => scala/Int#`>`(+3).
       |[9:56..9:57): x => local0
       |[9:65..9:66): y => local1
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
       |[9:15..9:21): x <- m <= local2
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t173.m.
       |[9:23..9:48): y <- for (z <- m) yield z <= local9
       |[9:23..9:24): y <= local1
       |[9:33..9:39): z <- m <= local5
       |[9:33..9:34): z <= local4
       |[9:38..9:39): m => _empty_/t173.m.
       |[9:47..9:48): z => local4
       |[9:49..9:57): if y > x <= local7
       |[9:52..9:53): y => local1
       |[9:54..9:55): > => scala/Int#`>`(+3).
       |[9:56..9:57): x => local0
       |[9:65..9:66): y => local1
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
       |local4 => param z: Int
       |  Int => scala/Int#
       |local5 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local6 => synthetic method f(z: Int): Int <: _empty_/F#f().
       |  z => local4
       |  Int => scala/Int#
       |local7 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local8 => synthetic method f(y: Int): Boolean <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local9 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local10 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local1
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
       |[9:15..9:16): x <= local0
       |[9:15..9:49): x <- for (y <- m if y > 0) yield y <= local8
       |[9:15..9:49): x <- for (y <- m if y > 0) yield y <= local9
       |[9:25..9:26): y <= local1
       |[9:25..9:31): y <- m <= local4
       |[9:25..9:31): y <- m <= local5
       |[9:30..9:31): m => _empty_/t174.m.
       |[9:32..9:40): if y > 0 <= local2
       |[9:32..9:40): if y > 0 <= local3
       |[9:35..9:36): y => local1
       |[9:37..9:38): > => scala/Int#`>`(+3).
       |[9:48..9:49): y => local1
       |[9:50..9:58): if x > 0 <= local6
       |[9:50..9:58): if x > 0 <= local7
       |[9:53..9:54): x => local0
       |[9:55..9:56): > => scala/Int#`>`(+3).
       |[9:66..9:67): x => local0
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
       |[9:15..9:16): x <= local0
       |[9:25..9:31): y <- m <= local4
       |[9:25..9:26): y <= local1
       |[9:30..9:31): m => _empty_/t174.m.
       |[9:32..9:40): if y > 0 <= local2
       |[9:35..9:36): y => local1
       |[9:37..9:38): > => scala/Int#`>`(+3).
       |[9:48..9:49): y => local1
       |[9:50..9:58): if x > 0 <= local6
       |[9:53..9:54): x => local0
       |[9:55..9:56): > => scala/Int#`>`(+3).
       |[9:66..9:67): x => local0
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
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic method f(y: Int): Boolean <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local4 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local5 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local1
       |  Int => scala/Int#
       |local6 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local7 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local8 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local9 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local0
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
       |[9:15..9:16): x <= local0
       |[9:15..9:40): x <- for (y <- m) yield y <= local9
       |[9:15..9:40): x <- for (y <- m) yield y <= local10
       |[9:25..9:26): y <= local3
       |[9:25..9:31): y <- m <= local4
       |[9:25..9:31): y <- m <= local5
       |[9:30..9:31): m => _empty_/t175.m.
       |[9:39..9:40): y => local3
       |[9:42..9:43): v <= local6
       |[9:46..9:47): x => local0
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
       |[9:15..9:16): x <= local0
       |[9:25..9:31): y <- m <= local4
       |[9:25..9:26): y <= local3
       |[9:30..9:31): m => _empty_/t175.m.
       |[9:39..9:40): y => local3
       |[9:42..9:43): v <= local6
       |[9:46..9:47): x => local0
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
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => synthetic param x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local2 => synthetic param x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local3 => param y: Int
       |  Int => scala/Int#
       |local4 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local5 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local3
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
       |  x$1 => local1
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local9 => synthetic class $anon extends Object with F[Tuple2[Int, Int], Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local10 => synthetic method f(x$2: Tuple2[Int, Int]): Int <: _empty_/F#f().
       |  x$2 => local2
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
       |[9:15..9:16): x <= local0
       |[9:15..9:74): x <- for (y <- m if (for (z <- m) yield z) ne null) yield y <= local9
       |[9:15..9:74): x <- for (y <- m if (for (z <- m) yield z) ne null) yield y <= local10
       |[9:25..9:26): y <= local1
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
       |[9:73..9:74): y => local1
       |[9:82..9:83): x => local0
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
       |[9:15..9:16): x <= local0
       |[9:25..9:31): y <- m <= local7
       |[9:25..9:26): y <= local1
       |[9:30..9:31): m => _empty_/t176.m.
       |[9:32..9:65): if (for (z <- m) yield z) ne null <= local2
       |[9:41..9:47): z <- m <= local5
       |[9:41..9:42): z <= local4
       |[9:46..9:47): m => _empty_/t176.m.
       |[9:55..9:56): z => local4
       |[9:58..9:60): ne => java/lang/Object#ne().
       |[9:73..9:74): y => local1
       |[9:82..9:83): x => local0
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
       |local0 => param x: Int
       |  Int => scala/Int#
       |local1 => param y: Int
       |  Int => scala/Int#
       |local2 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic method f(y: Int): Boolean <: _empty_/F#f().
       |  y => local1
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
       |  y => local1
       |  Int => scala/Int#
       |local9 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local10 => synthetic method f(x: Int): Int <: _empty_/F#f().
       |  x => local0
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
       |[9:15..9:21): x <- m <= local3
       |[9:15..9:21): x <- m <= local4
       |[9:20..9:21): m => _empty_/t177.m.
       |[9:22..9:30): if x > 0 <= local1
       |[9:22..9:30): if x > 0 <= local2
       |[9:25..9:26): x => local0
       |[9:27..9:28): > => scala/Int#`>`(+3).
       |[9:43..9:44): y <= local5
       |[9:43..9:49): y <- m <= local8
       |[9:43..9:49): y <- m <= local9
       |[9:48..9:49): m => _empty_/t177.m.
       |[9:50..9:58): if y > x <= local6
       |[9:50..9:58): if y > x <= local7
       |[9:53..9:54): y => local5
       |[9:55..9:56): > => scala/Int#`>`(+3).
       |[9:57..9:58): x => local0
       |[9:66..9:67): y => local5
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
       |[9:15..9:21): x <- m <= local3
       |[9:15..9:16): x <= local0
       |[9:20..9:21): m => _empty_/t177.m.
       |[9:22..9:30): if x > 0 <= local1
       |[9:25..9:26): x => local0
       |[9:27..9:28): > => scala/Int#`>`(+3).
       |[9:43..9:49): y <- m <= local8
       |[9:43..9:44): y <= local5
       |[9:48..9:49): m => _empty_/t177.m.
       |[9:50..9:58): if y > x <= local6
       |[9:53..9:54): y => local5
       |[9:55..9:56): > => scala/Int#`>`(+3).
       |[9:57..9:58): x => local0
       |[9:66..9:67): y => local5
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
       |local1 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local2 => synthetic method f(x: Int): Boolean <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local3 => synthetic class $anon extends Object with F[Int, M[Int]] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local4 => synthetic method f(x: Int): M[Int] <: _empty_/F#f().
       |  x => local0
       |  Int => scala/Int#
       |  M => _empty_/M#
       |local5 => param y: Int
       |  Int => scala/Int#
       |local6 => synthetic class $anon extends Object with F[Int, Boolean] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local7 => synthetic method f(y: Int): Boolean <: _empty_/F#f().
       |  y => local5
       |  Int => scala/Int#
       |  Boolean => scala/Boolean#
       |local8 => synthetic class $anon extends Object with F[Int, Int] { +1 decls }
       |  Object => java/lang/Object#
       |  F => _empty_/F#
       |  Int => scala/Int#
       |local9 => synthetic method f(y: Int): Int <: _empty_/F#f().
       |  y => local5
       |  Int => scala/Int#
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

}
