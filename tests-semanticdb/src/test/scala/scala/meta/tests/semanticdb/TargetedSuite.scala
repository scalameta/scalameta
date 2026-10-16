package scala.meta.tests
package semanticdb

// Contributing tips:
// - To run an individual test: ~testsJVM/testOnly *TargetedSuite -- -z "package YYY"
//   and replace YYY with the package name of your test.
// - On test failure, the obtained output is printed to the console for
//   easy copy-paste to replace the current expected output.
// - Try to follow the alphabetical order of the enclosing package, at the time
//   of this writing the latest package is `j`, so the next package should be `l`.
// - glhf, and if you have any questions don't hesitate to ask in the gitter channel :)
class TargetedSuite extends SemanticdbSuite {

  targeted(
    // curried function application with named args, #648
    """|package a
       |object Curry {
       |  def bar(children: Int)(x: Int) = children + x
       |  <<bar>>(children = 4)(3)
       |}
       |""".stripMargin,
    (_, second) => assertEquals(second, "a/Curry.bar()."),
  )

  targeted(
    """|
       |package b
       |case class User(name: String, age: Int)
       |object M {
       |  val u: User = ???
       |  u.<<copy>>(<<age>> = 43)
       |}
       |""".stripMargin,
    { (_, copy, age) =>
      assertEquals(copy, "b/User#copy().")
      assertEquals(age, "b/User#copy().(age)")
    },
  )

  // Checks def macros that we can't test in expect tests because expect tests have no dependencies.
  occurrences(
    """|package e
       |import scala.meta._
       |import munit._
       |object x extends FunSuite {
       |  val x = q"Foo"
       |  val y = q"Bar"
       |  val z = q"$x + $y"
       |  val k = sourcecode.Name.generate
       |  assert(x.value == "Foo")
       |}
       |""".stripMargin,
    """|[0:8..0:9): e <= e/
       |[1:7..1:12): scala => scala/
       |[1:13..1:17): meta => scala/meta/
       |[2:7..2:12): munit => munit/
       |[3:7..3:8): x <= e/x.
       |[3:17..3:25): FunSuite => munit/FunSuite#
       |[3:25..3:25):  => munit/FunSuite#`<init>`().
       |[4:6..4:7): x <= e/x.x.
       |[4:10..4:11): q => scala/meta/internal/quasiquotes/Unlift.
       |[5:6..5:7): y <= e/x.y.
       |[5:10..5:11): q => scala/meta/internal/quasiquotes/Unlift.
       |[6:6..6:7): z <= e/x.z.
       |[6:10..6:11): q => scala/meta/internal/quasiquotes/Unlift.
       |[6:13..6:14): x => e/x.x.
       |[6:18..6:19): y => e/x.y.
       |[7:6..7:7): k <= e/x.k.
       |[7:10..7:20): sourcecode => sourcecode/
       |[7:21..7:25): Name => sourcecode/Name.
       |[7:26..7:34): generate => sourcecode/NameMacros#generate().
       |[8:2..8:8): assert => munit/Assertions#assert().
       |[8:9..8:10): x => e/x.x.
       |[8:11..8:16): value => scala/meta/Term.Name#value().
       |[8:17..8:19): == => java/lang/Object#`==`().
       |""".stripMargin,
  )

  targeted(
    """|package f
       |import scala.List
       |object an {
       |  for {
       |    i <- List.apply(1, 2)
       |    <<j>> <- List.apply(3, 4)
       |  } yield j
       |}
       |""".stripMargin,
    { (db, j) =>
      val denot = db.symbols.find(_.symbol == j).get
      assert(denot.symbol.startsWith("local"))
    },
  )

  targeted(
    """|package g
       |object ao {
       |  object <<foo>>
       |  def <<foo>>(a: Int): Unit = ()
       |  def <<foo>>(a: String): Unit = ()
       |}
       |""".stripMargin,
    (doc, foo1, foo2, foo3) => {
      assertEquals(foo1, "g/ao.foo.")
      assertEquals(foo2, "g/ao.foo().")
      assertEquals(foo3, "g/ao.foo(+1).")
    },
  )

  targeted(
    """|package h
       |object ao {
       |  val local = 1
       |  def foo(a: Int = 1, b: Int = 2, c: Int = 3): Int = a + b + c
       |  def baseCase = <<foo>>(<<local>>, <<c>> = 3)
       |}
       |""".stripMargin,
    (doc, foo1, local, c) => {
      assertEquals(foo1, "h/ao.foo().")
      assertEquals(local, "h/ao.local.")
      assertEquals(c, "h/ao.foo().(c)")
    },
  )

  targeted(
    """|package i
       |object ao {
       |  val local = 1
       |  def foo(a: Int = 1, b: Int = 2, c: Int = 3): Int = a + b + c
       |  def recursive = <<foo>>(<<local>>, <<c>> = <<foo>>(<<local>>, <<c>> = 3))
       |}
       |""".stripMargin,
    (doc, foo1, local1, c1, foo2, local2, c2) => {
      assertEquals(foo1, "i/ao.foo().")
      assertEquals(foo2, "i/ao.foo().")
      assertEquals(local1, "i/ao.local.")
      assertEquals(local2, "i/ao.local.")
      assertEquals(c1, "i/ao.foo().(c)")
      assertEquals(c2, "i/ao.foo().(c)")
    },
  )

  targeted(
    """|package j
       |object ao {
       |  case class Msg(body: String, head: String = "default", tail: String)
       |  val bodyText = "body"
       |  val msg = <<Msg>>(<<bodyText>>, tail = "tail")
       |}
       |""".stripMargin,
    (doc, msg, bodyText) => {
      assertEquals(msg, "j/ao.Msg.")
      assertEquals(bodyText, "j/ao.bodyText.")
    },
  )

  targeted(
    """|package k
       |class target {
       |  def foo(a: Int, b: Int = 1, c: Int = 2): Int = ???
       |}
       |object consumer {
       |  def unstableQual = new target
       |  unstableQual.<<foo>>(1, <<c>> = 1)
       |}
       |""".stripMargin,
    (doc, foo, c) => {
      assertEquals(foo, "k/target#foo().")
      assertEquals(c, "k/target#foo().(c)")
    },
  )

  targeted(
    """|package l
       |trait GrandParent{ def method: String }
       |trait Parent extends GrandParent{ override def method: String = "" }
       |trait Child extends Parent
       |object Max extends Child{ 
       |  override def method: String = "" 
       |  override def toString = "a"
       |}
       |
       |""".stripMargin,
    doc => {

      def overriddenSymbols(sym: String) = doc.symbols.find(_.symbol == sym)
        .map(info => info.overriddenSymbols)
      assertEquals(overriddenSymbols("l/GrandParent#method()."), Some(Nil))
      assertEquals(overriddenSymbols("l/Parent#method()."), Some(List("l/GrandParent#method().")))
      assertEquals(overriddenSymbols("l/Child#method()."), None)
      assertEquals(
        overriddenSymbols("l/Max.method()."),
        Some(List("l/Parent#method().", "l/GrandParent#method().")),
      )
      assertEquals(
        overriddenSymbols("l/Max.toString()."),
        Some(List("java/lang/Object#toString().", "scala/Any#toString().")),
      )

    },
  )

  targeted(
    """|package m
       |  import scala.languageFeature.implicitConversions
       |  object ImplicitConversion {
       |    val a: Int = 5
       |    val b: Long = <<a>>
       |    val c: Double = <<a>>
       |    val d: Float = <<a>>
       |    val toLong: Int = 42
       |    val e: Long = <<toLong>>
       |}
       |""".stripMargin,
    (_, long, double, float, toInt) => {
      assertEquals(long, "m/ImplicitConversion.a.")
      assertEquals(double, "m/ImplicitConversion.a.")
      assertEquals(float, "m/ImplicitConversion.a.")
      assertEquals(toInt, "scala/Int#toLong().")
    },
  )

  targeted(
    """|
       |  package n
       | object ForCompWithFilter {
       |  val foo: Option[(Int, Int)] = None
       |  for {
       |    (_, _) <- <<foo>>
       |    (_, _) <- <<foo>>
       |  } yield ()
       |}
       |""".stripMargin,
    (_, foo1, foo2) => {
      assertEquals(foo1, "n/ForCompWithFilter.foo.")
      assertEquals(foo2, "n/ForCompWithFilter.foo.")
    },
  )

  targeted(
    """|
       |package o
       |object O {
       |  val Some(<<a>>) = Some(123)
       |}
       |""".stripMargin,
    (_, foo1) => assertEquals(foo1, "o/O.a."),
  )

  // A `val`/`var` primary-constructor parameter defines both an accessor and the constructor
  // PARAMETER symbol, so its definition site has two overlapping occurrences (#1327). A plain
  // member (`y` below) keeps its single occurrence.
  occurrences(
    """|package p
       |class C(val x: Int) {
       |  val y: Int = x
       |}
       |""".stripMargin,
    """|[0:8..0:9): p <= p/
       |[1:6..1:7): C <= p/C#
       |[1:7..1:7):  <= p/C#`<init>`().
       |[1:12..1:13): x <= p/C#`<init>`().(x)
       |[1:12..1:13): x <= p/C#x.
       |[1:15..1:18): Int => scala/Int#
       |[2:6..2:7): y <= p/C#y.
       |[2:9..2:12): Int => scala/Int#
       |[2:15..2:16): x => p/C#x.
       |""".stripMargin,
  )

  occurrences(
    """|package q
       |case class C(xs: List[Int])
       |""".stripMargin,
    """|[0:8..0:9): q <= q/
       |[1:11..1:12): C <= q/C#
       |[1:12..1:12):  <= q/C#`<init>`().
       |[1:13..1:15): xs <= q/C#`<init>`().(xs)
       |[1:13..1:15): xs <= q/C#xs.
       |[1:17..1:21): List => scala/package.List#
       |[1:22..1:25): Int => scala/Int#
       |""".stripMargin,
  )

  // meta-annotations (#2085)
  occurrences(
    """|package t
       |import scala.annotation.meta.field
       |class C1(@(deprecated @field) val x: Int)
       |class C2(@(deprecated @field)("m", "s") val x: Int)
       |""".stripMargin,
    """|[0:8..0:9): t <= t/
       |[1:7..1:12): scala => scala/
       |[1:13..1:23): annotation => scala/annotation/
       |[1:24..1:28): meta => scala/annotation/meta/
       |[1:29..1:34): field => scala/annotation/meta/field#
       |[2:6..2:8): C1 <= t/C1#
       |[2:8..2:8):  <= t/C1#`<init>`().
       |[2:11..2:21): deprecated => scala/deprecated#
       |[2:23..2:28): field => scala/annotation/meta/field#
       |[2:28..2:28):  => scala/annotation/meta/field#`<init>`().
       |[2:29..2:29):  => scala/deprecated#`<init>`().
       |[2:34..2:35): x <= t/C1#`<init>`().(x)
       |[2:34..2:35): x <= t/C1#x.
       |[2:37..2:40): Int => scala/Int#
       |[3:6..3:8): C2 <= t/C2#
       |[3:8..3:8):  <= t/C2#`<init>`().
       |[3:11..3:21): deprecated => scala/deprecated#
       |[3:23..3:28): field => scala/annotation/meta/field#
       |[3:28..3:28):  => scala/annotation/meta/field#`<init>`().
       |[3:29..3:29):  => scala/deprecated#`<init>`().
       |[3:44..3:45): x <= t/C2#`<init>`().(x)
       |[3:44..3:45): x <= t/C2#x.
       |[3:47..3:50): Int => scala/Int#
       |""".stripMargin,
  )

  // annotated types (#2085)
  occurrences(
    """|package u
       |class C[T <: AnyRef @unchecked] {
       |  val a: Int @unchecked = 1
       |  def b(x: Option[Int @unchecked]): Unit = ()
       |  val c = (1: @unchecked)
       |  val d = new (Object @unchecked)
       |}
       |""".stripMargin,
    """|[0:8..0:9): u <= u/
       |[1:6..1:7): C <= u/C#
       |[1:8..1:9): T <= u/C#[T]
       |[1:13..1:19): AnyRef => scala/AnyRef#
       |[1:21..1:30): unchecked => scala/unchecked#
       |[1:30..1:30):  => scala/unchecked#`<init>`().
       |[1:32..1:32):  <= u/C#`<init>`().
       |[2:6..2:7): a <= u/C#a.
       |[2:9..2:12): Int => scala/Int#
       |[2:14..2:23): unchecked => scala/unchecked#
       |[2:23..2:23):  => scala/unchecked#`<init>`().
       |[3:6..3:7): b <= u/C#b().
       |[3:8..3:9): x <= u/C#b().(x)
       |[3:11..3:17): Option => scala/Option#
       |[3:18..3:21): Int => scala/Int#
       |[3:23..3:32): unchecked => scala/unchecked#
       |[3:32..3:32):  => scala/unchecked#`<init>`().
       |[3:36..3:40): Unit => scala/Unit#
       |[4:6..4:7): c <= u/C#c.
       |[4:15..4:24): unchecked => scala/unchecked#
       |[4:24..4:24):  => scala/unchecked#`<init>`().
       |[5:6..5:7): d <= u/C#d.
       |[5:15..5:21): Object => java/lang/Object#
       |[5:23..5:32): unchecked => scala/unchecked#
       |[5:32..5:32):  => scala/unchecked#`<init>`().
       |[5:33..5:33):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  // default arguments of qualified targets
  occurrences(
    """|package v
       |object n { class D(x: Int = 1); def f(x: Int = 1) = x }
       |class C { new n.D(); n.f(); v.n.f() }
       |""".stripMargin,
    """|[0:8..0:9): v <= v/
       |[1:7..1:8): n <= v/n.
       |[1:17..1:18): D <= v/n.D#
       |[1:18..1:18):  <= v/n.D#`<init>`().
       |[1:19..1:20): x <= v/n.D#`<init>`().(x)
       |[1:19..1:20): x <= v/n.D#x.
       |[1:22..1:25): Int => scala/Int#
       |[1:36..1:37): f <= v/n.f().
       |[1:38..1:39): x <= v/n.f().(x)
       |[1:41..1:44): Int => scala/Int#
       |[1:52..1:53): x => v/n.f().(x)
       |[2:6..2:7): C <= v/C#
       |[2:8..2:8):  <= v/C#`<init>`().
       |[2:14..2:15): n => v/n.
       |[2:16..2:17): D => v/n.D#
       |[2:17..2:17):  => v/n.D#`<init>`().
       |[2:21..2:22): n => v/n.
       |[2:23..2:24): f => v/n.f().
       |[2:28..2:29): v => v/
       |[2:30..2:31): n => v/n.
       |[2:32..2:33): f => v/n.f().
       |""".stripMargin,
  )

  // qualified and polymorphic parents (#1845, #1887)
  occurrences(
    """|package r
       |object n { class B(x: Int); class P[T](x: T) }
       |class C1 extends n.B(1)
       |class C2 extends n.P(1)
       |class C3 extends n.P[Int](1)
       |class C4 { new n.B(1); new n.P(1) }
       |class C5 extends r.n.P(1)
       |""".stripMargin,
    """|[0:8..0:9): r <= r/
       |[1:7..1:8): n <= r/n.
       |[1:17..1:18): B <= r/n.B#
       |[1:18..1:18):  <= r/n.B#`<init>`().
       |[1:19..1:20): x <= r/n.B#`<init>`().(x)
       |[1:19..1:20): x <= r/n.B#x.
       |[1:22..1:25): Int => scala/Int#
       |[1:34..1:35): P <= r/n.P#
       |[1:36..1:37): T <= r/n.P#[T]
       |[1:38..1:38):  <= r/n.P#`<init>`().
       |[1:39..1:40): x <= r/n.P#`<init>`().(x)
       |[1:39..1:40): x <= r/n.P#x.
       |[1:42..1:43): T => r/n.P#[T]
       |[2:6..2:8): C1 <= r/C1#
       |[2:9..2:9):  <= r/C1#`<init>`().
       |[2:17..2:18): n => r/n.
       |[2:19..2:20): B => r/n.B#
       |[2:20..2:20):  => r/n.B#`<init>`().
       |[3:6..3:8): C2 <= r/C2#
       |[3:9..3:9):  <= r/C2#`<init>`().
       |[3:17..3:18): n => r/n.
       |[3:19..3:20): P => r/n.P#
       |[3:20..3:20):  => r/n.P#`<init>`().
       |[4:6..4:8): C3 <= r/C3#
       |[4:9..4:9):  <= r/C3#`<init>`().
       |[4:17..4:18): n => r/n.
       |[4:19..4:20): P => r/n.P#
       |[4:21..4:24): Int => scala/Int#
       |[4:25..4:25):  => r/n.P#`<init>`().
       |[5:6..5:8): C4 <= r/C4#
       |[5:9..5:9):  <= r/C4#`<init>`().
       |[5:15..5:16): n => r/n.
       |[5:17..5:18): B => r/n.B#
       |[5:18..5:18):  => r/n.B#`<init>`().
       |[5:27..5:28): n => r/n.
       |[5:29..5:30): P => r/n.P#
       |[5:30..5:30):  => r/n.P#`<init>`().
       |[6:6..6:8): C5 <= r/C5#
       |[6:9..6:9):  <= r/C5#`<init>`().
       |[6:17..6:18): r => r/
       |[6:19..6:20): n => r/n.
       |[6:21..6:22): P => r/n.P#
       |[6:22..6:22):  => r/n.P#`<init>`().
       |""".stripMargin,
  )

  // unqualified parents, for comparison
  occurrences(
    """|package s
       |class B(x: Int); class P[T](x: T)
       |class C1 extends B(1)
       |class C2 extends P(1)
       |""".stripMargin,
    """|[0:8..0:9): s <= s/
       |[1:6..1:7): B <= s/B#
       |[1:7..1:7):  <= s/B#`<init>`().
       |[1:8..1:9): x <= s/B#`<init>`().(x)
       |[1:8..1:9): x <= s/B#x.
       |[1:11..1:14): Int => scala/Int#
       |[1:23..1:24): P <= s/P#
       |[1:25..1:26): T <= s/P#[T]
       |[1:27..1:27):  <= s/P#`<init>`().
       |[1:28..1:29): x <= s/P#`<init>`().(x)
       |[1:28..1:29): x <= s/P#x.
       |[1:31..1:32): T => s/P#[T]
       |[2:6..2:8): C1 <= s/C1#
       |[2:9..2:9):  <= s/C1#`<init>`().
       |[2:17..2:18): B => s/B#
       |[2:18..2:18):  => s/B#`<init>`().
       |[3:6..3:8): C2 <= s/C2#
       |[3:9..3:9):  <= s/C2#`<init>`().
       |[3:17..3:18): P => s/P#
       |[3:18..3:18):  => s/P#`<init>`().
       |""".stripMargin,
  )

  // trait parents
  occurrences(
    """|package c
       |import scala.annotation.StaticAnnotation
       |trait U
       |object o { trait V }
       |class A1 extends StaticAnnotation
       |class A2 extends scala.annotation.StaticAnnotation
       |class A3 extends U
       |class A4 extends c.U
       |class A5 extends c.o.V
       |class A6 extends c.U {}
       |""".stripMargin,
    """|[0:8..0:9): c <= c/
       |[1:7..1:12): scala => scala/
       |[1:13..1:23): annotation => scala/annotation/
       |[1:24..1:40): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:6..2:7): U <= c/U#
       |[3:7..3:8): o <= c/o.
       |[3:17..3:18): V <= c/o.V#
       |[4:6..4:8): A1 <= c/A1#
       |[4:9..4:9):  <= c/A1#`<init>`().
       |[4:17..4:33): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[4:33..4:33):  => scala/annotation/Annotation#`<init>`().
       |[5:6..5:8): A2 <= c/A2#
       |[5:9..5:9):  <= c/A2#`<init>`().
       |[5:17..5:22): scala => scala/
       |[5:23..5:33): annotation => scala/annotation/
       |[5:34..5:50): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[5:50..5:50):  => scala/annotation/Annotation#`<init>`().
       |[6:6..6:8): A3 <= c/A3#
       |[6:9..6:9):  <= c/A3#`<init>`().
       |[6:17..6:18): U => c/U#
       |[6:18..6:18):  => java/lang/Object#`<init>`().
       |[7:6..7:8): A4 <= c/A4#
       |[7:9..7:9):  <= c/A4#`<init>`().
       |[7:17..7:18): c => c/
       |[7:19..7:20): U => c/U#
       |[7:20..7:20):  => java/lang/Object#`<init>`().
       |[8:6..8:8): A5 <= c/A5#
       |[8:9..8:9):  <= c/A5#`<init>`().
       |[8:17..8:18): c => c/
       |[8:19..8:20): o => c/o.
       |[8:21..8:22): V => c/o.V#
       |[8:22..8:22):  => java/lang/Object#`<init>`().
       |[9:6..9:8): A6 <= c/A6#
       |[9:9..9:9):  <= c/A6#`<init>`().
       |[9:17..9:18): c => c/
       |[9:19..9:20): U => c/U#
       |[9:20..9:20):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  // a trait parent on the last line
  occurrences(
    """|package d
       |trait U
       |class A1 extends U
       |""".stripMargin,
    """|[0:8..0:9): d <= d/
       |[1:6..1:7): U <= d/U#
       |[2:6..2:8): A1 <= d/A1#
       |[2:9..2:9):  <= d/A1#`<init>`().
       |[2:17..2:18): U => d/U#
       |[2:18..2:18):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  // a qualified trait parent on the last line
  occurrences(
    """|package y
       |trait U
       |class A1 extends y.U
       |""".stripMargin,
    """|[0:8..0:9): y <= y/
       |[1:6..1:7): U <= y/U#
       |[2:6..2:8): A1 <= y/A1#
       |[2:9..2:9):  <= y/A1#`<init>`().
       |[2:17..2:18): y => y/
       |[2:19..2:20): U => y/U#
       |[2:20..2:20):  => java/lang/Object#`<init>`().
       |""".stripMargin,
  )

  test("named-args") {
    val code =
      """|@deprecated(since = "123")
         |class CLS(name: String) {
         |  def this(name2: Int) = this(name = name2.toString)
         |}
         |""".stripMargin
    val expected =
      """|Summary:
         |Schema => SemanticDB v4
         |Text => non-empty
         |Language => Scala
         |Symbols => 6 entries
         |Occurrences => 15 entries
         |
         |Symbols:
         |_empty_/CLS# => @deprecated class CLS extends AnyRef { +3 decls }
         |  deprecated => scala/deprecated#
         |  AnyRef => scala/AnyRef#
         |_empty_/CLS#`<init>`(). => primary ctor <init>(name: String)
         |  name => _empty_/CLS#`<init>`().(name)
         |  String => scala/Predef.String#
         |_empty_/CLS#`<init>`().(name) => param name: String
         |  String => scala/Predef.String#
         |_empty_/CLS#`<init>`(+1). => ctor <init>(name2: Int)
         |  name2 => _empty_/CLS#`<init>`(+1).(name2)
         |  Int => scala/Int#
         |_empty_/CLS#`<init>`(+1).(name2) => param name2: Int
         |  Int => scala/Int#
         |_empty_/CLS#name. => private[this] val method name: String
         |  String => scala/Predef.String#
         |
         |Occurrences:
         |[0:1..0:11): deprecated => scala/deprecated#
         |[0:11..0:11):  => scala/deprecated#`<init>`().
         |[0:12..0:17): since => scala/deprecated#`<init>`().(since)
         |[1:6..1:9): CLS <= _empty_/CLS#
         |[1:9..1:9):  <= _empty_/CLS#`<init>`().
         |[1:10..1:14): name <= _empty_/CLS#`<init>`().(name)
         |[1:10..1:14): name <= _empty_/CLS#name.
         |[1:16..1:22): String => scala/Predef.String#
         |[2:6..2:10): this <= _empty_/CLS#`<init>`(+1).
         |[2:11..2:16): name2 <= _empty_/CLS#`<init>`(+1).(name2)
         |[2:18..2:21): Int => scala/Int#
         |[2:29..2:29):  => _empty_/CLS#`<init>`().
         |[2:30..2:34): name => _empty_/CLS#`<init>`().(name)
         |[2:37..2:42): name2 => _empty_/CLS#`<init>`(+1).(name2)
         |[2:43..2:51): toString => scala/Any#toString().
         |""".stripMargin

    val expectedPrevious213 = expected.replace("Symbols => 6 entries", "Symbols => 8 entries")
      .replace(
        """|
           |Occurrences:""".stripMargin,
        """|local0 => val local x$1: "123"
           |local1 => val local x$2: String
           |  String => scala/Predef.String#
           |
           |Occurrences:""".stripMargin,
      )

    val expected212 = expectedPrevious213
      .replace("Occurrences => 15 entries", "Occurrences => 15 entries\nDiagnostics => 1 entries")
      .replace(
        """|local0 => val local x$1: "123"
           |""".stripMargin,
        """|local0 => val local x$1: String
           |  String => java/lang/String#
           |""".stripMargin,
      ) +
      """|
         |Diagnostics:
         |[0:1..0:26) [warning] Usage of named or default arguments transformed this annotation
         |constructor call into a block. The corresponding AnnotationInfo
         |will contain references to local values and default getters instead
         |of the actual argument trees
         |""".stripMargin

    val expectedCompat = ScalaVersion.getExpected(
      compat = List(
        ScalaVersion.Full("2.13.14") -> expectedPrevious213,
        ScalaVersion.Full("2.13.15") -> expectedPrevious213,
        ScalaVersion.Full("2.13.16") -> expectedPrevious213,
        ScalaVersion.Scala212 -> expected212,
        ScalaVersion.Scala211 -> expected212,
        ScalaVersion.Scala213 -> expected,
      ),
      expected,
    )
    assertEquals(
      computePayloadFromSnippet(code).linesIterator.drop(3).filter(!_.startsWith("Uri => "))
        .mkString("", "\n", "\n"),
      expectedCompat,
    )
  }

  locally { // #3738
    val code =
      """|trait AmbiguousMend {
         |  def x
         |}
         |""".stripMargin
    test(code) {
      assertEquals(
        computePayloadFromSnippet(code).linesIterator.drop(3).filter(!_.startsWith("Uri => "))
          .mkString("", "\n", "\n"),
        """|Summary:
           |Schema => SemanticDB v4
           |Text => non-empty
           |Language => Scala
           |Symbols => 2 entries
           |Occurrences => 2 entries
           |
           |Symbols:
           |_empty_/AmbiguousMend# => trait AmbiguousMend extends AnyRef { +1 decls }
           |  AnyRef => scala/AnyRef#
           |_empty_/AmbiguousMend#x(). => abstract method x: Unit
           |  Unit => scala/Unit#
           |
           |Occurrences:
           |[0:6..0:19): AmbiguousMend <= _empty_/AmbiguousMend#
           |[1:6..1:7): x <= _empty_/AmbiguousMend#x().
           |""".stripMargin,
      )
    }
  }

}
