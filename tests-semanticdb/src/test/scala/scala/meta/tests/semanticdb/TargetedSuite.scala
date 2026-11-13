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
       |[5:6..5:7): y <= e/x.y.
       |[6:6..6:7): z <= e/x.z.
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
    (_, foo1, foo2, foo3) => {
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
    (_, foo1, local, c) => {
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
    (_, foo1, local1, c1, foo2, local2, c2) => {
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
    (_, msg, bodyText) => {
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
    (_, foo, c) => {
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

  // named arguments after a space
  occurrences(
    """|package u
       |object O {
       |  def f(x: Int) = x
       |  val y = f (x = 1)
       |  class K(x: Int)
       |  val k = new K (x = 1)
       |}
       |""".stripMargin,
    """|[0:8..0:9): u <= u/
       |[1:7..1:8): O <= u/O.
       |[2:6..2:7): f <= u/O.f().
       |[2:8..2:9): x <= u/O.f().(x)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): x => u/O.f().(x)
       |[3:6..3:7): y <= u/O.y.
       |[3:10..3:11): f => u/O.f().
       |[3:13..3:14): x => u/O.f().(x)
       |[4:8..4:9): K <= u/O.K#
       |[4:9..4:9):  <= u/O.K#`<init>`().
       |[4:10..4:11): x <= u/O.K#`<init>`().(x)
       |[4:10..4:11): x <= u/O.K#x.
       |[4:13..4:16): Int => scala/Int#
       |[5:6..5:7): k <= u/O.k.
       |[5:14..5:15): K => u/O.K#
       |[5:16..5:16):  => u/O.K#`<init>`().
       |[5:17..5:18): x => u/O.K#`<init>`().(x)
       |""".stripMargin,
  )

  checkSection(
    "named arguments of a method after each gap",
    """|package ua
       |object A {
       |  def f(x: Int) = x
       |  val a = f(x = 1)
       |  val b = f  (x = 1)
       |  val c = (f
       |    (x = 1))
       |  val d = f /* c */ (x = 1)
       |  val e = (f /* c */
       |    (x = 1))
       |}
       |""".stripMargin,
    """|[0:8..0:10): ua <= ua/
       |[1:7..1:8): A <= ua/A.
       |[2:6..2:7): f <= ua/A.f().
       |[2:8..2:9): x <= ua/A.f().(x)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): x => ua/A.f().(x)
       |[3:6..3:7): a <= ua/A.a.
       |[3:10..3:11): f => ua/A.f().
       |[3:12..3:13): x => ua/A.f().(x)
       |[4:6..4:7): b <= ua/A.b.
       |[4:10..4:11): f => ua/A.f().
       |[4:14..4:15): x => ua/A.f().(x)
       |[5:6..5:7): c <= ua/A.c.
       |[5:11..5:12): f => ua/A.f().
       |[6:5..6:6): x => ua/A.f().(x)
       |[7:6..7:7): d <= ua/A.d.
       |[7:10..7:11): f => ua/A.f().
       |[7:21..7:22): x => ua/A.f().(x)
       |[8:6..8:7): e <= ua/A.e.
       |[8:11..8:12): f => ua/A.f().
       |[9:5..9:6): x => ua/A.f().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of applies, type arguments and later clauses after a space",
    """|package ub
       |object B {
       |  object o { def apply(x: Int) = x }
       |  val a = o(x = 1)
       |  val b = o (x = 1)
       |  val c = o.apply (x = 1)
       |  def g[T](x: T) = x
       |  val d = g[Int](x = 1)
       |  val e = g[Int] (x = 1)
       |  val f = g [Int](x = 1)
       |  def h(x: Int)(y: Int) = y
       |  val i = h(1)(y = 2)
       |  val j = h(1) (y = 2)
       |  val k = h (x = 1)(y = 2)
       |  def m(x: Int) = x
       |  val l = this m (x = 1)
       |  val n = this.m (x = 1)
       |  val p = this m(x = 1)
       |}
       |""".stripMargin,
    """|[0:8..0:10): ub <= ub/
       |[1:7..1:8): B <= ub/B.
       |[2:9..2:10): o <= ub/B.o.
       |[2:17..2:22): apply <= ub/B.o.apply().
       |[2:23..2:24): x <= ub/B.o.apply().(x)
       |[2:26..2:29): Int => scala/Int#
       |[2:33..2:34): x => ub/B.o.apply().(x)
       |[3:6..3:7): a <= ub/B.a.
       |[3:10..3:11): o => ub/B.o.
       |[3:12..3:13): x => ub/B.o.apply().(x)
       |[4:6..4:7): b <= ub/B.b.
       |[4:10..4:11): o => ub/B.o.
       |[4:13..4:14): x => ub/B.o.apply().(x)
       |[5:6..5:7): c <= ub/B.c.
       |[5:10..5:11): o => ub/B.o.
       |[5:12..5:17): apply => ub/B.o.apply().
       |[5:19..5:20): x => ub/B.o.apply().(x)
       |[6:6..6:7): g <= ub/B.g().
       |[6:8..6:9): T <= ub/B.g().[T]
       |[6:11..6:12): x <= ub/B.g().(x)
       |[6:14..6:15): T => ub/B.g().[T]
       |[6:19..6:20): x => ub/B.g().(x)
       |[7:6..7:7): d <= ub/B.d.
       |[7:10..7:11): g => ub/B.g().
       |[7:12..7:15): Int => scala/Int#
       |[7:17..7:18): x => ub/B.g().(x)
       |[8:6..8:7): e <= ub/B.e.
       |[8:10..8:11): g => ub/B.g().
       |[8:12..8:15): Int => scala/Int#
       |[8:18..8:19): x => ub/B.g().(x)
       |[9:6..9:7): f <= ub/B.f.
       |[9:10..9:11): g => ub/B.g().
       |[9:13..9:16): Int => scala/Int#
       |[9:18..9:19): x => ub/B.g().(x)
       |[10:6..10:7): h <= ub/B.h().
       |[10:8..10:9): x <= ub/B.h().(x)
       |[10:11..10:14): Int => scala/Int#
       |[10:16..10:17): y <= ub/B.h().(y)
       |[10:19..10:22): Int => scala/Int#
       |[10:26..10:27): y => ub/B.h().(y)
       |[11:6..11:7): i <= ub/B.i.
       |[11:10..11:11): h => ub/B.h().
       |[11:15..11:16): y => ub/B.h().(y)
       |[12:6..12:7): j <= ub/B.j.
       |[12:10..12:11): h => ub/B.h().
       |[12:16..12:17): y => ub/B.h().(y)
       |[13:6..13:7): k <= ub/B.k.
       |[13:10..13:11): h => ub/B.h().
       |[13:13..13:14): x => ub/B.h().(x)
       |[13:20..13:21): y => ub/B.h().(y)
       |[14:6..14:7): m <= ub/B.m().
       |[14:8..14:9): x <= ub/B.m().(x)
       |[14:11..14:14): Int => scala/Int#
       |[14:18..14:19): x => ub/B.m().(x)
       |[15:6..15:7): l <= ub/B.l.
       |[15:15..15:16): m => ub/B.m().
       |[15:18..15:19): x => ub/B.m().(x)
       |[16:6..16:7): n <= ub/B.n.
       |[16:15..16:16): m => ub/B.m().
       |[16:18..16:19): x => ub/B.m().(x)
       |[17:6..17:7): p <= ub/B.p.
       |[17:15..17:16): m => ub/B.m().
       |[17:17..17:18): x => ub/B.m().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of super calls and constructors after a space",
    """|package uc
       |class P { def f(x: Int) = x }
       |class Q extends P { override def f(x: Int) = super.f(x = 1) + super.f (x = 2) }
       |class K(x: Int) { def this() = this (x = 1) }
       |class L extends K (x = 1)
       |class M extends K(x = 1)
       |class ann(x: Int) extends scala.annotation.StaticAnnotation
       |@ann (x = 1) class N
       |@ann(x = 1) class N2
       |class R extends P { def g = super [P].f(x = 1) + super[P] .f(x = 2) }
       |class G[T](x: T)
       |class H { new G[Int](x = 1); new G[Int] (x = 2) }
       |""".stripMargin,
    """|[0:8..0:10): uc <= uc/
       |[1:6..1:7): P <= uc/P#
       |[1:8..1:8):  <= uc/P#`<init>`().
       |[1:14..1:15): f <= uc/P#f().
       |[1:16..1:17): x <= uc/P#f().(x)
       |[1:19..1:22): Int => scala/Int#
       |[1:26..1:27): x => uc/P#f().(x)
       |[2:6..2:7): Q <= uc/Q#
       |[2:8..2:8):  <= uc/Q#`<init>`().
       |[2:16..2:17): P => uc/P#
       |[2:17..2:17):  => uc/P#`<init>`().
       |[2:33..2:34): f <= uc/Q#f().
       |[2:35..2:36): x <= uc/Q#f().(x)
       |[2:38..2:41): Int => scala/Int#
       |[2:45..2:45):  => uc/Q#
       |[2:50..2:50):  => uc/P#
       |[2:51..2:52): f => uc/P#f().
       |[2:53..2:54): x => uc/P#f().(x)
       |[2:60..2:61): + => scala/Int#`+`(+4).
       |[2:62..2:62):  => uc/Q#
       |[2:67..2:67):  => uc/P#
       |[2:68..2:69): f => uc/P#f().
       |[2:71..2:72): x => uc/P#f().(x)
       |[3:6..3:7): K <= uc/K#
       |[3:7..3:7):  <= uc/K#`<init>`().
       |[3:8..3:9): x <= uc/K#`<init>`().(x)
       |[3:8..3:9): x <= uc/K#x.
       |[3:11..3:14): Int => scala/Int#
       |[3:22..3:26): this <= uc/K#`<init>`(+1).
       |[3:36..3:36):  => uc/K#`<init>`().
       |[3:37..3:38): x => uc/K#`<init>`().(x)
       |[4:6..4:7): L <= uc/L#
       |[4:8..4:8):  <= uc/L#`<init>`().
       |[4:16..4:17): K => uc/K#
       |[4:18..4:18):  => uc/K#`<init>`().
       |[4:19..4:20): x => uc/K#`<init>`().(x)
       |[5:6..5:7): M <= uc/M#
       |[5:8..5:8):  <= uc/M#`<init>`().
       |[5:16..5:17): K => uc/K#
       |[5:17..5:17):  => uc/K#`<init>`().
       |[5:18..5:19): x => uc/K#`<init>`().(x)
       |[6:6..6:9): ann <= uc/ann#
       |[6:9..6:9):  <= uc/ann#`<init>`().
       |[6:10..6:11): x <= uc/ann#`<init>`().(x)
       |[6:10..6:11): x <= uc/ann#x.
       |[6:13..6:16): Int => scala/Int#
       |[6:26..6:31): scala => scala/
       |[6:32..6:42): annotation => scala/annotation/
       |[6:43..6:59): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[6:59..6:59):  => scala/annotation/Annotation#`<init>`().
       |[7:1..7:4): ann => uc/ann#
       |[7:5..7:5):  => uc/ann#`<init>`().
       |[7:6..7:7): x => uc/ann#`<init>`().(x)
       |[7:19..7:20): N <= uc/N#
       |[7:20..7:20):  <= uc/N#`<init>`().
       |[8:1..8:4): ann => uc/ann#
       |[8:4..8:4):  => uc/ann#`<init>`().
       |[8:5..8:6): x => uc/ann#`<init>`().(x)
       |[8:18..8:20): N2 <= uc/N2#
       |[8:20..8:20):  <= uc/N2#`<init>`().
       |[9:6..9:7): R <= uc/R#
       |[9:8..9:8):  <= uc/R#`<init>`().
       |[9:16..9:17): P => uc/P#
       |[9:17..9:17):  => uc/P#`<init>`().
       |[9:24..9:25): g <= uc/R#g().
       |[9:28..9:28):  => uc/R#
       |[9:35..9:36): P => uc/P#
       |[9:38..9:39): f => uc/P#f().
       |[9:40..9:41): x => uc/P#f().(x)
       |[9:47..9:48): + => scala/Int#`+`(+4).
       |[9:49..9:49):  => uc/R#
       |[9:55..9:56): P => uc/P#
       |[9:59..9:60): f => uc/P#f().
       |[9:61..9:62): x => uc/P#f().(x)
       |[10:6..10:7): G <= uc/G#
       |[10:8..10:9): T <= uc/G#[T]
       |[10:10..10:10):  <= uc/G#`<init>`().
       |[10:11..10:12): x <= uc/G#`<init>`().(x)
       |[10:11..10:12): x <= uc/G#x.
       |[10:14..10:15): T => uc/G#[T]
       |[11:6..11:7): H <= uc/H#
       |[11:8..11:8):  <= uc/H#`<init>`().
       |[11:14..11:15): G => uc/G#
       |[11:16..11:19): Int => scala/Int#
       |[11:20..11:20):  => uc/G#`<init>`().
       |[11:21..11:22): x => uc/G#`<init>`().(x)
       |[11:33..11:34): G => uc/G#
       |[11:35..11:38): Int => scala/Int#
       |[11:40..11:40):  => uc/G#`<init>`().
       |[11:41..11:42): x => uc/G#`<init>`().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of copy, defaults, varargs and a qualifier after a space",
    """|package ud
       |case class C(x: Int, y: Int)
       |object D {
       |  val c = C(1, 2)
       |  val a = c.copy(y = 3)
       |  val b = c.copy (y = 3)
       |  def d(x: Int = 0, y: Int = 0) = x
       |  val e = d(y = 1)
       |  val f = d (y = 1)
       |  def v(x: Int, ys: Int*) = x
       |  val g = v(x = 1, 2)
       |  val h = v (x = 1, 2)
       |  def mk() = this
       |  def m(x: Int, y: Int) = x
       |  val i = mk().m(y = 1, x = 2)
       |  val j = mk().m (y = 1, x = 2)
       |  val k = mk() .m (y = 1, x = 2)
       |}
       |""".stripMargin,
    """|[0:8..0:10): ud <= ud/
       |[1:11..1:12): C <= ud/C#
       |[1:12..1:12):  <= ud/C#`<init>`().
       |[1:13..1:14): x <= ud/C#`<init>`().(x)
       |[1:13..1:14): x <= ud/C#x.
       |[1:16..1:19): Int => scala/Int#
       |[1:21..1:22): y <= ud/C#`<init>`().(y)
       |[1:21..1:22): y <= ud/C#y.
       |[1:24..1:27): Int => scala/Int#
       |[2:7..2:8): D <= ud/D.
       |[3:6..3:7): c <= ud/D.c.
       |[3:10..3:11): C => ud/C.
       |[4:6..4:7): a <= ud/D.a.
       |[4:10..4:11): c => ud/D.c.
       |[4:12..4:16): copy => ud/C#copy().
       |[4:17..4:18): y => ud/C#copy().(y)
       |[5:6..5:7): b <= ud/D.b.
       |[5:10..5:11): c => ud/D.c.
       |[5:12..5:16): copy => ud/C#copy().
       |[5:18..5:19): y => ud/C#copy().(y)
       |[6:6..6:7): d <= ud/D.d().
       |[6:8..6:9): x <= ud/D.d().(x)
       |[6:11..6:14): Int => scala/Int#
       |[6:20..6:21): y <= ud/D.d().(y)
       |[6:23..6:26): Int => scala/Int#
       |[6:34..6:35): x => ud/D.d().(x)
       |[7:6..7:7): e <= ud/D.e.
       |[7:10..7:11): d => ud/D.d().
       |[7:12..7:13): y => ud/D.d().(y)
       |[8:6..8:7): f <= ud/D.f.
       |[8:10..8:11): d => ud/D.d().
       |[8:13..8:14): y => ud/D.d().(y)
       |[9:6..9:7): v <= ud/D.v().
       |[9:8..9:9): x <= ud/D.v().(x)
       |[9:11..9:14): Int => scala/Int#
       |[9:16..9:18): ys <= ud/D.v().(ys)
       |[9:20..9:23): Int => scala/Int#
       |[9:28..9:29): x => ud/D.v().(x)
       |[10:6..10:7): g <= ud/D.g.
       |[10:10..10:11): v => ud/D.v().
       |[10:12..10:13): x => ud/D.v().(x)
       |[11:6..11:7): h <= ud/D.h.
       |[11:10..11:11): v => ud/D.v().
       |[11:13..11:14): x => ud/D.v().(x)
       |[12:6..12:8): mk <= ud/D.mk().
       |[13:6..13:7): m <= ud/D.m().
       |[13:8..13:9): x <= ud/D.m().(x)
       |[13:11..13:14): Int => scala/Int#
       |[13:16..13:17): y <= ud/D.m().(y)
       |[13:19..13:22): Int => scala/Int#
       |[13:26..13:27): x => ud/D.m().(x)
       |[14:6..14:7): i <= ud/D.i.
       |[14:10..14:12): mk => ud/D.mk().
       |[14:15..14:16): m => ud/D.m().
       |[14:17..14:18): y => ud/D.m().(y)
       |[14:24..14:25): x => ud/D.m().(x)
       |[15:6..15:7): j <= ud/D.j.
       |[15:10..15:12): mk => ud/D.mk().
       |[15:15..15:16): m => ud/D.m().
       |[15:18..15:19): y => ud/D.m().(y)
       |[15:25..15:26): x => ud/D.m().(x)
       |[16:6..16:7): k <= ud/D.k.
       |[16:10..16:12): mk => ud/D.mk().
       |[16:16..16:17): m => ud/D.m().
       |[16:19..16:20): y => ud/D.m().(y)
       |[16:26..16:27): x => ud/D.m().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "assignments in a block argument",
    """|package ue
       |object E {
       |  def g(x: Unit) = x
       |  var x = 0
       |  val a = g {x = 1}
       |  val b = g { x = 1 }
       |  val c = g ({ x = 1 })
       |}
       |""".stripMargin,
    """|[0:8..0:10): ue <= ue/
       |[1:7..1:8): E <= ue/E.
       |[2:6..2:7): g <= ue/E.g().
       |[2:8..2:9): x <= ue/E.g().(x)
       |[2:11..2:15): Unit => scala/Unit#
       |[2:19..2:20): x => ue/E.g().(x)
       |[3:6..3:7): x <= ue/E.x().
       |[4:6..4:7): a <= ue/E.a.
       |[4:10..4:11): g => ue/E.g().
       |[4:13..4:14): x => ue/E.`x_=`().
       |[5:6..5:7): b <= ue/E.b.
       |[5:10..5:11): g => ue/E.g().
       |[5:14..5:15): x => ue/E.`x_=`().
       |[6:6..6:7): c <= ue/E.c.
       |[6:10..6:11): g => ue/E.g().
       |[6:15..6:16): x => ue/E.`x_=`().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an empty List after a space",
    """|package uf
       |object F {
       |  val a = List()
       |  val b = List ()
       |  val c = List [Int]()
       |  val d = List[Int] ()
       |}
       |""".stripMargin,
    """|[0:8..0:10): uf <= uf/
       |[1:7..1:8): F <= uf/F.
       |[2:6..2:7): a <= uf/F.a.
       |[2:10..2:14): List => scala/collection/immutable/Nil.
       |[3:6..3:7): b <= uf/F.b.
       |[3:10..3:14): List => scala/collection/immutable/Nil.
       |[4:6..4:7): c <= uf/F.c.
       |[4:10..4:14): List => scala/collection/immutable/Nil.
       |[4:16..4:19): Int => scala/Int#
       |[5:6..5:7): d <= uf/F.d.
       |[5:10..5:14): List => scala/collection/immutable/Nil.
       |[5:15..5:18): Int => scala/Int#
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an apply of an object after a space",
    """|package ug
       |object G {
       |  object o { def apply(x: Int) = x }
       |  val a = o(1)
       |  val b = o (1)
       |  val c = o (x = 1)
       |  def g[T](x: T) = x
       |  val d = g (1)
       |  val e = g[Int] (1)
       |}
       |""".stripMargin,
    """|[3:10..3:11): o => *.apply
       |  apply => ug/G.o.apply().
       |[4:10..4:11): o => *.apply
       |  apply => ug/G.o.apply().
       |[5:10..5:11): o => *.apply
       |  apply => ug/G.o.apply().
       |[7:10..7:11): g => *[Int]
       |  Int => scala/Int#
       |""".stripMargin,
    "Synthetics",
  )

  private val conversionOfNamedArguments =
    """|package uh
       |import scala.language.implicitConversions
       |object H {
       |  class S(val v: Int)
       |  implicit def conv(x: Int): S = new S(x)
       |  def f(x: Int) = x
       |  val a: S = f(x = 1)
       |  val b: S = f (x = 1)
       |  class T(val v: Int)
       |  implicit def toT(y: Int): T = new T(y)
       |  val c: T = f(x = 1)
       |  def g(z: Int) = z
       |  val d: S = g(z = 1)
       |}
       |""".stripMargin

  checkSection(
    "an implicit conversion of named arguments with a parameter of the same name",
    conversionOfNamedArguments,
    """|[0:8..0:10): uh <= uh/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:41): implicitConversions => scala/language.implicitConversions.
       |[2:7..2:8): H <= uh/H.
       |[3:8..3:9): S <= uh/H.S#
       |[3:9..3:9):  <= uh/H.S#`<init>`().
       |[3:14..3:15): v <= uh/H.S#`<init>`().(v)
       |[3:14..3:15): v <= uh/H.S#v.
       |[3:17..3:20): Int => scala/Int#
       |[4:15..4:19): conv <= uh/H.conv().
       |[4:20..4:21): x <= uh/H.conv().(x)
       |[4:23..4:26): Int => scala/Int#
       |[4:29..4:30): S => uh/H.S#
       |[4:37..4:38): S => uh/H.S#
       |[4:38..4:38):  => uh/H.S#`<init>`().
       |[4:39..4:40): x => uh/H.conv().(x)
       |[5:6..5:7): f <= uh/H.f().
       |[5:8..5:9): x <= uh/H.f().(x)
       |[5:11..5:14): Int => scala/Int#
       |[5:18..5:19): x => uh/H.f().(x)
       |[6:6..6:7): a <= uh/H.a.
       |[6:9..6:10): S => uh/H.S#
       |[6:13..6:14): f => uh/H.f().
       |[6:15..6:16): x => uh/H.f().(x)
       |[7:6..7:7): b <= uh/H.b.
       |[7:9..7:10): S => uh/H.S#
       |[7:13..7:14): f => uh/H.f().
       |[7:16..7:17): x => uh/H.f().(x)
       |[8:8..8:9): T <= uh/H.T#
       |[8:9..8:9):  <= uh/H.T#`<init>`().
       |[8:14..8:15): v <= uh/H.T#`<init>`().(v)
       |[8:14..8:15): v <= uh/H.T#v.
       |[8:17..8:20): Int => scala/Int#
       |[9:15..9:18): toT <= uh/H.toT().
       |[9:19..9:20): y <= uh/H.toT().(y)
       |[9:22..9:25): Int => scala/Int#
       |[9:28..9:29): T => uh/H.T#
       |[9:36..9:37): T => uh/H.T#
       |[9:37..9:37):  => uh/H.T#`<init>`().
       |[9:38..9:39): y => uh/H.toT().(y)
       |[10:6..10:7): c <= uh/H.c.
       |[10:9..10:10): T => uh/H.T#
       |[10:13..10:14): f => uh/H.f().
       |[10:15..10:16): x => uh/H.f().(x)
       |[11:6..11:7): g <= uh/H.g().
       |[11:8..11:9): z <= uh/H.g().(z)
       |[11:11..11:14): Int => scala/Int#
       |[11:18..11:19): z => uh/H.g().(z)
       |[12:6..12:7): d <= uh/H.d.
       |[12:9..12:10): S => uh/H.S#
       |[12:13..12:14): g => uh/H.g().
       |[12:15..12:16): z => uh/H.g().(z)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an implicit conversion of named arguments with a parameter of the same name: synthetics",
    conversionOfNamedArguments,
    """|[6:13..6:21): f(x = 1) => conv(*)
       |  conv => uh/H.conv().
       |[7:13..7:22): f (x = 1) => conv(*)
       |  conv => uh/H.conv().
       |[10:13..10:21): f(x = 1) => toT(*)
       |  toT => uh/H.toT().
       |[12:13..12:21): g(z = 1) => conv(*)
       |  conv => uh/H.conv().
       |""".stripMargin,
    "Synthetics",
  )

  checkSection(
    "named arguments of a second constructor clause after a space",
    """|package ui
       |class K(x: Int)(y: Int)
       |object I {
       |  val a = new K(1)(y = 2)
       |  val b = new K(1) (y = 2)
       |  val c = new K (x = 1) (y = 2)
       |}
       |class L extends K(1)(y = 2)
       |class M extends K(1) (y = 2)
       |""".stripMargin,
    """|[0:8..0:10): ui <= ui/
       |[1:6..1:7): K <= ui/K#
       |[1:7..1:7):  <= ui/K#`<init>`().
       |[1:8..1:9): x <= ui/K#`<init>`().(x)
       |[1:8..1:9): x <= ui/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[1:16..1:17): y <= ui/K#`<init>`().(y)
       |[1:16..1:17): y <= ui/K#y.
       |[1:19..1:22): Int => scala/Int#
       |[2:7..2:8): I <= ui/I.
       |[3:6..3:7): a <= ui/I.a.
       |[3:14..3:15): K => ui/K#
       |[3:15..3:15):  => ui/K#`<init>`().
       |[3:19..3:20): y => ui/K#`<init>`().(y)
       |[4:6..4:7): b <= ui/I.b.
       |[4:14..4:15): K => ui/K#
       |[4:15..4:15):  => ui/K#`<init>`().
       |[4:20..4:21): y => ui/K#`<init>`().(y)
       |[5:6..5:7): c <= ui/I.c.
       |[5:14..5:15): K => ui/K#
       |[5:16..5:16):  => ui/K#`<init>`().
       |[5:17..5:18): x => ui/K#`<init>`().(x)
       |[5:25..5:26): y => ui/K#`<init>`().(y)
       |[7:6..7:7): L <= ui/L#
       |[7:8..7:8):  <= ui/L#`<init>`().
       |[7:16..7:17): K => ui/K#
       |[7:17..7:17):  => ui/K#`<init>`().
       |[7:21..7:22): y => ui/K#`<init>`().(y)
       |[8:6..8:7): M <= ui/M#
       |[8:8..8:8):  <= ui/M#`<init>`().
       |[8:16..8:17): K => ui/K#
       |[8:17..8:17):  => ui/K#`<init>`().
       |[8:22..8:23): y => ui/K#`<init>`().(y)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an infix call with type arguments",
    """|package uj
       |object J {
       |  def m[T](x: T, y: T) = x
       |  val a = this m[Int] (y = 1, x = 2)
       |  val b = this m[Int](y = 1, x = 2)
       |  val c = this.m[Int](y = 1, x = 2)
       |  val d = this.m[Int] (y = 1, x = 2)
       |}
       |""".stripMargin,
    """|[0:8..0:10): uj <= uj/
       |[1:7..1:8): J <= uj/J.
       |[2:6..2:7): m <= uj/J.m().
       |[2:8..2:9): T <= uj/J.m().[T]
       |[2:11..2:12): x <= uj/J.m().(x)
       |[2:14..2:15): T => uj/J.m().[T]
       |[2:17..2:18): y <= uj/J.m().(y)
       |[2:20..2:21): T => uj/J.m().[T]
       |[2:25..2:26): x => uj/J.m().(x)
       |[3:6..3:7): a <= uj/J.a.
       |[3:15..3:16): m => uj/J.m().
       |[3:17..3:20): Int => scala/Int#
       |[3:23..3:24): y => uj/J.m().(y)
       |[3:30..3:31): x => uj/J.m().(x)
       |[4:6..4:7): b <= uj/J.b.
       |[4:15..4:16): m => uj/J.m().
       |[4:17..4:20): Int => scala/Int#
       |[4:22..4:23): y => uj/J.m().(y)
       |[4:29..4:30): x => uj/J.m().(x)
       |[5:6..5:7): c <= uj/J.c.
       |[5:15..5:16): m => uj/J.m().
       |[5:17..5:20): Int => scala/Int#
       |[5:22..5:23): y => uj/J.m().(y)
       |[5:29..5:30): x => uj/J.m().(x)
       |[6:6..6:7): d <= uj/J.d.
       |[6:15..6:16): m => uj/J.m().
       |[6:17..6:20): Int => scala/Int#
       |[6:23..6:24): y => uj/J.m().(y)
       |[6:30..6:31): x => uj/J.m().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments out of order with defaults",
    """|package uk
       |object K {
       |  def mk() = this
       |  def m(x: Int, y: Int = 0, z: Int = 0) = x
       |  val a = m(z = 1, x = 2)
       |  val b = m (z = 1, x = 2)
       |  val c = mk().m(z = 1, x = 2)
       |  val d = mk().m (z = 1, x = 2)
       |  val e = mk().m(x = 2)
       |  val f = mk().m (x = 2)
       |}
       |""".stripMargin,
    """|[0:8..0:10): uk <= uk/
       |[1:7..1:8): K <= uk/K.
       |[2:6..2:8): mk <= uk/K.mk().
       |[3:6..3:7): m <= uk/K.m().
       |[3:8..3:9): x <= uk/K.m().(x)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): y <= uk/K.m().(y)
       |[3:19..3:22): Int => scala/Int#
       |[3:28..3:29): z <= uk/K.m().(z)
       |[3:31..3:34): Int => scala/Int#
       |[3:42..3:43): x => uk/K.m().(x)
       |[4:6..4:7): a <= uk/K.a.
       |[4:10..4:11): m => uk/K.m().
       |[4:12..4:13): z => uk/K.m().(z)
       |[4:19..4:20): x => uk/K.m().(x)
       |[5:6..5:7): b <= uk/K.b.
       |[5:10..5:11): m => uk/K.m().
       |[5:13..5:14): z => uk/K.m().(z)
       |[5:20..5:21): x => uk/K.m().(x)
       |[6:6..6:7): c <= uk/K.c.
       |[6:10..6:12): mk => uk/K.mk().
       |[6:15..6:16): m => uk/K.m().
       |[6:17..6:18): z => uk/K.m().(z)
       |[6:24..6:25): x => uk/K.m().(x)
       |[7:6..7:7): d <= uk/K.d.
       |[7:10..7:12): mk => uk/K.mk().
       |[7:15..7:16): m => uk/K.m().
       |[7:18..7:19): z => uk/K.m().(z)
       |[7:25..7:26): x => uk/K.m().(x)
       |[8:6..8:7): e <= uk/K.e.
       |[8:10..8:12): mk => uk/K.mk().
       |[8:15..8:16): m => uk/K.m().
       |[8:17..8:18): x => uk/K.m().(x)
       |[9:6..9:7): f <= uk/K.f.
       |[9:10..9:12): mk => uk/K.mk().
       |[9:15..9:16): m => uk/K.m().
       |[9:18..9:19): x => uk/K.m().(x)
       |""".stripMargin,
    "Occurrences",
  )

  private val implicitArgumentsNextToNamedArguments =
    """|package ul
       |object L {
       |  implicit val i: Int = 0
       |  def f(x: Int)(implicit y: Int) = x
       |  val a = f(x = 1)
       |  val b = f (x = 1)
       |  val c = f(x = 1)(y = 2)
       |  val d = f(x = 1) (y = 2)
       |}
       |""".stripMargin

  checkSection(
    "named arguments next to implicit arguments",
    implicitArgumentsNextToNamedArguments,
    """|[0:8..0:10): ul <= ul/
       |[1:7..1:8): L <= ul/L.
       |[2:15..2:16): i <= ul/L.i.
       |[2:18..2:21): Int => scala/Int#
       |[3:6..3:7): f <= ul/L.f().
       |[3:8..3:9): x <= ul/L.f().(x)
       |[3:11..3:14): Int => scala/Int#
       |[3:25..3:26): y <= ul/L.f().(y)
       |[3:28..3:31): Int => scala/Int#
       |[3:35..3:36): x => ul/L.f().(x)
       |[4:6..4:7): a <= ul/L.a.
       |[4:10..4:11): f => ul/L.f().
       |[4:12..4:13): x => ul/L.f().(x)
       |[5:6..5:7): b <= ul/L.b.
       |[5:10..5:11): f => ul/L.f().
       |[5:13..5:14): x => ul/L.f().(x)
       |[6:6..6:7): c <= ul/L.c.
       |[6:10..6:11): f => ul/L.f().
       |[6:12..6:13): x => ul/L.f().(x)
       |[6:19..6:20): y => ul/L.f().(y)
       |[7:6..7:7): d <= ul/L.d.
       |[7:10..7:11): f => ul/L.f().
       |[7:12..7:13): x => ul/L.f().(x)
       |[7:20..7:21): y => ul/L.f().(y)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments next to implicit arguments: synthetics",
    implicitArgumentsNextToNamedArguments,
    """|[4:10..4:18): f(x = 1) => *(i)
       |  i => ul/L.i.
       |[5:10..5:19): f (x = 1) => *(i)
       |  i => ul/L.i.
       |""".stripMargin,
    "Synthetics",
  )

  checkSection(
    "named arguments of a call that ends a longer call",
    """|package um
       |object M {
       |  def g(y: Int) = y
       |  def h(x: Int) = x
       |  def p(f: Int) = f
       |  def m(y: Int) = y
       |  def v_=(y: Int) = ()
       |  def v = 0
       |  val a = 1 -> g(y = 2)
       |  val b = 1 -> g (y = 2)
       |  val c = this m g(y = 2)
       |  val d = this m g (y = 2)
       |  val arr = Array(0)
       |  arr(0) = h(x = 1)
       |  arr(0) = h (x = 1)
       |  arr(0) += h(x = 1)
       |  arr(0) += h (x = 1)
       |  this.v = g(y = 2)
       |  this.v = g (y = 2)
       |  var w = 0
       |  w += h(x = 1)
       |  w += h (x = 1)
       |  val e = for (a <- Option(1)) yield p(f = a)
       |  val f = for (a <- Option(1)) yield p (f = a)
       |  for (a <- Option(1)) p(f = a)
       |  for (a <- Option(1)) p (f = a)
       |}
       |""".stripMargin,
    """|[0:8..0:10): um <= um/
       |[1:7..1:8): M <= um/M.
       |[2:6..2:7): g <= um/M.g().
       |[2:8..2:9): y <= um/M.g().(y)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): y => um/M.g().(y)
       |[3:6..3:7): h <= um/M.h().
       |[3:8..3:9): x <= um/M.h().(x)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): x => um/M.h().(x)
       |[4:6..4:7): p <= um/M.p().
       |[4:8..4:9): f <= um/M.p().(f)
       |[4:11..4:14): Int => scala/Int#
       |[4:18..4:19): f => um/M.p().(f)
       |[5:6..5:7): m <= um/M.m().
       |[5:8..5:9): y <= um/M.m().(y)
       |[5:11..5:14): Int => scala/Int#
       |[5:18..5:19): y => um/M.m().(y)
       |[6:6..6:9): v_= <= um/M.`v_=`().
       |[6:10..6:11): y <= um/M.`v_=`().(y)
       |[6:13..6:16): Int => scala/Int#
       |[7:6..7:7): v <= um/M.v().
       |[8:6..8:7): a <= um/M.a.
       |[8:12..8:14): -> => scala/Predef.ArrowAssoc#`->`().
       |[8:15..8:16): g => um/M.g().
       |[8:17..8:18): y => um/M.g().(y)
       |[9:6..9:7): b <= um/M.b.
       |[9:12..9:14): -> => scala/Predef.ArrowAssoc#`->`().
       |[9:15..9:16): g => um/M.g().
       |[9:18..9:19): y => um/M.g().(y)
       |[10:6..10:7): c <= um/M.c.
       |[10:15..10:16): m => um/M.m().
       |[10:17..10:18): g => um/M.g().
       |[10:19..10:20): y => um/M.g().(y)
       |[11:6..11:7): d <= um/M.d.
       |[11:15..11:16): m => um/M.m().
       |[11:17..11:18): g => um/M.g().
       |[11:20..11:21): y => um/M.g().(y)
       |[12:6..12:9): arr <= um/M.arr.
       |[12:12..12:17): Array => scala/Array.
       |[13:2..13:5): arr => um/M.arr.
       |[13:11..13:12): h => um/M.h().
       |[13:13..13:14): x => um/M.h().(x)
       |[14:2..14:5): arr => um/M.arr.
       |[14:11..14:12): h => um/M.h().
       |[14:14..14:15): x => um/M.h().(x)
       |[15:2..15:5): arr => um/M.arr.
       |[15:9..15:11): += => scala/Int#`+`(+4).
       |[15:12..15:13): h => um/M.h().
       |[15:14..15:15): x => um/M.h().(x)
       |[16:2..16:5): arr => um/M.arr.
       |[16:9..16:11): += => scala/Int#`+`(+4).
       |[16:12..16:13): h => um/M.h().
       |[16:15..16:16): x => um/M.h().(x)
       |[17:7..17:8): v => um/M.`v_=`().
       |[17:11..17:12): g => um/M.g().
       |[17:13..17:14): y => um/M.g().(y)
       |[18:7..18:8): v => um/M.`v_=`().
       |[18:11..18:12): g => um/M.g().
       |[18:14..18:15): y => um/M.g().(y)
       |[19:6..19:7): w <= um/M.w().
       |[20:2..20:3): w => um/M.`w_=`().
       |[20:4..20:6): += => scala/Int#`+`(+4).
       |[20:7..20:8): h => um/M.h().
       |[20:9..20:10): x => um/M.h().(x)
       |[21:2..21:3): w => um/M.`w_=`().
       |[21:4..21:6): += => scala/Int#`+`(+4).
       |[21:7..21:8): h => um/M.h().
       |[21:10..21:11): x => um/M.h().(x)
       |[22:6..22:7): e <= um/M.e.
       |[22:15..22:16): a <= local0
       |[22:20..22:26): Option => scala/Option.
       |[22:37..22:38): p => um/M.p().
       |[22:39..22:40): f => um/M.p().(f)
       |[22:43..22:44): a => local0
       |[23:6..23:7): f <= um/M.f.
       |[23:15..23:16): a <= local1
       |[23:20..23:26): Option => scala/Option.
       |[23:37..23:38): p => um/M.p().
       |[23:40..23:41): f => um/M.p().(f)
       |[23:44..23:45): a => local1
       |[24:7..24:8): a <= local2
       |[24:12..24:18): Option => scala/Option.
       |[24:23..24:24): p => um/M.p().
       |[24:25..24:26): f => um/M.p().(f)
       |[24:29..24:30): a => local2
       |[25:7..25:8): a <= local3
       |[25:12..25:18): Option => scala/Option.
       |[25:23..25:24): p => um/M.p().
       |[25:26..25:27): f => um/M.p().(f)
       |[25:30..25:31): a => local3
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an inserted apply with inferred type arguments",
    """|package un
       |object P { def apply[T](z: T) = z }
       |object N {
       |  def k(z: Int) = P
       |  val a = k(z = 1)(z = 2)
       |  val b = k (z = 1)(z = 2)
       |  val c = k(z = 1) (z = 2)
       |  val d = k(z = 1).apply(z = 2)
       |}
       |""".stripMargin,
    """|[0:8..0:10): un <= un/
       |[1:7..1:8): P <= un/P.
       |[1:15..1:20): apply <= un/P.apply().
       |[1:21..1:22): T <= un/P.apply().[T]
       |[1:24..1:25): z <= un/P.apply().(z)
       |[1:27..1:28): T => un/P.apply().[T]
       |[1:32..1:33): z => un/P.apply().(z)
       |[2:7..2:8): N <= un/N.
       |[3:6..3:7): k <= un/N.k().
       |[3:8..3:9): z <= un/N.k().(z)
       |[3:11..3:14): Int => scala/Int#
       |[3:18..3:19): P => un/P.
       |[4:6..4:7): a <= un/N.a.
       |[4:10..4:11): k => un/N.k().
       |[4:12..4:13): z => un/N.k().(z)
       |[4:19..4:20): z => un/P.apply().(z)
       |[5:6..5:7): b <= un/N.b.
       |[5:10..5:11): k => un/N.k().
       |[5:13..5:14): z => un/N.k().(z)
       |[5:20..5:21): z => un/P.apply().(z)
       |[6:6..6:7): c <= un/N.c.
       |[6:10..6:11): k => un/N.k().
       |[6:12..6:13): z => un/N.k().(z)
       |[6:20..6:21): z => un/P.apply().(z)
       |[7:6..7:7): d <= un/N.d.
       |[7:10..7:11): k => un/N.k().
       |[7:12..7:13): z => un/N.k().(z)
       |[7:19..7:24): apply => un/P.apply().
       |[7:25..7:26): z => un/P.apply().(z)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a qualifier of an implicit view",
    """|package uo
       |import scala.language.implicitConversions
       |object O {
       |  class E { def ext = 1 }
       |  implicit def conv(x: Int): E = new E
       |  def mk(x: Int) = x
       |  val a = mk(x = 1).ext
       |  val b = mk (x = 1).ext
       |}
       |""".stripMargin,
    """|[0:8..0:10): uo <= uo/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:41): implicitConversions => scala/language.implicitConversions.
       |[2:7..2:8): O <= uo/O.
       |[3:8..3:9): E <= uo/O.E#
       |[3:10..3:10):  <= uo/O.E#`<init>`().
       |[3:16..3:19): ext <= uo/O.E#ext().
       |[4:15..4:19): conv <= uo/O.conv().
       |[4:20..4:21): x <= uo/O.conv().(x)
       |[4:23..4:26): Int => scala/Int#
       |[4:29..4:30): E => uo/O.E#
       |[4:37..4:38): E => uo/O.E#
       |[4:38..4:38):  => uo/O.E#`<init>`().
       |[5:6..5:8): mk <= uo/O.mk().
       |[5:9..5:10): x <= uo/O.mk().(x)
       |[5:12..5:15): Int => scala/Int#
       |[5:19..5:20): x => uo/O.mk().(x)
       |[6:6..6:7): a <= uo/O.a.
       |[6:10..6:12): mk => uo/O.mk().
       |[6:13..6:14): x => uo/O.mk().(x)
       |[6:20..6:23): ext => uo/O.E#ext().
       |[7:6..7:7): b <= uo/O.b.
       |[7:10..7:12): mk => uo/O.mk().
       |[7:14..7:15): x => uo/O.mk().(x)
       |[7:21..7:24): ext => uo/O.E#ext().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an anonymous class",
    """|package up
       |class K(x: Int)
       |class L(x: Int)(y: Int)
       |trait T
       |object P {
       |  val a = new K(x = 1) {}
       |  val b = new K (x = 1) {}
       |  val c = new K(x = 1) with T
       |  val d = new K (x = 1) with T
       |  val e = new L(x = 1)(y = 2) {}
       |  val f = new L (x = 1) (y = 2) {}
       |}
       |""".stripMargin,
    """|[0:8..0:10): up <= up/
       |[1:6..1:7): K <= up/K#
       |[1:7..1:7):  <= up/K#`<init>`().
       |[1:8..1:9): x <= up/K#`<init>`().(x)
       |[1:8..1:9): x <= up/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[2:6..2:7): L <= up/L#
       |[2:7..2:7):  <= up/L#`<init>`().
       |[2:8..2:9): x <= up/L#`<init>`().(x)
       |[2:8..2:9): x <= up/L#x.
       |[2:11..2:14): Int => scala/Int#
       |[2:16..2:17): y <= up/L#`<init>`().(y)
       |[2:16..2:17): y <= up/L#y.
       |[2:19..2:22): Int => scala/Int#
       |[3:6..3:7): T <= up/T#
       |[4:7..4:8): P <= up/P.
       |[5:6..5:7): a <= up/P.a.
       |[5:14..5:14):  <= local0
       |[5:14..5:15): K => up/K#
       |[5:15..5:15):  => up/K#`<init>`().
       |[5:16..5:17): x => up/K#`<init>`().(x)
       |[6:6..6:7): b <= up/P.b.
       |[6:14..6:14):  <= local1
       |[6:14..6:15): K => up/K#
       |[6:16..6:16):  => up/K#`<init>`().
       |[6:17..6:18): x => up/K#`<init>`().(x)
       |[7:6..7:7): c <= up/P.c.
       |[7:14..7:14):  <= local2
       |[7:14..7:15): K => up/K#
       |[7:15..7:15):  => up/K#`<init>`().
       |[7:16..7:17): x => up/K#`<init>`().(x)
       |[7:28..7:29): T => up/T#
       |[8:6..8:7): d <= up/P.d.
       |[8:14..8:14):  <= local3
       |[8:14..8:15): K => up/K#
       |[8:16..8:16):  => up/K#`<init>`().
       |[8:17..8:18): x => up/K#`<init>`().(x)
       |[8:29..8:30): T => up/T#
       |[9:6..9:7): e <= up/P.e.
       |[9:14..9:14):  <= local4
       |[9:14..9:15): L => up/L#
       |[9:15..9:15):  => up/L#`<init>`().
       |[9:16..9:17): x => up/L#`<init>`().(x)
       |[9:23..9:24): y => up/L#`<init>`().(y)
       |[10:6..10:7): f <= up/P.f.
       |[10:14..10:14):  <= local5
       |[10:14..10:15): L => up/L#
       |[10:16..10:16):  => up/L#`<init>`().
       |[10:17..10:18): x => up/L#`<init>`().(x)
       |[10:25..10:26): y => up/L#`<init>`().(y)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of self invocations and annotations with two clauses",
    """|package uq
       |class K(x: Int)(y: Int) {
       |  def this(s: String) = this(x = 1)(y = 2)
       |  def this(b: Boolean) = this (x = 1) (y = 2)
       |  def this(c: Char) = this(x = 1) (y = 2)
       |}
       |class ann(x: Int)(y: Int) extends scala.annotation.StaticAnnotation
       |object Q {
       |  @ann(x = 1)(y = 2) def a = 1
       |  @ann (x = 1) (y = 2) def b = 1
       |}
       |""".stripMargin,
    """|[0:8..0:10): uq <= uq/
       |[1:6..1:7): K <= uq/K#
       |[1:7..1:7):  <= uq/K#`<init>`().
       |[1:8..1:9): x <= uq/K#`<init>`().(x)
       |[1:8..1:9): x <= uq/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[1:16..1:17): y <= uq/K#`<init>`().(y)
       |[1:16..1:17): y <= uq/K#y.
       |[1:19..1:22): Int => scala/Int#
       |[2:6..2:10): this <= uq/K#`<init>`(+1).
       |[2:11..2:12): s <= uq/K#`<init>`(+1).(s)
       |[2:14..2:20): String => scala/Predef.String#
       |[2:28..2:28):  => uq/K#`<init>`().
       |[2:29..2:30): x => uq/K#`<init>`().(x)
       |[2:36..2:37): y => uq/K#`<init>`().(y)
       |[3:6..3:10): this <= uq/K#`<init>`(+2).
       |[3:11..3:12): b <= uq/K#`<init>`(+2).(b)
       |[3:14..3:21): Boolean => scala/Boolean#
       |[3:30..3:30):  => uq/K#`<init>`().
       |[3:31..3:32): x => uq/K#`<init>`().(x)
       |[3:39..3:40): y => uq/K#`<init>`().(y)
       |[4:6..4:10): this <= uq/K#`<init>`(+3).
       |[4:11..4:12): c <= uq/K#`<init>`(+3).(c)
       |[4:14..4:18): Char => scala/Char#
       |[4:26..4:26):  => uq/K#`<init>`().
       |[4:27..4:28): x => uq/K#`<init>`().(x)
       |[4:35..4:36): y => uq/K#`<init>`().(y)
       |[6:6..6:9): ann <= uq/ann#
       |[6:9..6:9):  <= uq/ann#`<init>`().
       |[6:10..6:11): x <= uq/ann#`<init>`().(x)
       |[6:10..6:11): x <= uq/ann#x.
       |[6:13..6:16): Int => scala/Int#
       |[6:18..6:19): y <= uq/ann#`<init>`().(y)
       |[6:18..6:19): y <= uq/ann#y.
       |[6:21..6:24): Int => scala/Int#
       |[6:34..6:39): scala => scala/
       |[6:40..6:50): annotation => scala/annotation/
       |[6:51..6:67): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[6:67..6:67):  => scala/annotation/Annotation#`<init>`().
       |[7:7..7:8): Q <= uq/Q.
       |[8:3..8:6): ann => uq/ann#
       |[8:6..8:6):  => uq/ann#`<init>`().
       |[8:7..8:8): x => uq/ann#`<init>`().(x)
       |[8:14..8:15): y => uq/ann#`<init>`().(y)
       |[8:25..8:26): a <= uq/Q.a().
       |[9:3..9:6): ann => uq/ann#
       |[9:7..9:7):  => uq/ann#`<init>`().
       |[9:8..9:9): x => uq/ann#`<init>`().(x)
       |[9:16..9:17): y => uq/ann#`<init>`().(y)
       |[9:27..9:28): b <= uq/Q.b().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a Dynamic receiver",
    """|package ur
       |import scala.language.dynamics
       |class D extends Dynamic {
       |  def applyDynamicNamed(name: String)(args: (String, Int)*) = name
       |}
       |object R {
       |  val d = new D
       |  val a = d.foo(name = 1)
       |  val b = d.foo (name = 1)
       |  val c = d.applyDynamicNamed("foo")(("name", 1))
       |}
       |""".stripMargin,
    """|[0:8..0:10): ur <= ur/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:30): dynamics => scala/language.dynamics.
       |[2:6..2:7): D <= ur/D#
       |[2:8..2:8):  <= ur/D#`<init>`().
       |[2:16..2:23): Dynamic => scala/Dynamic#
       |[2:23..2:23):  => java/lang/Object#`<init>`().
       |[3:6..3:23): applyDynamicNamed <= ur/D#applyDynamicNamed().
       |[3:24..3:28): name <= ur/D#applyDynamicNamed().(name)
       |[3:30..3:36): String => scala/Predef.String#
       |[3:38..3:42): args <= ur/D#applyDynamicNamed().(args)
       |[3:45..3:51): String => scala/Predef.String#
       |[3:53..3:56): Int => scala/Int#
       |[3:62..3:66): name => ur/D#applyDynamicNamed().(name)
       |[5:7..5:8): R <= ur/R.
       |[6:6..6:7): d <= ur/R.d.
       |[6:14..6:15): D => ur/D#
       |[6:15..6:15):  => ur/D#`<init>`().
       |[7:6..7:7): a <= ur/R.a.
       |[7:10..7:11): d => ur/R.d.
       |[8:6..8:7): b <= ur/R.b.
       |[8:10..8:11): d => ur/R.d.
       |[9:6..9:7): c <= ur/R.c.
       |[9:10..9:11): d => ur/R.d.
       |[9:12..9:29): applyDynamicNamed => ur/D#applyDynamicNamed().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments with a value in parentheses",
    """|package us
       |object S {
       |  def f(x: Int) = x
       |  val a = f(x = (1))
       |  val b = f (x = (1))
       |  val c = f(x = ((1)))
       |}
       |""".stripMargin,
    """|[0:8..0:10): us <= us/
       |[1:7..1:8): S <= us/S.
       |[2:6..2:7): f <= us/S.f().
       |[2:8..2:9): x <= us/S.f().(x)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): x => us/S.f().(x)
       |[3:6..3:7): a <= us/S.a.
       |[3:10..3:11): f => us/S.f().
       |[3:12..3:13): x => us/S.f().(x)
       |[4:6..4:7): b <= us/S.b.
       |[4:10..4:11): f => us/S.f().
       |[4:13..4:14): x => us/S.f().(x)
       |[5:6..5:7): c <= us/S.c.
       |[5:10..5:11): f => us/S.f().
       |[5:12..5:13): x => us/S.f().(x)
       |""".stripMargin,
    "Occurrences",
  )

  private val viewOfNamedValue =
    """|package ut
       |import scala.language.implicitConversions
       |class S
       |object T {
       |  implicit def conv(x: Int): S = new S
       |  def f(x: S) = x
       |  val a = f(x = 1)
       |  val b = f (x = 1)
       |}
       |""".stripMargin

  checkSection(
    "an implicit view of a named value",
    viewOfNamedValue,
    """|[0:8..0:10): ut <= ut/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:41): implicitConversions => scala/language.implicitConversions.
       |[2:6..2:7): S <= ut/S#
       |[2:7..2:7):  <= ut/S#`<init>`().
       |[3:7..3:8): T <= ut/T.
       |[4:15..4:19): conv <= ut/T.conv().
       |[4:20..4:21): x <= ut/T.conv().(x)
       |[4:23..4:26): Int => scala/Int#
       |[4:29..4:30): S => ut/S#
       |[4:37..4:38): S => ut/S#
       |[4:38..4:38):  => ut/S#`<init>`().
       |[5:6..5:7): f <= ut/T.f().
       |[5:8..5:9): x <= ut/T.f().(x)
       |[5:11..5:12): S => ut/S#
       |[5:16..5:17): x => ut/T.f().(x)
       |[6:6..6:7): a <= ut/T.a.
       |[6:10..6:11): f => ut/T.f().
       |[6:12..6:13): x => ut/T.f().(x)
       |[7:6..7:7): b <= ut/T.b.
       |[7:10..7:11): f => ut/T.f().
       |[7:13..7:14): x => ut/T.f().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an implicit view of a named value: synthetics",
    viewOfNamedValue,
    """|[6:16..6:17): 1 => conv(*)
       |  conv => ut/T.conv().
       |[7:17..7:18): 1 => conv(*)
       |  conv => ut/T.conv().
       |""".stripMargin,
    "Synthetics",
  )

  private val viewOfNamedCall =
    """|package uu
       |import scala.language.implicitConversions
       |class S
       |object U {
       |  implicit def conv(y: Int): S = new S
       |  def f(x: Int) = x
       |  def o(y: S) = y
       |  val a = o(y = f(x = 1))
       |  val b = o (y = f(x = 1))
       |  val c = o(y = f (x = 1))
       |  val d = o (y = f (x = 1))
       |}
       |""".stripMargin

  checkSection(
    "an implicit view of a call that is a named value",
    viewOfNamedCall,
    """|[0:8..0:10): uu <= uu/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:41): implicitConversions => scala/language.implicitConversions.
       |[2:6..2:7): S <= uu/S#
       |[2:7..2:7):  <= uu/S#`<init>`().
       |[3:7..3:8): U <= uu/U.
       |[4:15..4:19): conv <= uu/U.conv().
       |[4:20..4:21): y <= uu/U.conv().(y)
       |[4:23..4:26): Int => scala/Int#
       |[4:29..4:30): S => uu/S#
       |[4:37..4:38): S => uu/S#
       |[4:38..4:38):  => uu/S#`<init>`().
       |[5:6..5:7): f <= uu/U.f().
       |[5:8..5:9): x <= uu/U.f().(x)
       |[5:11..5:14): Int => scala/Int#
       |[5:18..5:19): x => uu/U.f().(x)
       |[6:6..6:7): o <= uu/U.o().
       |[6:8..6:9): y <= uu/U.o().(y)
       |[6:11..6:12): S => uu/S#
       |[6:16..6:17): y => uu/U.o().(y)
       |[7:6..7:7): a <= uu/U.a.
       |[7:10..7:11): o => uu/U.o().
       |[7:12..7:13): y => uu/U.o().(y)
       |[7:16..7:17): f => uu/U.f().
       |[7:18..7:19): x => uu/U.f().(x)
       |[8:6..8:7): b <= uu/U.b.
       |[8:10..8:11): o => uu/U.o().
       |[8:13..8:14): y => uu/U.o().(y)
       |[8:17..8:18): f => uu/U.f().
       |[8:19..8:20): x => uu/U.f().(x)
       |[9:6..9:7): c <= uu/U.c.
       |[9:10..9:11): o => uu/U.o().
       |[9:12..9:13): y => uu/U.o().(y)
       |[9:16..9:17): f => uu/U.f().
       |[9:19..9:20): x => uu/U.f().(x)
       |[10:6..10:7): d <= uu/U.d.
       |[10:10..10:11): o => uu/U.o().
       |[10:13..10:14): y => uu/U.o().(y)
       |[10:17..10:18): f => uu/U.f().
       |[10:20..10:21): x => uu/U.f().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an implicit view of a call that is a named value: synthetics",
    viewOfNamedCall,
    """|[7:16..7:24): f(x = 1) => conv(*)
       |  conv => uu/U.conv().
       |[8:17..8:25): f(x = 1) => conv(*)
       |  conv => uu/U.conv().
       |[9:16..9:25): f (x = 1) => conv(*)
       |  conv => uu/U.conv().
       |[10:17..10:26): f (x = 1) => conv(*)
       |  conv => uu/U.conv().
       |""".stripMargin,
    "Synthetics",
  )

  checkSection(
    "named arguments of a Dynamic receiver with tuple field names",
    """|package uv
       |import scala.language.dynamics
       |class D extends Dynamic {
       |  def applyDynamicNamed(name: String)(args: (String, Int)*) = name
       |}
       |object V {
       |  val d = new D
       |  val a = d.foo(_2 = 1)
       |  val b = d.foo (_2 = 1)
       |  val c = d.foo(_1 = 1)
       |}
       |""".stripMargin,
    """|[0:8..0:10): uv <= uv/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:30): dynamics => scala/language.dynamics.
       |[2:6..2:7): D <= uv/D#
       |[2:8..2:8):  <= uv/D#`<init>`().
       |[2:16..2:23): Dynamic => scala/Dynamic#
       |[2:23..2:23):  => java/lang/Object#`<init>`().
       |[3:6..3:23): applyDynamicNamed <= uv/D#applyDynamicNamed().
       |[3:24..3:28): name <= uv/D#applyDynamicNamed().(name)
       |[3:30..3:36): String => scala/Predef.String#
       |[3:38..3:42): args <= uv/D#applyDynamicNamed().(args)
       |[3:45..3:51): String => scala/Predef.String#
       |[3:53..3:56): Int => scala/Int#
       |[3:62..3:66): name => uv/D#applyDynamicNamed().(name)
       |[5:7..5:8): V <= uv/V.
       |[6:6..6:7): d <= uv/V.d.
       |[6:14..6:15): D => uv/D#
       |[6:15..6:15):  => uv/D#`<init>`().
       |[7:6..7:7): a <= uv/V.a.
       |[7:10..7:11): d => uv/V.d.
       |[8:6..8:7): b <= uv/V.b.
       |[8:10..8:11): d => uv/V.d.
       |[9:6..9:7): c <= uv/V.c.
       |[9:10..9:11): d => uv/V.d.
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an update assignment",
    """|package uw
       |class M { def update(k: Int, v: Int): Unit = () }
       |object W {
       |  val arr = Array(1, 2)
       |  arr(i = 0) = 5
       |  arr (i = 0) = 5
       |  arr.update(i = 0, x = 5)
       |  val m = new M
       |  m(k = 0) = 5
       |  m (k = 0) = 5
       |}
       |""".stripMargin,
    """|[0:8..0:10): uw <= uw/
       |[1:6..1:7): M <= uw/M#
       |[1:8..1:8):  <= uw/M#`<init>`().
       |[1:14..1:20): update <= uw/M#update().
       |[1:21..1:22): k <= uw/M#update().(k)
       |[1:24..1:27): Int => scala/Int#
       |[1:29..1:30): v <= uw/M#update().(v)
       |[1:32..1:35): Int => scala/Int#
       |[1:38..1:42): Unit => scala/Unit#
       |[2:7..2:8): W <= uw/W.
       |[3:6..3:9): arr <= uw/W.arr.
       |[3:12..3:17): Array => scala/Array.
       |[4:2..4:5): arr => uw/W.arr.
       |[4:6..4:7): i => scala/Array#update().(i)
       |[5:2..5:5): arr => uw/W.arr.
       |[5:7..5:8): i => scala/Array#update().(i)
       |[6:2..6:5): arr => uw/W.arr.
       |[6:6..6:12): update => scala/Array#update().
       |[6:13..6:14): i => scala/Array#update().(i)
       |[6:20..6:21): x => scala/Array#update().(x)
       |[7:6..7:7): m <= uw/W.m.
       |[7:14..7:15): M => uw/M#
       |[7:15..7:15):  => uw/M#`<init>`().
       |[8:2..8:3): m => uw/W.m.
       |[8:4..8:5): k => uw/M#update().(k)
       |[9:2..9:3): m => uw/W.m.
       |[9:5..9:6): k => uw/M#update().(k)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a Java annotation",
    """|package ux
       |object X {
       |  @SuppressWarnings(value = Array("x")) def a = 1
       |  @SuppressWarnings (value = Array("x")) def b = 1
       |  @SuppressWarnings(Array("x")) def c = 1
       |}
       |""".stripMargin,
    """|[0:8..0:10): ux <= ux/
       |[1:7..1:8): X <= ux/X.
       |[2:3..2:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[2:20..2:25): value => java/lang/SuppressWarnings#value().
       |[2:28..2:33): Array => scala/Array.
       |[2:44..2:45): a <= ux/X.a().
       |[3:3..3:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[3:21..3:26): value => java/lang/SuppressWarnings#value().
       |[3:29..3:34): Array => scala/Array.
       |[3:45..3:46): b <= ux/X.b().
       |[4:3..4:19): SuppressWarnings => java/lang/SuppressWarnings#
       |[4:20..4:25): Array => scala/Array.
       |[4:36..4:37): c <= ux/X.c().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "reordered named arguments of a super call and a constructor",
    """|package uy
       |class K(x: Int, y: Int)
       |class A extends K(y = 1, x = 2)
       |class B extends K (y = 1, x = 2)
       |object Y {
       |  val a = new K(y = 1, x = 2)
       |  val b = new K (y = 1, x = 2)
       |}
       |""".stripMargin,
    """|[0:8..0:10): uy <= uy/
       |[1:6..1:7): K <= uy/K#
       |[1:7..1:7):  <= uy/K#`<init>`().
       |[1:8..1:9): x <= uy/K#`<init>`().(x)
       |[1:8..1:9): x <= uy/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[1:16..1:17): y <= uy/K#`<init>`().(y)
       |[1:16..1:17): y <= uy/K#y.
       |[1:19..1:22): Int => scala/Int#
       |[2:6..2:7): A <= uy/A#
       |[2:8..2:8):  <= uy/A#`<init>`().
       |[2:16..2:17): K => uy/K#
       |[2:17..2:17):  => uy/K#`<init>`().
       |[2:18..2:19): y => uy/K#`<init>`().(y)
       |[2:25..2:26): x => uy/K#`<init>`().(x)
       |[3:6..3:7): B <= uy/B#
       |[3:8..3:8):  <= uy/B#`<init>`().
       |[3:16..3:17): K => uy/K#
       |[3:18..3:18):  => uy/K#`<init>`().
       |[3:19..3:20): y => uy/K#`<init>`().(y)
       |[3:26..3:27): x => uy/K#`<init>`().(x)
       |[4:7..4:8): Y <= uy/Y.
       |[5:6..5:7): a <= uy/Y.a.
       |[5:14..5:15): K => uy/K#
       |[5:15..5:15):  => uy/K#`<init>`().
       |[5:16..5:17): y => uy/K#`<init>`().(y)
       |[5:23..5:24): x => uy/K#`<init>`().(x)
       |[6:6..6:7): b <= uy/Y.b.
       |[6:14..6:15): K => uy/K#
       |[6:16..6:16):  => uy/K#`<init>`().
       |[6:17..6:18): y => uy/K#`<init>`().(y)
       |[6:24..6:25): x => uy/K#`<init>`().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments with a block value",
    """|package uz
       |object Z {
       |  def f(x: Int) = x
       |  val a = f(x = { 1 })
       |  val b = f (x = { 1 })
       |}
       |""".stripMargin,
    """|[0:8..0:10): uz <= uz/
       |[1:7..1:8): Z <= uz/Z.
       |[2:6..2:7): f <= uz/Z.f().
       |[2:8..2:9): x <= uz/Z.f().(x)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): x => uz/Z.f().(x)
       |[3:6..3:7): a <= uz/Z.a.
       |[3:10..3:11): f => uz/Z.f().
       |[3:12..3:13): x => uz/Z.f().(x)
       |[4:6..4:7): b <= uz/Z.b.
       |[4:10..4:11): f => uz/Z.f().
       |[4:13..4:14): x => uz/Z.f().(x)
       |""".stripMargin,
    "Occurrences",
  )

  private val viewOfConstructorCall =
    """|package va
       |import scala.language.implicitConversions
       |class S
       |class K(x: Int)
       |object A {
       |  implicit def conv(x: K): S = new S
       |  val a: S = new K(x = 1)
       |  val b: S = new K (x = 1)
       |}
       |""".stripMargin

  checkSection(
    "an implicit view of a constructor call",
    viewOfConstructorCall,
    """|[0:8..0:10): va <= va/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:41): implicitConversions => scala/language.implicitConversions.
       |[2:6..2:7): S <= va/S#
       |[2:7..2:7):  <= va/S#`<init>`().
       |[3:6..3:7): K <= va/K#
       |[3:7..3:7):  <= va/K#`<init>`().
       |[3:8..3:9): x <= va/K#`<init>`().(x)
       |[3:8..3:9): x <= va/K#x.
       |[3:11..3:14): Int => scala/Int#
       |[4:7..4:8): A <= va/A.
       |[5:15..5:19): conv <= va/A.conv().
       |[5:20..5:21): x <= va/A.conv().(x)
       |[5:23..5:24): K => va/K#
       |[5:27..5:28): S => va/S#
       |[5:35..5:36): S => va/S#
       |[5:36..5:36):  => va/S#`<init>`().
       |[6:6..6:7): a <= va/A.a.
       |[6:9..6:10): S => va/S#
       |[6:17..6:18): K => va/K#
       |[6:18..6:18):  => va/K#`<init>`().
       |[6:19..6:20): x => va/K#`<init>`().(x)
       |[7:6..7:7): b <= va/A.b.
       |[7:9..7:10): S => va/S#
       |[7:17..7:18): K => va/K#
       |[7:19..7:19):  => va/K#`<init>`().
       |[7:20..7:21): x => va/K#`<init>`().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an implicit view of a constructor call: synthetics",
    viewOfConstructorCall,
    """|[6:13..6:25): new K(x = 1) => conv(*)
       |  conv => va/A.conv().
       |[7:13..7:26): new K (x = 1) => conv(*)
       |  conv => va/A.conv().
       |""".stripMargin,
    "Synthetics",
  )

  checkSection(
    "named arguments of a method on a constructor call",
    """|package vb
       |class K(x: Int) { def m(x: Int) = x }
       |object B {
       |  val a = new K(x = 1).m(2)
       |  val b = new K(x = 1).m(x = 2)
       |  val c = new K (x = 1).m(x = 2)
       |  val d = new K(x = 1).m (x = 2)
       |  val e = new K (x = 1).m (x = 2)
       |}
       |""".stripMargin,
    """|[0:8..0:10): vb <= vb/
       |[1:6..1:7): K <= vb/K#
       |[1:7..1:7):  <= vb/K#`<init>`().
       |[1:8..1:9): x <= vb/K#`<init>`().(x)
       |[1:8..1:9): x <= vb/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[1:22..1:23): m <= vb/K#m().
       |[1:24..1:25): x <= vb/K#m().(x)
       |[1:27..1:30): Int => scala/Int#
       |[1:34..1:35): x => vb/K#m().(x)
       |[2:7..2:8): B <= vb/B.
       |[3:6..3:7): a <= vb/B.a.
       |[3:14..3:15): K => vb/K#
       |[3:15..3:15):  => vb/K#`<init>`().
       |[3:16..3:17): x => vb/K#`<init>`().(x)
       |[3:23..3:24): m => vb/K#m().
       |[4:6..4:7): b <= vb/B.b.
       |[4:14..4:15): K => vb/K#
       |[4:15..4:15):  => vb/K#`<init>`().
       |[4:16..4:17): x => vb/K#`<init>`().(x)
       |[4:23..4:24): m => vb/K#m().
       |[4:25..4:26): x => vb/K#m().(x)
       |[5:6..5:7): c <= vb/B.c.
       |[5:14..5:15): K => vb/K#
       |[5:16..5:16):  => vb/K#`<init>`().
       |[5:17..5:18): x => vb/K#`<init>`().(x)
       |[5:24..5:25): m => vb/K#m().
       |[5:26..5:27): x => vb/K#m().(x)
       |[6:6..6:7): d <= vb/B.d.
       |[6:14..6:15): K => vb/K#
       |[6:15..6:15):  => vb/K#`<init>`().
       |[6:16..6:17): x => vb/K#`<init>`().(x)
       |[6:23..6:24): m => vb/K#m().
       |[6:26..6:27): x => vb/K#m().(x)
       |[7:6..7:7): e <= vb/B.e.
       |[7:14..7:15): K => vb/K#
       |[7:16..7:16):  => vb/K#`<init>`().
       |[7:17..7:18): x => vb/K#`<init>`().(x)
       |[7:24..7:25): m => vb/K#m().
       |[7:27..7:28): x => vb/K#m().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "reordered named arguments of a super call with a class parameter",
    """|package vc
       |class K(x: Int, y: Int)
       |class X(z: Int) extends K(y = z + 1, x = 2)
       |class W(z: Int) extends K (y = z + 1, x = 2)
       |""".stripMargin,
    """|[0:8..0:10): vc <= vc/
       |[1:6..1:7): K <= vc/K#
       |[1:7..1:7):  <= vc/K#`<init>`().
       |[1:8..1:9): x <= vc/K#`<init>`().(x)
       |[1:8..1:9): x <= vc/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[1:16..1:17): y <= vc/K#`<init>`().(y)
       |[1:16..1:17): y <= vc/K#y.
       |[1:19..1:22): Int => scala/Int#
       |[2:6..2:7): X <= vc/X#
       |[2:7..2:7):  <= vc/X#`<init>`().
       |[2:8..2:9): z <= vc/X#`<init>`().(z)
       |[2:8..2:9): z <= vc/X#z.
       |[2:11..2:14): Int => scala/Int#
       |[2:24..2:25): K => vc/K#
       |[2:25..2:25):  => vc/K#`<init>`().
       |[2:26..2:27): y => vc/K#`<init>`().(y)
       |[2:30..2:31): z => vc/X#`<init>`().(z)
       |[2:32..2:33): + => scala/Int#`+`(+4).
       |[2:37..2:38): x => vc/K#`<init>`().(x)
       |[3:6..3:7): W <= vc/W#
       |[3:7..3:7):  <= vc/W#`<init>`().
       |[3:8..3:9): z <= vc/W#`<init>`().(z)
       |[3:8..3:9): z <= vc/W#z.
       |[3:11..3:14): Int => scala/Int#
       |[3:24..3:25): K => vc/K#
       |[3:26..3:26):  => vc/K#`<init>`().
       |[3:27..3:28): y => vc/K#`<init>`().(y)
       |[3:31..3:32): z => vc/W#`<init>`().(z)
       |[3:33..3:34): + => scala/Int#`+`(+4).
       |[3:38..3:39): x => vc/K#`<init>`().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "reordered named arguments in two clauses",
    """|package vd
       |object D {
       |  def h(a: Int, b: Int)(c: Int, d: Int) = a
       |  val p = h(b = 1, a = 2)(d = 3, c = 4)
       |  val q = h (b = 1, a = 2)(d = 3, c = 4)
       |  val r = h(b = 1, a = 2) (d = 3, c = 4)
       |  val s = h (b = 1, a = 2) (d = 3, c = 4)
       |}
       |""".stripMargin,
    """|[0:8..0:10): vd <= vd/
       |[1:7..1:8): D <= vd/D.
       |[2:6..2:7): h <= vd/D.h().
       |[2:8..2:9): a <= vd/D.h().(a)
       |[2:11..2:14): Int => scala/Int#
       |[2:16..2:17): b <= vd/D.h().(b)
       |[2:19..2:22): Int => scala/Int#
       |[2:24..2:25): c <= vd/D.h().(c)
       |[2:27..2:30): Int => scala/Int#
       |[2:32..2:33): d <= vd/D.h().(d)
       |[2:35..2:38): Int => scala/Int#
       |[2:42..2:43): a => vd/D.h().(a)
       |[3:6..3:7): p <= vd/D.p.
       |[3:10..3:11): h => vd/D.h().
       |[3:12..3:13): b => vd/D.h().(b)
       |[3:19..3:20): a => vd/D.h().(a)
       |[3:26..3:27): d => vd/D.h().(d)
       |[3:33..3:34): c => vd/D.h().(c)
       |[4:6..4:7): q <= vd/D.q.
       |[4:10..4:11): h => vd/D.h().
       |[4:13..4:14): b => vd/D.h().(b)
       |[4:20..4:21): a => vd/D.h().(a)
       |[4:27..4:28): d => vd/D.h().(d)
       |[4:34..4:35): c => vd/D.h().(c)
       |[5:6..5:7): r <= vd/D.r.
       |[5:10..5:11): h => vd/D.h().
       |[5:12..5:13): b => vd/D.h().(b)
       |[5:19..5:20): a => vd/D.h().(a)
       |[5:27..5:28): d => vd/D.h().(d)
       |[5:34..5:35): c => vd/D.h().(c)
       |[6:6..6:7): s <= vd/D.s.
       |[6:10..6:11): h => vd/D.h().
       |[6:13..6:14): b => vd/D.h().(b)
       |[6:20..6:21): a => vd/D.h().(a)
       |[6:28..6:29): d => vd/D.h().(d)
       |[6:35..6:36): c => vd/D.h().(c)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "reordered named arguments next to an implicit clause",
    """|package ve
       |object E {
       |  implicit val i: Int = 0
       |  def m(x: Int, y: Int)(implicit i: Int) = x
       |  val z = 3
       |  val a = m(y = 1, x = 2)
       |  val b = m (y = 1, x = 2)
       |  val c = m(y = z + 1, x = 2)
       |  val d = m (y = z + 1, x = 2)
       |}
       |""".stripMargin,
    """|[0:8..0:10): ve <= ve/
       |[1:7..1:8): E <= ve/E.
       |[2:15..2:16): i <= ve/E.i.
       |[2:18..2:21): Int => scala/Int#
       |[3:6..3:7): m <= ve/E.m().
       |[3:8..3:9): x <= ve/E.m().(x)
       |[3:11..3:14): Int => scala/Int#
       |[3:16..3:17): y <= ve/E.m().(y)
       |[3:19..3:22): Int => scala/Int#
       |[3:33..3:34): i <= ve/E.m().(i)
       |[3:36..3:39): Int => scala/Int#
       |[3:43..3:44): x => ve/E.m().(x)
       |[4:6..4:7): z <= ve/E.z.
       |[5:6..5:7): a <= ve/E.a.
       |[5:10..5:11): m => ve/E.m().
       |[5:12..5:13): y => ve/E.m().(y)
       |[5:19..5:20): x => ve/E.m().(x)
       |[6:6..6:7): b <= ve/E.b.
       |[6:10..6:11): m => ve/E.m().
       |[6:13..6:14): y => ve/E.m().(y)
       |[6:20..6:21): x => ve/E.m().(x)
       |[7:6..7:7): c <= ve/E.c.
       |[7:10..7:11): m => ve/E.m().
       |[7:12..7:13): y => ve/E.m().(y)
       |[7:16..7:17): z => ve/E.z.
       |[7:18..7:19): + => scala/Int#`+`(+4).
       |[7:23..7:24): x => ve/E.m().(x)
       |[8:6..8:7): d <= ve/E.d.
       |[8:10..8:11): m => ve/E.m().
       |[8:13..8:14): y => ve/E.m().(y)
       |[8:17..8:18): z => ve/E.z.
       |[8:19..8:20): + => scala/Int#`+`(+4).
       |[8:24..8:25): x => ve/E.m().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an update with an operator",
    """|package vf
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object F {
       |  val arr = Array(1, 2)
       |  arr(i = 0) += 5
       |  arr (i = 0) += 5
       |  val m = new M
       |  m(k = 0) += 5
       |  m (k = 0) += 5
       |}
       |""".stripMargin,
    """|[0:8..0:10): vf <= vf/
       |[1:6..1:7): M <= vf/M#
       |[1:8..1:8):  <= vf/M#`<init>`().
       |[1:14..1:19): apply <= vf/M#apply().
       |[1:20..1:21): k <= vf/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => vf/M#apply().(k)
       |[1:42..1:48): update <= vf/M#update().
       |[1:49..1:50): k <= vf/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= vf/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): F <= vf/F.
       |[3:6..3:9): arr <= vf/F.arr.
       |[3:12..3:17): Array => scala/Array.
       |[4:2..4:5): arr => vf/F.arr.
       |[4:6..4:7): i => scala/Array#apply().(i)
       |[4:13..4:15): += => scala/Int#`+`(+4).
       |[5:2..5:5): arr => vf/F.arr.
       |[5:7..5:8): i => scala/Array#apply().(i)
       |[5:14..5:16): += => scala/Int#`+`(+4).
       |[6:6..6:7): m <= vf/F.m.
       |[6:14..6:15): M => vf/M#
       |[6:15..6:15):  => vf/M#`<init>`().
       |[7:2..7:3): m => vf/F.m.
       |[7:4..7:5): k => vf/M#apply().(k)
       |[7:11..7:13): += => scala/Int#`+`(+4).
       |[8:2..8:3): m => vf/F.m.
       |[8:5..8:6): k => vf/M#apply().(k)
       |[8:12..8:14): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  private val extensionOfImplicitClass =
    """|package vg
       |object G {
       |  implicit class E(n: Int) { def ext(x: Int) = n + x }
       |  val a = 1.ext(x = 1)
       |  val b = 1.ext (x = 1)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an extension method of an implicit class",
    extensionOfImplicitClass,
    """|[0:8..0:10): vg <= vg/
       |[1:7..1:8): G <= vg/G.
       |[2:17..2:18): E <= vg/G.E#
       |[2:18..2:18):  <= vg/G.E#`<init>`().
       |[2:19..2:20): n <= vg/G.E#`<init>`().(n)
       |[2:19..2:20): n <= vg/G.E#n.
       |[2:22..2:25): Int => scala/Int#
       |[2:33..2:36): ext <= vg/G.E#ext().
       |[2:37..2:38): x <= vg/G.E#ext().(x)
       |[2:40..2:43): Int => scala/Int#
       |[2:47..2:48): n => vg/G.E#n.
       |[2:49..2:50): + => scala/Int#`+`(+4).
       |[2:51..2:52): x => vg/G.E#ext().(x)
       |[3:6..3:7): a <= vg/G.a.
       |[3:12..3:15): ext => vg/G.E#ext().
       |[3:16..3:17): x => vg/G.E#ext().(x)
       |[4:6..4:7): b <= vg/G.b.
       |[4:12..4:15): ext => vg/G.E#ext().
       |[4:17..4:18): x => vg/G.E#ext().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an extension method of an implicit class: synthetics",
    extensionOfImplicitClass,
    """|[3:10..3:11): 1 => E(*)
       |  E => vg/G.E().
       |[4:10..4:11): 1 => E(*)
       |  E => vg/G.E().
       |""".stripMargin,
    "Synthetics",
  )

  if (ScalaVersion.is212) checkSection(
    "an assignment to a var in an argument clause",
    """|package vh
       |object H {
       |  var x = 0
       |  def g(y: Unit) = y
       |  g(x = 1)
       |  g (x = 1)
       |}
       |""".stripMargin,
    """|[0:8..0:10): vh <= vh/
       |[1:7..1:8): H <= vh/H.
       |[2:6..2:7): x <= vh/H.x().
       |[3:6..3:7): g <= vh/H.g().
       |[3:8..3:9): y <= vh/H.g().(y)
       |[3:11..3:15): Unit => scala/Unit#
       |[3:19..3:20): y => vh/H.g().(y)
       |[4:2..4:3): g => vh/H.g().
       |[4:4..4:5): x => vh/H.`x_=`().
       |[5:2..5:3): g => vh/H.g().
       |[5:5..5:6): x => vh/H.`x_=`().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a nested Java annotation",
    """|package vi
       |import issue1672.{Inner, Outer}
       |object I {
       |  @Outer(value = new Inner(value = "i")) def a = 1
       |  @Outer (value = new Inner (value = "i")) def b = 1
       |  @Outer(new Inner(value = "i")) def c = 1
       |}
       |""".stripMargin,
    """|[0:8..0:10): vi <= vi/
       |[1:7..1:16): issue1672 => issue1672/
       |[1:18..1:23): Inner => issue1672/Inner#
       |[1:25..1:30): Outer => issue1672/Outer#
       |[2:7..2:8): I <= vi/I.
       |[3:3..3:8): Outer => issue1672/Outer#
       |[3:9..3:14): value => issue1672/Outer#value().
       |[3:21..3:26): Inner => issue1672/Inner#
       |[3:27..3:32): value => issue1672/Inner#value().
       |[3:45..3:46): a <= vi/I.a().
       |[4:3..4:8): Outer => issue1672/Outer#
       |[4:10..4:15): value => issue1672/Outer#value().
       |[4:22..4:27): Inner => issue1672/Inner#
       |[4:29..4:34): value => issue1672/Inner#value().
       |[4:47..4:48): b <= vi/I.b().
       |[5:3..5:8): Outer => issue1672/Outer#
       |[5:13..5:18): Inner => issue1672/Inner#
       |[5:19..5:24): value => issue1672/Inner#value().
       |[5:37..5:38): c <= vi/I.c().
       |""".stripMargin,
    "Occurrences",
  )

  if (ScalaVersion.is212) checkSection(
    "named arguments of a Scala classfile annotation",
    """|package vj
       |class ca(x: Int) extends scala.annotation.ClassfileAnnotation
       |object J {
       |  @ca(x = 1) def a = 1
       |  @ca (x = 1) def b = 1
       |}
       |""".stripMargin,
    """|[0:8..0:10): vj <= vj/
       |[1:6..1:8): ca <= vj/ca#
       |[1:8..1:8):  <= vj/ca#`<init>`().
       |[1:9..1:10): x <= vj/ca#`<init>`().(x)
       |[1:9..1:10): x <= vj/ca#x.
       |[1:12..1:15): Int => scala/Int#
       |[1:25..1:30): scala => scala/
       |[1:31..1:41): annotation => scala/annotation/
       |[1:42..1:61): ClassfileAnnotation => scala/annotation/ClassfileAnnotation#
       |[1:61..1:61):  => scala/annotation/Annotation#`<init>`().
       |[2:7..2:8): J <= vj/J.
       |[3:3..3:5): ca => vj/ca#
       |[3:5..3:5):  => vj/ca#`<init>`().
       |[3:6..3:7): x => vj/ca#`<init>`().(x)
       |[3:17..3:18): a <= vj/J.a().
       |[4:3..4:5): ca => vj/ca#
       |[4:6..4:6):  => vj/ca#`<init>`().
       |[4:7..4:8): x => vj/ca#`<init>`().(x)
       |[4:18..4:19): b <= vj/J.b().
       |""".stripMargin,
    "Occurrences",
  )
  else checkSection(
    "named arguments of a Scala constant annotation",
    """|package vj
       |class ca(x: Int) extends scala.annotation.ConstantAnnotation
       |object J {
       |  @ca(x = 1) def a = 1
       |  @ca (x = 1) def b = 1
       |  @ca(1) def c = 1
       |}
       |""".stripMargin,
    """|[0:8..0:10): vj <= vj/
       |[1:6..1:8): ca <= vj/ca#
       |[1:8..1:8):  <= vj/ca#`<init>`().
       |[1:9..1:10): x <= vj/ca#`<init>`().(x)
       |[1:9..1:10): x <= vj/ca#x.
       |[1:12..1:15): Int => scala/Int#
       |[1:25..1:30): scala => scala/
       |[1:31..1:41): annotation => scala/annotation/
       |[1:42..1:60): ConstantAnnotation => scala/annotation/ConstantAnnotation#
       |[1:60..1:60):  => scala/annotation/Annotation#`<init>`().
       |[2:7..2:8): J <= vj/J.
       |[3:3..3:5): ca => vj/ca#
       |[3:5..3:5):  => vj/ca#`<init>`().
       |[3:6..3:7): x => vj/ca#`<init>`().(x)
       |[3:17..3:18): a <= vj/J.a().
       |[4:3..4:5): ca => vj/ca#
       |[4:6..4:6):  => vj/ca#`<init>`().
       |[4:7..4:8): x => vj/ca#`<init>`().(x)
       |[4:18..4:19): b <= vj/J.b().
       |[5:3..5:5): ca => vj/ca#
       |[5:5..5:5):  => vj/ca#`<init>`().
       |[5:13..5:14): c <= vj/J.c().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an apply on a constructor call",
    """|package vk
       |class K(x: Int, a: Int = 0) { def apply(a: Int, b: Int) = a }
       |object O {
       |  val p = new K(x = 1)(a = 1, b = 2)
       |  val q = new K (x = 1)(a = 1, b = 2)
       |  val r = new K(x = 1) (a = 1, b = 2)
       |  val s = new K (x = 1) (a = 1, b = 2)
       |}
       |""".stripMargin,
    """|[0:8..0:10): vk <= vk/
       |[1:6..1:7): K <= vk/K#
       |[1:7..1:7):  <= vk/K#`<init>`().
       |[1:8..1:9): x <= vk/K#`<init>`().(x)
       |[1:8..1:9): x <= vk/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[1:16..1:17): a <= vk/K#`<init>`().(a)
       |[1:16..1:17): a <= vk/K#a.
       |[1:19..1:22): Int => scala/Int#
       |[1:34..1:39): apply <= vk/K#apply().
       |[1:40..1:41): a <= vk/K#apply().(a)
       |[1:43..1:46): Int => scala/Int#
       |[1:48..1:49): b <= vk/K#apply().(b)
       |[1:51..1:54): Int => scala/Int#
       |[1:58..1:59): a => vk/K#apply().(a)
       |[2:7..2:8): O <= vk/O.
       |[3:6..3:7): p <= vk/O.p.
       |[3:14..3:15): K => vk/K#
       |[3:15..3:15):  => vk/K#`<init>`().
       |[3:16..3:17): x => vk/K#`<init>`().(x)
       |[3:23..3:24): a => vk/K#apply().(a)
       |[3:30..3:31): b => vk/K#apply().(b)
       |[4:6..4:7): q <= vk/O.q.
       |[4:14..4:15): K => vk/K#
       |[4:16..4:16):  => vk/K#`<init>`().
       |[4:17..4:18): x => vk/K#`<init>`().(x)
       |[4:24..4:25): a => vk/K#apply().(a)
       |[4:31..4:32): b => vk/K#apply().(b)
       |[5:6..5:7): r <= vk/O.r.
       |[5:14..5:15): K => vk/K#
       |[5:15..5:15):  => vk/K#`<init>`().
       |[5:16..5:17): x => vk/K#`<init>`().(x)
       |[5:24..5:25): a => vk/K#apply().(a)
       |[5:31..5:32): b => vk/K#apply().(b)
       |[6:6..6:7): s <= vk/O.s.
       |[6:14..6:15): K => vk/K#
       |[6:16..6:16):  => vk/K#`<init>`().
       |[6:17..6:18): x => vk/K#`<init>`().(x)
       |[6:25..6:26): a => vk/K#apply().(a)
       |[6:32..6:33): b => vk/K#apply().(b)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "reordered named arguments of an apply on a constructor call",
    """|package vl
       |class K(x: Int, a: Int = 0) { def apply(a: Int, b: Int) = a }
       |object O {
       |  val p = new K(x = 1)(b = 2, a = 1)
       |  val q = new K (x = 1)(b = 2, a = 1)
       |  val r = new K(x = 1) (b = 2, a = 1)
       |  val s = new K (x = 1) (b = 2, a = 1)
       |}
       |""".stripMargin,
    """|[0:8..0:10): vl <= vl/
       |[1:6..1:7): K <= vl/K#
       |[1:7..1:7):  <= vl/K#`<init>`().
       |[1:8..1:9): x <= vl/K#`<init>`().(x)
       |[1:8..1:9): x <= vl/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[1:16..1:17): a <= vl/K#`<init>`().(a)
       |[1:16..1:17): a <= vl/K#a.
       |[1:19..1:22): Int => scala/Int#
       |[1:34..1:39): apply <= vl/K#apply().
       |[1:40..1:41): a <= vl/K#apply().(a)
       |[1:43..1:46): Int => scala/Int#
       |[1:48..1:49): b <= vl/K#apply().(b)
       |[1:51..1:54): Int => scala/Int#
       |[1:58..1:59): a => vl/K#apply().(a)
       |[2:7..2:8): O <= vl/O.
       |[3:6..3:7): p <= vl/O.p.
       |[3:14..3:15): K => vl/K#
       |[3:15..3:15):  => vl/K#`<init>`().
       |[3:16..3:17): x => vl/K#`<init>`().(x)
       |[3:23..3:24): b => vl/K#apply().(b)
       |[3:30..3:31): a => vl/K#apply().(a)
       |[4:6..4:7): q <= vl/O.q.
       |[4:14..4:15): K => vl/K#
       |[4:16..4:16):  => vl/K#`<init>`().
       |[4:17..4:18): x => vl/K#`<init>`().(x)
       |[4:24..4:25): b => vl/K#apply().(b)
       |[4:31..4:32): a => vl/K#apply().(a)
       |[5:6..5:7): r <= vl/O.r.
       |[5:14..5:15): K => vl/K#
       |[5:15..5:15):  => vl/K#`<init>`().
       |[5:16..5:17): x => vl/K#`<init>`().(x)
       |[5:24..5:25): b => vl/K#apply().(b)
       |[5:31..5:32): a => vl/K#apply().(a)
       |[6:6..6:7): s <= vl/O.s.
       |[6:14..6:15): K => vl/K#
       |[6:16..6:16):  => vl/K#`<init>`().
       |[6:17..6:18): x => vl/K#`<init>`().(x)
       |[6:25..6:26): b => vl/K#apply().(b)
       |[6:32..6:33): a => vl/K#apply().(a)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an auto-tupled call",
    """|package vm
       |object M {
       |  def f(p: (Int, Int)) = p
       |  val a = f(_1 = 1, _2 = 2)
       |  val b = f (_1 = 1, _2 = 2)
       |}
       |""".stripMargin,
    """|[0:8..0:10): vm <= vm/
       |[1:7..1:8): M <= vm/M.
       |[2:6..2:7): f <= vm/M.f().
       |[2:8..2:9): p <= vm/M.f().(p)
       |[2:12..2:15): Int => scala/Int#
       |[2:17..2:20): Int => scala/Int#
       |[2:25..2:26): p => vm/M.f().(p)
       |[3:6..3:7): a <= vm/M.a.
       |[3:10..3:11): f => vm/M.f().
       |[3:12..3:14): _1 => scala/Tuple2.apply().(_1)
       |[3:20..3:22): _2 => scala/Tuple2.apply().(_2)
       |[4:6..4:7): b <= vm/M.b.
       |[4:10..4:11): f => vm/M.f().
       |[4:13..4:15): _1 => scala/Tuple2.apply().(_1)
       |[4:21..4:23): _2 => scala/Tuple2.apply().(_2)
       |""".stripMargin,
    "Occurrences",
  )

  private val updateWithVariableIndex =
    """|package vn
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object N {
       |  var n = 0
       |  val arr = Array(1, 2)
       |  val m = new M
       |  arr(n) = 0
       |  arr (n) = 0
       |  arr(n) += 1
       |  arr (n) += 1
       |  m(n) = 0
       |  m (n) = 0
       |  m(n) += 1
       |  m (n) += 1
       |  arr(i = 0) = 0
       |  arr (i = 0) = 0
       |  m(k = 0) = 0
       |  m (k = 0) = 0
       |}
       |""".stripMargin

  checkSection(
    "an update with a variable index",
    updateWithVariableIndex,
    """|[0:8..0:10): vn <= vn/
       |[1:6..1:7): M <= vn/M#
       |[1:8..1:8):  <= vn/M#`<init>`().
       |[1:14..1:19): apply <= vn/M#apply().
       |[1:20..1:21): k <= vn/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => vn/M#apply().(k)
       |[1:42..1:48): update <= vn/M#update().
       |[1:49..1:50): k <= vn/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= vn/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): N <= vn/N.
       |[3:6..3:7): n <= vn/N.n().
       |[4:6..4:9): arr <= vn/N.arr.
       |[4:12..4:17): Array => scala/Array.
       |[5:6..5:7): m <= vn/N.m.
       |[5:14..5:15): M => vn/M#
       |[5:15..5:15):  => vn/M#`<init>`().
       |[6:2..6:5): arr => vn/N.arr.
       |[6:6..6:7): n => vn/N.n().
       |[7:2..7:5): arr => vn/N.arr.
       |[7:7..7:8): n => vn/N.n().
       |[8:2..8:5): arr => vn/N.arr.
       |[8:6..8:7): n => vn/N.n().
       |[8:9..8:11): += => scala/Int#`+`(+4).
       |[9:2..9:5): arr => vn/N.arr.
       |[9:7..9:8): n => vn/N.n().
       |[9:10..9:12): += => scala/Int#`+`(+4).
       |[10:2..10:3): m => vn/N.m.
       |[10:4..10:5): n => vn/N.n().
       |[11:2..11:3): m => vn/N.m.
       |[11:5..11:6): n => vn/N.n().
       |[12:2..12:3): m => vn/N.m.
       |[12:4..12:5): n => vn/N.n().
       |[12:7..12:9): += => scala/Int#`+`(+4).
       |[13:2..13:3): m => vn/N.m.
       |[13:5..13:6): n => vn/N.n().
       |[13:8..13:10): += => scala/Int#`+`(+4).
       |[14:2..14:5): arr => vn/N.arr.
       |[14:6..14:7): i => scala/Array#update().(i)
       |[15:2..15:5): arr => vn/N.arr.
       |[15:7..15:8): i => scala/Array#update().(i)
       |[16:2..16:3): m => vn/N.m.
       |[16:4..16:5): k => vn/M#update().(k)
       |[17:2..17:3): m => vn/N.m.
       |[17:5..17:6): k => vn/M#update().(k)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update with a variable index: synthetics",
    updateWithVariableIndex,
    """|[4:12..4:17): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[6:2..6:5): arr => *.update
       |  update => scala/Array#update().
       |[7:2..7:5): arr => *.update
       |  update => scala/Array#update().
       |[8:2..8:5): arr => *.apply
       |  apply => scala/Array#apply().
       |[9:2..9:5): arr => *.apply
       |  apply => scala/Array#apply().
       |[10:2..10:3): m => *.update
       |  update => vn/M#update().
       |[11:2..11:3): m => *.update
       |  update => vn/M#update().
       |[12:2..12:3): m => *.apply
       |  apply => vn/M#apply().
       |[13:2..13:3): m => *.apply
       |  apply => vn/M#apply().
       |[14:2..14:5): arr => *.update
       |  update => scala/Array#update().
       |[15:2..15:5): arr => *.update
       |  update => scala/Array#update().
       |[16:2..16:3): m => *.update
       |  update => vn/M#update().
       |[17:2..17:3): m => *.update
       |  update => vn/M#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateWithOperatorAndVariableIndex =
    """|package vo
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  var n = 0
       |  val arr = Array(1, 2)
       |  val m = new M
       |  arr(i = n) += 5
       |  arr (i = n) += 5
       |  m(k = n) += 5
       |  m (k = n) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an update with an operator and a variable index",
    updateWithOperatorAndVariableIndex,
    """|[0:8..0:10): vo <= vo/
       |[1:6..1:7): M <= vo/M#
       |[1:8..1:8):  <= vo/M#`<init>`().
       |[1:14..1:19): apply <= vo/M#apply().
       |[1:20..1:21): k <= vo/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => vo/M#apply().(k)
       |[1:42..1:48): update <= vo/M#update().
       |[1:49..1:50): k <= vo/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= vo/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): O <= vo/O.
       |[3:6..3:7): n <= vo/O.n().
       |[4:6..4:9): arr <= vo/O.arr.
       |[4:12..4:17): Array => scala/Array.
       |[5:6..5:7): m <= vo/O.m.
       |[5:14..5:15): M => vo/M#
       |[5:15..5:15):  => vo/M#`<init>`().
       |[6:2..6:5): arr => vo/O.arr.
       |[6:6..6:7): i => scala/Array#apply().(i)
       |[6:10..6:11): n => vo/O.n().
       |[6:13..6:15): += => scala/Int#`+`(+4).
       |[7:2..7:5): arr => vo/O.arr.
       |[7:7..7:8): i => scala/Array#apply().(i)
       |[7:11..7:12): n => vo/O.n().
       |[7:14..7:16): += => scala/Int#`+`(+4).
       |[8:2..8:3): m => vo/O.m.
       |[8:4..8:5): k => vo/M#apply().(k)
       |[8:8..8:9): n => vo/O.n().
       |[8:11..8:13): += => scala/Int#`+`(+4).
       |[9:2..9:3): m => vo/O.m.
       |[9:5..9:6): k => vo/M#apply().(k)
       |[9:9..9:10): n => vo/O.n().
       |[9:12..9:14): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an update with an operator and a variable index: synthetics",
    updateWithOperatorAndVariableIndex,
    """|[4:12..4:17): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[6:2..6:5): arr => *.apply
       |  apply => scala/Array#apply().
       |[7:2..7:5): arr => *.apply
       |  apply => scala/Array#apply().
       |[8:2..8:3): m => *.apply
       |  apply => vo/M#apply().
       |[9:2..9:3): m => *.apply
       |  apply => vo/M#apply().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateWithOperatorAndNamedCallInIndex =
    """|package vp
       |object O {
       |  def h(x: Int) = x
       |  val arr = Array(1, 2)
       |  arr(i = h(x = 1)) += 5
       |  arr (i = h(x = 1)) += 5
       |  arr(i = h (x = 1)) += 5
       |  arr (i = h (x = 1)) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an update with an operator and a named call in the index",
    updateWithOperatorAndNamedCallInIndex,
    """|[0:8..0:10): vp <= vp/
       |[1:7..1:8): O <= vp/O.
       |[2:6..2:7): h <= vp/O.h().
       |[2:8..2:9): x <= vp/O.h().(x)
       |[2:11..2:14): Int => scala/Int#
       |[2:18..2:19): x => vp/O.h().(x)
       |[3:6..3:9): arr <= vp/O.arr.
       |[3:12..3:17): Array => scala/Array.
       |[4:2..4:5): arr => vp/O.arr.
       |[4:6..4:7): i => scala/Array#apply().(i)
       |[4:10..4:11): h => vp/O.h().
       |[4:12..4:13): x => vp/O.h().(x)
       |[4:20..4:22): += => scala/Int#`+`(+4).
       |[5:2..5:5): arr => vp/O.arr.
       |[5:7..5:8): i => scala/Array#apply().(i)
       |[5:11..5:12): h => vp/O.h().
       |[5:13..5:14): x => vp/O.h().(x)
       |[5:21..5:23): += => scala/Int#`+`(+4).
       |[6:2..6:5): arr => vp/O.arr.
       |[6:6..6:7): i => scala/Array#apply().(i)
       |[6:10..6:11): h => vp/O.h().
       |[6:13..6:14): x => vp/O.h().(x)
       |[6:21..6:23): += => scala/Int#`+`(+4).
       |[7:2..7:5): arr => vp/O.arr.
       |[7:7..7:8): i => scala/Array#apply().(i)
       |[7:11..7:12): h => vp/O.h().
       |[7:14..7:15): x => vp/O.h().(x)
       |[7:22..7:24): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an update with an operator and a named call in the index: synthetics",
    updateWithOperatorAndNamedCallInIndex,
    """|[3:12..3:17): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[4:2..4:5): arr => *.apply
       |  apply => scala/Array#apply().
       |[5:2..5:5): arr => *.apply
       |  apply => scala/Array#apply().
       |[6:2..6:5): arr => *.apply
       |  apply => scala/Array#apply().
       |[7:2..7:5): arr => *.apply
       |  apply => scala/Array#apply().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateOfElementWithMemberOperator =
    """|package vq
       |class B { def +=(y: Int) = this }
       |class M { def apply(k: Int) = new B; def update(k: Int, b: B): Unit = () }
       |object O {
       |  val m = new M
       |  m(0) += 5
       |  m(k = 0) += 5
       |  m (k = 0) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an element with a member operator",
    updateOfElementWithMemberOperator,
    """|[0:8..0:10): vq <= vq/
       |[1:6..1:7): B <= vq/B#
       |[1:8..1:8):  <= vq/B#`<init>`().
       |[1:14..1:16): += <= vq/B#`+=`().
       |[1:17..1:18): y <= vq/B#`+=`().(y)
       |[1:20..1:23): Int => scala/Int#
       |[2:6..2:7): M <= vq/M#
       |[2:8..2:8):  <= vq/M#`<init>`().
       |[2:14..2:19): apply <= vq/M#apply().
       |[2:20..2:21): k <= vq/M#apply().(k)
       |[2:23..2:26): Int => scala/Int#
       |[2:34..2:35): B => vq/B#
       |[2:35..2:35):  => vq/B#`<init>`().
       |[2:41..2:47): update <= vq/M#update().
       |[2:48..2:49): k <= vq/M#update().(k)
       |[2:51..2:54): Int => scala/Int#
       |[2:56..2:57): b <= vq/M#update().(b)
       |[2:59..2:60): B => vq/B#
       |[2:63..2:67): Unit => scala/Unit#
       |[3:7..3:8): O <= vq/O.
       |[4:6..4:7): m <= vq/O.m.
       |[4:14..4:15): M => vq/M#
       |[4:15..4:15):  => vq/M#`<init>`().
       |[5:2..5:3): m => vq/O.m.
       |[5:7..5:9): += => vq/B#`+=`().
       |[6:2..6:3): m => vq/O.m.
       |[6:4..6:5): k => vq/M#apply().(k)
       |[6:11..6:13): += => vq/B#`+=`().
       |[7:2..7:3): m => vq/O.m.
       |[7:5..7:6): k => vq/M#apply().(k)
       |[7:12..7:14): += => vq/B#`+=`().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an element with a member operator: synthetics",
    updateOfElementWithMemberOperator,
    """|[5:2..5:3): m => *.apply
       |  apply => vq/M#apply().
       |[6:2..6:3): m => *.apply
       |  apply => vq/M#apply().
       |[7:2..7:3): m => *.apply
       |  apply => vq/M#apply().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateWithOperatorAndOtherIndexName =
    """|package vr
       |class M { def apply(k: Int): Int = k; def update(j: Int, v: Int): Unit = () }
       |object O {
       |  val m = new M
       |  m(k = 0) += 5
       |  m (k = 0) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an update with an operator and another index name",
    updateWithOperatorAndOtherIndexName,
    """|[0:8..0:10): vr <= vr/
       |[1:6..1:7): M <= vr/M#
       |[1:8..1:8):  <= vr/M#`<init>`().
       |[1:14..1:19): apply <= vr/M#apply().
       |[1:20..1:21): k <= vr/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => vr/M#apply().(k)
       |[1:42..1:48): update <= vr/M#update().
       |[1:49..1:50): j <= vr/M#update().(j)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= vr/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): O <= vr/O.
       |[3:6..3:7): m <= vr/O.m.
       |[3:14..3:15): M => vr/M#
       |[3:15..3:15):  => vr/M#`<init>`().
       |[4:2..4:3): m => vr/O.m.
       |[4:4..4:5): k => vr/M#apply().(k)
       |[4:11..4:13): += => scala/Int#`+`(+4).
       |[5:2..5:3): m => vr/O.m.
       |[5:5..5:6): k => vr/M#apply().(k)
       |[5:12..5:14): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an update with an operator and another index name: synthetics",
    updateWithOperatorAndOtherIndexName,
    """|[4:2..4:3): m => *.apply
       |  apply => vr/M#apply().
       |[5:2..5:3): m => *.apply
       |  apply => vr/M#apply().
       |""".stripMargin,
    "Synthetics",
  )

  checkSection(
    "named arguments of an apply on a constructor call with a parameter of the same name",
    """|package vt
       |class K(a: Int) { def apply(a: Int, b: Int) = a }
       |object O {
       |  val p = new K(a = 1)(a = 2, b = 3)
       |  val q = new K(a = 1)(b = 3, a = 2)
       |  val r = new K (a = 1) (a = 2, b = 3)
       |  val s = new K (a = 1) (b = 3, a = 2)
       |}
       |""".stripMargin,
    """|[0:8..0:10): vt <= vt/
       |[1:6..1:7): K <= vt/K#
       |[1:7..1:7):  <= vt/K#`<init>`().
       |[1:8..1:9): a <= vt/K#`<init>`().(a)
       |[1:8..1:9): a <= vt/K#a.
       |[1:11..1:14): Int => scala/Int#
       |[1:22..1:27): apply <= vt/K#apply().
       |[1:28..1:29): a <= vt/K#apply().(a)
       |[1:31..1:34): Int => scala/Int#
       |[1:36..1:37): b <= vt/K#apply().(b)
       |[1:39..1:42): Int => scala/Int#
       |[1:46..1:47): a => vt/K#apply().(a)
       |[2:7..2:8): O <= vt/O.
       |[3:6..3:7): p <= vt/O.p.
       |[3:14..3:15): K => vt/K#
       |[3:15..3:15):  => vt/K#`<init>`().
       |[3:16..3:17): a => vt/K#`<init>`().(a)
       |[3:23..3:24): a => vt/K#apply().(a)
       |[3:30..3:31): b => vt/K#apply().(b)
       |[4:6..4:7): q <= vt/O.q.
       |[4:14..4:15): K => vt/K#
       |[4:15..4:15):  => vt/K#`<init>`().
       |[4:16..4:17): a => vt/K#`<init>`().(a)
       |[4:23..4:24): b => vt/K#apply().(b)
       |[4:30..4:31): a => vt/K#apply().(a)
       |[5:6..5:7): r <= vt/O.r.
       |[5:14..5:15): K => vt/K#
       |[5:16..5:16):  => vt/K#`<init>`().
       |[5:17..5:18): a => vt/K#`<init>`().(a)
       |[5:25..5:26): a => vt/K#apply().(a)
       |[5:32..5:33): b => vt/K#apply().(b)
       |[6:6..6:7): s <= vt/O.s.
       |[6:14..6:15): K => vt/K#
       |[6:16..6:16):  => vt/K#`<init>`().
       |[6:17..6:18): a => vt/K#`<init>`().(a)
       |[6:25..6:26): b => vt/K#apply().(b)
       |[6:32..6:33): a => vt/K#apply().(a)
       |""".stripMargin,
    "Occurrences",
  )

  private val implicitConstructorClause =
    """|package vu
       |class K(x: Int)(implicit i: Int)
       |object O {
       |  implicit val z: Int = 0
       |  val a = new K(x = 1)(i = 2)
       |  val b = new K (x = 1)(i = 2)
       |  val c = new K(x = 1) (i = 2)
       |  val d = new K (x = 1) (i = 2)
       |  val e = new K(x = 1)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an implicit constructor clause",
    implicitConstructorClause,
    """|[0:8..0:10): vu <= vu/
       |[1:6..1:7): K <= vu/K#
       |[1:7..1:7):  <= vu/K#`<init>`().
       |[1:8..1:9): x <= vu/K#`<init>`().(x)
       |[1:8..1:9): x <= vu/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[1:25..1:26): i <= vu/K#`<init>`().(i)
       |[1:25..1:26): i <= vu/K#i.
       |[1:28..1:31): Int => scala/Int#
       |[2:7..2:8): O <= vu/O.
       |[3:15..3:16): z <= vu/O.z.
       |[3:18..3:21): Int => scala/Int#
       |[4:6..4:7): a <= vu/O.a.
       |[4:14..4:15): K => vu/K#
       |[4:15..4:15):  => vu/K#`<init>`().
       |[4:16..4:17): x => vu/K#`<init>`().(x)
       |[4:23..4:24): i => vu/K#`<init>`().(i)
       |[5:6..5:7): b <= vu/O.b.
       |[5:14..5:15): K => vu/K#
       |[5:16..5:16):  => vu/K#`<init>`().
       |[5:17..5:18): x => vu/K#`<init>`().(x)
       |[5:24..5:25): i => vu/K#`<init>`().(i)
       |[6:6..6:7): c <= vu/O.c.
       |[6:14..6:15): K => vu/K#
       |[6:15..6:15):  => vu/K#`<init>`().
       |[6:16..6:17): x => vu/K#`<init>`().(x)
       |[6:24..6:25): i => vu/K#`<init>`().(i)
       |[7:6..7:7): d <= vu/O.d.
       |[7:14..7:15): K => vu/K#
       |[7:16..7:16):  => vu/K#`<init>`().
       |[7:17..7:18): x => vu/K#`<init>`().(x)
       |[7:25..7:26): i => vu/K#`<init>`().(i)
       |[8:6..8:7): e <= vu/O.e.
       |[8:14..8:15): K => vu/K#
       |[8:15..8:15):  => vu/K#`<init>`().
       |[8:16..8:17): x => vu/K#`<init>`().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an implicit constructor clause: synthetics",
    implicitConstructorClause,
    """|[8:10..8:22): new K(x = 1) => *(z)
       |  z => vu/O.z.
       |""".stripMargin,
    "Synthetics",
  )

  checkSection(
    "named arguments of a super call with a class parameter of the same name",
    """|package vv
       |class K(x: Int)
       |class X(x: Int) extends K(x = x + 1)
       |class Y(x: Int) extends K (x = x + 1)
       |""".stripMargin,
    """|[0:8..0:10): vv <= vv/
       |[1:6..1:7): K <= vv/K#
       |[1:7..1:7):  <= vv/K#`<init>`().
       |[1:8..1:9): x <= vv/K#`<init>`().(x)
       |[1:8..1:9): x <= vv/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[2:6..2:7): X <= vv/X#
       |[2:7..2:7):  <= vv/X#`<init>`().
       |[2:8..2:9): x <= vv/X#`<init>`().(x)
       |[2:8..2:9): x <= vv/X#x.
       |[2:11..2:14): Int => scala/Int#
       |[2:24..2:25): K => vv/K#
       |[2:25..2:25):  => vv/K#`<init>`().
       |[2:26..2:27): x => vv/K#`<init>`().(x)
       |[2:30..2:31): x => vv/X#`<init>`().(x)
       |[2:32..2:33): + => scala/Int#`+`(+4).
       |[3:6..3:7): Y <= vv/Y#
       |[3:7..3:7):  <= vv/Y#`<init>`().
       |[3:8..3:9): x <= vv/Y#`<init>`().(x)
       |[3:8..3:9): x <= vv/Y#x.
       |[3:11..3:14): Int => scala/Int#
       |[3:24..3:25): K => vv/K#
       |[3:26..3:26):  => vv/K#`<init>`().
       |[3:27..3:28): x => vv/K#`<init>`().(x)
       |[3:31..3:32): x => vv/Y#`<init>`().(x)
       |[3:33..3:34): + => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  locally {
    val expected =
      """|[0:8..0:10): vw <= vw/
         |[1:6..1:7): L <= vw/L#
         |[1:7..1:7):  <= vw/L#`<init>`().
         |[1:8..1:9): x <= vw/L#`<init>`().(x)
         |[1:8..1:9): x <= vw/L#x.
         |[1:11..1:14): Int => scala/Int#
         |[2:6..2:7): K <= vw/K#
         |[2:7..2:7):  <= vw/K#`<init>`().
         |[2:8..2:9): x <= vw/K#`<init>`().(x)
         |[2:8..2:9): x <= vw/K#x.
         |[2:11..2:12): L => vw/L#
         |[2:14..2:15): y <= vw/K#`<init>`().(y)
         |[2:14..2:15): y <= vw/K#y.
         |[2:17..2:20): Int => scala/Int#
         |[3:6..3:7): A <= vw/A#
         |[3:8..3:8):  <= vw/A#`<init>`().
         |[3:16..3:17): K => vw/K#
         |[3:17..3:17):  => vw/K#`<init>`().
         |[3:22..3:23): L => vw/L#
         |[3:23..3:23):  => vw/L#`<init>`().
         |[3:24..3:25): x => vw/L#`<init>`().(x)
         |[4:6..4:7): B <= vw/B#
         |[4:8..4:8):  <= vw/B#`<init>`().
         |[4:16..4:17): K => vw/K#
         |[4:18..4:18):  => vw/K#`<init>`().
         |[4:23..4:24): L => vw/L#
         |[4:25..4:25):  => vw/L#`<init>`().
         |[4:26..4:27): x => vw/L#`<init>`().(x)
         |[5:6..5:7): C <= vw/C#
         |[5:8..5:8):  <= vw/C#`<init>`().
         |[5:16..5:17): K => vw/K#
         |[5:17..5:17):  => vw/K#`<init>`().
         |[5:18..5:19): x => vw/K#`<init>`().(x)
         |[5:26..5:27): L => vw/L#
         |[5:27..5:27):  => vw/L#`<init>`().
         |[5:28..5:29): x => vw/L#`<init>`().(x)
         |[5:36..5:37): y => vw/K#`<init>`().(y)
         |[6:6..6:7): D <= vw/D#
         |[6:8..6:8):  <= vw/D#`<init>`().
         |[6:16..6:17): K => vw/K#
         |[6:18..6:18):  => vw/K#`<init>`().
         |[6:19..6:20): x => vw/K#`<init>`().(x)
         |[6:27..6:28): L => vw/L#
         |[6:29..6:29):  => vw/L#`<init>`().
         |[6:30..6:31): x => vw/L#`<init>`().(x)
         |[6:38..6:39): y => vw/K#`<init>`().(y)
         |[7:6..7:7): E <= vw/E#
         |[7:8..7:8):  <= vw/E#`<init>`().
         |[7:16..7:17): K => vw/K#
         |[7:17..7:17):  => vw/K#`<init>`().
         |[7:18..7:19): y => vw/K#`<init>`().(y)
         |[7:25..7:26): x => vw/K#`<init>`().(x)
         |[7:33..7:34): L => vw/L#
         |[7:34..7:34):  => vw/L#`<init>`().
         |[7:35..7:36): x => vw/L#`<init>`().(x)
         |[8:6..8:7): F <= vw/F#
         |[8:8..8:8):  <= vw/F#`<init>`().
         |[8:16..8:17): K => vw/K#
         |[8:18..8:18):  => vw/K#`<init>`().
         |[8:19..8:20): y => vw/K#`<init>`().(y)
         |[8:26..8:27): x => vw/K#`<init>`().(x)
         |[8:34..8:35): L => vw/L#
         |[8:36..8:36):  => vw/L#`<init>`().
         |[8:37..8:38): x => vw/L#`<init>`().(x)
         |""".stripMargin
    checkSection(
      "named arguments of a constructor call in a super call",
      """|package vw
         |class L(x: Int)
         |class K(x: L, y: Int = 0)
         |class A extends K(new L(x = 1))
         |class B extends K (new L (x = 1))
         |class C extends K(x = new L(x = 1), y = 0)
         |class D extends K (x = new L (x = 1), y = 0)
         |class E extends K(y = 0, x = new L(x = 1))
         |class F extends K (y = 0, x = new L (x = 1))
         |""".stripMargin,
      expected,
      "Occurrences",
    )
  }

  checkSection(
    "named arguments of a constructor call in a self invocation",
    """|package vx
       |class L(x: Int)
       |class K(x: L, y: Int = 0) {
       |  def this(a: Byte) = this(new L(x = 1))
       |  def this(a: Short) = this (new L (x = 1))
       |  def this(a: Char) = this(x = new L(x = 1), y = 0)
       |  def this(a: Long) = this (x = new L (x = 1), y = 0)
       |  def this(a: Float) = this(y = 0, x = new L(x = 1))
       |  def this(a: Double) = this (y = 0, x = new L (x = 1))
       |}
       |""".stripMargin,
    """|[0:8..0:10): vx <= vx/
       |[1:6..1:7): L <= vx/L#
       |[1:7..1:7):  <= vx/L#`<init>`().
       |[1:8..1:9): x <= vx/L#`<init>`().(x)
       |[1:8..1:9): x <= vx/L#x.
       |[1:11..1:14): Int => scala/Int#
       |[2:6..2:7): K <= vx/K#
       |[2:7..2:7):  <= vx/K#`<init>`().
       |[2:8..2:9): x <= vx/K#`<init>`().(x)
       |[2:8..2:9): x <= vx/K#x.
       |[2:11..2:12): L => vx/L#
       |[2:14..2:15): y <= vx/K#`<init>`().(y)
       |[2:14..2:15): y <= vx/K#y.
       |[2:17..2:20): Int => scala/Int#
       |[3:6..3:10): this <= vx/K#`<init>`(+1).
       |[3:11..3:12): a <= vx/K#`<init>`(+1).(a)
       |[3:14..3:18): Byte => scala/Byte#
       |[3:26..3:26):  => vx/K#`<init>`().
       |[3:31..3:32): L => vx/L#
       |[3:32..3:32):  => vx/L#`<init>`().
       |[3:33..3:34): x => vx/L#`<init>`().(x)
       |[4:6..4:10): this <= vx/K#`<init>`(+2).
       |[4:11..4:12): a <= vx/K#`<init>`(+2).(a)
       |[4:14..4:19): Short => scala/Short#
       |[4:28..4:28):  => vx/K#`<init>`().
       |[4:33..4:34): L => vx/L#
       |[4:35..4:35):  => vx/L#`<init>`().
       |[4:36..4:37): x => vx/L#`<init>`().(x)
       |[5:6..5:10): this <= vx/K#`<init>`(+3).
       |[5:11..5:12): a <= vx/K#`<init>`(+3).(a)
       |[5:14..5:18): Char => scala/Char#
       |[5:26..5:26):  => vx/K#`<init>`().
       |[5:27..5:28): x => vx/K#`<init>`().(x)
       |[5:35..5:36): L => vx/L#
       |[5:36..5:36):  => vx/L#`<init>`().
       |[5:37..5:38): x => vx/L#`<init>`().(x)
       |[5:45..5:46): y => vx/K#`<init>`().(y)
       |[6:6..6:10): this <= vx/K#`<init>`(+4).
       |[6:11..6:12): a <= vx/K#`<init>`(+4).(a)
       |[6:14..6:18): Long => scala/Long#
       |[6:27..6:27):  => vx/K#`<init>`().
       |[6:28..6:29): x => vx/K#`<init>`().(x)
       |[6:36..6:37): L => vx/L#
       |[6:38..6:38):  => vx/L#`<init>`().
       |[6:39..6:40): x => vx/L#`<init>`().(x)
       |[6:47..6:48): y => vx/K#`<init>`().(y)
       |[7:6..7:10): this <= vx/K#`<init>`(+5).
       |[7:11..7:12): a <= vx/K#`<init>`(+5).(a)
       |[7:14..7:19): Float => scala/Float#
       |[7:27..7:27):  => vx/K#`<init>`().
       |[7:28..7:29): y => vx/K#`<init>`().(y)
       |[7:35..7:36): x => vx/K#`<init>`().(x)
       |[7:43..7:44): L => vx/L#
       |[7:44..7:44):  => vx/L#`<init>`().
       |[7:45..7:46): x => vx/L#`<init>`().(x)
       |[8:6..8:10): this <= vx/K#`<init>`(+6).
       |[8:11..8:12): a <= vx/K#`<init>`(+6).(a)
       |[8:14..8:20): Double => scala/Double#
       |[8:29..8:29):  => vx/K#`<init>`().
       |[8:30..8:31): y => vx/K#`<init>`().(y)
       |[8:37..8:38): x => vx/K#`<init>`().(x)
       |[8:45..8:46): L => vx/L#
       |[8:47..8:47):  => vx/L#`<init>`().
       |[8:48..8:49): x => vx/L#`<init>`().(x)
       |""".stripMargin,
    "Occurrences",
  )

  locally {
    val expected =
      """|[0:8..0:10): vy <= vy/
         |[1:6..1:7): L <= vy/L#
         |[1:7..1:7):  <= vy/L#`<init>`().
         |[1:8..1:9): x <= vy/L#`<init>`().(x)
         |[1:8..1:9): x <= vy/L#x.
         |[1:11..1:14): Int => scala/Int#
         |[2:6..2:7): K <= vy/K#
         |[2:7..2:7):  <= vy/K#`<init>`().
         |[2:8..2:9): x <= vy/K#`<init>`().(x)
         |[2:8..2:9): x <= vy/K#x.
         |[2:11..2:12): L => vy/L#
         |[2:14..2:15): y <= vy/K#`<init>`().(y)
         |[2:14..2:15): y <= vy/K#y.
         |[2:17..2:20): Int => scala/Int#
         |[3:7..3:8): O <= vy/O.
         |[4:6..4:6):  <= local0
         |[4:6..4:7): K => vy/K#
         |[4:7..4:7):  => vy/K#`<init>`().
         |[4:12..4:13): L => vy/L#
         |[4:13..4:13):  => vy/L#`<init>`().
         |[4:14..4:15): x => vy/L#`<init>`().(x)
         |[5:6..5:6):  <= local1
         |[5:6..5:7): K => vy/K#
         |[5:8..5:8):  => vy/K#`<init>`().
         |[5:13..5:14): L => vy/L#
         |[5:15..5:15):  => vy/L#`<init>`().
         |[5:16..5:17): x => vy/L#`<init>`().(x)
         |[6:6..6:6):  <= local2
         |[6:6..6:7): K => vy/K#
         |[6:7..6:7):  => vy/K#`<init>`().
         |[6:8..6:9): x => vy/K#`<init>`().(x)
         |[6:16..6:17): L => vy/L#
         |[6:17..6:17):  => vy/L#`<init>`().
         |[6:18..6:19): x => vy/L#`<init>`().(x)
         |[6:26..6:27): y => vy/K#`<init>`().(y)
         |[7:6..7:6):  <= local3
         |[7:6..7:7): K => vy/K#
         |[7:8..7:8):  => vy/K#`<init>`().
         |[7:9..7:10): x => vy/K#`<init>`().(x)
         |[7:17..7:18): L => vy/L#
         |[7:19..7:19):  => vy/L#`<init>`().
         |[7:20..7:21): x => vy/L#`<init>`().(x)
         |[7:28..7:29): y => vy/K#`<init>`().(y)
         |[8:6..8:6):  <= local4
         |[8:6..8:7): K => vy/K#
         |[8:7..8:7):  => vy/K#`<init>`().
         |[8:8..8:9): y => vy/K#`<init>`().(y)
         |[8:15..8:16): x => vy/K#`<init>`().(x)
         |[8:23..8:24): L => vy/L#
         |[8:24..8:24):  => vy/L#`<init>`().
         |[8:25..8:26): x => vy/L#`<init>`().(x)
         |[9:6..9:6):  <= local5
         |[9:6..9:7): K => vy/K#
         |[9:8..9:8):  => vy/K#`<init>`().
         |[9:9..9:10): y => vy/K#`<init>`().(y)
         |[9:16..9:17): x => vy/K#`<init>`().(x)
         |[9:24..9:25): L => vy/L#
         |[9:26..9:26):  => vy/L#`<init>`().
         |[9:27..9:28): x => vy/L#`<init>`().(x)
         |""".stripMargin
    checkSection(
      "named arguments of a constructor call in an anonymous class",
      """|package vy
         |class L(x: Int)
         |class K(x: L, y: Int = 0)
         |object O {
         |  new K(new L(x = 1)) {}
         |  new K (new L (x = 1)) {}
         |  new K(x = new L(x = 1), y = 0) {}
         |  new K (x = new L (x = 1), y = 0) {}
         |  new K(y = 0, x = new L(x = 1)) {}
         |  new K (y = 0, x = new L (x = 1)) {}
         |}
         |""".stripMargin,
      expected,
      "Occurrences",
    )
  }

  private val operatorOnMethodResult =
    """|package vz
       |class B { def +=(y: Int) = this }
       |object O {
       |  def m(k: Int) = new B
       |  m(k = 0) += 5
       |  m (k = 0) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of a method result with a member operator",
    operatorOnMethodResult,
    """|[0:8..0:10): vz <= vz/
       |[1:6..1:7): B <= vz/B#
       |[1:8..1:8):  <= vz/B#`<init>`().
       |[1:14..1:16): += <= vz/B#`+=`().
       |[1:17..1:18): y <= vz/B#`+=`().(y)
       |[1:20..1:23): Int => scala/Int#
       |[2:7..2:8): O <= vz/O.
       |[3:6..3:7): m <= vz/O.m().
       |[3:8..3:9): k <= vz/O.m().(k)
       |[3:11..3:14): Int => scala/Int#
       |[3:22..3:23): B => vz/B#
       |[3:23..3:23):  => vz/B#`<init>`().
       |[4:2..4:3): m => vz/O.m().
       |[4:4..4:5): k => vz/O.m().(k)
       |[4:11..4:13): += => vz/B#`+=`().
       |[5:2..5:3): m => vz/O.m().
       |[5:5..5:6): k => vz/O.m().(k)
       |[5:12..5:14): += => vz/B#`+=`().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a method result with a member operator: synthetics",
    operatorOnMethodResult,
    "",
    "Synthetics",
  )

  private val updateWithOtherIndexName =
    """|package wa
       |class M { def apply(k: Int): Int = k; def update(j: Int, v: Int): Unit = () }
       |object O {
       |  val m = new M
       |  m(j = 0) = 5
       |  m (j = 0) = 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an update assignment with another index name",
    updateWithOtherIndexName,
    """|[0:8..0:10): wa <= wa/
       |[1:6..1:7): M <= wa/M#
       |[1:8..1:8):  <= wa/M#`<init>`().
       |[1:14..1:19): apply <= wa/M#apply().
       |[1:20..1:21): k <= wa/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => wa/M#apply().(k)
       |[1:42..1:48): update <= wa/M#update().
       |[1:49..1:50): j <= wa/M#update().(j)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= wa/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): O <= wa/O.
       |[3:6..3:7): m <= wa/O.m.
       |[3:14..3:15): M => wa/M#
       |[3:15..3:15):  => wa/M#`<init>`().
       |[4:2..4:3): m => wa/O.m.
       |[4:4..4:5): j => wa/M#update().(j)
       |[5:2..5:3): m => wa/O.m.
       |[5:5..5:6): j => wa/M#update().(j)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an update assignment with another index name: synthetics",
    updateWithOtherIndexName,
    """|[4:2..4:3): m => *.update
       |  update => wa/M#update().
       |[5:2..5:3): m => *.update
       |  update => wa/M#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val operatorOnWrittenUpdate =
    """|package wb
       |class B { def +=(y: Int) = this }
       |object O {
       |  def update(k: Int) = new B
       |  update(k = 0) += 5
       |  update (k = 0) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of a written update with a member operator",
    operatorOnWrittenUpdate,
    """|[0:8..0:10): wb <= wb/
       |[1:6..1:7): B <= wb/B#
       |[1:8..1:8):  <= wb/B#`<init>`().
       |[1:14..1:16): += <= wb/B#`+=`().
       |[1:17..1:18): y <= wb/B#`+=`().(y)
       |[1:20..1:23): Int => scala/Int#
       |[2:7..2:8): O <= wb/O.
       |[3:6..3:12): update <= wb/O.update().
       |[3:13..3:14): k <= wb/O.update().(k)
       |[3:16..3:19): Int => scala/Int#
       |[3:27..3:28): B => wb/B#
       |[3:28..3:28):  => wb/B#`<init>`().
       |[4:2..4:8): update => wb/O.update().
       |[4:9..4:10): k => wb/O.update().(k)
       |[4:16..4:18): += => wb/B#`+=`().
       |[5:2..5:8): update => wb/O.update().
       |[5:10..5:11): k => wb/O.update().(k)
       |[5:17..5:19): += => wb/B#`+=`().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a written update with a member operator: synthetics",
    operatorOnWrittenUpdate,
    "",
    "Synthetics",
  )

  private val updateOfMethodResult =
    """|package wc
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object C {
       |  val m = new M
       |  def a(i: Int) = m
       |  a(i = 0)(k = 1) = 5
       |  a (i = 0)(k = 1) = 5
       |  a(i = 0) (k = 1) = 5
       |  a (i = 0) (k = 1) = 5
       |  a(i = 0)(k = 1) += 5
       |  a (i = 0)(k = 1) += 5
       |  a(i = 0) (k = 1) += 5
       |  a (i = 0) (k = 1) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an update of a method result",
    updateOfMethodResult,
    """|[0:8..0:10): wc <= wc/
       |[1:6..1:7): M <= wc/M#
       |[1:8..1:8):  <= wc/M#`<init>`().
       |[1:14..1:19): apply <= wc/M#apply().
       |[1:20..1:21): k <= wc/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => wc/M#apply().(k)
       |[1:42..1:48): update <= wc/M#update().
       |[1:49..1:50): k <= wc/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= wc/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): C <= wc/C.
       |[3:6..3:7): m <= wc/C.m.
       |[3:14..3:15): M => wc/M#
       |[3:15..3:15):  => wc/M#`<init>`().
       |[4:6..4:7): a <= wc/C.a().
       |[4:8..4:9): i <= wc/C.a().(i)
       |[4:11..4:14): Int => scala/Int#
       |[4:18..4:19): m => wc/C.m.
       |[5:2..5:3): a => wc/C.a().
       |[5:4..5:5): i => wc/C.a().(i)
       |[5:11..5:12): k => wc/M#update().(k)
       |[6:2..6:3): a => wc/C.a().
       |[6:5..6:6): i => wc/C.a().(i)
       |[6:12..6:13): k => wc/M#update().(k)
       |[7:2..7:3): a => wc/C.a().
       |[7:4..7:5): i => wc/C.a().(i)
       |[7:12..7:13): k => wc/M#update().(k)
       |[8:2..8:3): a => wc/C.a().
       |[8:5..8:6): i => wc/C.a().(i)
       |[8:13..8:14): k => wc/M#update().(k)
       |[9:2..9:3): a => wc/C.a().
       |[9:4..9:5): i => wc/C.a().(i)
       |[9:11..9:12): k => wc/M#apply().(k)
       |[9:18..9:20): += => scala/Int#`+`(+4).
       |[10:2..10:3): a => wc/C.a().
       |[10:5..10:6): i => wc/C.a().(i)
       |[10:12..10:13): k => wc/M#apply().(k)
       |[10:19..10:21): += => scala/Int#`+`(+4).
       |[11:2..11:3): a => wc/C.a().
       |[11:4..11:5): i => wc/C.a().(i)
       |[11:12..11:13): k => wc/M#apply().(k)
       |[11:19..11:21): += => scala/Int#`+`(+4).
       |[12:2..12:3): a => wc/C.a().
       |[12:5..12:6): i => wc/C.a().(i)
       |[12:13..12:14): k => wc/M#apply().(k)
       |[12:20..12:22): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an update of a method result: synthetics",
    updateOfMethodResult,
    """|[5:2..5:10): a(i = 0) => *.update
       |  update => wc/M#update().
       |[6:2..6:11): a (i = 0) => *.update
       |  update => wc/M#update().
       |[7:2..7:10): a(i = 0) => *.update
       |  update => wc/M#update().
       |[8:2..8:11): a (i = 0) => *.update
       |  update => wc/M#update().
       |""".stripMargin,
    "Synthetics",
  )

  locally {
    val expected =
      """|[0:8..0:10): wd <= wd/
         |[1:6..1:7): L <= wd/L#
         |[1:7..1:7):  <= wd/L#`<init>`().
         |[1:8..1:9): x <= wd/L#`<init>`().(x)
         |[1:8..1:9): x <= wd/L#x.
         |[1:11..1:14): Int => scala/Int#
         |[2:6..2:8): K2 <= wd/K2#
         |[2:8..2:8):  <= wd/K2#`<init>`().
         |[2:9..2:10): x <= wd/K2#`<init>`().(x)
         |[2:9..2:10): x <= wd/K2#x.
         |[2:12..2:13): L => wd/L#
         |[2:15..2:16): y <= wd/K2#`<init>`().(y)
         |[2:15..2:16): y <= wd/K2#y.
         |[2:18..2:21): Int => scala/Int#
         |[3:6..3:7): P <= wd/P#
         |[3:8..3:8):  <= wd/P#`<init>`().
         |[3:16..3:18): K2 => wd/K2#
         |[3:18..3:18):  => wd/K2#`<init>`().
         |[3:23..3:24): L => wd/L#
         |[3:24..3:24):  => wd/L#`<init>`().
         |[3:25..3:26): x => wd/L#`<init>`().(x)
         |[3:33..3:34): y => wd/K2#`<init>`().(y)
         |[4:6..4:7): Q <= wd/Q#
         |[4:8..4:8):  <= wd/Q#`<init>`().
         |[4:16..4:18): K2 => wd/K2#
         |[4:19..4:19):  => wd/K2#`<init>`().
         |[4:24..4:25): L => wd/L#
         |[4:26..4:26):  => wd/L#`<init>`().
         |[4:27..4:28): x => wd/L#`<init>`().(x)
         |[4:36..4:37): y => wd/K2#`<init>`().(y)
         |""".stripMargin
    checkSection(
      "named arguments of a constructor call in a curried super call",
      """|package wd
         |class L(x: Int)
         |class K2(x: L)(y: Int)
         |class P extends K2(new L(x = 1))(y = 2)
         |class Q extends K2 (new L (x = 1)) (y = 2)
         |""".stripMargin,
      expected,
      "Occurrences",
    )
  }

  locally {
    val expected =
      """|[0:8..0:10): we <= we/
         |[1:6..1:7): L <= we/L#
         |[1:7..1:7):  <= we/L#`<init>`().
         |[1:8..1:9): x <= we/L#`<init>`().(x)
         |[1:8..1:9): x <= we/L#x.
         |[1:11..1:14): Int => scala/Int#
         |[2:6..2:8): K2 <= we/K2#
         |[2:8..2:8):  <= we/K2#`<init>`().
         |[2:9..2:10): x <= we/K2#`<init>`().(x)
         |[2:9..2:10): x <= we/K2#x.
         |[2:12..2:13): L => we/L#
         |[2:15..2:16): y <= we/K2#`<init>`().(y)
         |[2:15..2:16): y <= we/K2#y.
         |[2:18..2:21): Int => scala/Int#
         |[3:7..3:8): O <= we/O.
         |[4:6..4:6):  <= local0
         |[4:6..4:8): K2 => we/K2#
         |[4:8..4:8):  => we/K2#`<init>`().
         |[4:13..4:14): L => we/L#
         |[4:14..4:14):  => we/L#`<init>`().
         |[4:15..4:16): x => we/L#`<init>`().(x)
         |[4:23..4:24): y => we/K2#`<init>`().(y)
         |[5:6..5:6):  <= local1
         |[5:6..5:8): K2 => we/K2#
         |[5:9..5:9):  => we/K2#`<init>`().
         |[5:14..5:15): L => we/L#
         |[5:16..5:16):  => we/L#`<init>`().
         |[5:17..5:18): x => we/L#`<init>`().(x)
         |[5:26..5:27): y => we/K2#`<init>`().(y)
         |""".stripMargin
    checkSection(
      "named arguments of a constructor call in a curried anonymous class",
      """|package we
         |class L(x: Int)
         |class K2(x: L)(y: Int)
         |object O {
         |  new K2(new L(x = 1))(y = 2) {}
         |  new K2 (new L (x = 1)) (y = 2) {}
         |}
         |""".stripMargin,
      expected,
      "Occurrences",
    )
  }

  locally {
    val expected =
      """|[0:8..0:10): wf <= wf/
         |[1:6..1:7): L <= wf/L#
         |[1:7..1:7):  <= wf/L#`<init>`().
         |[1:8..1:9): x <= wf/L#`<init>`().(x)
         |[1:8..1:9): x <= wf/L#x.
         |[1:11..1:14): Int => scala/Int#
         |[2:6..2:7): K <= wf/K#
         |[2:7..2:7):  <= wf/K#`<init>`().
         |[2:8..2:9): x <= wf/K#`<init>`().(x)
         |[2:8..2:9): x <= wf/K#x.
         |[2:11..2:12): L => wf/L#
         |[2:14..2:15): y <= wf/K#`<init>`().(y)
         |[2:14..2:15): y <= wf/K#y.
         |[2:17..2:20): Int => scala/Int#
         |[3:6..3:7): T <= wf/T#
         |[4:6..4:7): R <= wf/R#
         |[4:8..4:8):  <= wf/R#`<init>`().
         |[4:16..4:17): K => wf/K#
         |[4:17..4:17):  => wf/K#`<init>`().
         |[4:22..4:23): L => wf/L#
         |[4:23..4:23):  => wf/L#`<init>`().
         |[4:24..4:25): x => wf/L#`<init>`().(x)
         |[4:37..4:38): T => wf/T#
         |[5:6..5:7): S <= wf/S#
         |[5:8..5:8):  <= wf/S#`<init>`().
         |[5:16..5:17): K => wf/K#
         |[5:18..5:18):  => wf/K#`<init>`().
         |[5:23..5:24): L => wf/L#
         |[5:25..5:25):  => wf/L#`<init>`().
         |[5:26..5:27): x => wf/L#`<init>`().(x)
         |[5:39..5:40): T => wf/T#
         |""".stripMargin
    checkSection(
      "named arguments of a constructor call in a super call with a trait",
      """|package wf
         |class L(x: Int)
         |class K(x: L, y: Int = 0)
         |trait T
         |class R extends K(new L(x = 1)) with T
         |class S extends K (new L (x = 1)) with T
         |""".stripMargin,
      expected,
      "Occurrences",
    )
  }

  checkSection(
    "named arguments of a constructor call in an annotation",
    """|package wg
       |class L(x: Int)
       |class an(x: L) extends scala.annotation.StaticAnnotation
       |object O {
       |  @an(new L(x = 1)) def a = 1
       |  @an (new L (x = 1)) def b = 1
       |}
       |""".stripMargin,
    """|[0:8..0:10): wg <= wg/
       |[1:6..1:7): L <= wg/L#
       |[1:7..1:7):  <= wg/L#`<init>`().
       |[1:8..1:9): x <= wg/L#`<init>`().(x)
       |[1:8..1:9): x <= wg/L#x.
       |[1:11..1:14): Int => scala/Int#
       |[2:6..2:8): an <= wg/an#
       |[2:8..2:8):  <= wg/an#`<init>`().
       |[2:9..2:10): x <= wg/an#`<init>`().(x)
       |[2:9..2:10): x <= wg/an#x.
       |[2:12..2:13): L => wg/L#
       |[2:23..2:28): scala => scala/
       |[2:29..2:39): annotation => scala/annotation/
       |[2:40..2:56): StaticAnnotation => scala/annotation/StaticAnnotation#
       |[2:56..2:56):  => scala/annotation/Annotation#`<init>`().
       |[3:7..3:8): O <= wg/O.
       |[4:3..4:5): an => wg/an#
       |[4:5..4:5):  => wg/an#`<init>`().
       |[4:10..4:11): L => wg/L#
       |[4:11..4:11):  => wg/L#`<init>`().
       |[4:12..4:13): x => wg/L#`<init>`().(x)
       |[4:24..4:25): a <= wg/O.a().
       |[5:3..5:5): an => wg/an#
       |[5:6..5:6):  => wg/an#`<init>`().
       |[5:11..5:12): L => wg/L#
       |[5:13..5:13):  => wg/L#`<init>`().
       |[5:14..5:15): x => wg/L#`<init>`().(x)
       |[5:26..5:27): b <= wg/O.b().
       |""".stripMargin,
    "Occurrences",
  )

  private val dottedOperatorOnUpdate =
    """|package wh
       |class M { def apply(k: Int): Int = k; def update(j: Int, v: Int): Unit = () }
       |object O {
       |  val arr = Array(1)
       |  val m = new M
       |  arr(i = 0).+=(5)
       |  arr (i = 0).+=(5)
       |  m(k = 0).+=(5)
       |  m (k = 0).+=(5)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an update with a dotted operator",
    dottedOperatorOnUpdate,
    """|[0:8..0:10): wh <= wh/
       |[1:6..1:7): M <= wh/M#
       |[1:8..1:8):  <= wh/M#`<init>`().
       |[1:14..1:19): apply <= wh/M#apply().
       |[1:20..1:21): k <= wh/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => wh/M#apply().(k)
       |[1:42..1:48): update <= wh/M#update().
       |[1:49..1:50): j <= wh/M#update().(j)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= wh/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): O <= wh/O.
       |[3:6..3:9): arr <= wh/O.arr.
       |[3:12..3:17): Array => scala/Array.
       |[4:6..4:7): m <= wh/O.m.
       |[4:14..4:15): M => wh/M#
       |[4:15..4:15):  => wh/M#`<init>`().
       |[5:2..5:5): arr => wh/O.arr.
       |[5:6..5:7): i => scala/Array#apply().(i)
       |[5:13..5:15): += => scala/Int#`+`(+4).
       |[6:2..6:5): arr => wh/O.arr.
       |[6:7..6:8): i => scala/Array#apply().(i)
       |[6:14..6:16): += => scala/Int#`+`(+4).
       |[7:2..7:3): m => wh/O.m.
       |[7:4..7:5): k => wh/M#apply().(k)
       |[7:11..7:13): += => scala/Int#`+`(+4).
       |[8:2..8:3): m => wh/O.m.
       |[8:5..8:6): k => wh/M#apply().(k)
       |[8:12..8:14): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an update with a dotted operator: synthetics",
    dottedOperatorOnUpdate,
    """|[3:12..3:17): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[5:2..5:5): arr => *.apply
       |  apply => scala/Array#apply().
       |[6:2..6:5): arr => *.apply
       |  apply => scala/Array#apply().
       |[7:2..7:3): m => *.apply
       |  apply => wh/M#apply().
       |[8:2..8:3): m => *.apply
       |  apply => wh/M#apply().
       |""".stripMargin,
    "Synthetics",
  )

  private val defaultOfEarlierClause =
    """|package wi
       |object O {
       |  def m(a: Int)(b: Int = a, c: Int = 0) = 0
       |  m(a = 5)(c = 1)
       |  m (a = 5)(c = 1)
       |  m(a = 5) (c = 1)
       |  m (a = 5) (c = 1)
       |}
       |""".stripMargin

  checkSection(
    "named arguments next to a default of an earlier clause",
    defaultOfEarlierClause,
    """|[0:8..0:10): wi <= wi/
       |[1:7..1:8): O <= wi/O.
       |[2:6..2:7): m <= wi/O.m().
       |[2:8..2:9): a <= wi/O.m().(a)
       |[2:11..2:14): Int => scala/Int#
       |[2:16..2:17): b <= wi/O.m().(b)
       |[2:19..2:22): Int => scala/Int#
       |[2:25..2:26): a => wi/O.m().(a)
       |[2:28..2:29): c <= wi/O.m().(c)
       |[2:31..2:34): Int => scala/Int#
       |[3:2..3:3): m => wi/O.m().
       |[3:4..3:5): a => wi/O.m().(a)
       |[3:11..3:12): c => wi/O.m().(c)
       |[4:2..4:3): m => wi/O.m().
       |[4:5..4:6): a => wi/O.m().(a)
       |[4:12..4:13): c => wi/O.m().(c)
       |[5:2..5:3): m => wi/O.m().
       |[5:4..5:5): a => wi/O.m().(a)
       |[5:12..5:13): c => wi/O.m().(c)
       |[6:2..6:3): m => wi/O.m().
       |[6:5..6:6): a => wi/O.m().(a)
       |[6:13..6:14): c => wi/O.m().(c)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments next to a default of an earlier clause: synthetics",
    defaultOfEarlierClause,
    "",
    "Synthetics",
  )

  private val qualifiedConstructors =
    """|package wj
       |package p {
       |  class L(x: Int)
       |  class K(x: Int)
       |  class KL(x: L)
       |  class an(x: Int) extends scala.annotation.StaticAnnotation
       |}
       |class X extends p.K(x = 1)
       |class Y extends p.K (x = 1)
       |class Z extends p.KL(new p.L(x = 1))
       |object O {
       |  new p.K(x = 1) {}
       |  new p.K (x = 1) {}
       |  @p.an(x = 1) def f = 0
       |  @p.an (x = 1) def g = 0
       |}
       |""".stripMargin

  locally {
    val expected =
      """|[0:8..0:10): wj <= wj/
         |[1:8..1:9): p <= wj/p/
         |[2:8..2:9): L <= wj/p/L#
         |[2:9..2:9):  <= wj/p/L#`<init>`().
         |[2:10..2:11): x <= wj/p/L#`<init>`().(x)
         |[2:10..2:11): x <= wj/p/L#x.
         |[2:13..2:16): Int => scala/Int#
         |[3:8..3:9): K <= wj/p/K#
         |[3:9..3:9):  <= wj/p/K#`<init>`().
         |[3:10..3:11): x <= wj/p/K#`<init>`().(x)
         |[3:10..3:11): x <= wj/p/K#x.
         |[3:13..3:16): Int => scala/Int#
         |[4:8..4:10): KL <= wj/p/KL#
         |[4:10..4:10):  <= wj/p/KL#`<init>`().
         |[4:11..4:12): x <= wj/p/KL#`<init>`().(x)
         |[4:11..4:12): x <= wj/p/KL#x.
         |[4:14..4:15): L => wj/p/L#
         |[5:8..5:10): an <= wj/p/an#
         |[5:10..5:10):  <= wj/p/an#`<init>`().
         |[5:11..5:12): x <= wj/p/an#`<init>`().(x)
         |[5:11..5:12): x <= wj/p/an#x.
         |[5:14..5:17): Int => scala/Int#
         |[5:27..5:32): scala => scala/
         |[5:33..5:43): annotation => scala/annotation/
         |[5:44..5:60): StaticAnnotation => scala/annotation/StaticAnnotation#
         |[5:60..5:60):  => scala/annotation/Annotation#`<init>`().
         |[7:6..7:7): X <= wj/X#
         |[7:8..7:8):  <= wj/X#`<init>`().
         |[7:16..7:17): p => wj/p/
         |[7:18..7:19): K => wj/p/K#
         |[7:19..7:19):  => wj/p/K#`<init>`().
         |[7:20..7:21): x => wj/p/K#`<init>`().(x)
         |[8:6..8:7): Y <= wj/Y#
         |[8:8..8:8):  <= wj/Y#`<init>`().
         |[8:16..8:17): p => wj/p/
         |[8:18..8:19): K => wj/p/K#
         |[8:20..8:20):  => wj/p/K#`<init>`().
         |[8:21..8:22): x => wj/p/K#`<init>`().(x)
         |[9:6..9:7): Z <= wj/Z#
         |[9:8..9:8):  <= wj/Z#`<init>`().
         |[9:16..9:17): p => wj/p/
         |[9:18..9:20): KL => wj/p/KL#
         |[9:20..9:20):  => wj/p/KL#`<init>`().
         |[9:25..9:26): p => wj/p/
         |[9:27..9:28): L => wj/p/L#
         |[9:28..9:28):  => wj/p/L#`<init>`().
         |[9:29..9:30): x => wj/p/L#`<init>`().(x)
         |[10:7..10:8): O <= wj/O.
         |[11:6..11:6):  <= local0
         |[11:6..11:7): p => wj/p/
         |[11:8..11:9): K => wj/p/K#
         |[11:9..11:9):  => wj/p/K#`<init>`().
         |[11:10..11:11): x => wj/p/K#`<init>`().(x)
         |[12:6..12:6):  <= local1
         |[12:6..12:7): p => wj/p/
         |[12:8..12:9): K => wj/p/K#
         |[12:10..12:10):  => wj/p/K#`<init>`().
         |[12:11..12:12): x => wj/p/K#`<init>`().(x)
         |[13:3..13:4): p => wj/p/
         |[13:5..13:7): an => wj/p/an#
         |[13:7..13:7):  => wj/p/an#`<init>`().
         |[13:8..13:9): x => wj/p/an#`<init>`().(x)
         |[13:19..13:20): f <= wj/O.f().
         |[14:3..14:4): p => wj/p/
         |[14:5..14:7): an => wj/p/an#
         |[14:8..14:8):  => wj/p/an#`<init>`().
         |[14:9..14:10): x => wj/p/an#`<init>`().(x)
         |[14:20..14:21): g <= wj/O.g().
         |""".stripMargin
    checkSection(
      "named arguments of qualified constructors",
      qualifiedConstructors,
      expected,
      "Occurrences",
    )
  }

  checkSection(
    "named arguments of qualified constructors: synthetics",
    qualifiedConstructors,
    "",
    "Synthetics",
  )

  private val qualifiedConstructorsWithTypeArguments =
    """|package wk
       |package p {
       |  class G[T](x: T)
       |}
       |class X extends p.G[Int](x = 1)
       |class Y extends p.G[Int] (x = 1)
       |object O {
       |  new p.G[Int](x = 1) {}
       |  new p.G[Int] (x = 1) {}
       |}
       |""".stripMargin

  checkSection(
    "named arguments of qualified constructors with type arguments",
    qualifiedConstructorsWithTypeArguments,
    """|[0:8..0:10): wk <= wk/
       |[1:8..1:9): p <= wk/p/
       |[2:8..2:9): G <= wk/p/G#
       |[2:10..2:11): T <= wk/p/G#[T]
       |[2:12..2:12):  <= wk/p/G#`<init>`().
       |[2:13..2:14): x <= wk/p/G#`<init>`().(x)
       |[2:13..2:14): x <= wk/p/G#x.
       |[2:16..2:17): T => wk/p/G#[T]
       |[4:6..4:7): X <= wk/X#
       |[4:8..4:8):  <= wk/X#`<init>`().
       |[4:16..4:17): p => wk/p/
       |[4:18..4:19): G => wk/p/G#
       |[4:20..4:23): Int => scala/Int#
       |[4:24..4:24):  => wk/p/G#`<init>`().
       |[4:25..4:26): x => wk/p/G#`<init>`().(x)
       |[5:6..5:7): Y <= wk/Y#
       |[5:8..5:8):  <= wk/Y#`<init>`().
       |[5:16..5:17): p => wk/p/
       |[5:18..5:19): G => wk/p/G#
       |[5:20..5:23): Int => scala/Int#
       |[5:25..5:25):  => wk/p/G#`<init>`().
       |[5:26..5:27): x => wk/p/G#`<init>`().(x)
       |[6:7..6:8): O <= wk/O.
       |[7:6..7:6):  <= local0
       |[7:6..7:7): p => wk/p/
       |[7:8..7:9): G => wk/p/G#
       |[7:10..7:13): Int => scala/Int#
       |[7:14..7:14):  => wk/p/G#`<init>`().
       |[7:15..7:16): x => wk/p/G#`<init>`().(x)
       |[8:6..8:6):  <= local1
       |[8:6..8:7): p => wk/p/
       |[8:8..8:9): G => wk/p/G#
       |[8:10..8:13): Int => scala/Int#
       |[8:15..8:15):  => wk/p/G#`<init>`().
       |[8:16..8:17): x => wk/p/G#`<init>`().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of qualified constructors with type arguments: synthetics",
    qualifiedConstructorsWithTypeArguments,
    "",
    "Synthetics",
  )

  private val constructorsWithLongerQualifier =
    """|package wl
       |package a {
       |  package b {
       |    class K(x: Int)
       |  }
       |}
       |class X extends a.b.K(x = 1)
       |class Y extends a.b.K (x = 1)
       |object O {
       |  new a.b.K(x = 1) {}
       |  new a.b.K (x = 1) {}
       |}
       |""".stripMargin

  checkSection(
    "named arguments of constructors with a longer qualifier",
    constructorsWithLongerQualifier,
    """|[0:8..0:10): wl <= wl/
       |[1:8..1:9): a <= wl/a/
       |[2:10..2:11): b <= wl/a/b/
       |[3:10..3:11): K <= wl/a/b/K#
       |[3:11..3:11):  <= wl/a/b/K#`<init>`().
       |[3:12..3:13): x <= wl/a/b/K#`<init>`().(x)
       |[3:12..3:13): x <= wl/a/b/K#x.
       |[3:15..3:18): Int => scala/Int#
       |[6:6..6:7): X <= wl/X#
       |[6:8..6:8):  <= wl/X#`<init>`().
       |[6:16..6:17): a => wl/a/
       |[6:18..6:19): b => wl/a/b/
       |[6:20..6:21): K => wl/a/b/K#
       |[6:21..6:21):  => wl/a/b/K#`<init>`().
       |[6:22..6:23): x => wl/a/b/K#`<init>`().(x)
       |[7:6..7:7): Y <= wl/Y#
       |[7:8..7:8):  <= wl/Y#`<init>`().
       |[7:16..7:17): a => wl/a/
       |[7:18..7:19): b => wl/a/b/
       |[7:20..7:21): K => wl/a/b/K#
       |[7:22..7:22):  => wl/a/b/K#`<init>`().
       |[7:23..7:24): x => wl/a/b/K#`<init>`().(x)
       |[8:7..8:8): O <= wl/O.
       |[9:6..9:6):  <= local0
       |[9:6..9:7): a => wl/a/
       |[9:8..9:9): b => wl/a/b/
       |[9:10..9:11): K => wl/a/b/K#
       |[9:11..9:11):  => wl/a/b/K#`<init>`().
       |[9:12..9:13): x => wl/a/b/K#`<init>`().(x)
       |[10:6..10:6):  <= local1
       |[10:6..10:7): a => wl/a/
       |[10:8..10:9): b => wl/a/b/
       |[10:10..10:11): K => wl/a/b/K#
       |[10:12..10:12):  => wl/a/b/K#`<init>`().
       |[10:13..10:14): x => wl/a/b/K#`<init>`().(x)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of constructors with a longer qualifier: synthetics",
    constructorsWithLongerQualifier,
    "",
    "Synthetics",
  )

  private val dottedOperatorOnResultWithMember =
    """|package wm
       |class B { def +=(y: Int) = this }
       |class M { def apply(k: Int) = new B }
       |object O {
       |  val m = new M
       |  def f(k: Int) = new B
       |  m(k = 0).+=(5)
       |  m (k = 0).+=(5)
       |  f(k = 0).+=(5)
       |  f (k = 0).+=(5)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of a dotted operator on a result with the member",
    dottedOperatorOnResultWithMember,
    """|[0:8..0:10): wm <= wm/
       |[1:6..1:7): B <= wm/B#
       |[1:8..1:8):  <= wm/B#`<init>`().
       |[1:14..1:16): += <= wm/B#`+=`().
       |[1:17..1:18): y <= wm/B#`+=`().(y)
       |[1:20..1:23): Int => scala/Int#
       |[2:6..2:7): M <= wm/M#
       |[2:8..2:8):  <= wm/M#`<init>`().
       |[2:14..2:19): apply <= wm/M#apply().
       |[2:20..2:21): k <= wm/M#apply().(k)
       |[2:23..2:26): Int => scala/Int#
       |[2:34..2:35): B => wm/B#
       |[2:35..2:35):  => wm/B#`<init>`().
       |[3:7..3:8): O <= wm/O.
       |[4:6..4:7): m <= wm/O.m.
       |[4:14..4:15): M => wm/M#
       |[4:15..4:15):  => wm/M#`<init>`().
       |[5:6..5:7): f <= wm/O.f().
       |[5:8..5:9): k <= wm/O.f().(k)
       |[5:11..5:14): Int => scala/Int#
       |[5:22..5:23): B => wm/B#
       |[5:23..5:23):  => wm/B#`<init>`().
       |[6:2..6:3): m => wm/O.m.
       |[6:4..6:5): k => wm/M#apply().(k)
       |[6:11..6:13): += => wm/B#`+=`().
       |[7:2..7:3): m => wm/O.m.
       |[7:5..7:6): k => wm/M#apply().(k)
       |[7:12..7:14): += => wm/B#`+=`().
       |[8:2..8:3): f => wm/O.f().
       |[8:4..8:5): k => wm/O.f().(k)
       |[8:11..8:13): += => wm/B#`+=`().
       |[9:2..9:3): f => wm/O.f().
       |[9:5..9:6): k => wm/O.f().(k)
       |[9:12..9:14): += => wm/B#`+=`().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a dotted operator on a result with the member: synthetics",
    dottedOperatorOnResultWithMember,
    """|[6:2..6:3): m => *.apply
       |  apply => wm/M#apply().
       |[7:2..7:3): m => *.apply
       |  apply => wm/M#apply().
       |""".stripMargin,
    "Synthetics",
  )

  private val dottedOperatorOnUpdateOfMethodResult =
    """|package wn
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object C {
       |  val m = new M
       |  def a(i: Int) = m
       |  a(i = 0)(k = 1).+=(5)
       |  a (i = 0)(k = 1).+=(5)
       |  a(i = 0) (k = 1).+=(5)
       |  a (i = 0) (k = 1).+=(5)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of a dotted operator on an update of a method result",
    dottedOperatorOnUpdateOfMethodResult,
    """|[0:8..0:10): wn <= wn/
       |[1:6..1:7): M <= wn/M#
       |[1:8..1:8):  <= wn/M#`<init>`().
       |[1:14..1:19): apply <= wn/M#apply().
       |[1:20..1:21): k <= wn/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => wn/M#apply().(k)
       |[1:42..1:48): update <= wn/M#update().
       |[1:49..1:50): k <= wn/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= wn/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): C <= wn/C.
       |[3:6..3:7): m <= wn/C.m.
       |[3:14..3:15): M => wn/M#
       |[3:15..3:15):  => wn/M#`<init>`().
       |[4:6..4:7): a <= wn/C.a().
       |[4:8..4:9): i <= wn/C.a().(i)
       |[4:11..4:14): Int => scala/Int#
       |[4:18..4:19): m => wn/C.m.
       |[5:2..5:3): a => wn/C.a().
       |[5:4..5:5): i => wn/C.a().(i)
       |[5:11..5:12): k => wn/M#apply().(k)
       |[5:18..5:20): += => scala/Int#`+`(+4).
       |[6:2..6:3): a => wn/C.a().
       |[6:5..6:6): i => wn/C.a().(i)
       |[6:12..6:13): k => wn/M#apply().(k)
       |[6:19..6:21): += => scala/Int#`+`(+4).
       |[7:2..7:3): a => wn/C.a().
       |[7:4..7:5): i => wn/C.a().(i)
       |[7:12..7:13): k => wn/M#apply().(k)
       |[7:19..7:21): += => scala/Int#`+`(+4).
       |[8:2..8:3): a => wn/C.a().
       |[8:5..8:6): i => wn/C.a().(i)
       |[8:13..8:14): k => wn/M#apply().(k)
       |[8:20..8:22): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a dotted operator on an update of a method result: synthetics",
    dottedOperatorOnUpdateOfMethodResult,
    "",
    "Synthetics",
  )

  private val dottedOperatorOnApply =
    """|package wo
       |object O {
       |  val arr = Array(1)
       |  arr(i = 0).+(5)
       |  arr (i = 0).+(5)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an apply with a dotted operator",
    dottedOperatorOnApply,
    """|[0:8..0:10): wo <= wo/
       |[1:7..1:8): O <= wo/O.
       |[2:6..2:9): arr <= wo/O.arr.
       |[2:12..2:17): Array => scala/Array.
       |[3:2..3:5): arr => wo/O.arr.
       |[3:6..3:7): i => scala/Array#apply().(i)
       |[3:13..3:14): + => scala/Int#`+`(+4).
       |[4:2..4:5): arr => wo/O.arr.
       |[4:7..4:8): i => scala/Array#apply().(i)
       |[4:14..4:15): + => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an apply with a dotted operator: synthetics",
    dottedOperatorOnApply,
    """|[2:12..2:17): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[3:2..3:5): arr => *.apply
       |  apply => scala/Array#apply().
       |[4:2..4:5): arr => *.apply
       |  apply => scala/Array#apply().
       |""".stripMargin,
    "Synthetics",
  )

  private val constructorDefaultOfEarlierClause =
    """|package wp
       |class K(a: Int)(b: Int = a, c: Int = 0)
       |class X extends K(a = 5)(c = 1)
       |class Y extends K (a = 5)(c = 1)
       |class Z extends K(a = 5) (c = 1)
       |class W extends K (a = 5) (c = 1)
       |object O {
       |  new K(a = 5)(c = 1)
       |  new K (a = 5)(c = 1)
       |  new K(a = 5) (c = 1)
       |  new K (a = 5) (c = 1)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of a constructor next to a default of an earlier clause",
    constructorDefaultOfEarlierClause,
    """|[0:8..0:10): wp <= wp/
       |[1:6..1:7): K <= wp/K#
       |[1:7..1:7):  <= wp/K#`<init>`().
       |[1:8..1:9): a <= wp/K#`<init>`().(a)
       |[1:8..1:9): a <= wp/K#a.
       |[1:11..1:14): Int => scala/Int#
       |[1:16..1:17): b <= wp/K#`<init>`().(b)
       |[1:16..1:17): b <= wp/K#b.
       |[1:19..1:22): Int => scala/Int#
       |[1:25..1:26): a => wp/K#`<init>`().(a)
       |[1:28..1:29): c <= wp/K#`<init>`().(c)
       |[1:28..1:29): c <= wp/K#c.
       |[1:31..1:34): Int => scala/Int#
       |[2:6..2:7): X <= wp/X#
       |[2:8..2:8):  <= wp/X#`<init>`().
       |[2:16..2:17): K => wp/K#
       |[2:17..2:17):  => wp/K#`<init>`().
       |[2:18..2:19): a => wp/K#`<init>`().(a)
       |[2:25..2:26): c => wp/K#`<init>`().(c)
       |[3:6..3:7): Y <= wp/Y#
       |[3:8..3:8):  <= wp/Y#`<init>`().
       |[3:16..3:17): K => wp/K#
       |[3:18..3:18):  => wp/K#`<init>`().
       |[3:19..3:20): a => wp/K#`<init>`().(a)
       |[3:26..3:27): c => wp/K#`<init>`().(c)
       |[4:6..4:7): Z <= wp/Z#
       |[4:8..4:8):  <= wp/Z#`<init>`().
       |[4:16..4:17): K => wp/K#
       |[4:17..4:17):  => wp/K#`<init>`().
       |[4:18..4:19): a => wp/K#`<init>`().(a)
       |[4:26..4:27): c => wp/K#`<init>`().(c)
       |[5:6..5:7): W <= wp/W#
       |[5:8..5:8):  <= wp/W#`<init>`().
       |[5:16..5:17): K => wp/K#
       |[5:18..5:18):  => wp/K#`<init>`().
       |[5:19..5:20): a => wp/K#`<init>`().(a)
       |[5:27..5:28): c => wp/K#`<init>`().(c)
       |[6:7..6:8): O <= wp/O.
       |[7:6..7:7): K => wp/K#
       |[7:7..7:7):  => wp/K#`<init>`().
       |[7:8..7:9): a => wp/K#`<init>`().(a)
       |[7:15..7:16): c => wp/K#`<init>`().(c)
       |[8:6..8:7): K => wp/K#
       |[8:8..8:8):  => wp/K#`<init>`().
       |[8:9..8:10): a => wp/K#`<init>`().(a)
       |[8:16..8:17): c => wp/K#`<init>`().(c)
       |[9:6..9:7): K => wp/K#
       |[9:7..9:7):  => wp/K#`<init>`().
       |[9:8..9:9): a => wp/K#`<init>`().(a)
       |[9:16..9:17): c => wp/K#`<init>`().(c)
       |[10:6..10:7): K => wp/K#
       |[10:8..10:8):  => wp/K#`<init>`().
       |[10:9..10:10): a => wp/K#`<init>`().(a)
       |[10:17..10:18): c => wp/K#`<init>`().(c)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a constructor next to a default of an earlier clause: synthetics",
    constructorDefaultOfEarlierClause,
    "",
    "Synthetics",
  )

  private val variableNextToDefaultOfEarlierClause =
    """|package wq
       |object O {
       |  def m(a: Int)(b: Int = a, c: Int = 0) = 0
       |  var n = 5
       |  m(a = n)(c = 1)
       |  m (a = n)(c = 1)
       |  m(a = n) (c = 1)
       |  m (a = n) (c = 1)
       |}
       |""".stripMargin

  checkSection(
    "named arguments with a variable next to a default of an earlier clause",
    variableNextToDefaultOfEarlierClause,
    """|[0:8..0:10): wq <= wq/
       |[1:7..1:8): O <= wq/O.
       |[2:6..2:7): m <= wq/O.m().
       |[2:8..2:9): a <= wq/O.m().(a)
       |[2:11..2:14): Int => scala/Int#
       |[2:16..2:17): b <= wq/O.m().(b)
       |[2:19..2:22): Int => scala/Int#
       |[2:25..2:26): a => wq/O.m().(a)
       |[2:28..2:29): c <= wq/O.m().(c)
       |[2:31..2:34): Int => scala/Int#
       |[3:6..3:7): n <= wq/O.n().
       |[4:2..4:3): m => wq/O.m().
       |[4:4..4:5): a => wq/O.m().(a)
       |[4:8..4:9): n => wq/O.n().
       |[4:11..4:12): c => wq/O.m().(c)
       |[5:2..5:3): m => wq/O.m().
       |[5:5..5:6): a => wq/O.m().(a)
       |[5:9..5:10): n => wq/O.n().
       |[5:12..5:13): c => wq/O.m().(c)
       |[6:2..6:3): m => wq/O.m().
       |[6:4..6:5): a => wq/O.m().(a)
       |[6:8..6:9): n => wq/O.n().
       |[6:12..6:13): c => wq/O.m().(c)
       |[7:2..7:3): m => wq/O.m().
       |[7:5..7:6): a => wq/O.m().(a)
       |[7:9..7:10): n => wq/O.n().
       |[7:13..7:14): c => wq/O.m().(c)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments with a variable next to a default of an earlier clause: synthetics",
    variableNextToDefaultOfEarlierClause,
    "",
    "Synthetics",
  )

  private val operatorOnLaterClauseOfNew =
    """|package wr
       |class K(x: Int) {
       |  def apply(a: Int): Int = a
       |  def update(a: Int, v: Int): Unit = ()
       |}
       |object O {
       |  new K(x = 1)(a = 0) += 5
       |  new K (x = 1) (a = 0) += 5
       |  new K(x = 1)(a = 0).+=(5)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an operator on a later clause of a constructor",
    operatorOnLaterClauseOfNew,
    """|[0:8..0:10): wr <= wr/
       |[1:6..1:7): K <= wr/K#
       |[1:7..1:7):  <= wr/K#`<init>`().
       |[1:8..1:9): x <= wr/K#`<init>`().(x)
       |[1:8..1:9): x <= wr/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[2:6..2:11): apply <= wr/K#apply().
       |[2:12..2:13): a <= wr/K#apply().(a)
       |[2:15..2:18): Int => scala/Int#
       |[2:21..2:24): Int => scala/Int#
       |[2:27..2:28): a => wr/K#apply().(a)
       |[3:6..3:12): update <= wr/K#update().
       |[3:13..3:14): a <= wr/K#update().(a)
       |[3:16..3:19): Int => scala/Int#
       |[3:21..3:22): v <= wr/K#update().(v)
       |[3:24..3:27): Int => scala/Int#
       |[3:30..3:34): Unit => scala/Unit#
       |[5:7..5:8): O <= wr/O.
       |[6:6..6:7): K => wr/K#
       |[6:7..6:7):  => wr/K#`<init>`().
       |[6:8..6:9): x => wr/K#`<init>`().(x)
       |[6:15..6:16): a => wr/K#apply().(a)
       |[6:22..6:24): += => scala/Int#`+`(+4).
       |[7:6..7:7): K => wr/K#
       |[7:8..7:8):  => wr/K#`<init>`().
       |[7:9..7:10): x => wr/K#`<init>`().(x)
       |[7:17..7:18): a => wr/K#apply().(a)
       |[7:24..7:26): += => scala/Int#`+`(+4).
       |[8:6..8:7): K => wr/K#
       |[8:7..8:7):  => wr/K#`<init>`().
       |[8:8..8:9): x => wr/K#`<init>`().(x)
       |[8:15..8:16): a => wr/K#apply().(a)
       |[8:22..8:24): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an operator on a later clause of a constructor: synthetics",
    operatorOnLaterClauseOfNew,
    "",
    "Synthetics",
  )

  private val writtenApplyWithOperatorAssignment =
    """|package ws
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  val m = new M
       |  m.apply(k = 0) += 5
       |  m.apply (k = 0) += 5
       |  m.update(k = 0, v = 5)
       |  m.update (k = 0, v = 5)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of a written apply with += and of a written update",
    writtenApplyWithOperatorAssignment,
    """|[0:8..0:10): ws <= ws/
       |[1:6..1:7): M <= ws/M#
       |[1:8..1:8):  <= ws/M#`<init>`().
       |[1:14..1:19): apply <= ws/M#apply().
       |[1:20..1:21): k <= ws/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => ws/M#apply().(k)
       |[1:42..1:48): update <= ws/M#update().
       |[1:49..1:50): k <= ws/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= ws/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): O <= ws/O.
       |[3:6..3:7): m <= ws/O.m.
       |[3:14..3:15): M => ws/M#
       |[3:15..3:15):  => ws/M#`<init>`().
       |[4:2..4:3): m => ws/O.m.
       |[4:17..4:19): += => scala/Int#`+`(+4).
       |[5:2..5:3): m => ws/O.m.
       |[5:18..5:20): += => scala/Int#`+`(+4).
       |[6:2..6:3): m => ws/O.m.
       |[6:4..6:10): update => ws/M#update().
       |[6:11..6:12): k => ws/M#update().(k)
       |[6:18..6:19): v => ws/M#update().(v)
       |[7:2..7:3): m => ws/O.m.
       |[7:4..7:10): update => ws/M#update().
       |[7:12..7:13): k => ws/M#update().(k)
       |[7:19..7:20): v => ws/M#update().(v)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a written apply with += and of a written update: synthetics",
    writtenApplyWithOperatorAssignment,
    """|[4:2..4:2):  => *.update
       |  update => ws/M#update().
       |[5:2..5:2):  => *.update
       |  update => ws/M#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val constructorsAroundLocalCaseClass =
    """|package wt
       |class K(x: Int)
       |object O {
       |  new K(x = { case class C(x: Int); C(x = 2).x })
       |  new K (x = { case class C(x: Int); C(x = 2).x })
       |}
       |class X extends K(x = { case class C(x: Int); C(2).x })
       |class Y extends K (x = { case class C(x: Int); C(2).x })
       |""".stripMargin

  checkSection(
    "named arguments of constructors around a local case class",
    constructorsAroundLocalCaseClass,
    """|[0:8..0:10): wt <= wt/
       |[1:6..1:7): K <= wt/K#
       |[1:7..1:7):  <= wt/K#`<init>`().
       |[1:8..1:9): x <= wt/K#`<init>`().(x)
       |[1:8..1:9): x <= wt/K#x.
       |[1:11..1:14): Int => scala/Int#
       |[2:7..2:8): O <= wt/O.
       |[3:6..3:7): K => wt/K#
       |[3:7..3:7):  => wt/K#`<init>`().
       |[3:8..3:9): x => wt/K#`<init>`().(x)
       |[3:25..3:26): C <= local0
       |[3:26..3:26):  <= local2
       |[3:27..3:28): x <= local1
       |[3:30..3:33): Int => scala/Int#
       |[3:36..3:37): C => local20
       |[3:38..3:39): x => local84
       |[3:45..3:46): x => local1
       |[4:6..4:7): K => wt/K#
       |[4:8..4:8):  => wt/K#`<init>`().
       |[4:9..4:10): x => wt/K#`<init>`().(x)
       |[4:26..4:27): C <= local21
       |[4:27..4:27):  <= local23
       |[4:28..4:29): x <= local22
       |[4:31..4:34): Int => scala/Int#
       |[4:37..4:38): C => local41
       |[4:39..4:40): x => local85
       |[4:46..4:47): x => local22
       |[6:6..6:7): X <= wt/X#
       |[6:8..6:8):  <= wt/X#`<init>`().
       |[6:16..6:17): K => wt/K#
       |[6:17..6:17):  => wt/K#`<init>`().
       |[6:18..6:19): x => wt/K#`<init>`().(x)
       |[6:35..6:36): C <= local42
       |[6:36..6:36):  <= local44
       |[6:37..6:38): x <= local43
       |[6:40..6:43): Int => scala/Int#
       |[6:46..6:47): C => local62
       |[6:51..6:52): x => local43
       |[7:6..7:7): Y <= wt/Y#
       |[7:8..7:8):  <= wt/Y#`<init>`().
       |[7:16..7:17): K => wt/K#
       |[7:18..7:18):  => wt/K#`<init>`().
       |[7:19..7:20): x => wt/K#`<init>`().(x)
       |[7:36..7:37): C <= local63
       |[7:37..7:37):  <= local65
       |[7:38..7:39): x <= local64
       |[7:41..7:44): Int => scala/Int#
       |[7:47..7:48): C => local83
       |[7:52..7:53): x => local64
       |""".stripMargin,
    "Occurrences",
    ScalaVersion.Scala212 ->
      """|[0:8..0:10): wt <= wt/
         |[1:6..1:7): K <= wt/K#
         |[1:7..1:7):  <= wt/K#`<init>`().
         |[1:8..1:9): x <= wt/K#`<init>`().(x)
         |[1:8..1:9): x <= wt/K#x.
         |[1:11..1:14): Int => scala/Int#
         |[2:7..2:8): O <= wt/O.
         |[3:6..3:7): K => wt/K#
         |[3:7..3:7):  => wt/K#`<init>`().
         |[3:8..3:9): x => wt/K#`<init>`().(x)
         |[3:25..3:26): C <= local0
         |[3:26..3:26):  <= local2
         |[3:27..3:28): x <= local1
         |[3:30..3:33): Int => scala/Int#
         |[3:36..3:37): C => local18
         |[3:38..3:39): x => local76
         |[3:45..3:46): x => local1
         |[4:6..4:7): K => wt/K#
         |[4:8..4:8):  => wt/K#`<init>`().
         |[4:9..4:10): x => wt/K#`<init>`().(x)
         |[4:26..4:27): C <= local19
         |[4:27..4:27):  <= local21
         |[4:28..4:29): x <= local20
         |[4:31..4:34): Int => scala/Int#
         |[4:37..4:38): C => local37
         |[4:39..4:40): x => local77
         |[4:46..4:47): x => local20
         |[6:6..6:7): X <= wt/X#
         |[6:8..6:8):  <= wt/X#`<init>`().
         |[6:16..6:17): K => wt/K#
         |[6:17..6:17):  => wt/K#`<init>`().
         |[6:18..6:19): x => wt/K#`<init>`().(x)
         |[6:35..6:36): C <= local38
         |[6:36..6:36):  <= local40
         |[6:37..6:38): x <= local39
         |[6:40..6:43): Int => scala/Int#
         |[6:46..6:47): C => local56
         |[6:51..6:52): x => local39
         |[7:6..7:7): Y <= wt/Y#
         |[7:8..7:8):  <= wt/Y#`<init>`().
         |[7:16..7:17): K => wt/K#
         |[7:18..7:18):  => wt/K#`<init>`().
         |[7:19..7:20): x => wt/K#`<init>`().(x)
         |[7:36..7:37): C <= local57
         |[7:37..7:37):  <= local59
         |[7:38..7:39): x <= local58
         |[7:41..7:44): Int => scala/Int#
         |[7:47..7:48): C => local75
         |[7:52..7:53): x => local58
         |""".stripMargin,
  )

  checkSection(
    "named arguments of constructors around a local case class: synthetics",
    constructorsAroundLocalCaseClass,
    """|[3:36..3:37): C => *.apply
       |  apply => local19
       |[4:37..4:38): C => *.apply
       |  apply => local40
       |[6:46..6:47): C => *.apply
       |  apply => local61
       |[7:47..7:48): C => *.apply
       |  apply => local82
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[3:36..3:37): C => *.apply
         |  apply => local17
         |[4:37..4:38): C => *.apply
         |  apply => local36
         |[6:46..6:47): C => *.apply
         |  apply => local55
         |[7:47..7:48): C => *.apply
         |  apply => local74
         |""".stripMargin,
  )

  private val operatorOnConstructorWithTwoLists =
    """|package wu
       |class K2(x: Int)(y: Int) { def apply(a: Int): Int = a; def update(a: Int, v: Int): Unit = () }
       |object O {
       |  new K2(x = 1)(y = 2)(a = 0) += 5
       |  new K2 (x = 1) (y = 2) (a = 0) += 5
       |  new K2(x = 1)(y = 2)(a = 0).+=(5)
       |  new K2 (x = 1) (y = 2) (a = 0).+=(5)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an operator on a constructor with two lists",
    operatorOnConstructorWithTwoLists,
    """|[0:8..0:10): wu <= wu/
       |[1:6..1:8): K2 <= wu/K2#
       |[1:8..1:8):  <= wu/K2#`<init>`().
       |[1:9..1:10): x <= wu/K2#`<init>`().(x)
       |[1:9..1:10): x <= wu/K2#x.
       |[1:12..1:15): Int => scala/Int#
       |[1:17..1:18): y <= wu/K2#`<init>`().(y)
       |[1:17..1:18): y <= wu/K2#y.
       |[1:20..1:23): Int => scala/Int#
       |[1:31..1:36): apply <= wu/K2#apply().
       |[1:37..1:38): a <= wu/K2#apply().(a)
       |[1:40..1:43): Int => scala/Int#
       |[1:46..1:49): Int => scala/Int#
       |[1:52..1:53): a => wu/K2#apply().(a)
       |[1:59..1:65): update <= wu/K2#update().
       |[1:66..1:67): a <= wu/K2#update().(a)
       |[1:69..1:72): Int => scala/Int#
       |[1:74..1:75): v <= wu/K2#update().(v)
       |[1:77..1:80): Int => scala/Int#
       |[1:83..1:87): Unit => scala/Unit#
       |[2:7..2:8): O <= wu/O.
       |[3:6..3:8): K2 => wu/K2#
       |[3:8..3:8):  => wu/K2#`<init>`().
       |[3:9..3:10): x => wu/K2#`<init>`().(x)
       |[3:16..3:17): y => wu/K2#`<init>`().(y)
       |[3:23..3:24): a => wu/K2#apply().(a)
       |[3:30..3:32): += => scala/Int#`+`(+4).
       |[4:6..4:8): K2 => wu/K2#
       |[4:9..4:9):  => wu/K2#`<init>`().
       |[4:10..4:11): x => wu/K2#`<init>`().(x)
       |[4:18..4:19): y => wu/K2#`<init>`().(y)
       |[4:26..4:27): a => wu/K2#apply().(a)
       |[4:33..4:35): += => scala/Int#`+`(+4).
       |[5:6..5:8): K2 => wu/K2#
       |[5:8..5:8):  => wu/K2#`<init>`().
       |[5:9..5:10): x => wu/K2#`<init>`().(x)
       |[5:16..5:17): y => wu/K2#`<init>`().(y)
       |[5:23..5:24): a => wu/K2#apply().(a)
       |[5:30..5:32): += => scala/Int#`+`(+4).
       |[6:6..6:8): K2 => wu/K2#
       |[6:9..6:9):  => wu/K2#`<init>`().
       |[6:10..6:11): x => wu/K2#`<init>`().(x)
       |[6:18..6:19): y => wu/K2#`<init>`().(y)
       |[6:26..6:27): a => wu/K2#apply().(a)
       |[6:33..6:35): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an operator on a constructor with two lists: synthetics",
    operatorOnConstructorWithTwoLists,
    "",
    "Synthetics",
  )

  private val memberOperatorOnNew =
    """|package wv
       |class B(x: Int) { def +=(y: Int) = this }
       |object O {
       |  new B(x = 1) += 5
       |  new B (x = 1) += 5
       |  new B(x = 1).+=(5)
       |  new B (x = 1).+=(5)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of a member operator on a constructor",
    memberOperatorOnNew,
    """|[0:8..0:10): wv <= wv/
       |[1:6..1:7): B <= wv/B#
       |[1:7..1:7):  <= wv/B#`<init>`().
       |[1:8..1:9): x <= wv/B#`<init>`().(x)
       |[1:8..1:9): x <= wv/B#x.
       |[1:11..1:14): Int => scala/Int#
       |[1:22..1:24): += <= wv/B#`+=`().
       |[1:25..1:26): y <= wv/B#`+=`().(y)
       |[1:28..1:31): Int => scala/Int#
       |[2:7..2:8): O <= wv/O.
       |[3:6..3:7): B => wv/B#
       |[3:7..3:7):  => wv/B#`<init>`().
       |[3:8..3:9): x => wv/B#`<init>`().(x)
       |[3:15..3:17): += => wv/B#`+=`().
       |[4:6..4:7): B => wv/B#
       |[4:8..4:8):  => wv/B#`<init>`().
       |[4:9..4:10): x => wv/B#`<init>`().(x)
       |[4:16..4:18): += => wv/B#`+=`().
       |[5:6..5:7): B => wv/B#
       |[5:7..5:7):  => wv/B#`<init>`().
       |[5:8..5:9): x => wv/B#`<init>`().(x)
       |[5:15..5:17): += => wv/B#`+=`().
       |[6:6..6:7): B => wv/B#
       |[6:8..6:8):  => wv/B#`<init>`().
       |[6:9..6:10): x => wv/B#`<init>`().(x)
       |[6:16..6:18): += => wv/B#`+=`().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a member operator on a constructor: synthetics",
    memberOperatorOnNew,
    "",
    "Synthetics",
  )

  private val infixApplyWithOperatorAssignment =
    """|package ww
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  val m = new M
       |  m apply(k = 0) += 5
       |  m apply (k = 0) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an infix apply with +=",
    infixApplyWithOperatorAssignment,
    """|[0:8..0:10): ww <= ww/
       |[1:6..1:7): M <= ww/M#
       |[1:8..1:8):  <= ww/M#`<init>`().
       |[1:14..1:19): apply <= ww/M#apply().
       |[1:20..1:21): k <= ww/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => ww/M#apply().(k)
       |[1:42..1:48): update <= ww/M#update().
       |[1:49..1:50): k <= ww/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= ww/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): O <= ww/O.
       |[3:6..3:7): m <= ww/O.m.
       |[3:14..3:15): M => ww/M#
       |[3:15..3:15):  => ww/M#`<init>`().
       |[4:2..4:3): m => ww/O.m.
       |[4:17..4:19): += => scala/Int#`+`(+4).
       |[5:2..5:3): m => ww/O.m.
       |[5:18..5:20): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an infix apply with +=: synthetics",
    infixApplyWithOperatorAssignment,
    """|[4:2..4:2):  => *.update
       |  update => ww/M#update().
       |[5:2..5:2):  => *.update
       |  update => ww/M#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val constructorsAroundXmlLiterals =
    """|package wx
       |class E(label: scala.xml.Elem)
       |class A(key: scala.xml.Elem)
       |object O {
       |  new E(label = <a/>)
       |  new E (label = <a/>)
       |  new A(key = <a b="c"/>)
       |  new A (key = <a b="c"/>)
       |}
       |class X extends E(label = <a/>)
       |class Y extends E (label = <a/>)
       |""".stripMargin

  checkSection(
    "named arguments of constructors around XML literals",
    constructorsAroundXmlLiterals,
    """|[0:8..0:10): wx <= wx/
       |[1:6..1:7): E <= wx/E#
       |[1:7..1:7):  <= wx/E#`<init>`().
       |[1:8..1:13): label <= wx/E#`<init>`().(label)
       |[1:8..1:13): label <= wx/E#label.
       |[1:15..1:20): scala => scala/
       |[1:21..1:24): xml => scala/xml/
       |[1:25..1:29): Elem => scala/xml/Elem#
       |[2:6..2:7): A <= wx/A#
       |[2:7..2:7):  <= wx/A#`<init>`().
       |[2:8..2:11): key <= wx/A#`<init>`().(key)
       |[2:8..2:11): key <= wx/A#key.
       |[2:13..2:18): scala => scala/
       |[2:19..2:22): xml => scala/xml/
       |[2:23..2:27): Elem => scala/xml/Elem#
       |[3:7..3:8): O <= wx/O.
       |[4:6..4:7): E => wx/E#
       |[4:7..4:7):  => wx/E#`<init>`().
       |[4:8..4:13): label => wx/E#`<init>`().(label)
       |[5:6..5:7): E => wx/E#
       |[5:8..5:8):  => wx/E#`<init>`().
       |[5:9..5:14): label => wx/E#`<init>`().(label)
       |[6:6..6:7): A => wx/A#
       |[6:7..6:7):  => wx/A#`<init>`().
       |[6:8..6:11): key => wx/A#`<init>`().(key)
       |[7:6..7:7): A => wx/A#
       |[7:8..7:8):  => wx/A#`<init>`().
       |[7:9..7:12): key => wx/A#`<init>`().(key)
       |[9:6..9:7): X <= wx/X#
       |[9:8..9:8):  <= wx/X#`<init>`().
       |[9:16..9:17): E => wx/E#
       |[9:17..9:17):  => wx/E#`<init>`().
       |[9:18..9:23): label => wx/E#`<init>`().(label)
       |[10:6..10:7): Y <= wx/Y#
       |[10:8..10:8):  <= wx/Y#`<init>`().
       |[10:16..10:17): E => wx/E#
       |[10:18..10:18):  => wx/E#`<init>`().
       |[10:19..10:24): label => wx/E#`<init>`().(label)
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of constructors around XML literals: synthetics",
    constructorsAroundXmlLiterals,
    "",
    "Synthetics",
  )

  private val applyWithImplicitsAndOperatorAssignment =
    """|package wy
       |class I
       |class M {
       |  def apply(k: Int)(implicit i: I): Int = k
       |  def update(k: Int, v: Int)(implicit i: I): Unit = ()
       |}
       |object O {
       |  implicit val i: I = new I
       |  val m = new M
       |  def n = 0
       |  m.apply(k = 0) += 5
       |  m.apply (k = 0) += 5
       |  m apply (k = 0) += 5
       |  m(k = 0) += 5
       |  m.apply(k = n) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an apply with an implicit list and +=",
    applyWithImplicitsAndOperatorAssignment,
    """|[0:8..0:10): wy <= wy/
       |[1:6..1:7): I <= wy/I#
       |[1:7..1:7):  <= wy/I#`<init>`().
       |[2:6..2:7): M <= wy/M#
       |[2:8..2:8):  <= wy/M#`<init>`().
       |[3:6..3:11): apply <= wy/M#apply().
       |[3:12..3:13): k <= wy/M#apply().(k)
       |[3:15..3:18): Int => scala/Int#
       |[3:29..3:30): i <= wy/M#apply().(i)
       |[3:32..3:33): I => wy/I#
       |[3:36..3:39): Int => scala/Int#
       |[3:42..3:43): k => wy/M#apply().(k)
       |[4:6..4:12): update <= wy/M#update().
       |[4:13..4:14): k <= wy/M#update().(k)
       |[4:16..4:19): Int => scala/Int#
       |[4:21..4:22): v <= wy/M#update().(v)
       |[4:24..4:27): Int => scala/Int#
       |[4:38..4:39): i <= wy/M#update().(i)
       |[4:41..4:42): I => wy/I#
       |[4:45..4:49): Unit => scala/Unit#
       |[6:7..6:8): O <= wy/O.
       |[7:15..7:16): i <= wy/O.i.
       |[7:18..7:19): I => wy/I#
       |[7:26..7:27): I => wy/I#
       |[7:27..7:27):  => wy/I#`<init>`().
       |[8:6..8:7): m <= wy/O.m.
       |[8:14..8:15): M => wy/M#
       |[8:15..8:15):  => wy/M#`<init>`().
       |[9:6..9:7): n <= wy/O.n().
       |[10:2..10:3): m => wy/O.m.
       |[10:17..10:19): += => scala/Int#`+`(+4).
       |[11:2..11:3): m => wy/O.m.
       |[11:18..11:20): += => scala/Int#`+`(+4).
       |[12:2..12:3): m => wy/O.m.
       |[12:18..12:20): += => scala/Int#`+`(+4).
       |[13:2..13:3): m => wy/O.m.
       |[13:4..13:5): k => wy/M#apply().(k)
       |[13:11..13:13): += => scala/Int#`+`(+4).
       |[14:2..14:3): m => wy/O.m.
       |[14:14..14:15): n => wy/O.n().
       |[14:17..14:19): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
    ScalaVersion.Scala212 ->
      """|[0:8..0:10): wy <= wy/
         |[1:6..1:7): I <= wy/I#
         |[1:7..1:7):  <= wy/I#`<init>`().
         |[2:6..2:7): M <= wy/M#
         |[2:8..2:8):  <= wy/M#`<init>`().
         |[3:6..3:11): apply <= wy/M#apply().
         |[3:12..3:13): k <= wy/M#apply().(k)
         |[3:15..3:18): Int => scala/Int#
         |[3:29..3:30): i <= wy/M#apply().(i)
         |[3:32..3:33): I => wy/I#
         |[3:36..3:39): Int => scala/Int#
         |[3:42..3:43): k => wy/M#apply().(k)
         |[4:6..4:12): update <= wy/M#update().
         |[4:13..4:14): k <= wy/M#update().(k)
         |[4:16..4:19): Int => scala/Int#
         |[4:21..4:22): v <= wy/M#update().(v)
         |[4:24..4:27): Int => scala/Int#
         |[4:38..4:39): i <= wy/M#update().(i)
         |[4:41..4:42): I => wy/I#
         |[4:45..4:49): Unit => scala/Unit#
         |[6:7..6:8): O <= wy/O.
         |[7:15..7:16): i <= wy/O.i.
         |[7:18..7:19): I => wy/I#
         |[7:26..7:27): I => wy/I#
         |[7:27..7:27):  => wy/I#`<init>`().
         |[8:6..8:7): m <= wy/O.m.
         |[8:14..8:15): M => wy/M#
         |[8:15..8:15):  => wy/M#`<init>`().
         |[9:6..9:7): n <= wy/O.n().
         |[10:2..10:3): m => wy/O.m.
         |[10:10..10:11): k => wy/M#apply().(k)
         |[10:17..10:19): += => scala/Int#`+`(+4).
         |[11:2..11:3): m => wy/O.m.
         |[11:11..11:12): k => wy/M#apply().(k)
         |[11:18..11:20): += => scala/Int#`+`(+4).
         |[12:2..12:3): m => wy/O.m.
         |[12:11..12:12): k => wy/M#apply().(k)
         |[12:18..12:20): += => scala/Int#`+`(+4).
         |[13:2..13:3): m => wy/O.m.
         |[13:4..13:5): k => wy/M#apply().(k)
         |[13:11..13:13): += => scala/Int#`+`(+4).
         |[14:2..14:3): m => wy/O.m.
         |[14:10..14:11): k => wy/M#apply().(k)
         |[14:14..14:15): n => wy/O.n().
         |[14:17..14:19): += => scala/Int#`+`(+4).
         |""".stripMargin,
  )

  checkSection(
    "named arguments of an apply with an implicit list and +=: synthetics",
    applyWithImplicitsAndOperatorAssignment,
    """|[10:17..10:21): += 5 => *(i)
       |  i => wy/O.i.
       |[11:18..11:22): += 5 => *(i)
       |  i => wy/O.i.
       |[12:18..12:22): += 5 => *(i)
       |  i => wy/O.i.
       |[13:2..13:3): m => *.apply
       |  apply => wy/M#apply().
       |[13:2..13:15): m(k = 0) += 5 => *(i)
       |  i => wy/O.i.
       |[14:17..14:21): += 5 => *(i)
       |  i => wy/O.i.
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[10:2..10:3): m => *.apply
         |  apply => wy/M#apply().
         |[10:20..10:21): 5 => *(i)
         |  i => wy/O.i.
         |[11:2..11:3): m => *.apply
         |  apply => wy/M#apply().
         |[11:21..11:22): 5 => *(i)
         |  i => wy/O.i.
         |[12:2..12:3): m => *.apply
         |  apply => wy/M#apply().
         |[12:21..12:22): 5 => *(i)
         |  i => wy/O.i.
         |[13:2..13:3): m => *.apply
         |  apply => wy/M#apply().
         |[13:2..13:15): m(k = 0) += 5 => *(i)
         |  i => wy/O.i.
         |[14:2..14:3): m => *.apply
         |  apply => wy/M#apply().
         |[14:20..14:21): 5 => *(i)
         |  i => wy/O.i.
         |""".stripMargin,
    ScalaVersion.AtMost213(15) ->
      """|[10:20..10:21): 5 => *(i)
         |  i => wy/O.i.
         |[11:21..11:22): 5 => *(i)
         |  i => wy/O.i.
         |[12:21..12:22): 5 => *(i)
         |  i => wy/O.i.
         |[13:2..13:3): m => *.apply
         |  apply => wy/M#apply().
         |[13:2..13:15): m(k = 0) += 5 => *(i)
         |  i => wy/O.i.
         |[14:20..14:21): 5 => *(i)
         |  i => wy/O.i.
         |""".stripMargin,
  )

  private val applyWithTypeArgumentsAndOperatorAssignment =
    """|package wz
       |class M { def apply[T](k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  val m = new M
       |  m.apply[Int](k = 0) += 5
       |  m.apply[Int] (k = 0) += 5
       |  m apply[Int] (k = 0) += 5
       |  m[Int](k = 0) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an apply with type arguments and +=",
    applyWithTypeArgumentsAndOperatorAssignment,
    """|[0:8..0:10): wz <= wz/
       |[1:6..1:7): M <= wz/M#
       |[1:8..1:8):  <= wz/M#`<init>`().
       |[1:14..1:19): apply <= wz/M#apply().
       |[1:20..1:21): T <= wz/M#apply().[T]
       |[1:23..1:24): k <= wz/M#apply().(k)
       |[1:26..1:29): Int => scala/Int#
       |[1:32..1:35): Int => scala/Int#
       |[1:38..1:39): k => wz/M#apply().(k)
       |[1:45..1:51): update <= wz/M#update().
       |[1:52..1:53): k <= wz/M#update().(k)
       |[1:55..1:58): Int => scala/Int#
       |[1:60..1:61): v <= wz/M#update().(v)
       |[1:63..1:66): Int => scala/Int#
       |[1:69..1:73): Unit => scala/Unit#
       |[2:7..2:8): O <= wz/O.
       |[3:6..3:7): m <= wz/O.m.
       |[3:14..3:15): M => wz/M#
       |[3:15..3:15):  => wz/M#`<init>`().
       |[4:2..4:3): m => wz/O.m.
       |[4:22..4:24): += => scala/Int#`+`(+4).
       |[5:2..5:3): m => wz/O.m.
       |[5:23..5:25): += => scala/Int#`+`(+4).
       |[6:2..6:3): m => wz/O.m.
       |[6:23..6:25): += => scala/Int#`+`(+4).
       |[7:2..7:3): m => wz/O.m.
       |[7:9..7:10): k => wz/M#apply().(k)
       |[7:16..7:18): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an apply with type arguments and +=: synthetics",
    applyWithTypeArgumentsAndOperatorAssignment,
    """|[4:2..4:2):  => *.update
       |  update => wz/M#update().
       |[5:2..5:2):  => *.update
       |  update => wz/M#update().
       |[6:2..6:2):  => *.update
       |  update => wz/M#update().
       |[7:2..7:3): m => *.apply[Nothing]
       |  apply => wz/M#apply().
       |  Nothing => scala/Nothing#
       |""".stripMargin,
    "Synthetics",
  )

  private val applyWithTypeArgumentsImplicitsAndOperatorAssignment =
    """|package xa
       |class I
       |class M {
       |  def apply[T](k: Int)(implicit i: I): Int = k
       |  def update(k: Int, v: Int)(implicit i: I): Unit = ()
       |}
       |object O {
       |  implicit val i: I = new I
       |  val m = new M
       |  m.apply[Int](k = 0) += 5
       |  m.apply[Int] (k = 0) += 5
       |  m apply[Int] (k = 0) += 5
       |  m[Int](k = 0) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an apply with type arguments, an implicit list and +=",
    applyWithTypeArgumentsImplicitsAndOperatorAssignment,
    """|[0:8..0:10): xa <= xa/
       |[1:6..1:7): I <= xa/I#
       |[1:7..1:7):  <= xa/I#`<init>`().
       |[2:6..2:7): M <= xa/M#
       |[2:8..2:8):  <= xa/M#`<init>`().
       |[3:6..3:11): apply <= xa/M#apply().
       |[3:12..3:13): T <= xa/M#apply().[T]
       |[3:15..3:16): k <= xa/M#apply().(k)
       |[3:18..3:21): Int => scala/Int#
       |[3:32..3:33): i <= xa/M#apply().(i)
       |[3:35..3:36): I => xa/I#
       |[3:39..3:42): Int => scala/Int#
       |[3:45..3:46): k => xa/M#apply().(k)
       |[4:6..4:12): update <= xa/M#update().
       |[4:13..4:14): k <= xa/M#update().(k)
       |[4:16..4:19): Int => scala/Int#
       |[4:21..4:22): v <= xa/M#update().(v)
       |[4:24..4:27): Int => scala/Int#
       |[4:38..4:39): i <= xa/M#update().(i)
       |[4:41..4:42): I => xa/I#
       |[4:45..4:49): Unit => scala/Unit#
       |[6:7..6:8): O <= xa/O.
       |[7:15..7:16): i <= xa/O.i.
       |[7:18..7:19): I => xa/I#
       |[7:26..7:27): I => xa/I#
       |[7:27..7:27):  => xa/I#`<init>`().
       |[8:6..8:7): m <= xa/O.m.
       |[8:14..8:15): M => xa/M#
       |[8:15..8:15):  => xa/M#`<init>`().
       |[9:2..9:3): m => xa/O.m.
       |[9:22..9:24): += => scala/Int#`+`(+4).
       |[10:2..10:3): m => xa/O.m.
       |[10:23..10:25): += => scala/Int#`+`(+4).
       |[11:2..11:3): m => xa/O.m.
       |[11:23..11:25): += => scala/Int#`+`(+4).
       |[12:2..12:3): m => xa/O.m.
       |[12:9..12:10): k => xa/M#apply().(k)
       |[12:16..12:18): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
    ScalaVersion.Scala212 ->
      """|[0:8..0:10): xa <= xa/
         |[1:6..1:7): I <= xa/I#
         |[1:7..1:7):  <= xa/I#`<init>`().
         |[2:6..2:7): M <= xa/M#
         |[2:8..2:8):  <= xa/M#`<init>`().
         |[3:6..3:11): apply <= xa/M#apply().
         |[3:12..3:13): T <= xa/M#apply().[T]
         |[3:15..3:16): k <= xa/M#apply().(k)
         |[3:18..3:21): Int => scala/Int#
         |[3:32..3:33): i <= xa/M#apply().(i)
         |[3:35..3:36): I => xa/I#
         |[3:39..3:42): Int => scala/Int#
         |[3:45..3:46): k => xa/M#apply().(k)
         |[4:6..4:12): update <= xa/M#update().
         |[4:13..4:14): k <= xa/M#update().(k)
         |[4:16..4:19): Int => scala/Int#
         |[4:21..4:22): v <= xa/M#update().(v)
         |[4:24..4:27): Int => scala/Int#
         |[4:38..4:39): i <= xa/M#update().(i)
         |[4:41..4:42): I => xa/I#
         |[4:45..4:49): Unit => scala/Unit#
         |[6:7..6:8): O <= xa/O.
         |[7:15..7:16): i <= xa/O.i.
         |[7:18..7:19): I => xa/I#
         |[7:26..7:27): I => xa/I#
         |[7:27..7:27):  => xa/I#`<init>`().
         |[8:6..8:7): m <= xa/O.m.
         |[8:14..8:15): M => xa/M#
         |[8:15..8:15):  => xa/M#`<init>`().
         |[9:2..9:3): m => xa/O.m.
         |[9:15..9:16): k => xa/M#apply().(k)
         |[9:22..9:24): += => scala/Int#`+`(+4).
         |[10:2..10:3): m => xa/O.m.
         |[10:16..10:17): k => xa/M#apply().(k)
         |[10:23..10:25): += => scala/Int#`+`(+4).
         |[11:2..11:3): m => xa/O.m.
         |[11:16..11:17): k => xa/M#apply().(k)
         |[11:23..11:25): += => scala/Int#`+`(+4).
         |[12:2..12:3): m => xa/O.m.
         |[12:9..12:10): k => xa/M#apply().(k)
         |[12:16..12:18): += => scala/Int#`+`(+4).
         |""".stripMargin,
  )

  checkSection(
    "named arguments of an apply with type arguments, an implicit list and +=: synthetics",
    applyWithTypeArgumentsImplicitsAndOperatorAssignment,
    """|[9:22..9:26): += 5 => *(i)
       |  i => xa/O.i.
       |[10:23..10:27): += 5 => *(i)
       |  i => xa/O.i.
       |[11:23..11:27): += 5 => *(i)
       |  i => xa/O.i.
       |[12:2..12:3): m => *.apply[Nothing]
       |  apply => xa/M#apply().
       |  Nothing => scala/Nothing#
       |[12:2..12:20): m[Int](k = 0) += 5 => *(i)
       |  i => xa/O.i.
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[9:2..9:3): m => *.apply[Nothing]
         |  apply => xa/M#apply().
         |  Nothing => scala/Nothing#
         |[9:25..9:26): 5 => *(i)
         |  i => xa/O.i.
         |[10:2..10:3): m => *.apply[Nothing]
         |  apply => xa/M#apply().
         |  Nothing => scala/Nothing#
         |[10:26..10:27): 5 => *(i)
         |  i => xa/O.i.
         |[11:2..11:3): m => *.apply[Nothing]
         |  apply => xa/M#apply().
         |  Nothing => scala/Nothing#
         |[11:26..11:27): 5 => *(i)
         |  i => xa/O.i.
         |[12:2..12:3): m => *.apply[Nothing]
         |  apply => xa/M#apply().
         |  Nothing => scala/Nothing#
         |[12:2..12:20): m[Int](k = 0) += 5 => *(i)
         |  i => xa/O.i.
         |""".stripMargin,
    ScalaVersion.AtMost213(15) ->
      """|[9:25..9:26): 5 => *(i)
         |  i => xa/O.i.
         |[10:26..10:27): 5 => *(i)
         |  i => xa/O.i.
         |[11:26..11:27): 5 => *(i)
         |  i => xa/O.i.
         |[12:2..12:3): m => *.apply[Nothing]
         |  apply => xa/M#apply().
         |  Nothing => scala/Nothing#
         |[12:2..12:20): m[Int](k = 0) += 5 => *(i)
         |  i => xa/O.i.
         |""".stripMargin,
  )

  private val viewToApplyWithOperatorAssignment =
    """|package xb
       |import scala.language.implicitConversions
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |class X
       |object O {
       |  implicit def conv(x: X): M = new M
       |  val x = new X
       |  x.apply(k = 0) += 5
       |  x.apply (k = 0) += 5
       |  x apply(k = 0) += 5
       |  x apply (k = 0) += 5
       |  x(k = 0) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an apply through an implicit view with +=",
    viewToApplyWithOperatorAssignment,
    """|[0:8..0:10): xb <= xb/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:41): implicitConversions => scala/language.implicitConversions.
       |[2:6..2:7): M <= xb/M#
       |[2:8..2:8):  <= xb/M#`<init>`().
       |[2:14..2:19): apply <= xb/M#apply().
       |[2:20..2:21): k <= xb/M#apply().(k)
       |[2:23..2:26): Int => scala/Int#
       |[2:29..2:32): Int => scala/Int#
       |[2:35..2:36): k => xb/M#apply().(k)
       |[2:42..2:48): update <= xb/M#update().
       |[2:49..2:50): k <= xb/M#update().(k)
       |[2:52..2:55): Int => scala/Int#
       |[2:57..2:58): v <= xb/M#update().(v)
       |[2:60..2:63): Int => scala/Int#
       |[2:66..2:70): Unit => scala/Unit#
       |[3:6..3:7): X <= xb/X#
       |[3:7..3:7):  <= xb/X#`<init>`().
       |[4:7..4:8): O <= xb/O.
       |[5:15..5:19): conv <= xb/O.conv().
       |[5:20..5:21): x <= xb/O.conv().(x)
       |[5:23..5:24): X => xb/X#
       |[5:27..5:28): M => xb/M#
       |[5:35..5:36): M => xb/M#
       |[5:36..5:36):  => xb/M#`<init>`().
       |[6:6..6:7): x <= xb/O.x.
       |[6:14..6:15): X => xb/X#
       |[6:15..6:15):  => xb/X#`<init>`().
       |[7:2..7:3): x => xb/O.x.
       |[7:17..7:19): += => scala/Int#`+`(+4).
       |[8:2..8:3): x => xb/O.x.
       |[8:18..8:20): += => scala/Int#`+`(+4).
       |[9:2..9:3): x => xb/O.x.
       |[9:17..9:19): += => scala/Int#`+`(+4).
       |[10:2..10:3): x => xb/O.x.
       |[10:18..10:20): += => scala/Int#`+`(+4).
       |[11:2..11:3): x => xb/O.x.
       |[11:4..11:5): k => xb/M#apply().(k)
       |[11:11..11:13): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an apply through an implicit view with +=: synthetics",
    viewToApplyWithOperatorAssignment,
    """|[7:2..7:2):  => *.update
       |  update => xb/M#update().
       |[7:2..7:3): x => conv(*)
       |  conv => xb/O.conv().
       |[8:2..8:2):  => *.update
       |  update => xb/M#update().
       |[8:2..8:3): x => conv(*)
       |  conv => xb/O.conv().
       |[9:2..9:2):  => *.update
       |  update => xb/M#update().
       |[9:2..9:3): x => conv(*)
       |  conv => xb/O.conv().
       |[10:2..10:2):  => *.update
       |  update => xb/M#update().
       |[10:2..10:3): x => conv(*)
       |  conv => xb/O.conv().
       |[11:2..11:3): x => conv(*)
       |  conv => xb/O.conv().
       |""".stripMargin,
    "Synthetics",
  )

  private val writtenApplyWithDottedOperatorAssignment =
    """|package xc
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  val m = new M
       |  m.apply(k = 0).+=(5)
       |  m.apply (k = 0).+=(5)
       |}
       |""".stripMargin

  checkSection(
    "named arguments of a written apply with a dotted +=",
    writtenApplyWithDottedOperatorAssignment,
    """|[0:8..0:10): xc <= xc/
       |[1:6..1:7): M <= xc/M#
       |[1:8..1:8):  <= xc/M#`<init>`().
       |[1:14..1:19): apply <= xc/M#apply().
       |[1:20..1:21): k <= xc/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => xc/M#apply().(k)
       |[1:42..1:48): update <= xc/M#update().
       |[1:49..1:50): k <= xc/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= xc/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): O <= xc/O.
       |[3:6..3:7): m <= xc/O.m.
       |[3:14..3:15): M => xc/M#
       |[3:15..3:15):  => xc/M#`<init>`().
       |[4:2..4:3): m => xc/O.m.
       |[4:17..4:19): += => scala/Int#`+`(+4).
       |[5:2..5:3): m => xc/O.m.
       |[5:18..5:20): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a written apply with a dotted +=: synthetics",
    writtenApplyWithDottedOperatorAssignment,
    """|[4:2..4:2):  => *.update
       |  update => xc/M#update().
       |[5:2..5:2):  => *.update
       |  update => xc/M#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val writtenApplyWithMemberOperator =
    """|package xd
       |class B { def +=(y: Int) = this }
       |class M2 { def apply(k: Int) = new B }
       |object O {
       |  val m2 = new M2
       |  m2.apply(k = 0) += 5
       |  m2 apply (k = 0) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of a written apply whose result has a member operator",
    writtenApplyWithMemberOperator,
    """|[0:8..0:10): xd <= xd/
       |[1:6..1:7): B <= xd/B#
       |[1:8..1:8):  <= xd/B#`<init>`().
       |[1:14..1:16): += <= xd/B#`+=`().
       |[1:17..1:18): y <= xd/B#`+=`().(y)
       |[1:20..1:23): Int => scala/Int#
       |[2:6..2:8): M2 <= xd/M2#
       |[2:9..2:9):  <= xd/M2#`<init>`().
       |[2:15..2:20): apply <= xd/M2#apply().
       |[2:21..2:22): k <= xd/M2#apply().(k)
       |[2:24..2:27): Int => scala/Int#
       |[2:35..2:36): B => xd/B#
       |[2:36..2:36):  => xd/B#`<init>`().
       |[3:7..3:8): O <= xd/O.
       |[4:6..4:8): m2 <= xd/O.m2.
       |[4:15..4:17): M2 => xd/M2#
       |[4:17..4:17):  => xd/M2#`<init>`().
       |[5:2..5:4): m2 => xd/O.m2.
       |[5:5..5:10): apply => xd/M2#apply().
       |[5:11..5:12): k => xd/M2#apply().(k)
       |[5:18..5:20): += => xd/B#`+=`().
       |[6:2..6:4): m2 => xd/O.m2.
       |[6:5..6:10): apply => xd/M2#apply().
       |[6:12..6:13): k => xd/M2#apply().(k)
       |[6:19..6:21): += => xd/B#`+=`().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a written apply whose result has a member operator: synthetics",
    writtenApplyWithMemberOperator,
    "",
    "Synthetics",
  )

  private val applyWithDefaultImplicitAsQualifier =
    """|package xe
       |class MD { def apply(k: Int)(implicit j: Int = k): Int = k }
       |object O {
       |  val md = new MD
       |  md.apply(k = 0).toString
       |  md.apply (k = 0).toString
       |  md(k = 0).toString
       |}
       |""".stripMargin

  checkSection(
    "named arguments of an apply with a default implicit as a qualifier",
    applyWithDefaultImplicitAsQualifier,
    """|[0:8..0:10): xe <= xe/
       |[1:6..1:8): MD <= xe/MD#
       |[1:9..1:9):  <= xe/MD#`<init>`().
       |[1:15..1:20): apply <= xe/MD#apply().
       |[1:21..1:22): k <= xe/MD#apply().(k)
       |[1:24..1:27): Int => scala/Int#
       |[1:38..1:39): j <= xe/MD#apply().(j)
       |[1:41..1:44): Int => scala/Int#
       |[1:47..1:48): k => xe/MD#apply().(k)
       |[1:51..1:54): Int => scala/Int#
       |[1:57..1:58): k => xe/MD#apply().(k)
       |[2:7..2:8): O <= xe/O.
       |[3:6..3:8): md <= xe/O.md.
       |[3:15..3:17): MD => xe/MD#
       |[3:17..3:17):  => xe/MD#`<init>`().
       |[4:2..4:4): md => xe/O.md.
       |[4:5..4:10): apply => xe/MD#apply().
       |[4:11..4:12): k => xe/MD#apply().(k)
       |[4:18..4:26): toString => scala/Any#toString().
       |[5:2..5:4): md => xe/O.md.
       |[5:5..5:10): apply => xe/MD#apply().
       |[5:12..5:13): k => xe/MD#apply().(k)
       |[5:19..5:27): toString => scala/Any#toString().
       |[6:2..6:4): md => xe/O.md.
       |[6:5..6:6): k => xe/MD#apply().(k)
       |[6:12..6:20): toString => scala/Any#toString().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of an apply with a default implicit as a qualifier: synthetics",
    applyWithDefaultImplicitAsQualifier,
    """|[4:2..4:17): md.apply(k = 0) => *(x$2)
       |  x$2 => local0
       |[5:2..5:18): md.apply (k = 0) => *(x$4)
       |  x$4 => local1
       |[6:2..6:4): md => *.apply
       |  apply => xe/MD#apply().
       |[6:2..6:11): md(k = 0) => *(x$6)
       |  x$6 => local2
       |""".stripMargin,
    "Synthetics",
  )

  private val writtenApplyOnReorderedCallWithOperatorAssignment =
    """|package xf
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  def mk(x: Int, y: Int): M = new M
       |  mk(y = 1, x = 2) apply (k = 0) += 5
       |  mk(y = 1, x = 2).apply(k = 0) += 5
       |}
       |""".stripMargin

  checkSection(
    "named arguments of a written apply on a reordered call with +=",
    writtenApplyOnReorderedCallWithOperatorAssignment,
    """|[0:8..0:10): xf <= xf/
       |[1:6..1:7): M <= xf/M#
       |[1:8..1:8):  <= xf/M#`<init>`().
       |[1:14..1:19): apply <= xf/M#apply().
       |[1:20..1:21): k <= xf/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => xf/M#apply().(k)
       |[1:42..1:48): update <= xf/M#update().
       |[1:49..1:50): k <= xf/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= xf/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): O <= xf/O.
       |[3:6..3:8): mk <= xf/O.mk().
       |[3:9..3:10): x <= xf/O.mk().(x)
       |[3:12..3:15): Int => scala/Int#
       |[3:17..3:18): y <= xf/O.mk().(y)
       |[3:20..3:23): Int => scala/Int#
       |[3:26..3:27): M => xf/M#
       |[3:34..3:35): M => xf/M#
       |[3:35..3:35):  => xf/M#`<init>`().
       |[4:2..4:4): mk => xf/O.mk().
       |[4:5..4:6): y => xf/O.mk().(y)
       |[4:12..4:13): x => xf/O.mk().(x)
       |[4:26..4:27): k => xf/M#apply().(k)
       |[4:33..4:35): += => scala/Int#`+`(+4).
       |[5:2..5:4): mk => xf/O.mk().
       |[5:5..5:6): y => xf/O.mk().(y)
       |[5:12..5:13): x => xf/O.mk().(x)
       |[5:25..5:26): k => xf/M#apply().(k)
       |[5:32..5:34): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "named arguments of a written apply on a reordered call with +=: synthetics",
    writtenApplyOnReorderedCallWithOperatorAssignment,
    "",
    "Synthetics",
  )

  private val operatorOnVarAndSetter =
    """|package xg
       |class X { var v = 0; def w: Int = 0; def w_=(x: Int): Unit = () }
       |object O {
       |  val x = new X
       |  var c = 0
       |  private[this] var p = 0
       |  val pt = new java.awt.Point
       |  def g = {
       |    var l = 0
       |    x.v += 1
       |    x.v = 1
       |    x.w += 1
       |    x.w = 1
       |    x.v.+=(1)
       |    c += 1
       |    c = 1
       |    p += 1
       |    l += 1
       |    pt.x += 1
       |  }
       |}
       |""".stripMargin

  checkSection(
    "an assignment operator on a var and on a setter",
    operatorOnVarAndSetter,
    """|[0:8..0:10): xg <= xg/
       |[1:6..1:7): X <= xg/X#
       |[1:8..1:8):  <= xg/X#`<init>`().
       |[1:14..1:15): v <= xg/X#v().
       |[1:25..1:26): w <= xg/X#w().
       |[1:28..1:31): Int => scala/Int#
       |[1:41..1:44): w_= <= xg/X#`w_=`().
       |[1:45..1:46): x <= xg/X#`w_=`().(x)
       |[1:48..1:51): Int => scala/Int#
       |[1:54..1:58): Unit => scala/Unit#
       |[2:7..2:8): O <= xg/O.
       |[3:6..3:7): x <= xg/O.x.
       |[3:14..3:15): X => xg/X#
       |[3:15..3:15):  => xg/X#`<init>`().
       |[4:6..4:7): c <= xg/O.c().
       |[5:20..5:21): p <= xg/O.p().
       |[6:6..6:8): pt <= xg/O.pt.
       |[6:15..6:19): java => java/
       |[6:20..6:23): awt => java/awt/
       |[6:24..6:29): Point => java/awt/Point#
       |[6:29..6:29):  => java/awt/Point#`<init>`().
       |[7:6..7:7): g <= xg/O.g().
       |[8:8..8:9): l <= local0
       |[9:4..9:5): x => xg/O.x.
       |[9:6..9:7): v => xg/X#`v_=`().
       |[9:8..9:10): += => scala/Int#`+`(+4).
       |[10:4..10:5): x => xg/O.x.
       |[10:6..10:7): v => xg/X#`v_=`().
       |[11:4..11:5): x => xg/O.x.
       |[11:6..11:7): w => xg/X#`w_=`().
       |[11:8..11:10): += => scala/Int#`+`(+4).
       |[12:4..12:5): x => xg/O.x.
       |[12:6..12:7): w => xg/X#`w_=`().
       |[13:4..13:5): x => xg/O.x.
       |[13:6..13:7): v => xg/X#`v_=`().
       |[13:8..13:10): += => scala/Int#`+`(+4).
       |[14:4..14:5): c => xg/O.`c_=`().
       |[14:6..14:8): += => scala/Int#`+`(+4).
       |[15:4..15:5): c => xg/O.`c_=`().
       |[16:4..16:5): p => xg/O.p().
       |[16:6..16:8): += => scala/Int#`+`(+4).
       |[17:4..17:5): l => local0
       |[17:6..17:8): += => scala/Int#`+`(+4).
       |[18:4..18:6): pt => xg/O.pt.
       |[18:7..18:8): x => java/awt/Point#x.
       |[18:9..18:11): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an assignment operator on a var and on a setter: synthetics",
    operatorOnVarAndSetter,
    "",
    "Synthetics",
  )

  private val operatorOnSetterOfMethodResult =
    """|package xh
       |class X { var v = 0; def w: Int = 0; def w_=(x: Int): Unit = () }
       |object O {
       |  def mk() = new X
       |  mk().v += 1
       |  mk().v = 1
       |  mk().w += 1
       |  mk().w = 1
       |}
       |""".stripMargin

  checkSection(
    "an assignment operator on a setter of a method result",
    operatorOnSetterOfMethodResult,
    """|[0:8..0:10): xh <= xh/
       |[1:6..1:7): X <= xh/X#
       |[1:8..1:8):  <= xh/X#`<init>`().
       |[1:14..1:15): v <= xh/X#v().
       |[1:25..1:26): w <= xh/X#w().
       |[1:28..1:31): Int => scala/Int#
       |[1:41..1:44): w_= <= xh/X#`w_=`().
       |[1:45..1:46): x <= xh/X#`w_=`().(x)
       |[1:48..1:51): Int => scala/Int#
       |[1:54..1:58): Unit => scala/Unit#
       |[2:7..2:8): O <= xh/O.
       |[3:6..3:8): mk <= xh/O.mk().
       |[3:17..3:18): X => xh/X#
       |[3:18..3:18):  => xh/X#`<init>`().
       |[4:2..4:4): mk => xh/O.mk().
       |[4:7..4:8): v => xh/X#`v_=`().
       |[4:9..4:11): += => scala/Int#`+`(+4).
       |[5:2..5:4): mk => xh/O.mk().
       |[5:7..5:8): v => xh/X#`v_=`().
       |[6:2..6:4): mk => xh/O.mk().
       |[6:7..6:8): w => xh/X#`w_=`().
       |[6:9..6:11): += => scala/Int#`+`(+4).
       |[7:2..7:4): mk => xh/O.mk().
       |[7:7..7:8): w => xh/X#`w_=`().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an assignment operator on a setter of a method result: synthetics",
    operatorOnSetterOfMethodResult,
    "",
    "Synthetics",
  )

  private val updateWithNestedIndex =
    """|package xi
       |object O {
       |  var n = 0
       |  val aa = Array(Array(1))
       |  aa(0)(1) += 1
       |  aa(0)(1) = 1
       |  aa(n)(n) += 1
       |}
       |""".stripMargin

  checkSection(
    "an update with a nested index",
    updateWithNestedIndex,
    """|[0:8..0:10): xi <= xi/
       |[1:7..1:8): O <= xi/O.
       |[2:6..2:7): n <= xi/O.n().
       |[3:6..3:8): aa <= xi/O.aa.
       |[3:11..3:16): Array => scala/Array.
       |[3:17..3:22): Array => scala/Array.
       |[4:2..4:4): aa => xi/O.aa.
       |[4:11..4:13): += => scala/Int#`+`(+4).
       |[5:2..5:4): aa => xi/O.aa.
       |[6:2..6:4): aa => xi/O.aa.
       |[6:5..6:6): n => xi/O.n().
       |[6:8..6:9): n => xi/O.n().
       |[6:11..6:13): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update with a nested index: synthetics",
    updateWithNestedIndex,
    """|[3:11..3:16): Array => *.apply[Array[Int]]
       |  apply => scala/Array.apply().
       |  Array => scala/Array#
       |  Int => scala/Int#
       |[3:11..3:26): Array(Array(1)) => *((`macro-expandee` : ClassTag[Array[Int]]))
       |  ClassTag => scala/reflect/ClassTag#
       |  Array => scala/Array#
       |  Int => scala/Int#
       |[3:17..3:22): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[4:2..4:4): aa => *.apply
       |  apply => scala/Array#apply().
       |[5:2..5:4): aa => *.apply
       |  apply => scala/Array#apply().
       |[5:2..5:7): aa(0) => *.update
       |  update => scala/Array#update().
       |[6:2..6:4): aa => *.apply
       |  apply => scala/Array#apply().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateOfMethodResultWithOperator =
    """|package xj
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  val arr = Array(1)
       |  def f() = arr
       |  def mk() = new M
       |  f()(0) += 1
       |  f()(0) = 1
       |  mk()(0) += 1
       |  mk()(0) = 1
       |}
       |""".stripMargin

  checkSection(
    "an update of a method result with an operator",
    updateOfMethodResultWithOperator,
    """|[0:8..0:10): xj <= xj/
       |[1:6..1:7): M <= xj/M#
       |[1:8..1:8):  <= xj/M#`<init>`().
       |[1:14..1:19): apply <= xj/M#apply().
       |[1:20..1:21): k <= xj/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => xj/M#apply().(k)
       |[1:42..1:48): update <= xj/M#update().
       |[1:49..1:50): k <= xj/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= xj/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): O <= xj/O.
       |[3:6..3:9): arr <= xj/O.arr.
       |[3:12..3:17): Array => scala/Array.
       |[4:6..4:7): f <= xj/O.f().
       |[4:12..4:15): arr => xj/O.arr.
       |[5:6..5:8): mk <= xj/O.mk().
       |[5:17..5:18): M => xj/M#
       |[5:18..5:18):  => xj/M#`<init>`().
       |[6:2..6:3): f => xj/O.f().
       |[6:9..6:11): += => scala/Int#`+`(+4).
       |[7:2..7:3): f => xj/O.f().
       |[8:2..8:4): mk => xj/O.mk().
       |[8:10..8:12): += => scala/Int#`+`(+4).
       |[9:2..9:4): mk => xj/O.mk().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update of a method result with an operator: synthetics",
    updateOfMethodResultWithOperator,
    """|[3:12..3:17): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[7:2..7:5): f() => *.update
       |  update => scala/Array#update().
       |[9:2..9:6): mk() => *.update
       |  update => xj/M#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateWithRightAssociativeOperator =
    """|package xk
       |object O {
       |  var n = 0
       |  val ys = Array(List(1))
       |  ys(0) ::= 2
       |  ys(n) ::= 2
       |  ys(0) = Nil
       |}
       |""".stripMargin

  checkSection(
    "an update with ::=",
    updateWithRightAssociativeOperator,
    """|[0:8..0:10): xk <= xk/
       |[1:7..1:8): O <= xk/O.
       |[2:6..2:7): n <= xk/O.n().
       |[3:6..3:8): ys <= xk/O.ys.
       |[3:11..3:16): Array => scala/Array.
       |[3:17..3:21): List => scala/package.List.
       |[4:2..4:4): ys => xk/O.ys.
       |[4:8..4:11): ::= => scala/collection/immutable/List#`::`().
       |[5:2..5:4): ys => xk/O.ys.
       |[5:5..5:6): n => xk/O.n().
       |[5:8..5:11): ::= => scala/collection/immutable/List#`::`().
       |[6:2..6:4): ys => xk/O.ys.
       |[6:10..6:13): Nil => scala/package.Nil.
       |""".stripMargin,
    "Occurrences",
    ScalaVersion.Scala212 ->
      """|[0:8..0:10): xk <= xk/
         |[1:7..1:8): O <= xk/O.
         |[2:6..2:7): n <= xk/O.n().
         |[3:6..3:8): ys <= xk/O.ys.
         |[3:11..3:16): Array => scala/Array.
         |[3:17..3:21): List => scala/collection/immutable/List.
         |[4:2..4:4): ys => xk/O.ys.
         |[4:8..4:11): ::= => scala/collection/immutable/List#`::`().
         |[5:2..5:4): ys => xk/O.ys.
         |[5:5..5:6): n => xk/O.n().
         |[5:8..5:11): ::= => scala/collection/immutable/List#`::`().
         |[6:2..6:4): ys => xk/O.ys.
         |[6:10..6:13): Nil => scala/collection/immutable/Nil.
         |""".stripMargin,
  )

  checkSection(
    "an update with ::=: synthetics",
    updateWithRightAssociativeOperator,
    """|[3:11..3:16): Array => *.apply[List[Int]]
       |  apply => scala/Array.apply().
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |[3:11..3:25): Array(List(1)) => *((`macro-expandee` : ClassTag[List[Int]]))
       |  ClassTag => scala/reflect/ClassTag#
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |[3:17..3:21): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |[4:2..4:4): ys => *.apply
       |  apply => scala/Array#apply().
       |[4:2..4:11): ys(0) ::= => *[Int]
       |  Int => scala/Int#
       |[5:2..5:4): ys => *.apply
       |  apply => scala/Array#apply().
       |[5:2..5:11): ys(n) ::= => *[Int]
       |  Int => scala/Int#
       |[6:2..6:4): ys => *.update
       |  update => scala/Array#update().
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[3:11..3:16): Array => *.apply[List[Int]]
         |  apply => scala/Array.apply().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[3:11..3:25): Array(List(1)) => *((`macro-expandee` : ClassTag[List[Int]]))
         |  ClassTag => scala/reflect/ClassTag#
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[3:17..3:21): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[4:2..4:4): ys => *.apply
         |  apply => scala/Array#apply().
         |[4:2..4:11): ys(0) ::= => *[Int]
         |  Int => scala/Int#
         |[5:2..5:4): ys => *.apply
         |  apply => scala/Array#apply().
         |[5:2..5:11): ys(n) ::= => *[Int]
         |  Int => scala/Int#
         |[6:2..6:4): ys => *.update
         |  update => scala/Array#update().
         |""".stripMargin,
  )

  private val updateOfVar =
    """|package xl
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |class X { var buf = Array(1) }
       |object O {
       |  var arr = Array(1)
       |  var m = new M
       |  val x = new X
       |  arr(0) += 1
       |  arr(0) = 1
       |  m(0) += 1
       |  m(0) = 1
       |  this.arr(0) += 1
       |  x.buf(0) += 1
       |  x.buf(0) = 1
       |}
       |""".stripMargin

  checkSection(
    "an update of a var",
    updateOfVar,
    """|[0:8..0:10): xl <= xl/
       |[1:6..1:7): M <= xl/M#
       |[1:8..1:8):  <= xl/M#`<init>`().
       |[1:14..1:19): apply <= xl/M#apply().
       |[1:20..1:21): k <= xl/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => xl/M#apply().(k)
       |[1:42..1:48): update <= xl/M#update().
       |[1:49..1:50): k <= xl/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= xl/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:6..2:7): X <= xl/X#
       |[2:8..2:8):  <= xl/X#`<init>`().
       |[2:14..2:17): buf <= xl/X#buf().
       |[2:20..2:25): Array => scala/Array.
       |[3:7..3:8): O <= xl/O.
       |[4:6..4:9): arr <= xl/O.arr().
       |[4:12..4:17): Array => scala/Array.
       |[5:6..5:7): m <= xl/O.m().
       |[5:14..5:15): M => xl/M#
       |[5:15..5:15):  => xl/M#`<init>`().
       |[6:6..6:7): x <= xl/O.x.
       |[6:14..6:15): X => xl/X#
       |[6:15..6:15):  => xl/X#`<init>`().
       |[7:2..7:5): arr => xl/O.arr().
       |[7:9..7:11): += => scala/Int#`+`(+4).
       |[8:2..8:5): arr => xl/O.arr().
       |[9:2..9:3): m => xl/O.m().
       |[9:7..9:9): += => scala/Int#`+`(+4).
       |[10:2..10:3): m => xl/O.m().
       |[11:7..11:10): arr => xl/O.arr().
       |[11:14..11:16): += => scala/Int#`+`(+4).
       |[12:2..12:3): x => xl/O.x.
       |[12:4..12:7): buf => xl/X#buf().
       |[12:11..12:13): += => scala/Int#`+`(+4).
       |[13:2..13:3): x => xl/O.x.
       |[13:4..13:7): buf => xl/X#buf().
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update of a var: synthetics",
    updateOfVar,
    """|[2:20..2:25): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[4:12..4:17): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[8:2..8:5): arr => *.update
       |  update => scala/Array#update().
       |[10:2..10:3): m => *.update
       |  update => xl/M#update().
       |[13:2..13:7): x.buf => *.update
       |  update => scala/Array#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateWithIndexOfApply =
    """|package xm
       |object O {
       |  val arr = Array(1)
       |  val counts = Array(0)
       |  counts(arr(0)) += 1
       |  counts(arr(0)) = 1
       |}
       |""".stripMargin

  checkSection(
    "an update with an index that is an apply",
    updateWithIndexOfApply,
    """|[0:8..0:10): xm <= xm/
       |[1:7..1:8): O <= xm/O.
       |[2:6..2:9): arr <= xm/O.arr.
       |[2:12..2:17): Array => scala/Array.
       |[3:6..3:12): counts <= xm/O.counts.
       |[3:15..3:20): Array => scala/Array.
       |[4:2..4:8): counts => xm/O.counts.
       |[4:9..4:12): arr => xm/O.arr.
       |[4:17..4:19): += => scala/Int#`+`(+4).
       |[5:2..5:8): counts => xm/O.counts.
       |[5:9..5:12): arr => xm/O.arr.
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update with an index that is an apply: synthetics",
    updateWithIndexOfApply,
    """|[2:12..2:17): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[3:15..3:20): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[4:2..4:8): counts => *.apply
       |  apply => scala/Array#apply().
       |[4:9..4:12): arr => *.apply
       |  apply => scala/Array#apply().
       |[5:2..5:8): counts => *.update
       |  update => scala/Array#update().
       |[5:9..5:12): arr => *.apply
       |  apply => scala/Array#apply().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateOfQualifiedValue =
    """|package xn
       |class X { val arr = Array(1) }
       |object R { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  val arr = Array(1)
       |  val x = new X
       |  O.arr(0) += 1
       |  O.arr(0) = 1
       |  this.arr(0) += 1
       |  x.arr(0) += 1
       |  x.arr(0) = 1
       |  R(0) += 1
       |  R(0) = 1
       |}
       |""".stripMargin

  checkSection(
    "an update of a qualified value",
    updateOfQualifiedValue,
    """|[0:8..0:10): xn <= xn/
       |[1:6..1:7): X <= xn/X#
       |[1:8..1:8):  <= xn/X#`<init>`().
       |[1:14..1:17): arr <= xn/X#arr.
       |[1:20..1:25): Array => scala/Array.
       |[2:7..2:8): R <= xn/R.
       |[2:15..2:20): apply <= xn/R.apply().
       |[2:21..2:22): k <= xn/R.apply().(k)
       |[2:24..2:27): Int => scala/Int#
       |[2:30..2:33): Int => scala/Int#
       |[2:36..2:37): k => xn/R.apply().(k)
       |[2:43..2:49): update <= xn/R.update().
       |[2:50..2:51): k <= xn/R.update().(k)
       |[2:53..2:56): Int => scala/Int#
       |[2:58..2:59): v <= xn/R.update().(v)
       |[2:61..2:64): Int => scala/Int#
       |[2:67..2:71): Unit => scala/Unit#
       |[3:7..3:8): O <= xn/O.
       |[4:6..4:9): arr <= xn/O.arr.
       |[4:12..4:17): Array => scala/Array.
       |[5:6..5:7): x <= xn/O.x.
       |[5:14..5:15): X => xn/X#
       |[5:15..5:15):  => xn/X#`<init>`().
       |[6:2..6:3): O => xn/O.
       |[6:4..6:7): arr => xn/O.arr.
       |[6:11..6:13): += => scala/Int#`+`(+4).
       |[7:2..7:3): O => xn/O.
       |[7:4..7:7): arr => xn/O.arr.
       |[8:7..8:10): arr => xn/O.arr.
       |[8:14..8:16): += => scala/Int#`+`(+4).
       |[9:2..9:3): x => xn/O.x.
       |[9:4..9:7): arr => xn/X#arr.
       |[9:11..9:13): += => scala/Int#`+`(+4).
       |[10:2..10:3): x => xn/O.x.
       |[10:4..10:7): arr => xn/X#arr.
       |[11:2..11:3): R => xn/R.
       |[11:7..11:9): += => scala/Int#`+`(+4).
       |[12:2..12:3): R => xn/R.
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update of a qualified value: synthetics",
    updateOfQualifiedValue,
    """|[1:20..1:25): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[4:12..4:17): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[6:2..6:7): O.arr => *.apply
       |  apply => scala/Array#apply().
       |[7:2..7:7): O.arr => *.update
       |  update => scala/Array#update().
       |[8:2..8:10): this.arr => *.apply
       |  apply => scala/Array#apply().
       |[9:2..9:7): x.arr => *.apply
       |  apply => scala/Array#apply().
       |[10:2..10:7): x.arr => *.update
       |  update => scala/Array#update().
       |[11:2..11:3): R => *.apply
       |  apply => xn/R.apply().
       |[12:2..12:3): R => *.update
       |  update => xn/R.update().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateOfMutableMap =
    """|package xo
       |import scala.collection.mutable
       |object O {
       |  val mm = mutable.Map("a" -> 1)
       |  mm("a") += 1
       |  mm("a") = 1
       |}
       |""".stripMargin

  checkSection(
    "an update of a mutable map",
    updateOfMutableMap,
    """|[0:8..0:10): xo <= xo/
       |[1:7..1:12): scala => scala/
       |[1:13..1:23): collection => scala/collection/
       |[1:24..1:31): mutable => scala/collection/mutable/
       |[2:7..2:8): O <= xo/O.
       |[3:6..3:8): mm <= xo/O.mm.
       |[3:11..3:18): mutable => scala/collection/mutable/
       |[3:19..3:22): Map => scala/collection/mutable/Map.
       |[3:27..3:29): -> => scala/Predef.ArrowAssoc#`->`().
       |[4:2..4:4): mm => xo/O.mm.
       |[4:10..4:12): += => scala/Int#`+`(+4).
       |[5:2..5:4): mm => xo/O.mm.
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update of a mutable map: synthetics",
    updateOfMutableMap,
    """|[3:11..3:22): mutable.Map => *.apply[String, Int]
       |  apply => scala/collection/MapFactory.Delegate#apply().
       |  String => java/lang/String#
       |  Int => scala/Int#
       |[3:23..3:26): "a" => Predef.ArrowAssoc[String](*)
       |  Predef => scala/Predef.
       |  ArrowAssoc => scala/Predef.ArrowAssoc().
       |  String => java/lang/String#
       |[3:23..3:29): "a" -> => *[Int]
       |  Int => scala/Int#
       |[4:2..4:4): mm => *.apply
       |  apply => scala/collection/MapOps#apply().
       |[5:2..5:4): mm => *.update
       |  update => scala/collection/mutable/MapOps#update().
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[3:11..3:22): mutable.Map => *.apply[String, Int]
         |  apply => scala/collection/generic/GenMapFactory#apply().
         |  String => java/lang/String#
         |  Int => scala/Int#
         |[3:23..3:26): "a" => Predef.ArrowAssoc[String](*)
         |  Predef => scala/Predef.
         |  ArrowAssoc => scala/Predef.ArrowAssoc().
         |  String => java/lang/String#
         |[3:23..3:29): "a" -> => *[Int]
         |  Int => scala/Int#
         |[4:2..4:4): mm => *.apply
         |  apply => scala/collection/MapLike#apply().
         |[5:2..5:4): mm => *.update
         |  update => scala/collection/mutable/MapLike#update().
         |""".stripMargin,
  )

  private val updateWithTwoIndices =
    """|package xp
       |class M2 { def apply(i: Int, j: Int): Int = i; def update(i: Int, j: Int, v: Int): Unit = () }
       |object O {
       |  val m = new M2
       |  m(0, 1) += 1
       |  m(0, 1) = 1
       |}
       |""".stripMargin

  checkSection(
    "an update with two indices",
    updateWithTwoIndices,
    """|[0:8..0:10): xp <= xp/
       |[1:6..1:8): M2 <= xp/M2#
       |[1:9..1:9):  <= xp/M2#`<init>`().
       |[1:15..1:20): apply <= xp/M2#apply().
       |[1:21..1:22): i <= xp/M2#apply().(i)
       |[1:24..1:27): Int => scala/Int#
       |[1:29..1:30): j <= xp/M2#apply().(j)
       |[1:32..1:35): Int => scala/Int#
       |[1:38..1:41): Int => scala/Int#
       |[1:44..1:45): i => xp/M2#apply().(i)
       |[1:51..1:57): update <= xp/M2#update().
       |[1:58..1:59): i <= xp/M2#update().(i)
       |[1:61..1:64): Int => scala/Int#
       |[1:66..1:67): j <= xp/M2#update().(j)
       |[1:69..1:72): Int => scala/Int#
       |[1:74..1:75): v <= xp/M2#update().(v)
       |[1:77..1:80): Int => scala/Int#
       |[1:83..1:87): Unit => scala/Unit#
       |[2:7..2:8): O <= xp/O.
       |[3:6..3:7): m <= xp/O.m.
       |[3:14..3:16): M2 => xp/M2#
       |[3:16..3:16):  => xp/M2#`<init>`().
       |[4:2..4:3): m => xp/O.m.
       |[4:10..4:12): += => scala/Int#`+`(+4).
       |[5:2..5:3): m => xp/O.m.
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update with two indices: synthetics",
    updateWithTwoIndices,
    """|[4:2..4:3): m => *.apply
       |  apply => xp/M2#apply().
       |[5:2..5:3): m => *.update
       |  update => xp/M2#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateWithViewOfElement =
    """|package xq
       |object O {
       |  val strs = Array("a")
       |  strs(0) *= 2
       |}
       |""".stripMargin

  checkSection(
    "an update with an implicit view of the element",
    updateWithViewOfElement,
    """|[0:8..0:10): xq <= xq/
       |[1:7..1:8): O <= xq/O.
       |[2:6..2:10): strs <= xq/O.strs.
       |[2:13..2:18): Array => scala/Array.
       |[3:2..3:6): strs => xq/O.strs.
       |[3:10..3:12): *= => scala/collection/StringOps#`*`().
       |""".stripMargin,
    "Occurrences",
    ScalaVersion.Scala212 ->
      """|[0:8..0:10): xq <= xq/
         |[1:7..1:8): O <= xq/O.
         |[2:6..2:10): strs <= xq/O.strs.
         |[2:13..2:18): Array => scala/Array.
         |[3:2..3:6): strs => xq/O.strs.
         |[3:10..3:12): *= => scala/collection/immutable/StringLike#`*`().
         |""".stripMargin,
  )

  checkSection(
    "an update with an implicit view of the element: synthetics",
    updateWithViewOfElement,
    """|[2:13..2:18): Array => *.apply[String]
       |  apply => scala/Array.apply().
       |  String => java/lang/String#
       |[2:13..2:23): Array("a") => *((`macro-expandee` : ClassTag[String]))
       |  ClassTag => scala/reflect/ClassTag#
       |  String => java/lang/String#
       |[3:2..3:6): strs => *.apply
       |  apply => scala/Array#apply().
       |[3:2..3:14): strs(0) *= 2 => augmentString(*)
       |  augmentString => scala/Predef.augmentString().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateInFunctionBody =
    """|package xr
       |object O {
       |  val arr = Array(1)
       |  for (i <- 0 until 1) arr(i) += 1
       |  arr.indices.foreach(i => arr(i) += 1)
       |}
       |""".stripMargin

  checkSection(
    "an update with an operator in a function body",
    updateInFunctionBody,
    """|[0:8..0:10): xr <= xr/
       |[1:7..1:8): O <= xr/O.
       |[2:6..2:9): arr <= xr/O.arr.
       |[2:12..2:17): Array => scala/Array.
       |[3:7..3:8): i <= local0
       |[3:14..3:19): until => scala/runtime/RichInt#until().
       |[3:23..3:26): arr => xr/O.arr.
       |[3:27..3:28): i => local0
       |[3:30..3:32): += => scala/Int#`+`(+4).
       |[4:2..4:5): arr => xr/O.arr.
       |[4:6..4:13): indices => scala/collection/ArrayOps#indices().
       |[4:14..4:21): foreach => scala/collection/immutable/Range#foreach().
       |[4:22..4:23): i <= local1
       |[4:27..4:30): arr => xr/O.arr.
       |[4:31..4:32): i => local1
       |[4:34..4:36): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
    ScalaVersion.Scala212 ->
      """|[0:8..0:10): xr <= xr/
         |[1:7..1:8): O <= xr/O.
         |[2:6..2:9): arr <= xr/O.arr.
         |[2:12..2:17): Array => scala/Array.
         |[3:7..3:8): i <= local0
         |[3:14..3:19): until => scala/runtime/RichInt#until().
         |[3:23..3:26): arr => xr/O.arr.
         |[3:27..3:28): i => local0
         |[3:30..3:32): += => scala/Int#`+`(+4).
         |[4:2..4:5): arr => xr/O.arr.
         |[4:6..4:13): indices => scala/collection/SeqLike#indices().
         |[4:14..4:21): foreach => scala/collection/immutable/Range#foreach().
         |[4:22..4:23): i <= local1
         |[4:27..4:30): arr => xr/O.arr.
         |[4:31..4:32): i => local1
         |[4:34..4:36): += => scala/Int#`+`(+4).
         |""".stripMargin,
  )

  checkSection(
    "an update with an operator in a function body: synthetics",
    updateInFunctionBody,
    """|[2:12..2:17): Array => *.apply
       |  apply => scala/Array.apply(+5).
       |[3:2..3:34): for (i <- 0 until 1) arr(i) += 1 => orig(0 until 1).foreach[Unit]({(i) => orig(arr(i) += 1)})
       |  foreach => scala/collection/immutable/Range#foreach().
       |  Unit => scala/Unit#
       |  i => local0
       |[3:12..3:13): 0 => intWrapper(*)
       |  intWrapper => scala/LowPriorityImplicits#intWrapper().
       |[3:23..3:26): arr => *.apply
       |  apply => scala/Array#apply().
       |[4:2..4:5): arr => intArrayOps(*)
       |  intArrayOps => scala/Predef.intArrayOps().
       |[4:2..4:21): arr.indices.foreach => *[Unit]
       |  Unit => scala/Unit#
       |[4:27..4:30): arr => *.apply
       |  apply => scala/Array#apply().
       |""".stripMargin,
    "Synthetics",
  )

  private val operatorWithImplicitList =
    """|package xs
       |class X { var l = List(1) }
       |object O {
       |  val ls = Array(List(1))
       |  val x = new X
       |  ls(0) ++= List(2)
       |  ls(0) :+= 2
       |  ls(0) = ls(0) ++ List(2)
       |  x.l ++= List(2)
       |  x.l = x.l ++ List(2)
       |  def g = {
       |    var k = List(1)
       |    k ++= List(2)
       |  }
       |}
       |""".stripMargin

  checkSection(
    "an assignment operator with a polymorphic operator",
    operatorWithImplicitList,
    """|[0:8..0:10): xs <= xs/
       |[1:6..1:7): X <= xs/X#
       |[1:8..1:8):  <= xs/X#`<init>`().
       |[1:14..1:15): l <= xs/X#l().
       |[1:18..1:22): List => scala/package.List.
       |[2:7..2:8): O <= xs/O.
       |[3:6..3:8): ls <= xs/O.ls.
       |[3:11..3:16): Array => scala/Array.
       |[3:17..3:21): List => scala/package.List.
       |[4:6..4:7): x <= xs/O.x.
       |[4:14..4:15): X => xs/X#
       |[4:15..4:15):  => xs/X#`<init>`().
       |[5:2..5:4): ls => xs/O.ls.
       |[5:8..5:11): ++= => scala/collection/IterableOps#`++`().
       |[5:12..5:16): List => scala/package.List.
       |[6:2..6:4): ls => xs/O.ls.
       |[6:8..6:11): :+= => scala/collection/SeqOps#`:+`().
       |[7:2..7:4): ls => xs/O.ls.
       |[7:10..7:12): ls => xs/O.ls.
       |[7:16..7:18): ++ => scala/collection/IterableOps#`++`().
       |[7:19..7:23): List => scala/package.List.
       |[8:2..8:3): x => xs/O.x.
       |[8:4..8:5): l => xs/X#`l_=`().
       |[8:6..8:9): ++= => scala/collection/IterableOps#`++`().
       |[8:10..8:14): List => scala/package.List.
       |[9:2..9:3): x => xs/O.x.
       |[9:4..9:5): l => xs/X#`l_=`().
       |[9:8..9:9): x => xs/O.x.
       |[9:10..9:11): l => xs/X#l().
       |[9:12..9:14): ++ => scala/collection/IterableOps#`++`().
       |[9:15..9:19): List => scala/package.List.
       |[10:6..10:7): g <= xs/O.g().
       |[11:8..11:9): k <= local0
       |[11:12..11:16): List => scala/package.List.
       |[12:4..12:5): k => local0
       |[12:6..12:9): ++= => scala/collection/IterableOps#`++`().
       |[12:10..12:14): List => scala/package.List.
       |""".stripMargin,
    "Occurrences",
    ScalaVersion.Scala212 ->
      """|[0:8..0:10): xs <= xs/
         |[1:6..1:7): X <= xs/X#
         |[1:8..1:8):  <= xs/X#`<init>`().
         |[1:14..1:15): l <= xs/X#l().
         |[1:18..1:22): List => scala/collection/immutable/List.
         |[2:7..2:8): O <= xs/O.
         |[3:6..3:8): ls <= xs/O.ls.
         |[3:11..3:16): Array => scala/Array.
         |[3:17..3:21): List => scala/collection/immutable/List.
         |[4:6..4:7): x <= xs/O.x.
         |[4:14..4:15): X => xs/X#
         |[4:15..4:15):  => xs/X#`<init>`().
         |[5:2..5:4): ls => xs/O.ls.
         |[5:8..5:11): ++= => scala/collection/immutable/List#`++`().
         |[5:12..5:16): List => scala/collection/immutable/List.
         |[6:2..6:4): ls => xs/O.ls.
         |[6:8..6:11): :+= => scala/collection/SeqLike#`:+`().
         |[7:2..7:4): ls => xs/O.ls.
         |[7:10..7:12): ls => xs/O.ls.
         |[7:16..7:18): ++ => scala/collection/immutable/List#`++`().
         |[7:19..7:23): List => scala/collection/immutable/List.
         |[8:2..8:3): x => xs/O.x.
         |[8:4..8:5): l => xs/X#`l_=`().
         |[8:6..8:9): ++= => scala/collection/immutable/List#`++`().
         |[8:10..8:14): List => scala/collection/immutable/List.
         |[9:2..9:3): x => xs/O.x.
         |[9:4..9:5): l => xs/X#`l_=`().
         |[9:8..9:9): x => xs/O.x.
         |[9:10..9:11): l => xs/X#l().
         |[9:12..9:14): ++ => scala/collection/immutable/List#`++`().
         |[9:15..9:19): List => scala/collection/immutable/List.
         |[10:6..10:7): g <= xs/O.g().
         |[11:8..11:9): k <= local0
         |[11:12..11:16): List => scala/collection/immutable/List.
         |[12:4..12:5): k => local0
         |[12:6..12:9): ++= => scala/collection/immutable/List#`++`().
         |[12:10..12:14): List => scala/collection/immutable/List.
         |""".stripMargin,
  )

  checkSection(
    "an assignment operator with a polymorphic operator: synthetics",
    operatorWithImplicitList,
    """|[1:18..1:22): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |[3:11..3:16): Array => *.apply[List[Int]]
       |  apply => scala/Array.apply().
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |[3:11..3:25): Array(List(1)) => *((`macro-expandee` : ClassTag[List[Int]]))
       |  ClassTag => scala/reflect/ClassTag#
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |[3:17..3:21): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |[5:2..5:4): ls => *.apply
       |  apply => scala/Array#apply().
       |[5:2..5:11): ls(0) ++= => *[Int]
       |  Int => scala/Int#
       |[5:12..5:16): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |[6:2..6:4): ls => *.apply
       |  apply => scala/Array#apply().
       |[6:2..6:11): ls(0) :+= => *[Int]
       |  Int => scala/Int#
       |[7:2..7:4): ls => *.update
       |  update => scala/Array#update().
       |[7:10..7:12): ls => *.apply
       |  apply => scala/Array#apply().
       |[7:10..7:18): ls(0) ++ => *[Int]
       |  Int => scala/Int#
       |[7:19..7:23): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |[8:10..8:14): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |[9:8..9:14): x.l ++ => *[Int]
       |  Int => scala/Int#
       |[9:15..9:19): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |[11:12..11:16): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |[12:10..12:14): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[1:18..1:22): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[3:11..3:16): Array => *.apply[List[Int]]
         |  apply => scala/Array.apply().
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[3:11..3:25): Array(List(1)) => *((`macro-expandee` : ClassTag[List[Int]]))
         |  ClassTag => scala/reflect/ClassTag#
         |  List => scala/collection/immutable/List#
         |  Int => scala/Int#
         |[3:17..3:21): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[5:2..5:4): ls => *.apply
         |  apply => scala/Array#apply().
         |[5:2..5:11): ls(0) ++= => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[5:2..5:19): ls(0) ++= List(2) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[5:12..5:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[6:2..6:4): ls => *.apply
         |  apply => scala/Array#apply().
         |[6:2..6:11): ls(0) :+= => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[6:2..6:13): ls(0) :+= 2 => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[7:2..7:4): ls => *.update
         |  update => scala/Array#update().
         |[7:10..7:12): ls => *.apply
         |  apply => scala/Array#apply().
         |[7:10..7:18): ls(0) ++ => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[7:10..7:26): ls(0) ++ List(2) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[7:19..7:23): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[8:2..8:17): x.l ++= List(2) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[8:10..8:14): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[9:8..9:14): x.l ++ => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[9:8..9:22): x.l ++ List(2) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[9:15..9:19): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[11:12..11:16): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[12:4..12:17): k ++= List(2) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[12:10..12:14): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |""".stripMargin,
  )

  private val viewToUpdateWithPositionalIndex =
    """|package xt
       |import scala.language.implicitConversions
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |class X
       |object O {
       |  implicit def conv(x: X): M = new M
       |  val x = new X
       |  x(0) += 1
       |  x(0) = 1
       |}
       |""".stripMargin

  checkSection(
    "an update through an implicit view with a positional index",
    viewToUpdateWithPositionalIndex,
    """|[0:8..0:10): xt <= xt/
       |[1:7..1:12): scala => scala/
       |[1:13..1:21): language => scala/language.
       |[1:22..1:41): implicitConversions => scala/language.implicitConversions.
       |[2:6..2:7): M <= xt/M#
       |[2:8..2:8):  <= xt/M#`<init>`().
       |[2:14..2:19): apply <= xt/M#apply().
       |[2:20..2:21): k <= xt/M#apply().(k)
       |[2:23..2:26): Int => scala/Int#
       |[2:29..2:32): Int => scala/Int#
       |[2:35..2:36): k => xt/M#apply().(k)
       |[2:42..2:48): update <= xt/M#update().
       |[2:49..2:50): k <= xt/M#update().(k)
       |[2:52..2:55): Int => scala/Int#
       |[2:57..2:58): v <= xt/M#update().(v)
       |[2:60..2:63): Int => scala/Int#
       |[2:66..2:70): Unit => scala/Unit#
       |[3:6..3:7): X <= xt/X#
       |[3:7..3:7):  <= xt/X#`<init>`().
       |[4:7..4:8): O <= xt/O.
       |[5:15..5:19): conv <= xt/O.conv().
       |[5:20..5:21): x <= xt/O.conv().(x)
       |[5:23..5:24): X => xt/X#
       |[5:27..5:28): M => xt/M#
       |[5:35..5:36): M => xt/M#
       |[5:36..5:36):  => xt/M#`<init>`().
       |[6:6..6:7): x <= xt/O.x.
       |[6:14..6:15): X => xt/X#
       |[6:15..6:15):  => xt/X#`<init>`().
       |[7:2..7:3): x => xt/O.x.
       |[7:7..7:9): += => scala/Int#`+`(+4).
       |[8:2..8:3): x => xt/O.x.
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update through an implicit view with a positional index: synthetics",
    viewToUpdateWithPositionalIndex,
    """|[7:2..7:3): x => conv(*)
       |  conv => xt/O.conv().
       |[8:2..8:3): x => *.update
       |  update => xt/M#update().
       |[8:2..8:3): x => conv(*)
       |  conv => xt/O.conv().
       |""".stripMargin,
    "Synthetics",
  )

  private val assignmentToUpdateWithImplicitList =
    """|package xu
       |class I
       |class M {
       |  def apply(k: Int)(implicit i: I): Int = k
       |  def update(k: Int, v: Int)(implicit i: I): Unit = ()
       |}
       |object O {
       |  implicit val i: I = new I
       |  val m = new M
       |  m(0) = 5
       |  m(k = 0) = 5
       |  m(0) += 5
       |}
       |""".stripMargin

  checkSection(
    "an assignment to an update with an implicit list",
    assignmentToUpdateWithImplicitList,
    """|[0:8..0:10): xu <= xu/
       |[1:6..1:7): I <= xu/I#
       |[1:7..1:7):  <= xu/I#`<init>`().
       |[2:6..2:7): M <= xu/M#
       |[2:8..2:8):  <= xu/M#`<init>`().
       |[3:6..3:11): apply <= xu/M#apply().
       |[3:12..3:13): k <= xu/M#apply().(k)
       |[3:15..3:18): Int => scala/Int#
       |[3:29..3:30): i <= xu/M#apply().(i)
       |[3:32..3:33): I => xu/I#
       |[3:36..3:39): Int => scala/Int#
       |[3:42..3:43): k => xu/M#apply().(k)
       |[4:6..4:12): update <= xu/M#update().
       |[4:13..4:14): k <= xu/M#update().(k)
       |[4:16..4:19): Int => scala/Int#
       |[4:21..4:22): v <= xu/M#update().(v)
       |[4:24..4:27): Int => scala/Int#
       |[4:38..4:39): i <= xu/M#update().(i)
       |[4:41..4:42): I => xu/I#
       |[4:45..4:49): Unit => scala/Unit#
       |[6:7..6:8): O <= xu/O.
       |[7:15..7:16): i <= xu/O.i.
       |[7:18..7:19): I => xu/I#
       |[7:26..7:27): I => xu/I#
       |[7:27..7:27):  => xu/I#`<init>`().
       |[8:6..8:7): m <= xu/O.m.
       |[8:14..8:15): M => xu/M#
       |[8:15..8:15):  => xu/M#`<init>`().
       |[9:2..9:3): m => xu/O.m.
       |[10:2..10:3): m => xu/O.m.
       |[10:4..10:5): k => xu/M#update().(k)
       |[11:2..11:3): m => xu/O.m.
       |[11:7..11:9): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an assignment to an update with an implicit list: synthetics",
    assignmentToUpdateWithImplicitList,
    """|[9:2..9:3): m => *.update
       |  update => xu/M#update().
       |[9:2..9:10): m(0) = 5 => *(i)
       |  i => xu/O.i.
       |[10:2..10:3): m => *.update
       |  update => xu/M#update().
       |[10:2..10:14): m(k = 0) = 5 => *(i)
       |  i => xu/O.i.
       |[11:2..11:3): m => *.apply
       |  apply => xu/M#apply().
       |[11:2..11:11): m(0) += 5 => *(i)
       |  i => xu/O.i.
       |""".stripMargin,
    "Synthetics",
  )

  private val updateWithElementOperatorAsValue =
    """|package xv
       |class B { def +=(x: Int): B = this }
       |class M { def apply(k: Int): B = new B; def update(k: Int, v: B): Unit = () }
       |object O {
       |  val m = new M
       |  val n = new M
       |  m(0) = m(0) += 1
       |  m(0) = n(0) += 1
       |  m.update(0, n(0) += 1)
       |  m(0) = n(0)
       |}
       |""".stripMargin

  checkSection(
    "an update whose value is an operator of the element",
    updateWithElementOperatorAsValue,
    """|[0:8..0:10): xv <= xv/
       |[1:6..1:7): B <= xv/B#
       |[1:8..1:8):  <= xv/B#`<init>`().
       |[1:14..1:16): += <= xv/B#`+=`().
       |[1:17..1:18): x <= xv/B#`+=`().(x)
       |[1:20..1:23): Int => scala/Int#
       |[1:26..1:27): B => xv/B#
       |[2:6..2:7): M <= xv/M#
       |[2:8..2:8):  <= xv/M#`<init>`().
       |[2:14..2:19): apply <= xv/M#apply().
       |[2:20..2:21): k <= xv/M#apply().(k)
       |[2:23..2:26): Int => scala/Int#
       |[2:29..2:30): B => xv/B#
       |[2:37..2:38): B => xv/B#
       |[2:38..2:38):  => xv/B#`<init>`().
       |[2:44..2:50): update <= xv/M#update().
       |[2:51..2:52): k <= xv/M#update().(k)
       |[2:54..2:57): Int => scala/Int#
       |[2:59..2:60): v <= xv/M#update().(v)
       |[2:62..2:63): B => xv/B#
       |[2:66..2:70): Unit => scala/Unit#
       |[3:7..3:8): O <= xv/O.
       |[4:6..4:7): m <= xv/O.m.
       |[4:14..4:15): M => xv/M#
       |[4:15..4:15):  => xv/M#`<init>`().
       |[5:6..5:7): n <= xv/O.n.
       |[5:14..5:15): M => xv/M#
       |[5:15..5:15):  => xv/M#`<init>`().
       |[6:2..6:3): m => xv/O.m.
       |[6:9..6:10): m => xv/O.m.
       |[6:14..6:16): += => xv/B#`+=`().
       |[7:2..7:3): m => xv/O.m.
       |[7:9..7:10): n => xv/O.n.
       |[7:14..7:16): += => xv/B#`+=`().
       |[8:2..8:3): m => xv/O.m.
       |[8:4..8:10): update => xv/M#update().
       |[8:14..8:15): n => xv/O.n.
       |[8:19..8:21): += => xv/B#`+=`().
       |[9:2..9:3): m => xv/O.m.
       |[9:9..9:10): n => xv/O.n.
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update whose value is an operator of the element: synthetics",
    updateWithElementOperatorAsValue,
    """|[6:2..6:3): m => *.update
       |  update => xv/M#update().
       |[6:9..6:10): m => *.apply
       |  apply => xv/M#apply().
       |[7:2..7:3): m => *.update
       |  update => xv/M#update().
       |[7:9..7:10): n => *.apply
       |  apply => xv/M#apply().
       |[8:14..8:15): n => *.apply
       |  apply => xv/M#apply().
       |[9:2..9:3): m => *.update
       |  update => xv/M#update().
       |[9:9..9:10): n => *.apply
       |  apply => xv/M#apply().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateOfReceiverWithTypeArguments =
    """|package xw
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |class G { def apply[T](k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  val x: Any = new M
       |  def g[A]: G = new G
       |  x.asInstanceOf[M](0) += 1
       |  x.asInstanceOf[M](0) = 1
       |  g[Int](0) += 1
       |  g[Int](0) = 1
       |}
       |""".stripMargin

  checkSection(
    "an update of a receiver with type arguments",
    updateOfReceiverWithTypeArguments,
    """|[0:8..0:10): xw <= xw/
       |[1:6..1:7): M <= xw/M#
       |[1:8..1:8):  <= xw/M#`<init>`().
       |[1:14..1:19): apply <= xw/M#apply().
       |[1:20..1:21): k <= xw/M#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => xw/M#apply().(k)
       |[1:42..1:48): update <= xw/M#update().
       |[1:49..1:50): k <= xw/M#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= xw/M#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:6..2:7): G <= xw/G#
       |[2:8..2:8):  <= xw/G#`<init>`().
       |[2:14..2:19): apply <= xw/G#apply().
       |[2:20..2:21): T <= xw/G#apply().[T]
       |[2:23..2:24): k <= xw/G#apply().(k)
       |[2:26..2:29): Int => scala/Int#
       |[2:32..2:35): Int => scala/Int#
       |[2:38..2:39): k => xw/G#apply().(k)
       |[2:45..2:51): update <= xw/G#update().
       |[2:52..2:53): k <= xw/G#update().(k)
       |[2:55..2:58): Int => scala/Int#
       |[2:60..2:61): v <= xw/G#update().(v)
       |[2:63..2:66): Int => scala/Int#
       |[2:69..2:73): Unit => scala/Unit#
       |[3:7..3:8): O <= xw/O.
       |[4:6..4:7): x <= xw/O.x.
       |[4:9..4:12): Any => scala/Any#
       |[4:19..4:20): M => xw/M#
       |[4:20..4:20):  => xw/M#`<init>`().
       |[5:6..5:7): g <= xw/O.g().
       |[5:8..5:9): A <= xw/O.g().[A]
       |[5:12..5:13): G => xw/G#
       |[5:20..5:21): G => xw/G#
       |[5:21..5:21):  => xw/G#`<init>`().
       |[6:2..6:3): x => xw/O.x.
       |[6:4..6:16): asInstanceOf => scala/Any#asInstanceOf().
       |[6:17..6:18): M => xw/M#
       |[6:23..6:25): += => scala/Int#`+`(+4).
       |[7:2..7:3): x => xw/O.x.
       |[7:4..7:16): asInstanceOf => scala/Any#asInstanceOf().
       |[7:17..7:18): M => xw/M#
       |[8:2..8:3): g => xw/O.g().
       |[8:4..8:7): Int => scala/Int#
       |[8:12..8:14): += => scala/Int#`+`(+4).
       |[9:2..9:3): g => xw/O.g().
       |[9:4..9:7): Int => scala/Int#
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update of a receiver with type arguments: synthetics",
    updateOfReceiverWithTypeArguments,
    """|[7:2..7:19): x.asInstanceOf[M] => *.update
       |  update => xw/M#update().
       |[8:2..8:8): g[Int] => *[Nothing]
       |  Nothing => scala/Nothing#
       |[9:2..9:8): g[Int] => *.update
       |  update => xw/G#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateWithOperatorThroughViewWithImplicitList =
    """|package xx
       |import scala.collection.mutable
       |import scala.math.Numeric.Implicits._
       |object O {
       |  def f[T: Numeric](arr: Array[T], m: mutable.Map[String, T]): Unit = {
       |    arr(0) += arr(1)
       |    m("a") += m("b")
       |    m("a") = m("a") + m("b")
       |  }
       |}
       |""".stripMargin

  checkSection(
    "an update with an operator through a view with an implicit list: synthetics",
    updateWithOperatorThroughViewWithImplicitList,
    """|[5:4..5:7): arr => *.apply
       |  apply => scala/Array#apply().
       |[5:4..5:20): arr(0) += arr(1) => Implicits.infixNumericOps[T](*)(evidence$1)
       |  Implicits => scala/math/Numeric.Implicits.
       |  infixNumericOps => scala/math/Numeric.ExtraImplicits#infixNumericOps().
       |  T => xx/O.f().[T]
       |  evidence$1 => xx/O.f().(evidence$1)
       |[5:14..5:17): arr => *.apply
       |  apply => scala/Array#apply().
       |[6:4..6:5): m => *.apply
       |  apply => scala/collection/MapOps#apply().
       |[6:4..6:20): m("a") += m("b") => Implicits.infixNumericOps[T](*)(evidence$1)
       |  Implicits => scala/math/Numeric.Implicits.
       |  infixNumericOps => scala/math/Numeric.ExtraImplicits#infixNumericOps().
       |  T => xx/O.f().[T]
       |  evidence$1 => xx/O.f().(evidence$1)
       |[6:14..6:15): m => *.apply
       |  apply => scala/collection/MapOps#apply().
       |[7:4..7:5): m => *.update
       |  update => scala/collection/mutable/MapOps#update().
       |[7:13..7:14): m => *.apply
       |  apply => scala/collection/MapOps#apply().
       |[7:13..7:19): m("a") => Implicits.infixNumericOps[T](*)(evidence$1)
       |  Implicits => scala/math/Numeric.Implicits.
       |  infixNumericOps => scala/math/Numeric.ExtraImplicits#infixNumericOps().
       |  T => xx/O.f().[T]
       |  evidence$1 => xx/O.f().(evidence$1)
       |[7:22..7:23): m => *.apply
       |  apply => scala/collection/MapOps#apply().
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[5:4..5:7): arr => *.apply
         |  apply => scala/Array#apply().
         |[5:4..5:20): arr(0) += arr(1) => Implicits.infixNumericOps[T](*)(evidence$1)
         |  Implicits => scala/math/Numeric.Implicits.
         |  infixNumericOps => scala/math/Numeric.ExtraImplicits#infixNumericOps().
         |  T => xx/O.f().[T]
         |  evidence$1 => xx/O.f().(evidence$1)
         |[5:14..5:17): arr => *.apply
         |  apply => scala/Array#apply().
         |[6:4..6:5): m => *.apply
         |  apply => scala/collection/MapLike#apply().
         |[6:4..6:20): m("a") += m("b") => Implicits.infixNumericOps[T](*)(evidence$1)
         |  Implicits => scala/math/Numeric.Implicits.
         |  infixNumericOps => scala/math/Numeric.ExtraImplicits#infixNumericOps().
         |  T => xx/O.f().[T]
         |  evidence$1 => xx/O.f().(evidence$1)
         |[6:14..6:15): m => *.apply
         |  apply => scala/collection/MapLike#apply().
         |[7:4..7:5): m => *.update
         |  update => scala/collection/mutable/MapLike#update().
         |[7:13..7:14): m => *.apply
         |  apply => scala/collection/MapLike#apply().
         |[7:13..7:19): m("a") => Implicits.infixNumericOps[T](*)(evidence$1)
         |  Implicits => scala/math/Numeric.Implicits.
         |  infixNumericOps => scala/math/Numeric.ExtraImplicits#infixNumericOps().
         |  T => xx/O.f().[T]
         |  evidence$1 => xx/O.f().(evidence$1)
         |[7:22..7:23): m => *.apply
         |  apply => scala/collection/MapLike#apply().
         |""".stripMargin,
  )

  private val updateWithViewOnOperatorResult =
    """|package xy
       |import scala.collection.mutable
       |object O {
       |  val m = mutable.Map[String, java.lang.Integer]()
       |  m("a") += 1
       |  m("a") = m("a") + 1
       |}
       |""".stripMargin

  checkSection(
    "an update with a view on the operator result: synthetics",
    updateWithViewOnOperatorResult,
    """|[3:10..3:21): mutable.Map => *.apply
       |  apply => scala/collection/MapFactory.Delegate#apply().
       |[4:2..4:3): m => *.apply
       |  apply => scala/collection/MapOps#apply().
       |[4:2..4:13): m("a") += 1 => int2Integer(*)
       |  int2Integer => scala/Predef.int2Integer().
       |[4:2..4:13): m("a") += 1 => Integer2int(*)
       |  Integer2int => scala/Predef.Integer2int().
       |[5:2..5:3): m => *.update
       |  update => scala/collection/mutable/MapOps#update().
       |[5:11..5:12): m => *.apply
       |  apply => scala/collection/MapOps#apply().
       |[5:11..5:17): m("a") => Integer2int(*)
       |  Integer2int => scala/Predef.Integer2int().
       |[5:11..5:21): m("a") + 1 => int2Integer(*)
       |  int2Integer => scala/Predef.int2Integer().
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[3:10..3:21): mutable.Map => *.apply
         |  apply => scala/collection/generic/GenMapFactory#apply().
         |[4:2..4:3): m => *.apply
         |  apply => scala/collection/MapLike#apply().
         |[4:2..4:13): m("a") += 1 => int2Integer(*)
         |  int2Integer => scala/Predef.int2Integer().
         |[4:2..4:13): m("a") += 1 => Integer2int(*)
         |  Integer2int => scala/Predef.Integer2int().
         |[5:2..5:3): m => *.update
         |  update => scala/collection/mutable/MapLike#update().
         |[5:11..5:12): m => *.apply
         |  apply => scala/collection/MapLike#apply().
         |[5:11..5:17): m("a") => Integer2int(*)
         |  Integer2int => scala/Predef.Integer2int().
         |[5:11..5:21): m("a") + 1 => int2Integer(*)
         |  int2Integer => scala/Predef.int2Integer().
         |""".stripMargin,
  )

  private val updateOnceWithViewOnOperatorResult =
    """|package xz
       |import scala.collection.mutable
       |object O {
       |  val m = mutable.Map[String, java.lang.Integer]()
       |  var k = "a"
       |  def mk() = m
       |  m(k) += 1
       |  mk()("a") += 1
       |  m(k) = m(k) + 1
       |}
       |""".stripMargin

  checkSection(
    "an update evaluated once with a view on the operator result: synthetics",
    updateOnceWithViewOnOperatorResult,
    """|[3:10..3:21): mutable.Map => *.apply
       |  apply => scala/collection/MapFactory.Delegate#apply().
       |[6:2..6:3): m => *.apply
       |  apply => scala/collection/MapOps#apply().
       |[6:2..6:11): m(k) += 1 => int2Integer(*)
       |  int2Integer => scala/Predef.int2Integer().
       |[6:2..6:11): m(k) += 1 => Integer2int(*)
       |  Integer2int => scala/Predef.Integer2int().
       |[7:2..7:16): mk()("a") += 1 => int2Integer(*)
       |  int2Integer => scala/Predef.int2Integer().
       |[7:2..7:16): mk()("a") += 1 => Integer2int(*)
       |  Integer2int => scala/Predef.Integer2int().
       |[8:2..8:3): m => *.update
       |  update => scala/collection/mutable/MapOps#update().
       |[8:9..8:10): m => *.apply
       |  apply => scala/collection/MapOps#apply().
       |[8:9..8:13): m(k) => Integer2int(*)
       |  Integer2int => scala/Predef.Integer2int().
       |[8:9..8:17): m(k) + 1 => int2Integer(*)
       |  int2Integer => scala/Predef.int2Integer().
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[3:10..3:21): mutable.Map => *.apply
         |  apply => scala/collection/generic/GenMapFactory#apply().
         |[6:2..6:3): m => *.apply
         |  apply => scala/collection/MapLike#apply().
         |[6:2..6:11): m(k) += 1 => int2Integer(*)
         |  int2Integer => scala/Predef.int2Integer().
         |[6:2..6:11): m(k) += 1 => Integer2int(*)
         |  Integer2int => scala/Predef.Integer2int().
         |[7:2..7:16): mk()("a") += 1 => int2Integer(*)
         |  int2Integer => scala/Predef.int2Integer().
         |[7:2..7:16): mk()("a") += 1 => Integer2int(*)
         |  Integer2int => scala/Predef.Integer2int().
         |[8:2..8:3): m => *.update
         |  update => scala/collection/mutable/MapLike#update().
         |[8:9..8:10): m => *.apply
         |  apply => scala/collection/MapLike#apply().
         |[8:9..8:13): m(k) => Integer2int(*)
         |  Integer2int => scala/Predef.Integer2int().
         |[8:9..8:17): m(k) + 1 => int2Integer(*)
         |  int2Integer => scala/Predef.int2Integer().
         |""".stripMargin,
  )

  private val updateWithViewsOfReceiverAndElement =
    """|package ya
       |import scala.language.implicitConversions
       |class M { def apply(k: Int): String = ""; def update(k: Int, v: String): Unit = () }
       |class X
       |class B
       |class N { def apply(k: Int): B = new B; def update(k: Int, v: B): Unit = () }
       |object O {
       |  implicit def conv(x: X): M = new M
       |  implicit class RichB(b: B) { def +(i: Int): B = b }
       |  val x = new X
       |  val n = new N
       |  x(0) *= 2
       |  n(0) += 1
       |  x(0) = x(0) * 2
       |}
       |""".stripMargin

  checkSection(
    "an update with views of the receiver and of the element: synthetics",
    updateWithViewsOfReceiverAndElement,
    """|[11:2..11:3): x => conv(*)
       |  conv => ya/O.conv().
       |[11:2..11:11): x(0) *= 2 => augmentString(*)
       |  augmentString => scala/Predef.augmentString().
       |[12:2..12:3): n => *.apply
       |  apply => ya/N#apply().
       |[12:2..12:11): n(0) += 1 => RichB(*)
       |  RichB => ya/O.RichB().
       |[13:2..13:3): x => *.update
       |  update => ya/M#update().
       |[13:2..13:3): x => conv(*)
       |  conv => ya/O.conv().
       |[13:9..13:10): x => *.apply
       |  apply => ya/M#apply().
       |[13:9..13:10): x => conv(*)
       |  conv => ya/O.conv().
       |[13:9..13:13): x(0) => augmentString(*)
       |  augmentString => scala/Predef.augmentString().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateOfParenthesizedReceiver =
    """|package yb
       |class N { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |class M { def apply(k: Int): N = new N }
       |class K(x: Int) { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |class P { def ++(q: P): N = new N }
       |object O {
       |  val m = new M
       |  val p = new P
       |  (m apply 0)(1) += 5
       |  (m apply 0)(1) = 5
       |  (new K(1))(0) += 5
       |  (p ++ p)(0) += 1
       |  (p ++ p)(0) = 1
       |}
       |""".stripMargin

  checkSection(
    "an update of a parenthesized receiver",
    updateOfParenthesizedReceiver,
    """|[0:8..0:10): yb <= yb/
       |[1:6..1:7): N <= yb/N#
       |[1:8..1:8):  <= yb/N#`<init>`().
       |[1:14..1:19): apply <= yb/N#apply().
       |[1:20..1:21): k <= yb/N#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => yb/N#apply().(k)
       |[1:42..1:48): update <= yb/N#update().
       |[1:49..1:50): k <= yb/N#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= yb/N#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:6..2:7): M <= yb/M#
       |[2:8..2:8):  <= yb/M#`<init>`().
       |[2:14..2:19): apply <= yb/M#apply().
       |[2:20..2:21): k <= yb/M#apply().(k)
       |[2:23..2:26): Int => scala/Int#
       |[2:29..2:30): N => yb/N#
       |[2:37..2:38): N => yb/N#
       |[2:38..2:38):  => yb/N#`<init>`().
       |[3:6..3:7): K <= yb/K#
       |[3:7..3:7):  <= yb/K#`<init>`().
       |[3:8..3:9): x <= yb/K#`<init>`().(x)
       |[3:8..3:9): x <= yb/K#x.
       |[3:11..3:14): Int => scala/Int#
       |[3:22..3:27): apply <= yb/K#apply().
       |[3:28..3:29): k <= yb/K#apply().(k)
       |[3:31..3:34): Int => scala/Int#
       |[3:37..3:40): Int => scala/Int#
       |[3:43..3:44): k => yb/K#apply().(k)
       |[3:50..3:56): update <= yb/K#update().
       |[3:57..3:58): k <= yb/K#update().(k)
       |[3:60..3:63): Int => scala/Int#
       |[3:65..3:66): v <= yb/K#update().(v)
       |[3:68..3:71): Int => scala/Int#
       |[3:74..3:78): Unit => scala/Unit#
       |[4:6..4:7): P <= yb/P#
       |[4:8..4:8):  <= yb/P#`<init>`().
       |[4:14..4:16): ++ <= yb/P#`++`().
       |[4:17..4:18): q <= yb/P#`++`().(q)
       |[4:20..4:21): P => yb/P#
       |[4:24..4:25): N => yb/N#
       |[4:32..4:33): N => yb/N#
       |[4:33..4:33):  => yb/N#`<init>`().
       |[5:7..5:8): O <= yb/O.
       |[6:6..6:7): m <= yb/O.m.
       |[6:14..6:15): M => yb/M#
       |[6:15..6:15):  => yb/M#`<init>`().
       |[7:6..7:7): p <= yb/O.p.
       |[7:14..7:15): P => yb/P#
       |[7:15..7:15):  => yb/P#`<init>`().
       |[8:3..8:4): m => yb/O.m.
       |[8:5..8:10): apply => yb/M#apply().
       |[8:17..8:19): += => scala/Int#`+`(+4).
       |[9:3..9:4): m => yb/O.m.
       |[9:5..9:10): apply => yb/M#apply().
       |[10:7..10:8): K => yb/K#
       |[10:8..10:8):  => yb/K#`<init>`().
       |[10:16..10:18): += => scala/Int#`+`(+4).
       |[11:3..11:4): p => yb/O.p.
       |[11:5..11:7): ++ => yb/P#`++`().
       |[11:8..11:9): p => yb/O.p.
       |[11:14..11:16): += => scala/Int#`+`(+4).
       |[12:3..12:4): p => yb/O.p.
       |[12:5..12:7): ++ => yb/P#`++`().
       |[12:8..12:9): p => yb/O.p.
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update of a parenthesized receiver: synthetics",
    updateOfParenthesizedReceiver,
    """|[9:3..9:12): m apply 0 => *.update
       |  update => yb/N#update().
       |[12:3..12:9): p ++ p => *.update
       |  update => yb/N#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateOfParameterlessApply =
    """|package yc
       |class N { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object R { def apply: N = new N }
       |object O {
       |  R.apply(0) += 5
       |  R.apply(0) = 5
       |  R apply 0 += 5
       |}
       |""".stripMargin

  checkSection(
    "an update of a parameterless apply",
    updateOfParameterlessApply,
    """|[0:8..0:10): yc <= yc/
       |[1:6..1:7): N <= yc/N#
       |[1:8..1:8):  <= yc/N#`<init>`().
       |[1:14..1:19): apply <= yc/N#apply().
       |[1:20..1:21): k <= yc/N#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => yc/N#apply().(k)
       |[1:42..1:48): update <= yc/N#update().
       |[1:49..1:50): k <= yc/N#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= yc/N#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:7..2:8): R <= yc/R.
       |[2:15..2:20): apply <= yc/R.apply().
       |[2:22..2:23): N => yc/N#
       |[2:30..2:31): N => yc/N#
       |[2:31..2:31):  => yc/N#`<init>`().
       |[3:7..3:8): O <= yc/O.
       |[4:2..4:3): R => yc/R.
       |[4:4..4:9): apply => yc/R.apply().
       |[4:13..4:15): += => scala/Int#`+`(+4).
       |[5:2..5:3): R => yc/R.
       |[5:4..5:9): apply => yc/R.apply().
       |[6:2..6:3): R => yc/R.
       |[6:4..6:9): apply => yc/R.apply().
       |[6:12..6:14): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update of a parameterless apply: synthetics",
    updateOfParameterlessApply,
    """|[5:2..5:9): R.apply => *.update
       |  update => yc/N#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateOfImplicitThis =
    """|package yd
       |class N {
       |  def apply(k: Int): Int = k
       |  def update(k: Int, v: Int): Unit = ()
       |  def f(): Unit = apply(0) += 5
       |}
       |""".stripMargin

  checkSection(
    "an update of an implicit this",
    updateOfImplicitThis,
    """|[0:8..0:10): yd <= yd/
       |[1:6..1:7): N <= yd/N#
       |[1:8..1:8):  <= yd/N#`<init>`().
       |[2:6..2:11): apply <= yd/N#apply().
       |[2:12..2:13): k <= yd/N#apply().(k)
       |[2:15..2:18): Int => scala/Int#
       |[2:21..2:24): Int => scala/Int#
       |[2:27..2:28): k => yd/N#apply().(k)
       |[3:6..3:12): update <= yd/N#update().
       |[3:13..3:14): k <= yd/N#update().(k)
       |[3:16..3:19): Int => scala/Int#
       |[3:21..3:22): v <= yd/N#update().(v)
       |[3:24..3:27): Int => scala/Int#
       |[3:30..3:34): Unit => scala/Unit#
       |[4:6..4:7): f <= yd/N#f().
       |[4:11..4:15): Unit => scala/Unit#
       |[4:27..4:29): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update of an implicit this: synthetics",
    updateOfImplicitThis,
    """|[4:18..4:18):  => *.update
       |  update => yd/N#update().
       |""".stripMargin,
    "Synthetics",
  )

  private val updateOfParameterlessInfixCall =
    """|package ye
       |class N { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |class P { def ++ : N = new N }
       |object O {
       |  val p = new P
       |  p ++ 0 += 5
       |}
       |""".stripMargin

  checkSection(
    "an update of a parameterless infix call",
    updateOfParameterlessInfixCall,
    """|[0:8..0:10): ye <= ye/
       |[1:6..1:7): N <= ye/N#
       |[1:8..1:8):  <= ye/N#`<init>`().
       |[1:14..1:19): apply <= ye/N#apply().
       |[1:20..1:21): k <= ye/N#apply().(k)
       |[1:23..1:26): Int => scala/Int#
       |[1:29..1:32): Int => scala/Int#
       |[1:35..1:36): k => ye/N#apply().(k)
       |[1:42..1:48): update <= ye/N#update().
       |[1:49..1:50): k <= ye/N#update().(k)
       |[1:52..1:55): Int => scala/Int#
       |[1:57..1:58): v <= ye/N#update().(v)
       |[1:60..1:63): Int => scala/Int#
       |[1:66..1:70): Unit => scala/Unit#
       |[2:6..2:7): P <= ye/P#
       |[2:8..2:8):  <= ye/P#`<init>`().
       |[2:14..2:16): ++ <= ye/P#`++`().
       |[2:19..2:20): N => ye/N#
       |[2:27..2:28): N => ye/N#
       |[2:28..2:28):  => ye/N#`<init>`().
       |[3:7..3:8): O <= ye/O.
       |[4:6..4:7): p <= ye/O.p.
       |[4:14..4:15): P => ye/P#
       |[4:15..4:15):  => ye/P#`<init>`().
       |[5:2..5:3): p => ye/O.p.
       |[5:4..5:6): ++ => ye/P#`++`().
       |[5:9..5:11): += => scala/Int#`+`(+4).
       |""".stripMargin,
    "Occurrences",
  )

  checkSection(
    "an update of a parameterless infix call: synthetics",
    updateOfParameterlessInfixCall,
    "",
    "Synthetics",
  )

  private val methodValuesOfWrittenApplyAndUpdate =
    """|package yz
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  val s = new M
       |  var m = new M
       |  def mk(): M = new M
       |  val ks = List(1, 2)
       |  ks.map(s.apply)
       |  ks.map(m.apply)
       |  ks.map(mk().apply)
       |  ks.map(m.apply _)
       |  val u: (Int, Int) => Unit = m.update
       |  val w = mk().update _
       |  mk().apply(k = 0)
       |  mk()(k = 0)
       |  mk().update(k = 0, v = 1)
       |}
       |""".stripMargin

  checkSection(
    "method values of a written apply and update: synthetics",
    methodValuesOfWrittenApplyAndUpdate,
    """|[6:11..6:15): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |[7:2..7:8): ks.map => *[Int]
       |  Int => scala/Int#
       |[8:2..8:8): ks.map => *[Int]
       |  Int => scala/Int#
       |[9:2..9:8): ks.map => *[Int]
       |  Int => scala/Int#
       |[10:2..10:8): ks.map => *[Int]
       |  Int => scala/Int#
       |[14:2..14:6): mk() => *.apply
       |  apply => yz/M#apply().
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[6:11..6:15): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[7:2..7:8): ks.map => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[7:2..7:17): ks.map(s.apply) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[8:2..8:8): ks.map => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[8:2..8:17): ks.map(m.apply) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[9:2..9:8): ks.map => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[9:2..9:20): ks.map(mk().apply) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[10:2..10:8): ks.map => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[10:2..10:19): ks.map(m.apply _) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[14:2..14:6): mk() => *.apply
         |  apply => yz/M#apply().
         |""".stripMargin,
  )

  private val methodValuesOnViewByNameAndFor =
    """|package za
       |class M { def apply(k: Int): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  implicit def toM(x: Int): M = new M
       |  var m = new M
       |  var n = 1
       |  val ks = List(1, 2)
       |  def f(x: => M) = ks.map(x.apply)
       |  ks.map(n.apply)
       |  val u: (Int, Int) => Unit = n.update
       |  for (k <- ks; g = m.update _) g(k, 1)
       |}
       |""".stripMargin

  checkSection(
    "method values of a written apply and update on a view, a by-name and in a for: synthetics",
    methodValuesOnViewByNameAndFor,
    """|[6:11..6:15): List => *.apply[Int]
       |  apply => scala/collection/IterableFactory#apply().
       |  Int => scala/Int#
       |[7:19..7:25): ks.map => *[Int]
       |  Int => scala/Int#
       |[8:2..8:8): ks.map => *[Int]
       |  Int => scala/Int#
       |[8:9..8:10): n => toM(*)
       |  toM => za/O.toM().
       |[9:30..9:31): n => toM(*)
       |  toM => za/O.toM().
       |[10:2..10:39): for (k <- ks; g = m.update _) g(k, 1) => orig(ks).map[Tuple2[Int, Function2[Int, Int, Unit]]]({(k) => orig(g = m.update _)}).foreach[Unit]({(x$1) => orig(g(k, 1))})
       |  map => scala/collection/immutable/List#map().
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |  Function2 => scala/Function2#
       |  Unit => scala/Unit#
       |  k => local0
       |  foreach => scala/collection/immutable/List#foreach().
       |  x$1 => local1
       |[10:32..10:33): g => *.apply
       |  apply => scala/Function2#apply().
       |""".stripMargin,
    "Synthetics",
    ScalaVersion.Scala212 ->
      """|[6:11..6:15): List => *.apply[Int]
         |  apply => scala/collection/immutable/List.apply().
         |  Int => scala/Int#
         |[7:19..7:25): ks.map => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[7:19..7:34): ks.map(x.apply) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[8:2..8:8): ks.map => *[Int, List[Int]]
         |  Int => scala/Int#
         |  List => scala/collection/immutable/List#
         |[8:2..8:17): ks.map(n.apply) => *(List.canBuildFrom[Int])
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  Int => scala/Int#
         |[8:9..8:10): n => toM(*)
         |  toM => za/O.toM().
         |[9:30..9:31): n => toM(*)
         |  toM => za/O.toM().
         |[10:2..10:39): for (k <- ks; g = m.update _) g(k, 1) => orig(ks).map[Tuple2[Int, Function2[Int, Int, Unit]], List[Tuple2[Int, Function2[Int, Int, Unit]]]]({(k) => orig(g = m.update _)})(List.canBuildFrom[Tuple2[Int, Function2[Int, Int, Unit]]]).foreach[Unit]({(x$1) => orig(g(k, 1))})
         |  map => scala/collection/immutable/List#map().
         |  Tuple2 => scala/Tuple2#
         |  Int => scala/Int#
         |  Function2 => scala/Function2#
         |  Unit => scala/Unit#
         |  List => scala/collection/immutable/List#
         |  k => local0
         |  List => scala/collection/immutable/List.
         |  canBuildFrom => scala/collection/immutable/List.canBuildFrom().
         |  foreach => scala/collection/immutable/List#foreach().
         |  x$1 => local1
         |[10:32..10:33): g => *.apply
         |  apply => scala/Function2#apply().
         |""".stripMargin,
  )

  private val inferredApplyWithDefaultsOnVar =
    """|package zb
       |class M { def apply(k: Int = 0): Int = k; def update(k: Int, v: Int): Unit = () }
       |object O {
       |  var m = new M
       |  def mk(): M = new M
       |  def g(f: Int => Unit): Unit = ()
       |  m()
       |  m(k = 1)
       |  mk()()
       |  g(m(0) += _)
       |}
       |""".stripMargin

  checkSection(
    "an inferred apply with defaults or named arguments, and an op= in a lambda: synthetics",
    inferredApplyWithDefaultsOnVar,
    """|[7:2..7:3): m => *.apply
       |  apply => zb/M#apply().
       |""".stripMargin,
    "Synthetics",
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

    val expected212 = expected
      .replace("Occurrences => 15 entries", "Occurrences => 15 entries\nDiagnostics => 1 entries") +
      """|
         |Diagnostics:
         |[0:1..0:11) [warning] Usage of named or default arguments transformed this annotation
         |constructor call into a block. The corresponding AnnotationInfo
         |will contain references to local values and default getters instead
         |of the actual argument trees
         |""".stripMargin

    val expectedCompat = ScalaVersion.getExpected(
      compat = List(ScalaVersion.Scala212 -> expected212, ScalaVersion.Scala211 -> expected212),
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
