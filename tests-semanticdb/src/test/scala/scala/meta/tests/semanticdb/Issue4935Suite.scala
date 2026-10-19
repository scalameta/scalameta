package scala.meta.tests.semanticdb

class Issue4935Suite extends SemanticdbSuite {

  symbols(
    """|object a {
       |  def m(): Unit = {
       |    val x = 1
       |    var y = 1
       |    implicit val i: Int = 1
       |    def d = 1
       |    def e() = 1
       |    println((x, y, i, d, e()))
       |  }
       |}
       |""".stripMargin,
    """|_empty_/a. => final object a extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/a.m(). => method m(): Unit
       |  Unit => scala/Unit#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => var local y: Int
       |  Int => scala/Int#
       |local2 => implicit val local i: Int
       |  Int => scala/Int#
       |local3 => method d: Int
       |  Int => scala/Int#
       |local4 => method e(): Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    """|object b {
       |  def m(): Int = {
       |    lazy val z = 1
       |    z
       |  }
       |}
       |""".stripMargin,
    """|_empty_/b. => final object b extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/b.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => lazy val local z: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    """|object c {
       |  def m(): Unit = {
       |    val (x, y) = (1, 2)
       |    var (v, w) = (1, 2)
       |    println((x, y, v, w))
       |  }
       |}
       |""".stripMargin,
    """|_empty_/c. => final object c extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/c.m(). => method m(): Unit
       |  Unit => scala/Unit#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => val local y: Int
       |  Int => scala/Int#
       |local2 => synthetic val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local3 => var local v: Int
       |  Int => scala/Int#
       |local4 => var local w: Int
       |  Int => scala/Int#
       |local5 => synthetic val local x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    """|object d {
       |  def m(): Int = {
       |    lazy val (x, y) = (1, 2)
       |    x + y
       |  }
       |}
       |""".stripMargin,
    """|_empty_/d. => final object d extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/d.m(). => method m(): Int
       |  Int => scala/Int#
       |local0 => lazy val local x: Int
       |  Int => scala/Int#
       |local1 => lazy val local y: Int
       |  Int => scala/Int#
       |local2 => synthetic lazy val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    """|object e {
       |  def m(): Int = {
       |    val Some(s) = Some(1)
       |    s
       |  }
       |}
       |""".stripMargin,
    if (!ScalaVersion.atLeast213_17)
      """|_empty_/e. => final object e extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e.m(). => method m(): Int
         |  Int => scala/Int#
         |local0 => val local s: Int
         |  Int => scala/Int#
         |local1 => val local s: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/e. => final object e extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/e.m(). => method m(): Int
         |  Int => scala/Int#
         |local0 => val local s: Int
         |  Int => scala/Int#
         |""".stripMargin,
  )

  symbols(
    """|object f {
       |  def m(): Unit = {
       |    val _ = 1
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/f. => final object f extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/f.m(). => method m(): Unit
         |  Unit => scala/Unit#
         |local0 => val local _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/f. => final object f extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/f.m(). => method m(): Unit
         |  Unit => scala/Unit#
         |local0 => synthetic val local x$1: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  symbols(
    """|class g {
       |  {
       |    val x = 1
       |    lazy val z = 1
       |    val (p, q) = (1, 2)
       |    def d = 1
       |    println((x, z, p, q, d))
       |  }
       |}
       |""".stripMargin,
    """|_empty_/g# => class g extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/g#`<init>`(). => primary ctor <init>()
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local1 => lazy val local z: Int
       |  Int => scala/Int#
       |local2 => val local p: Int
       |  Int => scala/Int#
       |local3 => val local q: Int
       |  Int => scala/Int#
       |local4 => synthetic val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local5 => method d: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    """|class h {
       |  {
       |    val _ = 1
       |  }
       |}
       |""".stripMargin,
    if (ScalaVersion.is212)
      """|_empty_/h# => class h extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/h#`<init>`(). => primary ctor <init>()
         |local0 => val local _: Int
         |  Int => scala/Int#
         |""".stripMargin
    else
      """|_empty_/h# => class h extends AnyRef { +1 decls }
         |  AnyRef => scala/AnyRef#
         |_empty_/h#`<init>`(). => primary ctor <init>()
         |local0 => synthetic val local x$1: Unit
         |  Unit => scala/Unit#
         |""".stripMargin,
  )

  symbols(
    """|object i {
       |  val r = for {
       |    x <- List(1)
       |    y = x + 1
       |    (p, q) = (x, y)
       |    (u, v) <- List((p, q))
       |  } yield u + v
       |}
       |""".stripMargin,
    """|_empty_/i. => final object i extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/i.r. => val method r: List[Int]
       |  List => scala/collection/immutable/List#
       |  Int => scala/Int#
       |local0 => val local x: Int
       |  Int => scala/Int#
       |local4 => val local y: Int
       |  Int => scala/Int#
       |local5 => val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local6 => val local p: Int
       |  Int => scala/Int#
       |local7 => val local q: Int
       |  Int => scala/Int#
       |local8 => synthetic val local x$2: Tuple3[Tuple2[Int, Int], Int, Int]
       |  Tuple3 => scala/Tuple3#
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local9 => val local u: Int
       |  Int => scala/Int#
       |local10 => val local v: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    """|object j {
       |  val f = (n: Int) => {
       |    val x = n
       |    lazy val z = n
       |    x + z
       |  }
       |}
       |""".stripMargin,
    """|_empty_/j. => final object j extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/j.f. => val method f: Function1[Int, Int]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |local0 => param n: Int
       |  Int => scala/Int#
       |local1 => val local x: Int
       |  Int => scala/Int#
       |local2 => lazy val local z: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    """|abstract class k {
       |  def m = {
       |    val xl: Int = ???
       |    type S = { val xs: Int; def ds[T](t: T): Int; var vs: Int }
       |    type E = xe.type forSome { val xe: AnyRef }
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|_empty_/k# => abstract class k extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/k#`<init>`(). => primary ctor <init>()
       |_empty_/k#m(). => method m: Unit
       |  Unit => scala/Unit#
       |local0 => val local xl: Int
       |  Int => scala/Int#
       |local1 => type S = AnyRef { val def xs: Int; def ds[T](t: T): Int; var def vs: Int; var def vs_=(unknown local8): Unit }
       |  AnyRef => scala/AnyRef#
       |  xs => local2
       |  Int => scala/Int#
       |  ds => local3
       |  T => local5
       |  t => local4
       |  vs => local6
       |  vs_= => local7
       |  local8 => local8
       |  Unit => scala/Unit#
       |local2 => abstract val method xs: Int
       |  Int => scala/Int#
       |local3 => abstract method ds[T](t: T): Int
       |  T => local5
       |  t => local4
       |  Int => scala/Int#
       |local4 => param t: T
       |  T => local5
       |local5 => typeparam T
       |local6 => abstract var method vs: Int
       |  Int => scala/Int#
       |local7 => abstract var method vs_=(unknown local8): Unit
       |  local8 => local8
       |  Unit => scala/Unit#
       |local9 => type E = xe.type forSome { type xe.type <: AnyRef with Singleton {} }
       |  xe.type => local10
       |  AnyRef => scala/AnyRef#
       |  Singleton => scala/Singleton#
       |local11 => method xeAnyRef
       |  AnyRef => scala/AnyRef#
       |""".stripMargin,
  )

  symbols(
    """|object l {
       |  def m(): Unit = {
       |    lazy val f: () => Int = () => 1
       |    implicit lazy val i: Int = 1
       |    def g[T](t: T)(implicit u: Int): T = t
       |    def h(x: => Int) = x
       |    object o { lazy val a = 1; val b = 2 }
       |    class c { self: AnyRef => lazy val a = 1; private[this] val b = 2; val (p, q) = (1, 2) }
       |    val n = new { lazy val a = 1; val b = 2 }
       |    type T[A] = List[A]
       |    println((f, i, g(1), h(1), o, n))
       |  }
       |}
       |""".stripMargin,
    """|_empty_/l. => final object l extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/l.m(). => method m(): Unit
       |  Unit => scala/Unit#
       |local0 => lazy val local f: Function0[Int]
       |  Function0 => scala/Function0#
       |  Int => scala/Int#
       |local1 => implicit lazy val local i: Int
       |  Int => scala/Int#
       |local2 => method g[T](t: T)(implicit u: Int): T
       |  T => local5
       |  t => local3
       |  u => local4
       |  Int => scala/Int#
       |local3 => param t: T
       |  T => local5
       |local4 => implicit param u: Int
       |  Int => scala/Int#
       |local5 => typeparam T
       |local6 => method h(x: => Int): Int
       |  x => local7
       |  Int => scala/Int#
       |local7 => param x: => Int
       |  Int => scala/Int#
       |local8 => final object o extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local9 => lazy val method a: Int
       |  Int => scala/Int#
       |local10 => val method b: Int
       |  Int => scala/Int#
       |local11 => class c extends AnyRef { self: AnyRef => +6 decls }
       |  AnyRef => scala/AnyRef#
       |local12 => primary ctor <init>()
       |local13 => lazy val method a: Int
       |  Int => scala/Int#
       |local14 => private[this] val method b: Int
       |  Int => scala/Int#
       |local15 => synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local16 => val method p: Int
       |  Int => scala/Int#
       |local17 => val method q: Int
       |  Int => scala/Int#
       |local18 => selfparam self: AnyRef
       |  AnyRef => scala/AnyRef#
       |local19 => final class $anon extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |local20 => lazy val method a: Int
       |  Int => scala/Int#
       |local21 => val method b: Int
       |  Int => scala/Int#
       |local22 => val local n: AnyRef { lazy val def a: Int; val def b: Int }
       |  AnyRef => scala/AnyRef#
       |  a => local20
       |  Int => scala/Int#
       |  b => local21
       |local23 => type T[A] = List[A]
       |  A => local24
       |  List => scala/package.List#
       |local24 => typeparam A
       |""".stripMargin,
  )

  symbols(
    """|class o {
       |  val (p, q) = (1, 2)
       |  lazy val (r, s) = (1, 2)
       |  private[this] lazy val t = 1
       |  def m = t
       |}
       |""".stripMargin,
    """|_empty_/o# => class o extends AnyRef { +9 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/o#`<init>`(). => primary ctor <init>()
       |_empty_/o#m(). => method m: Int
       |  Int => scala/Int#
       |_empty_/o#p. => val method p: Int
       |  Int => scala/Int#
       |_empty_/o#q. => val method q: Int
       |  Int => scala/Int#
       |_empty_/o#r. => lazy val method r: Int
       |  Int => scala/Int#
       |_empty_/o#s. => lazy val method s: Int
       |  Int => scala/Int#
       |_empty_/o#t. => private[this] lazy val method t: Int
       |  Int => scala/Int#
       |_empty_/o#x$1. => synthetic val method x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |_empty_/o#x$2. => synthetic lazy val method x$2: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local0 => val local p: Int
       |  Int => scala/Int#
       |local1 => val local q: Int
       |  Int => scala/Int#
       |local2 => val local r: Int
       |  Int => scala/Int#
       |local3 => val local s: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  symbols(
    """|object p {
       |  def m(x: Any): Int = x match {
       |    case n: Int =>
       |      lazy val z = n
       |      val (a, b) = (z, z)
       |      a + b
       |    case _ => 0
       |  }
       |  val g = { lazy val w = 1; w }
       |}
       |""".stripMargin,
    """|_empty_/p. => final object p extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/p.g. => val method g: Int
       |  Int => scala/Int#
       |_empty_/p.m(). => method m(x: Any): Int
       |  x => _empty_/p.m().(x)
       |  Any => scala/Any#
       |  Int => scala/Int#
       |_empty_/p.m().(x) => param x: Any
       |  Any => scala/Any#
       |local0 => val local n: Int
       |  Int => scala/Int#
       |local1 => lazy val local z: Int
       |  Int => scala/Int#
       |local2 => val local a: Int
       |  Int => scala/Int#
       |local3 => val local b: Int
       |  Int => scala/Int#
       |local4 => synthetic val local x$1: Tuple2[Int, Int]
       |  Tuple2 => scala/Tuple2#
       |  Int => scala/Int#
       |local5 => lazy val local w: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

}
