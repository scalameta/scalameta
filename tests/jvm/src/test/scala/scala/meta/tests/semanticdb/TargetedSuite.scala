package scala.meta.tests
package semanticdb

import scala.meta._
import scala.meta.internal.semanticdb.scalac._

// Contributing tips:
// - Create another suite like YYY.scala that extends DatabaseSuite,
//   add YYY.scala to your .gitignore, and run `> ~testsJVM/testOnly *YYY`.
//   That should give you a tight edit/run/debug cycle.
// - Go to DatabaseSuite and uncomment the "println" right above
//   "assertDenotationSignaturesAreParseable", that will print out the obtained
//   output into the console. You can copy-paste that output to put into the
//   expected part of the  multiline strings.
// - Try to follow the alphabetical order of the enclosing object, at the time
//   of this writing the latest object is `object ad`, so the next object should
//   be `object ae`.
// - glhf, and if you have any questions don't hesitate to ask in the gitter channel :)
class TargetedSuite extends DatabaseSuite(SemanticdbMode.Slim) {
  names(
    """
    |object A {
    |  def main(args: Array[String]): Unit = {
    |    val list = List(1, 2, 3)
    |    println(list)
    |  }
    |}
  """.trim.stripMargin,
    """
    |[7..8): A <= _empty_.A.
    |[17..21): main <= _empty_.A.main([Ljava/lang/String;)V.
    |[22..26): args <= _empty_.A.main([Ljava/lang/String;)V.(args)
    |[28..33): Array => _root_.scala.Array#
    |[34..40): String => _root_.scala.Predef.String#
    |[44..48): Unit => _root_.scala.Unit#
    |[61..65): list <= local0
    |[68..72): List => _root_.scala.collection.immutable.List.
    |[86..93): println => _root_.scala.Predef.println(Ljava/lang/Object;)V.
    |[94..98): list => local0
  """.trim.stripMargin
  )

  targeted(
    """
    |object <<B>> {
    |  def doSomething = {
    |    42
    |  }
    |}
  """.trim.stripMargin, { (db, second) =>
      assert(second === Symbol("_empty_.B."))
    }
  )

  names(
    """
    |import _root_.scala.List
    |
    |class C {
    |  _root_.scala.List
    |}
  """.trim.stripMargin,
    """
    |[7..13): _root_ => _root_.
    |[14..19): scala => _root_.scala.
    |[20..24): List => _root_.scala.package.List#;_root_.scala.package.List.
    |[32..33): C <= _empty_.C#
    |[34..34): ε <= _empty_.C#`<init>`()V.
    |[38..44): _root_ => _root_.
    |[45..50): scala => _root_.scala.
    |[51..55): List => _root_.scala.collection.immutable.
  """.trim.stripMargin
  )

  targeted(
    // curried function application with named args, #648
    """
      |object D {
      |  def bar(children: Int)(x: Int) = children + x
      |  <<bar>>(children = 4)(3)
      |}
    """.trim.stripMargin, { (db, second) =>
      assert(second === Symbol("_empty_.D.bar(II)I."))
    }
  )

  targeted(
    """
      |package e
      |case class User(name: String, age: Int)
      |object M {
      |  val u: User = ???
      |  u.<<copy>>(<<age>> = 43)
      |}
    """.trim.stripMargin, { (db, copy, age) =>
      assert(copy === Symbol("_root_.e.User#copy(Ljava/lang/String;I)Le/User;."))
      assert(age === Symbol("_root_.e.User#copy(Ljava/lang/String;I)Le/User;.(age)"))
    }
  )

  symbols(
    """
      |import scala.language.experimental.macros
      |
      |package f {
      |  class C1(p1: Int, val p2: Int, var p3: Int) {
      |    def this() = this(0, 0, 0)
      |    val f1 = {
      |      val l1 = ???
      |      var l2 = ???
      |      ???
      |    }
      |    var f2 = ???
      |    def m1[T](x: Int): Int = ???
      |    def m2 = macro ???
      |    type T1 <: Int
      |    type T2 = Int
      |  }
      |
      |  abstract class C2 {
      |    def m3: Int
      |    final def m4 = ???
      |  }
      |
      |  sealed class C3 extends C2 {
      |    def m3 = 42
      |    override def toString = ""
      |  }
      |
      |  trait T {
      |    private val f1 = ???
      |    private[this] val f2 = ???
      |    private[f] val f3 = ???
      |    protected var f4 = ???
      |    protected[this] var f5 = ???
      |    protected[f] var f6 = ???
      |  }
      |
      |  object M {
      |    implicit def i1 = ???
      |    lazy val l1 = ???
      |    case class C1()
      |    class C2[+T, -U]
      |  }
      |}
      |
      |package object F {
      |}
  """.trim.stripMargin,
    """|_root_.F.package. => packageobject package
       |_root_.f. => package f
       |_root_.f.C1# => class C1
       |_root_.f.C1#(p1) => param p1: Int
       |  [0..3): Int => _root_.scala.Int#
       |_root_.f.C1#(p2) => val param p2: Int
       |  [0..3): Int => _root_.scala.Int#
       |_root_.f.C1#(p3) => var param p3_=: (x$1: Int): Unit
       |  [6..9): Int => _root_.scala.Int#
       |  [12..16): Unit => _root_.scala.Unit#
       |_root_.f.C1#T1# => abstract type T1
       |_root_.f.C1#T2# => type T2: Int
       |  [0..3): Int => _root_.scala.Int#
       |_root_.f.C1#`<init>`()V. => secondaryctor <init>: (): C1
       |  [4..6): C1 => _root_.f.C1#
       |_root_.f.C1#`<init>`(III)V. => primaryctor <init>: (p1: Int, p2: Int, p3: Int): C1
       |  [5..8): Int => _root_.scala.Int#
       |  [14..17): Int => _root_.scala.Int#
       |  [23..26): Int => _root_.scala.Int#
       |  [29..31): C1 => _root_.f.C1#
       |_root_.f.C1#f1. => val f1: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.f.C1#f1.l1. => val l1: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.f.C1#f1.l2. => var l2: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.f.C1#f2. => var f2_=: (x$1: Nothing): Unit
       |  [6..13): Nothing => _root_.scala.Nothing#
       |  [16..20): Unit => _root_.scala.Unit#
       |_root_.f.C1#m1(I)I. => def m1: [T] => (x: Int): Int
       |  [11..14): Int => _root_.scala.Int#
       |  [17..20): Int => _root_.scala.Int#
       |_root_.f.C1#m1(I)I.(x) => param x: Int
       |  [0..3): Int => _root_.scala.Int#
       |_root_.f.C1#m1(I)I.T# => typeparam T
       |_root_.f.C1#m2()Lscala/Nothing;. => macro m2: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.f.C2# => abstract class C2
       |_root_.f.C2#`<init>`()V. => primaryctor <init>: (): C2
       |  [4..6): C2 => _root_.f.C2#
       |_root_.f.C2#m3()I. => abstract def m3: Int
       |  [0..3): Int => _root_.scala.Int#
       |_root_.f.C2#m4()Lscala/Nothing;. => final def m4: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.f.C3# => sealed class C3
       |_root_.f.C3#`<init>`()V. => primaryctor <init>: (): C3
       |  [4..6): C3 => _root_.f.C3#
       |_root_.f.C3#m3()I. => def m3: Int
       |  [0..3): Int => _root_.scala.Int#
       |_root_.f.C3#toString()Ljava/lang/String;. => def toString: (): String
       |  [4..10): String => _root_.java.lang.String#
       |_root_.f.M. => final object M
       |_root_.f.M.C1# => case class C1
       |_root_.f.M.C1#`<init>`()V. => primaryctor <init>: (): C1
       |  [4..6): C1 => _root_.f.M.C1#
       |_root_.f.M.C2# => class C2
       |_root_.f.M.C2#[T] => covariant typeparam T
       |_root_.f.M.C2#[U] => contravariant typeparam U
       |_root_.f.M.C2#`<init>`()V. => primaryctor <init>: (): C2[T, U]
       |  [4..6): C2 => _root_.f.M.C2#
       |  [7..8): T => _root_.f.M.C2#[T]
       |  [10..11): U => _root_.f.M.C2#[U]
       |_root_.f.M.i1()Lscala/Nothing;. => implicit def i1: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.f.M.l1. => lazy val l1: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.f.T# => trait T
       |_root_.f.T#$init$()V. => primaryctor $init$: (): Unit
       |  [4..8): Unit => _root_.scala.Unit#
       |_root_.f.T#f1. => private val f1: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.f.T#f2. => private val f2: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.f.T#f3. => private val f3: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.f.T#f4. => protected var f4_=: (x$1: Nothing): Unit
       |  [6..13): Nothing => _root_.scala.Nothing#
       |  [16..20): Unit => _root_.scala.Unit#
       |_root_.f.T#f5. => protected var f5_=: (x$1: Nothing): Unit
       |  [6..13): Nothing => _root_.scala.Nothing#
       |  [16..20): Unit => _root_.scala.Unit#
       |_root_.f.T#f6. => protected var f6_=: (x$1: Nothing): Unit
       |  [6..13): Nothing => _root_.scala.Nothing#
       |  [16..20): Unit => _root_.scala.Unit#
       |_root_.scala. => package scala
       |_root_.scala.Int# => abstract final class Int
       |_root_.scala.Predef.`???`()Lscala/Nothing;. => def ???: Nothing
       |  [0..7): Nothing => _root_.scala.Nothing#
       |_root_.scala.language. => final object language
       |_root_.scala.language.experimental. => final object experimental
       |_root_.scala.language.experimental.macros. => implicit lazy val macros: macros
       |  [0..6): macros => _root_.scala.languageFeature.experimental.macros#
  """.trim.stripMargin
  )

  synthetics(
    """
      |package g
      |import scala.language.higherKinds
      |trait CommandeerDSL[Host]
      |object CommandeerDSL {
      |  def apply[Host, DSL <: CommandeerDSL[Host]](host: Host)(implicit dsl: DSL): DSL = dsl
      |}
      |trait Foo
      |object Foo {
      |  implicit val fooDSL: FooDSL = new FooDSL {}
      |}
      |trait FooDSL extends CommandeerDSL[Foo]
      |object RunMe {
      |  CommandeerDSL(null.asInstanceOf[Foo])
      |}
    """.trim.stripMargin,
    """|[324..324): *.apply[Foo, FooDSL]
       |  [0..1): * => _star_.
       |  [2..7): apply => _root_.g.CommandeerDSL.apply(Ljava/lang/Object;Lg/CommandeerDSL;)Lg/CommandeerDSL;.
       |  [8..11): Foo => _root_.g.Foo#
       |  [13..19): FooDSL => _root_.g.FooDSL#
       |[348..348): *(g.Foo.fooDSL)
       |  [0..1): * => _star_.
       |  [8..14): fooDSL => _root_.g.Foo.fooDSL.
    """.trim.stripMargin
  )

  synthetics(
    """
      |package h
      |class C[T]
      |object C {
      |  implicit def int: C[Int] = new C[Int]
      |  implicit def list[T: C]: C[List[T]] = ???
      |}
      |
      |class X
      |object X {
      |  implicit def cvt[T: C](x: T): X = ???
      |}
      |
      |object M {
      |  Nil.map(x => 2)
      |
      |  def c[T: C] = ???
      |  M.c[List[Int]]
      |
      |  def x(x: X) = ???
      |  x(42)
      |
      |  def i[T](t: T) = ???
      |  i(new C[Int])
      |}
  """.trim.stripMargin,
    """|[201..201): *[Int, List[Int]]
       |  [0..1): * => _star_.
       |  [2..5): Int => _root_.scala.Int#
       |  [12..15): Int => _root_.scala.Int#
       |  [7..11): List => _root_.scala.collection.immutable.List#
       |[209..209): *(scala.collection.immutable.List.canBuildFrom[Int])
       |  [0..1): * => _star_.
       |  [47..50): Int => _root_.scala.Int#
       |  [34..46): canBuildFrom => _root_.scala.collection.immutable.List.canBuildFrom()Lscala/collection/generic/CanBuildFrom;.
       |[247..247): *(h.C.list[Int](h.C.int))
       |  [0..1): * => _star_.
       |  [11..14): Int => _root_.scala.Int#
       |  [6..10): list => _root_.h.C.list(Lh/C;)Lh/C;.
       |  [20..23): int => _root_.h.C.int()Lh/C;.
       |[273..275): h.X.cvt[Int](*)(h.C.int)
       |  [8..11): Int => _root_.scala.Int#
       |  [4..7): cvt => _root_.h.X.cvt(Ljava/lang/Object;Lh/C;)Lh/X;.
       |  [13..14): * => _star_.
       |  [20..23): int => _root_.h.C.int()Lh/C;.
       |[304..304): *[C[Int]]
       |  [0..1): * => _star_.
       |  [4..7): Int => _root_.scala.Int#
       |  [2..3): C => _root_.h.C#
  """.trim.stripMargin
  )

  symbols(
    """
      |package i
      |import scala.collection.mutable.ListBuffer
      |import scala.collection.mutable.{HashSet => HS}
      |trait B {
      |  type X
      |  def x: X
      |}
      |class E extends B {
      |  type X = ListBuffer[Int]
      |  def x = ListBuffer.empty
      |}
      |class D extends B {
      |  type X = HS[Int]
      |  def x = HS.empty
      |}
      |object a {
      |  val x = new E().x
      |  val y = new D().x
      |  def foo(implicit b: B): b.X = {
      |    val result = b.x
      |    result
      |  }
      |}
    """.stripMargin,
    """|_root_.i. => package i
       |_root_.i.B# => trait B
       |_root_.i.B#X# => abstract type X
       |_root_.i.B#x()Ljava/lang/Object;. => abstract def x: B.this.X
       |  [0..1): B => _root_.i.B#
       |  [7..8): X => _root_.i.B#X#
       |_root_.i.D# => class D
       |_root_.i.D#X# => type X: HashSet[Int]
       |  [0..7): HashSet => _root_.scala.collection.mutable.HashSet#
       |  [8..11): Int => _root_.scala.Int#
       |_root_.i.D#`<init>`()V. => primaryctor <init>: (): D
       |  [4..5): D => _root_.i.D#
       |_root_.i.D#x()Lscala/collection/mutable/HashSet;. => def x: HashSet[Int]
       |  [0..7): HashSet => _root_.scala.collection.mutable.HashSet#
       |  [8..11): Int => _root_.scala.Int#
       |_root_.i.E# => class E
       |_root_.i.E#X# => type X: ListBuffer[Int]
       |  [0..10): ListBuffer => _root_.scala.collection.mutable.ListBuffer#
       |  [11..14): Int => _root_.scala.Int#
       |_root_.i.E#`<init>`()V. => primaryctor <init>: (): E
       |  [4..5): E => _root_.i.E#
       |_root_.i.E#x()Lscala/collection/mutable/ListBuffer;. => def x: ListBuffer[Int]
       |  [0..10): ListBuffer => _root_.scala.collection.mutable.ListBuffer#
       |  [11..14): Int => _root_.scala.Int#
       |_root_.i.a. => final object a
       |_root_.i.a.foo(Li/B;)Ljava/lang/Object;. => def foo: (implicit b: B): b.X
       |  [13..14): B => _root_.i.B#
       |  [17..18): b => _root_.i.a.foo(Li/B;)Ljava/lang/Object;.(b)
       |  [19..20): X => _root_.i.B#X#
       |_root_.i.a.foo(Li/B;)Ljava/lang/Object;.(b) => implicit param b: B
       |  [0..1): B => _root_.i.B#
       |_root_.i.a.x. => val x: ListBuffer[Int]
       |  [0..10): ListBuffer => _root_.scala.collection.mutable.ListBuffer#
       |  [11..14): Int => _root_.scala.Int#
       |_root_.i.a.y. => val y: HashSet[Int]
       |  [0..7): HashSet => _root_.scala.collection.mutable.HashSet#
       |  [8..11): Int => _root_.scala.Int#
       |_root_.java.lang.Object#`<init>`()V. => javadefined primaryctor <init>: (): Object
       |  [4..10): Object => _root_.java.lang.Object#
       |_root_.scala. => package scala
       |_root_.scala.Int# => abstract final class Int
       |_root_.scala.collection. => package collection
       |_root_.scala.collection.generic.GenericCompanion#empty()Lscala/collection/GenTraversable;. => def empty: [A] => CC[A]
       |  [7..9): CC => _root_.scala.collection.generic.GenericCompanion#[CC]
       |  [10..11): A => _root_.scala.collection.generic.GenericCompanion#empty()Lscala/collection/GenTraversable;.[A]
       |_root_.scala.collection.mutable. => package mutable
       |_root_.scala.collection.mutable.HashSet# => class HashSet
       |_root_.scala.collection.mutable.HashSet. => final object HashSet
       |_root_.scala.collection.mutable.HashSet.empty()Lscala/collection/mutable/HashSet;. => def empty: [A] => HashSet[A]
       |  [7..14): HashSet => _root_.scala.collection.mutable.HashSet#
       |  [15..16): A => _root_.scala.collection.mutable.HashSet.empty()Lscala/collection/mutable/HashSet;.[A]
       |_root_.scala.collection.mutable.ListBuffer# => final class ListBuffer
       |_root_.scala.collection.mutable.ListBuffer. => final object ListBuffer
       |local0 => val result: b.X
       |  [0..1): b => _root_.i.a.foo(Li/B;)Ljava/lang/Object;.(b)
       |  [2..3): X => _root_.i.B#X#
    """.stripMargin.trim
  )

  synthetics(
    "class J[T: Manifest] { val arr = Array.empty[T] }",
    """|[47..47): *(J.this.evidence$1)
       |  [0..1): * => _star_.
       |  [9..19): evidence$1 => _empty_.J#(evidence$1)
       |""".trim.stripMargin
  )

  names(
    s"""
       |package k
       |object tup {
       |  val foo = (a: (Int, Boolean)) => 1
       |  foo(2, true)
       |  foo.apply(2, true)
       |}
    """.stripMargin,
    """
      |[9..10): k <= _root_.k.
      |[18..21): tup <= _root_.k.tup.
      |[30..33): foo <= _root_.k.tup.foo.
      |[37..38): a <= _root_.k.tup.foo.$anonfun.(a)
      |[41..44): Int => _root_.scala.Int#
      |[46..53): Boolean => _root_.scala.Boolean#
      |[63..66): foo => _root_.k.tup.foo.
      |[78..81): foo => _root_.k.tup.foo.
      |[82..87): apply => _root_.scala.Function1#apply(Ljava/lang/Object;)Ljava/lang/Object;.
    """.stripMargin.trim
  )

  messages(
    """
      |package l
      |import scala.collection.mutable. /* comment */{ Map, Set, ListBuffer }
      |import scala.concurrent._, collection.mutable.{HashSet, Buffer}
      |import scala.collection.{ /* comment */mutable /* comment */ => m}
      |object a {
      |  ListBuffer.empty[Int]
      |  HashSet.empty[Int]
      |}
    """.stripMargin.trim,
    """
      |[58..61): [warning] Unused import
      |[63..66): [warning] Unused import
      |[105..106): [warning] Unused import
      |[137..143): [warning] Unused import
      |[184..191): [warning] Unused import
    """.stripMargin.trim
  )

  names(
    s"""
       |package m
       |class C(x: Int) {
       |  def this() = this(0)
       |}
       |
       |object M {
       |  val c0 = new C()
       |  val c1 = new C(1)
       |}
    """.stripMargin,
    """
      |[9..10): m <= _root_.m.
      |[17..18): C <= _root_.m.C#
      |[18..18): ε <= _root_.m.C#`<init>`(I)V.
      |[19..20): x <= _root_.m.C#(x)
      |[22..25): Int => _root_.scala.Int#
      |[35..39): this <= _root_.m.C#`<init>`()V.
      |[48..48): ε => _root_.m.C#`<init>`(I)V.
      |[62..63): M <= _root_.m.M.
      |[72..74): c0 <= _root_.m.M.c0.
      |[81..82): C => _root_.m.C#
      |[82..82): ε => _root_.m.C#`<init>`()V.
      |[91..93): c1 <= _root_.m.M.c1.
      |[100..101): C => _root_.m.C#
      |[101..101): ε => _root_.m.C#`<init>`(I)V.
    """.stripMargin.trim
  )

  names(
    // See https://github.com/scalameta/scalameta/issues/977
    """|object n {
       |  val Name = "name:(.*)".r
       |  val x #:: xs = Stream(1, 2);
       |  val Name(name) = "name:foo"
       |  1 #:: 2 #:: Stream.empty
       |}""".stripMargin,
    """|[7..8): n <= _empty_.n.
       |[17..21): Name <= _empty_.n.Name.
       |[36..37): r => _root_.scala.collection.immutable.StringLike#r()Lscala/util/matching/Regex;.
       |[44..45): x <= _empty_.n.x$1.x.
       |[46..49): #:: => _root_.scala.package.`#::`.
       |[50..52): xs <= _empty_.n.x$1.xs.
       |[55..61): Stream => _root_.scala.package.Stream.
       |[75..79): Name => _empty_.n.Name.
       |[80..84): name <= _empty_.n.name.name.
       |[103..106): #:: => _root_.scala.collection.immutable.Stream.ConsWrapper#`#::`(Ljava/lang/Object;)Lscala/collection/immutable/Stream;.
       |[109..112): #:: => _root_.scala.collection.immutable.Stream.ConsWrapper#`#::`(Ljava/lang/Object;)Lscala/collection/immutable/Stream;.
       |[113..119): Stream => _root_.scala.package.Stream.
       |[120..125): empty => _root_.scala.collection.immutable.Stream.empty()Lscala/collection/immutable/Stream;.
       |""".stripMargin
  )

  symbols(
    """object o {
      |  List.newBuilder[Int].result
      |  List(1).head
      |}""".stripMargin,
    """|_empty_.o. => final object o
       |_root_.scala.Int# => abstract final class Int
       |_root_.scala.collection.IterableLike#head()Ljava/lang/Object;. => def head: A
       |  [0..1): A => _root_.scala.collection.IterableLike#[A]
       |_root_.scala.collection.immutable.List. => final object List
       |_root_.scala.collection.immutable.List.newBuilder()Lscala/collection/mutable/Builder;. => def newBuilder: [A] => Builder[A, List[A]]
       |  [7..14): Builder => _root_.scala.collection.mutable.Builder#
       |  [15..16): A => _root_.scala.collection.immutable.List.newBuilder()Lscala/collection/mutable/Builder;.[A]
       |  [18..22): List => _root_.scala.collection.immutable.List#
       |  [23..24): A => _root_.scala.collection.immutable.List.newBuilder()Lscala/collection/mutable/Builder;.[A]
       |_root_.scala.collection.mutable.Builder#result()Ljava/lang/Object;. => abstract def result: (): To
       |  [4..6): To => _root_.scala.collection.mutable.Builder#[To]
    """.stripMargin.trim
  )

  names(
    """|object p {
       |  val lst = 1 #:: 2 #:: Stream.empty
       |  lst + "foo"
       |}
    """.stripMargin,
    """|[7..8): p <= _empty_.p.
       |[17..20): lst <= _empty_.p.lst.
       |[25..28): #:: => _root_.scala.collection.immutable.Stream.ConsWrapper#`#::`(Ljava/lang/Object;)Lscala/collection/immutable/Stream;.
       |[31..34): #:: => _root_.scala.collection.immutable.Stream.ConsWrapper#`#::`(Ljava/lang/Object;)Lscala/collection/immutable/Stream;.
       |[35..41): Stream => _root_.scala.package.Stream.
       |[42..47): empty => _root_.scala.collection.immutable.Stream.empty()Lscala/collection/immutable/Stream;.
       |[50..53): lst => _empty_.p.lst.
       |[54..55): + => _root_.scala.Predef.any2stringadd#`+`(Ljava/lang/String;)Ljava/lang/String;.
       |""".stripMargin
  )

  synthetics(
    """|object q {
       |  List(1) + "blaH"
       |}
    """.stripMargin,
    """|[13..20): scala.Predef.any2stringadd[List[Int]](*)
       |  [27..31): List => _root_.scala.collection.immutable.List#
       |  [13..26): any2stringadd => _root_.scala.Predef.any2stringadd(Ljava/lang/Object;)Ljava/lang/Object;.
       |  [32..35): Int => _root_.scala.Int#
       |  [38..39): * => _star_.
       |[17..17): *.apply[Int]
       |  [0..1): * => _star_.
       |  [2..7): apply => _root_.scala.collection.immutable.List.apply(Lscala/collection/Seq;)Lscala/collection/immutable/List;.
       |  [8..11): Int => _root_.scala.Int#
       |""".stripMargin
  )

  synthetics(
    """|object r {
       |  class F
       |  implicit val ordering: Ordering[F] = ???
       |  val x: Ordered[F] = new F
       |}
    """.stripMargin,
    """|[86..91): scala.math.Ordered.orderingToOrdered[F](*)(r.this.ordering)
       |  [19..36): orderingToOrdered => _root_.scala.math.Ordered.orderingToOrdered(Ljava/lang/Object;Lscala/math/Ordering;)Lscala/math/Ordered;.
       |  [37..38): F => _empty_.r.F#
       |  [40..41): * => _star_.
       |  [50..58): ordering => _empty_.r.ordering.
       |""".stripMargin
  )

  synthetics(
    """|object s {
       |  def apply() = 2
       |  s()
       |  s.apply()
       |  case class Bar()
       |  Bar()
       |  1.asInstanceOf[Int => Int](2)
       |}
    """.stripMargin,
    """|[32..32): *.apply
       |  [0..1): * => _star_.
       |  [2..7): apply => _empty_.s.apply()I.
       |[71..71): *.apply
       |  [0..1): * => _star_.
       |  [2..7): apply => _empty_.s.Bar.apply()Ls/Bar;.
       |[102..102): *.apply
       |  [0..1): * => _star_.
       |  [2..7): apply => _root_.scala.Function1#apply(Ljava/lang/Object;)Ljava/lang/Object;.
       |""".stripMargin
  )

  messages(
    // See https://github.com/scalameta/scalameta/issues/899
    """import scala.io._
      |object t""".stripMargin,
    "[16..17): [warning] Unused import"
  )

  targeted(
    // See https://github.com/scalameta/scalameta/issues/830
    "case class u(a: Int); object ya { u.<<unapply>>(u(2)) }", { (db, first) =>
      val denotation = db.symbols.find(_.symbol == first).get.denotation
      assert(first == Symbol("_empty_.u.unapply(Lu;)Lscala/Option;."))
      assertNoDiff(
        denotation.toString,
        """case def unapply: (x$0: u): Option[Int]
          |  [6..7): u => _empty_.u#
          |  [10..16): Option => _root_.scala.Option#
          |  [17..20): Int => _root_.scala.Int#
        """.stripMargin
      )
    }
  )

  targeted(
    """
    object v {
      new Object().<<toString>>
      List(1).<<toString>>
    }
    """, { (db, objectToString, listToString) =>
      val denotation1 = db.symbols.find(_.symbol == objectToString).get.denotation
      val denotation2 = db.symbols.find(_.symbol == listToString).get.denotation
      assert(denotation1.isJavaDefined)
      assert(!denotation2.isJavaDefined)
    }
  )

  targeted(
    """
      |object w {
      |  val <<a>> = new java.lang.StringBuilder
      |  val <<b>> = new StringBuilder
      |  val <<c>>: Traversable[Int] = List(1)
      |  val <<d>> = Map(1 -> 2)
      |}
    """.stripMargin, { (db, a, b, c, d) =>
      def check(symbol: Symbol, info: String) = {
        assertNoDiff(db.symbols.find(_.symbol == symbol).get.denotation.signature, info)
      }
      check(a, "StringBuilder")
      check(b, "StringBuilder")
      check(c, "Traversable[Int]")
      check(d, "Map[Int, Int]")
    }
  )

  names(
    """|
       |import scala.meta._
       |import org.scalatest._
       |object x extends FunSuite {
       |  val x = q"Foo"
       |  val y = q"Bar"
       |  val z = q"$x + $y"
       |  val k = sourcecode.Name.generate
       |  assert(x.value == "Foo")
       |}
    """.stripMargin,
    """|
       |[8..13): scala => _root_.scala.
       |[14..18): meta => _root_.scala.meta.
       |[28..31): org => _root_.org.
       |[32..41): scalatest => _root_.org.scalatest.
       |[51..52): x <= _empty_.x.
       |[61..69): FunSuite => _root_.org.scalatest.FunSuite#
       |[70..70): ε => _root_.org.scalatest.FunSuite#`<init>`()V.
       |[78..79): x <= _empty_.x.x.
       |[82..83): q => _root_.scala.meta.internal.quasiquotes.Unlift.
       |[95..96): y <= _empty_.x.y.
       |[99..100): q => _root_.scala.meta.internal.quasiquotes.Unlift.
       |[112..113): z <= _empty_.x.z.
       |[116..117): q => _root_.scala.meta.internal.quasiquotes.Unlift.
       |[119..120): x => _empty_.x.x.
       |[124..125): y => _empty_.x.y.
       |[133..134): k <= _empty_.x.k.
       |[137..147): sourcecode => _root_.sourcecode.
       |[148..152): Name => _root_.sourcecode.Name.
       |[153..161): generate => _root_.sourcecode.Name.generate()Lsourcecode/Name;.
       |[164..170): assert => _root_.org.scalatest.Assertions#assert(ZLorg/scalactic/Prettifier;Lorg/scalactic/source/Position;)Lorg/scalatest/compatible/Assertion;.
       |[171..172): x => _empty_.x.x.
       |[173..178): value => _root_.scala.meta.Term.Name#value()Ljava/lang/String;.
       |[179..181): == => _root_.java.lang.Object#`==`(Ljava/lang/Object;)Z.
       |""".stripMargin
  )

  symbols(
    """object y {
      |  class Path {
      |    class B { class C }
      |    val x = new B
      |    val y = new x.C
      |  }
      |  implicit val b = new Path().x
      |}
    """.stripMargin,
    """
      |_empty_.y. => final object y
      |_empty_.y.Path# => class Path
      |_empty_.y.Path#B# => class B
      |_empty_.y.Path#B#C# => class C
      |_empty_.y.Path#B#C#`<init>`()V. => primaryctor <init>: (): B.this.C
      |  [4..5): B => _empty_.y.Path#B#
      |  [11..12): C => _empty_.y.Path#B#C#
      |_empty_.y.Path#B#`<init>`()V. => primaryctor <init>: (): Path.this.B
      |  [4..8): Path => _empty_.y.Path#
      |  [14..15): B => _empty_.y.Path#B#
      |_empty_.y.Path#`<init>`()V. => primaryctor <init>: (): Path
      |  [4..8): Path => _empty_.y.Path#
      |_empty_.y.Path#x. => val x: Path.this.B
      |  [0..4): Path => _empty_.y.Path#
      |  [10..11): B => _empty_.y.Path#B#
      |_empty_.y.Path#y. => val y: x.C
      |  [0..1): x => _empty_.y.Path#x.
      |  [2..3): C => _empty_.y.Path#B#C#
      |_empty_.y.b. => implicit val b: B
      |  [0..1): B => _empty_.y.Path#B#
    """.stripMargin
  )

  symbols(
    """
      |object z {
      |  val x = z
      |}
    """.stripMargin,
    """
      |_empty_.z. => final object z
      |_empty_.z.x. => val x: z.type
      |  [0..1): z => _empty_.z.
    """.stripMargin
  )

  symbols(
    """
      |class aa {
      |  val x = this
      |  val y: aa.this.type = this
      |}
    """.stripMargin,
    """
      |_empty_.aa# => class aa
      |_empty_.aa#`<init>`()V. => primaryctor <init>: (): aa
      |  [4..6): aa => _empty_.aa#
      |_empty_.aa#x. => val x: aa
      |  [0..2): aa => _empty_.aa#
      |_empty_.aa#y. => val y: aa.this.type
      |  [0..2): aa => _empty_.aa#
    """.stripMargin
  )

  symbols(
    """
      |object `ab ab`
    """.stripMargin,
    """
      |_empty_.`ab ab`. => final object `ab ab`
    """.stripMargin
  )

  symbols(
    """
      |object ac {
      |  val x = Int.MaxValue
      |  val y: Class[_] = ???
      |}
    """.stripMargin,
    """
      |_empty_.ac. => final object ac
      |_empty_.ac.x. => val x: Int
      |  [0..3): Int => _root_.scala.Int#
      |_empty_.ac.y. => val y: Class[_]
      |_root_.scala.Int. => final object Int
      |_root_.scala.Int.MaxValue. => final val MaxValue: Int
      |  [0..3): Int => _root_.scala.Int#
      |_root_.scala.Predef.Class# => type Class: [T] => Class[T]
      |  [7..12): Class => _root_.java.lang.Class#
      |  [13..14): T => _root_.scala.Predef.Class#[T]
      |_root_.scala.Predef.`???`()Lscala/Nothing;. => def ???: Nothing
      |  [0..7): Nothing => _root_.scala.Nothing#
    """.stripMargin
  )

  symbols(
    """
      |object ad {
      |  trait Foo
      |  class Bar
      |  val x = new Foo {
      |    val y = 2
      |    def z[T](e: T) = e
      |  }
      |  val z: AnyRef with Foo { val y: Int } = x
      |  val k: AnyRef with Foo { val y: Any } = x
      |  val zz = new Bar {
      |    val y = 2
      |  }
      |}
    """.stripMargin,
    // Note that _empty_.ab.$anon#y. matches both y: Int and  y: Any.
    """
      |_empty_.ad. => final object ad
      |_empty_.ad.$anon#y. => abstract val y: Any
      |  [0..3): Any => _root_.scala.Any#
      |_empty_.ad.Bar# => class Bar
      |_empty_.ad.Bar#`<init>`()V. => primaryctor <init>: (): Bar
      |  [4..7): Bar => _empty_.ad.Bar#
      |_empty_.ad.Foo# => trait Foo
      |_empty_.ad.k. => val k: AnyRef with Foo{val y: Any}
      |  [0..6): AnyRef => _root_.scala.AnyRef#
      |  [12..15): Foo => _empty_.ad.Foo#
      |  [20..21): y => _empty_.ad.$anon#y.
      |_empty_.ad.x. => val x: AnyRef with Foo{val y: Int; def z[T](e: T): T}
      |  [0..6): AnyRef => _root_.scala.AnyRef#
      |  [12..15): Foo => _empty_.ad.Foo#
      |  [20..21): y => _empty_.ad.x.$anon#y.
      |  [32..33): z => _empty_.ad.x.$anon#z(Ljava/lang/Object;)Ljava/lang/Object;.
      |_empty_.ad.x.$anon#y. => val y: Int
      |  [0..3): Int => _root_.scala.Int#
      |_empty_.ad.x.$anon#z(Ljava/lang/Object;)Ljava/lang/Object;. => def z: [T] => (e: T): T
      |  [11..12): T => _empty_.ad.x.$anon#z(Ljava/lang/Object;)Ljava/lang/Object;.[T]
      |  [15..16): T => _empty_.ad.x.$anon#z(Ljava/lang/Object;)Ljava/lang/Object;.[T]
      |_empty_.ad.x.$anon#z(Ljava/lang/Object;)Ljava/lang/Object;.(e) => param e: T
      |  [0..1): T => _empty_.ad.x.$anon#z(Ljava/lang/Object;)Ljava/lang/Object;.T#
      |_empty_.ad.x.$anon#z(Ljava/lang/Object;)Ljava/lang/Object;.T# => typeparam T
      |_empty_.ad.z. => val z: AnyRef with Foo{val y: Int}
      |  [0..6): AnyRef => _root_.scala.AnyRef#
      |  [12..15): Foo => _empty_.ad.Foo#
      |  [20..21): y => _empty_.ad.$anon#y.
      |_empty_.ad.zz. => val zz: Bar{val y: Int}
      |  [0..3): Bar => _empty_.ad.Bar#
      |  [8..9): y => _empty_.ad.zz.$anon#y.
      |_empty_.ad.zz.$anon#y. => val y: Int
      |  [0..3): Int => _root_.scala.Int#
      |_root_.java.lang.Object#`<init>`()V. => javadefined primaryctor <init>: (): Object
      |  [4..10): Object => _root_.java.lang.Object#
      |_root_.scala.Any# => abstract class Any
      |_root_.scala.AnyRef# => val AnyRef: AnyRef with Specializable{}
      |  [0..6): AnyRef => _root_.scala.AnyRef#
      |  [12..25): Specializable => _root_.scala.Specializable#
      |_root_.scala.Int# => abstract final class Int
    """.stripMargin
  )

  targeted(
    """
      |object ae {
      |  trait Foo
      |  val x = new Foo {
      |    val <<y>> = 2
      |    def <<z>>[T](e: T) = e
      |  }
      |}
      |object af {
      |  val y = ae.x.<<y>>
      |  val z = ae.x.<<z>>(2)
      |}
    """.stripMargin, { (db, y1, z1, y2, z2) =>
      assert(y1 == y2)
      assert(z1 == z2)
    }
  )

  names(
    """
      |object ag {
      | for (x <- 1 to 10; y <- 0 until 10) println(x -> x)
      | for (i <- 1 to 10; j <- 0 until 10) yield (i, j)
      | for (i <- 1 to 10; j <- 0 until 10 if i % 2 == 0) yield (i, j)
      |}
    """.trim.stripMargin,
    """
      |[7..9): ag <= _empty_.ag.
      |[18..19): x <= local0
      |[25..27): to => _root_.scala.runtime.RichInt#to(I)Lscala/collection/immutable/Range/Inclusive;.
      |[32..33): y <= local1
      |[39..44): until => _root_.scala.runtime.RichInt#until(I)Lscala/collection/immutable/Range;.
      |[49..56): println => _root_.scala.Predef.println(Ljava/lang/Object;)V.
      |[57..58): x => local0
      |[59..61): -> => _root_.scala.Predef.ArrowAssoc#`->`(Ljava/lang/Object;)Lscala/Tuple2;.
      |[62..63): x => local0
      |[71..72): i <= local2
      |[78..80): to => _root_.scala.runtime.RichInt#to(I)Lscala/collection/immutable/Range/Inclusive;.
      |[85..86): j <= local3
      |[92..97): until => _root_.scala.runtime.RichInt#until(I)Lscala/collection/immutable/Range;.
      |[109..110): i => local2
      |[112..113): j => local3
      |[121..122): i <= local4
      |[128..130): to => _root_.scala.runtime.RichInt#to(I)Lscala/collection/immutable/Range/Inclusive;.
      |[135..136): j <= local5
      |[142..147): until => _root_.scala.runtime.RichInt#until(I)Lscala/collection/immutable/Range;.
      |[154..155): i => local4
      |[156..157): % => _root_.scala.Int#`%`(I)I.
      |[160..162): == => _root_.scala.Int#`==`(I)Z.
      |[173..174): i => local4
      |[176..177): j => local6
    """.trim.stripMargin
  )

  synthetics(
    """
      |object ah {
      | for (x <- 1 to 10; y <- 0 until 10) println(x -> x)
      | for (i <- 1 to 10; j <- 0 until 10) yield (i, j)
      | for (i <- 1 to 10; j <- 0 until 10 if i % 2 == 0) yield (i, j)
      |}
    """.trim.stripMargin,
    """
      |[23..24): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => _root_.scala.LowPriorityImplicits#intWrapper(I)I.
      |  [24..25): * => _star_.
      |[30..30): *.foreach[Unit]
      |  [0..1): * => _star_.
      |  [2..9): foreach => _root_.scala.collection.immutable.Range#foreach(Lscala/Function1;)V.
      |  [10..14): Unit => _root_.scala.Unit#
      |[37..38): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => _root_.scala.LowPriorityImplicits#intWrapper(I)I.
      |  [24..25): * => _star_.
      |[47..47): *.foreach[Unit]
      |  [0..1): * => _star_.
      |  [2..9): foreach => _root_.scala.collection.immutable.Range#foreach(Lscala/Function1;)V.
      |  [10..14): Unit => _root_.scala.Unit#
      |[57..58): scala.Predef.ArrowAssoc[Int](*)
      |  [13..23): ArrowAssoc => _root_.scala.Predef.ArrowAssoc(Ljava/lang/Object;)Ljava/lang/Object;.
      |  [24..27): Int => _root_.scala.Int#
      |  [29..30): * => _star_.
      |[61..61): *[Int]
      |  [0..1): * => _star_.
      |  [2..5): Int => _root_.scala.Int#
      |[76..77): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => _root_.scala.LowPriorityImplicits#intWrapper(I)I.
      |  [24..25): * => _star_.
      |[83..83): *.flatMap[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
      |  [0..1): * => _star_.
      |  [2..9): flatMap => _root_.scala.collection.TraversableLike#flatMap(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;.
      |  [22..25): Int => _root_.scala.Int#
      |  [10..16): Tuple2 => _root_.scala.Tuple2#
      |  [17..20): Int => _root_.scala.Int#
      |  [28..38): IndexedSeq => _root_.scala.collection.immutable.IndexedSeq#
      |  [39..45): Tuple2 => _root_.scala.Tuple2#
      |  [46..49): Int => _root_.scala.Int#
      |  [51..54): Int => _root_.scala.Int#
      |  [58..59): * => _star_.
      |  [112..118): Tuple2 => _root_.scala.Tuple2#
      |  [119..122): Int => _root_.scala.Int#
      |  [124..127): Int => _root_.scala.Int#
      |  [99..111): canBuildFrom => _root_.scala.collection.immutable.IndexedSeq.canBuildFrom()Lscala/collection/generic/CanBuildFrom;.
      |[90..91): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => _root_.scala.LowPriorityImplicits#intWrapper(I)I.
      |  [24..25): * => _star_.
      |[100..100): *.map[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
      |  [0..1): * => _star_.
      |  [2..5): map => _root_.scala.collection.TraversableLike#map(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;.
      |  [18..21): Int => _root_.scala.Int#
      |  [6..12): Tuple2 => _root_.scala.Tuple2#
      |  [13..16): Int => _root_.scala.Int#
      |  [24..34): IndexedSeq => _root_.scala.collection.immutable.IndexedSeq#
      |  [35..41): Tuple2 => _root_.scala.Tuple2#
      |  [42..45): Int => _root_.scala.Int#
      |  [47..50): Int => _root_.scala.Int#
      |  [54..55): * => _star_.
      |  [108..114): Tuple2 => _root_.scala.Tuple2#
      |  [115..118): Int => _root_.scala.Int#
      |  [120..123): Int => _root_.scala.Int#
      |  [95..107): canBuildFrom => _root_.scala.collection.immutable.IndexedSeq.canBuildFrom()Lscala/collection/generic/CanBuildFrom;.
      |[126..127): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => _root_.scala.LowPriorityImplicits#intWrapper(I)I.
      |  [24..25): * => _star_.
      |[133..133): *.flatMap[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
      |  [0..1): * => _star_.
      |  [2..9): flatMap => _root_.scala.collection.TraversableLike#flatMap(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;.
      |  [22..25): Int => _root_.scala.Int#
      |  [10..16): Tuple2 => _root_.scala.Tuple2#
      |  [17..20): Int => _root_.scala.Int#
      |  [28..38): IndexedSeq => _root_.scala.collection.immutable.IndexedSeq#
      |  [39..45): Tuple2 => _root_.scala.Tuple2#
      |  [46..49): Int => _root_.scala.Int#
      |  [51..54): Int => _root_.scala.Int#
      |  [58..59): * => _star_.
      |  [112..118): Tuple2 => _root_.scala.Tuple2#
      |  [119..122): Int => _root_.scala.Int#
      |  [124..127): Int => _root_.scala.Int#
      |  [99..111): canBuildFrom => _root_.scala.collection.immutable.IndexedSeq.canBuildFrom()Lscala/collection/generic/CanBuildFrom;.
      |[140..141): scala.Predef.intWrapper(*)
      |  [13..23): intWrapper => _root_.scala.LowPriorityImplicits#intWrapper(I)I.
      |  [24..25): * => _star_.
      |[150..150): *.withFilter
      |  [0..1): * => _star_.
      |  [2..12): withFilter => _root_.scala.collection.TraversableLike#withFilter(Lscala/Function1;)Lscala/collection/generic/FilterMonadic;.
      |[164..164): *.map[Tuple2[Int, Int], IndexedSeq[Tuple2[Int, Int]]](*)(scala.collection.immutable.IndexedSeq.canBuildFrom[Tuple2[Int, Int]])
      |  [0..1): * => _star_.
      |  [2..5): map => _root_.scala.collection.generic.FilterMonadic#map(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;.
      |  [18..21): Int => _root_.scala.Int#
      |  [6..12): Tuple2 => _root_.scala.Tuple2#
      |  [13..16): Int => _root_.scala.Int#
      |  [24..34): IndexedSeq => _root_.scala.collection.immutable.IndexedSeq#
      |  [35..41): Tuple2 => _root_.scala.Tuple2#
      |  [42..45): Int => _root_.scala.Int#
      |  [47..50): Int => _root_.scala.Int#
      |  [54..55): * => _star_.
      |  [108..114): Tuple2 => _root_.scala.Tuple2#
      |  [115..118): Int => _root_.scala.Int#
      |  [120..123): Int => _root_.scala.Int#
      |  [95..107): canBuildFrom => _root_.scala.collection.immutable.IndexedSeq.canBuildFrom()Lscala/collection/generic/CanBuildFrom;.
    """.trim.stripMargin
  )

  synthetics(
    """
      |object ai {
      |  import scala.concurrent.ExecutionContext.Implicits.global
      |  for {
      |    a <- scala.concurrent.Future.successful(1)
      |    b <- scala.concurrent.Future.successful(2)
      |  } println(a)
      |  for {
      |    a <- scala.concurrent.Future.successful(1)
      |    b <- scala.concurrent.Future.successful(2)
      |    if a < b
      |  } yield a
      |}
    """.trim.stripMargin,
    """
      |[123..123): *[Int]
      |  [0..1): * => _star_.
      |  [2..5): Int => _root_.scala.Int#
      |[126..126): *.foreach[Unit](*)(scala.concurrent.ExecutionContext.Implicits.global)
      |  [0..1): * => _star_.
      |  [2..9): foreach => _root_.scala.concurrent.Future#foreach(Lscala/Function1;Lscala/concurrent/ExecutionContext;)V.
      |  [10..14): Unit => _root_.scala.Unit#
      |  [16..17): * => _star_.
      |  [63..69): global => _root_.scala.concurrent.ExecutionContext.Implicits.global.
      |[170..170): *[Int]
      |  [0..1): * => _star_.
      |  [2..5): Int => _root_.scala.Int#
      |[173..173): *.foreach[Unit](*)(scala.concurrent.ExecutionContext.Implicits.global)
      |  [0..1): * => _star_.
      |  [2..9): foreach => _root_.scala.concurrent.Future#foreach(Lscala/Function1;Lscala/concurrent/ExecutionContext;)V.
      |  [10..14): Unit => _root_.scala.Unit#
      |  [16..17): * => _star_.
      |  [63..69): global => _root_.scala.concurrent.ExecutionContext.Implicits.global.
      |[240..240): *[Int]
      |  [0..1): * => _star_.
      |  [2..5): Int => _root_.scala.Int#
      |[243..243): *.flatMap[Int](*)(scala.concurrent.ExecutionContext.Implicits.global)
      |  [0..1): * => _star_.
      |  [2..9): flatMap => _root_.scala.concurrent.Future#flatMap(Lscala/Function1;Lscala/concurrent/ExecutionContext;)Lscala/concurrent/Future;.
      |  [10..13): Int => _root_.scala.Int#
      |  [15..16): * => _star_.
      |  [62..68): global => _root_.scala.concurrent.ExecutionContext.Implicits.global.
      |[287..287): *[Int]
      |  [0..1): * => _star_.
      |  [2..5): Int => _root_.scala.Int#
      |[290..290): *.withFilter(*)(scala.concurrent.ExecutionContext.Implicits.global)
      |  [0..1): * => _star_.
      |  [2..12): withFilter => _root_.scala.concurrent.Future#withFilter(Lscala/Function1;Lscala/concurrent/ExecutionContext;)Lscala/concurrent/Future;.
      |  [13..14): * => _star_.
      |  [60..66): global => _root_.scala.concurrent.ExecutionContext.Implicits.global.
      |[303..303): *.map[Int](*)(scala.concurrent.ExecutionContext.Implicits.global)
      |  [0..1): * => _star_.
      |  [2..5): map => _root_.scala.concurrent.Future#map(Lscala/Function1;Lscala/concurrent/ExecutionContext;)Lscala/concurrent/Future;.
      |  [6..9): Int => _root_.scala.Int#
      |  [11..12): * => _star_.
      |  [58..64): global => _root_.scala.concurrent.ExecutionContext.Implicits.global.
    """.trim.stripMargin
  )

  targeted(
    """
      |object aj {
      |  None.<<fold>>(1)(identity)
      |  java.lang.String.<<format>>("%s%s", "", "")
      |  def <<a>>(b: => Int, c: String*): Unit = ()
      |}
    """.stripMargin, { (db, fold, format, a) =>
      val aDenot = db.symbols.find(_.symbol == a).get.denotation
      assertNoDiff(aDenot.signature, "(b: =>Int, c: String*): Unit")
      val foldDenot = db.symbols.find(_.symbol == fold).get.denotation
      assertNoDiff(foldDenot.signature, "[B] => (ifEmpty: =>B)(f: Function1[A, B]): B")
      // Check java repeated params have same * syntax
      val formatDenot = db.symbols.find(_.symbol == format).get.denotation
      assertNoDiff(formatDenot.signature, "(x$1: String, x$2: Object*): String")
    }
  )

  targeted(
    """package ak
      |trait Foo
      |object Foo {
      |  new <<Foo>> {}
      |}
    """.stripMargin, { (_, Foo) =>
      assertNoDiff(Foo.syntax, "_root_.ak.Foo#")
    }
  )

  targeted(
    """
      |package al
      |
      |trait EventBus {
      |  type Classifier
      |}
      |trait Foo { this: EventBus =>
      |  val <<x>>: Classifier
      |}
      |""".stripMargin, { (db, x) =>
      val sig = db.symbols.find(_.symbol == x).get.denotation.signature
      assertNoDiff(sig, "Foo.this.Classifier")
    }
  )

  targeted(
    """package am
      |trait A[T] { type Self; def self: Self }
      |object A {
      |  def <<foo>>[T] = null.asInstanceOf[A[T]].self
      |}
    """.trim.stripMargin, { (db, foo) =>
      val symbol = db.symbols.find(_.symbol == foo).get
      assertNoDiff(foo.syntax, "_root_.am.A.foo()Ljava/lang/Object;.")
      assertNoDiff(
        symbol.syntax,
        """
          |_root_.am.A.foo()Ljava/lang/Object;. => def foo: [T] => A[T]#Self
          |  [7..8): A => _root_.am.A#
          |  [9..10): T => _root_.am.A.foo()Ljava/lang/Object;.[T]
          |  [12..16): Self => _root_.am.A#Self#
        """.stripMargin
      )
    }
  )
}
