package scala.meta.tests

import scala.meta._
import scala.meta.internal.semanticdb.SemanticdbMode

class SemanticSuite extends DatabaseSuite(SemanticdbMode.Slim) {
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
    |[61..65): list <= <...>@57..81
    |[68..72): List => _root_.scala.collection.immutable.List.
    |[86..93): println => _root_.scala.Predef.println(Ljava/lang/Object;)V.
    |[94..98): list => <...>@57..81
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
    |[20..24): List => _root_.scala.package.List.;_root_.scala.package.List#
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
    """
      |_root_.F.package. => packageobject package
      |_root_.f. => package f
      |_root_.f.C1# => class C1
      |_root_.f.C1#(p1) => param p1: scala.Int
      |  [6..9): Int => _root_.scala.Int#
      |_root_.f.C1#(p2) => val param p2: scala.Int
      |  [6..9): Int => _root_.scala.Int#
      |_root_.f.C1#(p3) => var param p3_=: (x$1: scala.Int)scala.Unit
      |  [12..15): Int => _root_.scala.Int#
      |  [22..26): Unit => _root_.scala.Unit#
      |_root_.f.C1#T1# => abstract type T1:  <: Int
      |_root_.f.C1#T2# => type T2: scala.Int
      |  [6..9): Int => _root_.scala.Int#
      |_root_.f.C1#`<init>`()V. => secondaryctor <init>: ()f.C1
      |  [4..6): C1 => _root_.f.C1#
      |_root_.f.C1#`<init>`(III)V. => primaryctor <init>: (p1: scala.Int,p2: scala.Int,p3: scala.Int)f.C1
      |  [11..14): Int => _root_.scala.Int#
      |  [25..28): Int => _root_.scala.Int#
      |  [39..42): Int => _root_.scala.Int#
      |  [45..47): C1 => _root_.f.C1#
      |_root_.f.C1#f1. => val f1: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.f.C1#f1.l1. => val l1: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.f.C1#f1.l2. => var l2: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.f.C1#f2. => var f2_=: (x$1: scala.Nothing)scala.Unit
      |  [12..19): Nothing => _root_.scala.Nothing#
      |  [26..30): Unit => _root_.scala.Unit#
      |_root_.f.C1#m1(I)I. => def m1: [T](x: scala.Int)scala.Int
      |  [13..16): Int => _root_.scala.Int#
      |  [23..26): Int => _root_.scala.Int#
      |_root_.f.C1#m1(I)I.(x) => param x: scala.Int
      |  [6..9): Int => _root_.scala.Int#
      |_root_.f.C1#m1(I)I.T# => typeparam T
      |_root_.f.C1#m2()Lscala/Nothing;. => macro m2: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.f.C2# => abstract class C2
      |_root_.f.C2#`<init>`()V. => primaryctor <init>: ()f.C2
      |  [4..6): C2 => _root_.f.C2#
      |_root_.f.C2#m3()I. => abstract def m3: scala.Int
      |  [6..9): Int => _root_.scala.Int#
      |_root_.f.C2#m4()Lscala/Nothing;. => final def m4: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.f.C3# => sealed class C3
      |_root_.f.C3#`<init>`()V. => primaryctor <init>: ()f.C3
      |  [4..6): C3 => _root_.f.C3#
      |_root_.f.C3#m3()I. => def m3: scala.Int
      |  [6..9): Int => _root_.scala.Int#
      |_root_.f.C3#toString()Ljava/lang/String;. => def toString: ()java.lang.String
      |  [12..18): String => _root_.java.lang.String#
      |_root_.f.M. => final object M
      |_root_.f.M.C1# => case class C1
      |_root_.f.M.C1#`<init>`()V. => primaryctor <init>: ()f.M.C1
      |  [6..8): C1 => _root_.f.M.C1#
      |_root_.f.M.C2# => class C2
      |_root_.f.M.C2#[T] => covariant typeparam T
      |_root_.f.M.C2#[U] => contravariant typeparam U
      |_root_.f.M.C2#`<init>`()V. => primaryctor <init>: ()f.M.C2[T,U]
      |  [6..8): C2 => _root_.f.M.C2#
      |  [9..10): T => _root_.f.M.C2#[T]
      |  [11..12): U => _root_.f.M.C2#[U]
      |_root_.f.M.i1()Lscala/Nothing;. => implicit def i1: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.f.M.l1. => lazy val l1: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.f.T# => trait T
      |_root_.f.T#$init$()V. => primaryctor $init$: ()scala.Unit
      |  [8..12): Unit => _root_.scala.Unit#
      |_root_.f.T#f1. => private val f1: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.f.T#f2. => private val f2: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.f.T#f3. => private val f3: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.f.T#f4. => protected var f4_=: (x$1: scala.Nothing)scala.Unit
      |  [12..19): Nothing => _root_.scala.Nothing#
      |  [26..30): Unit => _root_.scala.Unit#
      |_root_.f.T#f5. => protected var f5_=: (x$1: scala.Nothing)scala.Unit
      |  [12..19): Nothing => _root_.scala.Nothing#
      |  [26..30): Unit => _root_.scala.Unit#
      |_root_.f.T#f6. => protected var f6_=: (x$1: scala.Nothing)scala.Unit
      |  [12..19): Nothing => _root_.scala.Nothing#
      |  [26..30): Unit => _root_.scala.Unit#
      |_root_.scala. => package scala
      |_root_.scala.Int# => abstract final class Int
      |_root_.scala.Int#`<init>`()V. => primaryctor <init>: ()scala.Int
      |  [8..11): Int => _root_.scala.Int#
      |_root_.scala.Predef.`???`()Lscala/Nothing;. => def ???: scala.Nothing
      |  [6..13): Nothing => _root_.scala.Nothing#
      |_root_.scala.language. => final object language
      |_root_.scala.language.experimental. => final object experimental
      |_root_.scala.language.experimental.macros. => implicit lazy val macros: scala.languageFeature.experimental.macros
      |  [6..21): languageFeature => _root_.scala.languageFeature.
      |  [22..34): experimental => _root_.scala.languageFeature.experimental.
      |  [35..41): macros => _root_.scala.languageFeature.experimental.macros#
  """.trim.stripMargin
  )

  sugars(
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
    """
      |[324..324): *.apply[g.Foo, g.FooDSL]
      |  [0..1): * => _star_.
      |  [2..7): apply => _root_.g.CommandeerDSL.apply(Ljava/lang/Object;Lg/CommandeerDSL;)Lg/CommandeerDSL;.
      |  [10..13): Foo => _root_.g.Foo#
      |  [17..23): FooDSL => _root_.g.FooDSL#
      |[348..348): *(g.Foo.fooDSL)
      |  [0..1): * => _star_.
      |  [8..14): fooDSL => _root_.g.Foo.fooDSL.
    """.trim.stripMargin
  )

  sugars(
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
    """
      |[201..201): *[scala.Int, scala.collection.immutable.List[scala.Int]]
      |  [0..1): * => _star_.
      |  [8..11): Int => _root_.scala.Int#
      |  [40..44): List => _root_.scala.collection.immutable.List#
      |  [51..54): Int => _root_.scala.Int#
      |[209..209): *(scala.collection.immutable.List.canBuildFrom[scala.Int])
      |  [0..1): * => _star_.
      |  [34..46): canBuildFrom => _root_.scala.collection.immutable.List.canBuildFrom()Lscala/collection/generic/CanBuildFrom;.
      |  [53..56): Int => _root_.scala.Int#
      |[247..247): *(h.C.list[scala.Int](h.C.int))
      |  [0..1): * => _star_.
      |  [17..20): Int => _root_.scala.Int#
      |  [26..29): int => _root_.h.C.int()Lh/C;.
      |  [6..10): list => _root_.h.C.list(Lh/C;)Lh/C;.
      |[273..275): h.X.cvt[scala.Int](*)(h.C.int)
      |  [4..7): cvt => _root_.h.X.cvt(Ljava/lang/Object;Lh/C;)Lh/X;.
      |  [14..17): Int => _root_.scala.Int#
      |  [19..20): * => _star_.
      |  [26..29): int => _root_.h.C.int()Lh/C;.
      |[304..304): *[h.C[scala.Int]]
      |  [0..1): * => _star_.
      |  [4..5): C => _root_.h.C#
      |  [12..15): Int => _root_.scala.Int#
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
    """
      |<...>@359..375 => val result: b.X
      |  [0..1): b => _root_.i.a.foo(Li/B;)Ljava/lang/Object;.(b)
      |  [2..3): X => _root_.i.B#X#
      |_root_.i. => package i
      |_root_.i.B# => trait B
      |_root_.i.B#X# => abstract type X
      |_root_.i.B#x()Ljava/lang/Object;. => abstract def x: B.this.X
      |  [7..8): X => _root_.i.B#X#
      |_root_.i.D# => class D
      |_root_.i.D#X# => type X: scala.collection.mutable.HashSet[scala.Int]
      |  [25..32): HashSet => _root_.scala.collection.mutable.HashSet#
      |  [39..42): Int => _root_.scala.Int#
      |_root_.i.D#`<init>`()V. => primaryctor <init>: ()i.D
      |  [4..5): D => _root_.i.D#
      |_root_.i.D#x()Lscala/collection/mutable/HashSet;. => def x: scala.collection.mutable.HashSet[scala.Int]
      |  [25..32): HashSet => _root_.scala.collection.mutable.HashSet#
      |  [39..42): Int => _root_.scala.Int#
      |_root_.i.E# => class E
      |_root_.i.E#X# => type X: scala.collection.mutable.ListBuffer[scala.Int]
      |  [25..35): ListBuffer => _root_.scala.collection.mutable.ListBuffer#
      |  [42..45): Int => _root_.scala.Int#
      |_root_.i.E#`<init>`()V. => primaryctor <init>: ()i.E
      |  [4..5): E => _root_.i.E#
      |_root_.i.E#x()Lscala/collection/mutable/ListBuffer;. => def x: scala.collection.mutable.ListBuffer[scala.Int]
      |  [25..35): ListBuffer => _root_.scala.collection.mutable.ListBuffer#
      |  [42..45): Int => _root_.scala.Int#
      |_root_.i.a. => final object a
      |_root_.i.a.foo(Li/B;)Ljava/lang/Object;. => def foo: (implicit b: i.B)b.X
      |  [15..16): B => _root_.i.B#
      |  [17..18): b => _root_.i.a.foo(Li/B;)Ljava/lang/Object;.(b)
      |  [19..20): X => _root_.i.B#X#
      |_root_.i.a.foo(Li/B;)Ljava/lang/Object;.(b) => implicit param b: i.B
      |  [2..3): B => _root_.i.B#
      |_root_.i.a.x. => val x: scala.collection.mutable.ListBuffer[scala.Int]
      |  [25..35): ListBuffer => _root_.scala.collection.mutable.ListBuffer#
      |  [42..45): Int => _root_.scala.Int#
      |_root_.i.a.y. => val y: scala.collection.mutable.HashSet[scala.Int]
      |  [25..32): HashSet => _root_.scala.collection.mutable.HashSet#
      |  [39..42): Int => _root_.scala.Int#
      |_root_.java.lang.Object#`<init>`()V. => primaryctor <init>: ()java.lang.Object
      |  [12..18): Object => _root_.java.lang.Object#
      |_root_.scala. => package scala
      |_root_.scala.Int# => abstract final class Int
      |_root_.scala.Int#`<init>`()V. => primaryctor <init>: ()scala.Int
      |  [8..11): Int => _root_.scala.Int#
      |_root_.scala.collection. => package collection
      |_root_.scala.collection.generic.GenericCompanion#empty()Lscala/collection/GenTraversable;. => def empty: [A]CC[A]
      |  [3..5): CC => _root_.scala.collection.generic.GenericCompanion#[CC]
      |  [6..7): A => _root_.scala.collection.generic.GenericCompanion#empty()Lscala/collection/GenTraversable;.[A]
      |_root_.scala.collection.mutable. => package mutable
      |_root_.scala.collection.mutable.HashSet# => class HashSet
      |_root_.scala.collection.mutable.HashSet#`<init>`(Lscala/collection/mutable/FlatHashTable/Contents;)V. => private primaryctor <init>: (contents: scala.collection.mutable.FlatHashTable.Contents[A])scala.collection.mutable.HashSet[A]
      |  [36..49): FlatHashTable => _root_.scala.collection.mutable.FlatHashTable.
      |  [50..58): Contents => _root_.scala.collection.mutable.FlatHashTable.Contents#
      |  [59..60): A => _root_.scala.collection.mutable.HashSet#[A]
      |  [87..94): HashSet => _root_.scala.collection.mutable.HashSet#
      |  [95..96): A => _root_.scala.collection.mutable.HashSet#[A]
      |_root_.scala.collection.mutable.HashSet. => final object HashSet
      |_root_.scala.collection.mutable.HashSet.;_root_.scala.collection.mutable.HashSet# => val <import scala.collection.mutable.HashSet>: scala.collection.mutable.HashSet.type <and> scala.collection.mutable.HashSet
      |_root_.scala.collection.mutable.HashSet.empty()Lscala/collection/mutable/HashSet;. => def empty: [A]scala.collection.mutable.HashSet[A]
      |  [28..35): HashSet => _root_.scala.collection.mutable.HashSet#
      |  [36..37): A => _root_.scala.collection.mutable.HashSet.empty()Lscala/collection/mutable/HashSet;.[A]
      |_root_.scala.collection.mutable.ListBuffer# => final class ListBuffer
      |_root_.scala.collection.mutable.ListBuffer#`<init>`()V. => primaryctor <init>: ()scala.collection.mutable.ListBuffer[A]
      |  [27..37): ListBuffer => _root_.scala.collection.mutable.ListBuffer#
      |  [38..39): A => _root_.scala.collection.mutable.ListBuffer#[A]
      |_root_.scala.collection.mutable.ListBuffer. => final object ListBuffer
      |_root_.scala.collection.mutable.ListBuffer.;_root_.scala.collection.mutable.ListBuffer# => val <import scala.collection.mutable.ListBuffer>: scala.collection.mutable.ListBuffer.type <and> scala.collection.mutable.ListBuffer
    """.stripMargin.trim
  )

  sugars(
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
    """
      |_empty_.o. => final object o
      |_root_.scala.Int# => abstract final class Int
      |_root_.scala.Int#`<init>`()V. => primaryctor <init>: ()scala.Int
      |  [8..11): Int => _root_.scala.Int#
      |_root_.scala.collection.IterableLike#head()Ljava/lang/Object;. => def head: A
      |  [0..1): A => _root_.scala.collection.IterableLike#[A]
      |_root_.scala.collection.immutable.List. => final object List
      |_root_.scala.collection.immutable.List.newBuilder()Lscala/collection/mutable/Builder;. => def newBuilder: [A]scala.collection.mutable.Builder[A,scala.collection.immutable.List[A]]
      |  [28..35): Builder => _root_.scala.collection.mutable.Builder#
      |  [36..37): A => _root_.scala.collection.immutable.List.newBuilder()Lscala/collection/mutable/Builder;.[A]
      |  [65..69): List => _root_.scala.collection.immutable.List#
      |  [70..71): A => _root_.scala.collection.immutable.List.newBuilder()Lscala/collection/mutable/Builder;.[A]
      |_root_.scala.collection.mutable.Builder#result()Ljava/lang/Object;. => abstract def result: ()To
      |  [2..4): To => _root_.scala.collection.mutable.Builder#[To]
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

  sugars(
    """|object q {
       |  List(1) + "blaH"
       |}
    """.stripMargin,
    """|
       |[13..20): scala.Predef.any2stringadd[scala.collection.immutable.List[scala.Int]](*)
       |  [13..26): any2stringadd => _root_.scala.Predef.any2stringadd(Ljava/lang/Object;)Ljava/lang/Object;.
       |  [65..68): Int => _root_.scala.Int#
       |  [54..58): List => _root_.scala.collection.immutable.List#
       |  [71..72): * => _star_.
       |[17..17): *.apply[scala.Int]
       |  [0..1): * => _star_.
       |  [2..7): apply => _root_.scala.collection.immutable.List.apply(Lscala/collection/Seq;)Lscala/collection/immutable/List;.
       |  [14..17): Int => _root_.scala.Int#
       |""".stripMargin
  )

  sugars(
    """|object r {
       |  class F
       |  implicit val ordering: Ordering[F] = ???
       |  val x: Ordered[F] = new F
       |}
    """.stripMargin,
    """|[86..91): scala.math.Ordered.orderingToOrdered[r.F](*)(r.this.ordering)
       |  [19..36): orderingToOrdered => _root_.scala.math.Ordered.orderingToOrdered(Ljava/lang/Object;Lscala/math/Ordering;)Lscala/math/Ordered;.
       |  [39..40): F => _empty_.r.F#
       |  [42..43): * => _star_.
       |  [52..60): ordering => _empty_.r.ordering.
       |""".stripMargin
  )

  sugars(
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
      val denot = db.symbols.find(_.symbol == first).get.denot
      assert(first == Symbol("_empty_.u.unapply(Lu;)Lscala/Option;."))
      assertNoDiff(
        denot.toString,
        """case def unapply: (x$0: u)scala.Option[scala.Int]
          |  [6..7): u => _empty_.u#
          |  [14..20): Option => _root_.scala.Option#
          |  [27..30): Int => _root_.scala.Int#
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
      val denot1 = db.symbols.find(_.symbol == objectToString).get.denot
      val denot2 = db.symbols.find(_.symbol == listToString).get.denot
      assert(denot1.isJavaDefined)
      assert(!denot2.isJavaDefined)
    }
  )

  targeted(
    """
      |object a {
      |  val <<a>> = new java.lang.StringBuilder
      |  val <<b>> = new StringBuilder
      |  val <<c>>: Traversable[Int] = List(1)
      |  val <<d>> = Map(1 -> 2)
      |}
    """.stripMargin, { (db, a, b, c, d) =>
      def check(symbol: Symbol, info: String) = {
        assertNoDiff(db.symbols.find(_.symbol == symbol).get.denot.info, info)
      }
      check(a, "java.lang.StringBuilder")
      check(b, "scala.collection.mutable.StringBuilder")
      check(c, "scala.package.Traversable[scala.Int]")
      check(d, "scala.collection.immutable.Map[scala.Int,scala.Int]")
    }
  )

  names(
    """|
       |import scala.meta._
       |import org.scalatest._
       |object w extends FunSuite {
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
       |[51..52): w <= _empty_.w.
       |[61..69): FunSuite => _root_.org.scalatest.FunSuite#
       |[70..70): ε => _root_.org.scalatest.FunSuite#`<init>`()V.
       |[78..79): x <= _empty_.w.x.
       |[82..83): q => _root_.scala.meta.internal.quasiquotes.Unlift.
       |[95..96): y <= _empty_.w.y.
       |[99..100): q => _root_.scala.meta.internal.quasiquotes.Unlift.
       |[112..113): z <= _empty_.w.z.
       |[116..117): q => _root_.scala.meta.internal.quasiquotes.Unlift.
       |[119..120): x => _empty_.w.x.
       |[124..125): y => _empty_.w.y.
       |[133..134): k <= _empty_.w.k.
       |[137..147): sourcecode => _root_.sourcecode.
       |[148..152): Name => _root_.sourcecode.Name.
       |[153..161): generate => _root_.sourcecode.Name.generate()Lsourcecode/Name;.
       |[164..170): assert => _root_.org.scalatest.Assertions#assert(ZLorg/scalactic/Prettifier;Lorg/scalactic/source/Position;)Lorg/scalatest/compatible/Assertion;.
       |[171..172): x => _empty_.w.x.
       |[173..178): value => _root_.scala.meta.Term.Name#value()Ljava/lang/String;.
       |[179..181): == => _root_.java.lang.Object#`==`(Ljava/lang/Object;)Z.
       |""".stripMargin
  )

}
