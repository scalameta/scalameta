package scala.meta.tests
package scalahost

import scala.meta._
import scala.meta.internal.semantic.SemanticdbMode

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
    |[7..8): A => _empty_.A.
    |[17..21): main => _empty_.A.main([Ljava/lang/String;)V.
    |[22..26): args => _empty_.A.main([Ljava/lang/String;)V.(args)
    |[28..33): Array => _root_.scala.Array#
    |[34..40): String => _root_.scala.Predef.String#
    |[44..48): Unit => _root_.scala.Unit#
    |[61..65): list => <...>@57..81
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
  """.trim.stripMargin, { implicit database => second =>
      assert(second.symbol === Symbol("_empty_.B."))
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
    |[32..33): C => _empty_.C#
    |[34..34): ε => _empty_.C#`<init>`()V.
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
    """.trim.stripMargin, { implicit database => second =>
      assert(second.symbol === Symbol("_empty_.D.bar(II)I."))
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
    """.trim.stripMargin, { implicit database => (copy, age) =>
      assert(copy.symbol === Symbol("_root_.e.User#copy(Ljava/lang/String;I)Le/User;."))
      assert(age.symbol === Symbol("_root_.e.User#copy(Ljava/lang/String;I)Le/User;.(age)"))
    }
  )

  denotations(
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
      |_root_.f.C1#(p1) => param p1: Int
      |_root_.f.C1#(p2) => val param p2: Int
      |_root_.f.C1#(p3) => var param p3_=: (x$1: Int)Unit
      |_root_.f.C1#T1# => abstract type T1:  <: Int
      |_root_.f.C1#T2# => type T2: Int
      |_root_.f.C1#`<init>`()V. => secondaryctor <init>: ()f.C1
      |_root_.f.C1#`<init>`(III)V. => primaryctor <init>: (p1: Int, p2: Int, p3: Int)f.C1
      |_root_.f.C1#f1. => val f1: Nothing
      |_root_.f.C1#f1.l1. => val l1: Nothing
      |_root_.f.C1#f1.l2. => var l2: Nothing
      |_root_.f.C1#f2. => var f2_=: (x$1: Nothing)Unit
      |_root_.f.C1#m1(I)I. => def m1: [T](x: Int)Int
      |_root_.f.C1#m1(I)I.(x) => param x: Int
      |_root_.f.C1#m1(I)I.T# => typeparam T
      |_root_.f.C1#m2()Lscala/Nothing;. => macro m2: Nothing
      |_root_.f.C2# => abstract class C2
      |_root_.f.C2#`<init>`()V. => primaryctor <init>: ()f.C2
      |_root_.f.C2#m3()I. => abstract def m3: Int
      |_root_.f.C2#m4()Lscala/Nothing;. => final def m4: Nothing
      |_root_.f.C3# => sealed class C3
      |_root_.f.C3#`<init>`()V. => primaryctor <init>: ()f.C3
      |_root_.f.C3#m3()I. => def m3: Int
      |_root_.f.C3#toString()Ljava/lang/String;. => def toString: ()String
      |_root_.f.M. => final object M
      |_root_.f.M.C1# => case class C1
      |_root_.f.M.C1#`<init>`()V. => primaryctor <init>: ()f.M.C1
      |_root_.f.M.C2# => class C2
      |_root_.f.M.C2#[T] => covariant typeparam T
      |_root_.f.M.C2#[U] => contravariant typeparam U
      |_root_.f.M.C2#`<init>`()V. => primaryctor <init>: ()f.M.C2[T,U]
      |_root_.f.M.i1()Lscala/Nothing;. => implicit def i1: Nothing
      |_root_.f.M.l1. => lazy val l1: Nothing
      |_root_.f.T# => trait T
      |_root_.f.T#$init$()V. => primaryctor $init$: ()Unit
      |_root_.f.T#f1. => private val f1: Nothing
      |_root_.f.T#f2. => private val f2: Nothing
      |_root_.f.T#f3. => private val f3: Nothing
      |_root_.f.T#f4. => protected var f4_=: (x$1: Nothing)Unit
      |_root_.f.T#f5. => protected var f5_=: (x$1: Nothing)Unit
      |_root_.f.T#f6. => protected var f6_=: (x$1: Nothing)Unit
      |_root_.scala. => package scala
      |_root_.scala.Int# => abstract final class Int
      |_root_.scala.Int#`<init>`()V. => primaryctor <init>: ()Int
      |_root_.scala.Predef.`???`()Lscala/Nothing;. => def ???: Nothing
      |_root_.scala.language. => final object language
      |_root_.scala.language.experimental. => final object experimental
      |_root_.scala.language.experimental.macros. => implicit lazy val macros: languageFeature.experimental.macros
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
      |[201..201): *[Int, List[Int]]
      |  [0..1): * => _star_.
      |[209..209): *(scala.collection.immutable.List.canBuildFrom[Int])
      |  [0..1): * => _star_.
      |  [34..46): canBuildFrom => _root_.scala.collection.immutable.List.canBuildFrom()Lscala/collection/generic/CanBuildFrom;.
      |[247..247): *(h.C.list[Int](h.C.int))
      |  [0..1): * => _star_.
      |  [6..10): list => _root_.h.C.list(Lh/C;)Lh/C;.
      |  [20..23): int => _root_.h.C.int()Lh/C;.
      |[273..275): h.X.cvt[Int](*)(h.C.int)
      |  [4..7): cvt => _root_.h.X.cvt(Ljava/lang/Object;Lh/C;)Lh/X;.
      |  [13..14): * => _star_.
      |  [20..23): int => _root_.h.C.int()Lh/C;.
      |[304..304): *[h.C[Int]]
      |  [0..1): * => _star_.
  """.trim.stripMargin
  )

  denotations(
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
      |  def foo(b: B): b.X = {
      |    val result = b.x
      |    result
      |  }
      |}
    """.stripMargin,
    """
      |<...>@350..366 => val result: b.X
      |_root_.i. => package i
      |_root_.i.B# => trait B
      |_root_.i.B#X# => abstract type X
      |_root_.i.B#x()Ljava/lang/Object;. => abstract def x: B.this.X
      |_root_.i.D# => class D
      |_root_.i.D#X# => type X: scala.collection.mutable.HashSet[Int]
      |_root_.i.D#`<init>`()V. => primaryctor <init>: ()i.D
      |_root_.i.D#x()Lscala/collection/mutable/HashSet;. => def x: scala.collection.mutable.HashSet[Int]
      |_root_.i.E# => class E
      |_root_.i.E#X# => type X: scala.collection.mutable.ListBuffer[Int]
      |_root_.i.E#`<init>`()V. => primaryctor <init>: ()i.E
      |_root_.i.E#x()Lscala/collection/mutable/ListBuffer;. => def x: scala.collection.mutable.ListBuffer[Int]
      |_root_.i.a. => final object a
      |_root_.i.a.foo(Li/B;)Ljava/lang/Object;. => def foo: (b: i.B)b.X
      |_root_.i.a.foo(Li/B;)Ljava/lang/Object;.(b) => param b: i.B
      |_root_.i.a.x. => val x: scala.collection.mutable.ListBuffer[Int]
      |_root_.i.a.y. => val y: scala.collection.mutable.HashSet[Int]
      |_root_.java.lang.Object#`<init>`()V. => primaryctor <init>: ()Object
      |_root_.scala. => package scala
      |_root_.scala.Int# => abstract final class Int
      |_root_.scala.Int#`<init>`()V. => primaryctor <init>: ()Int
      |_root_.scala.collection. => package collection
      |_root_.scala.collection.generic.GenericCompanion#empty()Lscala/collection/GenTraversable;. => def empty: [A]=> CC[A]
      |_root_.scala.collection.mutable. => package mutable
      |_root_.scala.collection.mutable.HashSet# => class HashSet
      |_root_.scala.collection.mutable.HashSet#`<init>`(Lscala/collection/mutable/FlatHashTable/Contents;)V. => private primaryctor <init>: (contents: scala.collection.mutable.FlatHashTable.Contents[A])scala.collection.mutable.HashSet[A]
      |_root_.scala.collection.mutable.HashSet. => final object HashSet
      |_root_.scala.collection.mutable.HashSet.;_root_.scala.collection.mutable.HashSet# => val <import scala.collection.mutable.HashSet>: scala.collection.mutable.HashSet.type <and> scala.collection.mutable.HashSet
      |_root_.scala.collection.mutable.HashSet.empty()Lscala/collection/mutable/HashSet;. => def empty: [A]=> scala.collection.mutable.HashSet[A]
      |_root_.scala.collection.mutable.ListBuffer# => final class ListBuffer
      |_root_.scala.collection.mutable.ListBuffer#`<init>`()V. => primaryctor <init>: ()scala.collection.mutable.ListBuffer[A]
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
      |[9..10): k => _root_.k.
      |[18..21): tup => _root_.k.tup.
      |[30..33): foo => _root_.k.tup.foo.
      |[37..38): a => _root_.k.tup.foo.$anonfun.(a)
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
      |[9..10): m => _root_.m.
      |[17..18): C => _root_.m.C#
      |[18..18): ε => _root_.m.C#`<init>`(I)V.
      |[19..20): x => _root_.m.C#(x)
      |[22..25): Int => _root_.scala.Int#
      |[35..39): this => _root_.m.C#`<init>`()V.
      |[48..48): ε => _root_.m.C#`<init>`(I)V.
      |[62..63): M => _root_.m.M.
      |[72..74): c0 => _root_.m.M.c0.
      |[81..82): C => _root_.m.C#
      |[82..82): ε => _root_.m.C#`<init>`()V.
      |[91..93): c1 => _root_.m.M.c1.
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
    """|[7..8): n => _empty_.n.
       |[17..21): Name => _empty_.n.Name.
       |[36..37): r => _root_.scala.collection.immutable.StringLike#r()Lscala/util/matching/Regex;.
       |[44..45): x => _empty_.n.x$1.x.
       |[46..49): #:: => _root_.scala.package.`#::`.
       |[50..52): xs => _empty_.n.x$1.xs.
       |[55..61): Stream => _root_.scala.package.Stream.
       |[75..79): Name => _empty_.n.Name.
       |[80..84): name => _empty_.n.name.name.
       |[103..106): #:: => _root_.scala.collection.immutable.Stream.ConsWrapper#`#::`(Ljava/lang/Object;)Lscala/collection/immutable/Stream;.
       |[109..112): #:: => _root_.scala.collection.immutable.Stream.ConsWrapper#`#::`(Ljava/lang/Object;)Lscala/collection/immutable/Stream;.
       |[113..119): Stream => _root_.scala.package.Stream.
       |[120..125): empty => _root_.scala.collection.immutable.Stream.empty()Lscala/collection/immutable/Stream;.
       |""".stripMargin
  )

  denotations(
    """
      |object o {
      |  List.newBuilder[Int].result
      |  List(1).head
      |}""".stripMargin,
    """_empty_.o. => final object o
      |_root_.scala.Int# => abstract final class Int
      |_root_.scala.Int#`<init>`()V. => primaryctor <init>: ()Int
      |_root_.scala.collection.IterableLike#head()Ljava/lang/Object;. => def head: A
      |_root_.scala.collection.immutable.List. => final object List
      |_root_.scala.collection.immutable.List.newBuilder()Lscala/collection/mutable/Builder;. => def newBuilder: [A]=> scala.collection.mutable.Builder[A,List[A]]
      |_root_.scala.collection.mutable.Builder#result()Ljava/lang/Object;. => abstract def result: ()To
    """.stripMargin.trim
  )

  names(
    """|object p {
       |  val lst = 1 #:: 2 #:: Stream.empty
       |  lst + "foo"
       |}
    """.stripMargin,
    """|[7..8): p => _empty_.p.
       |[17..20): lst => _empty_.p.lst.
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
    """|[13..20): scala.Predef.any2stringadd[List[Int]](*)
       |  [13..26): any2stringadd => _root_.scala.Predef.any2stringadd(Ljava/lang/Object;)Ljava/lang/Object;.
       |  [38..39): * => _star_.
       |[17..17): *.apply[Int]
       |  [0..1): * => _star_.
       |  [2..7): apply => _root_.scala.collection.immutable.List.apply(Lscala/collection/Seq;)Lscala/collection/immutable/List;.
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
    "case class u(a: Int); object ya { u.<<unapply>>(u(2)) }", { implicit database => first =>
      assert(first.symbol == Symbol("_empty_.u.unapply(Lu;)Lscala/Option;."))
      assert(first.symbol.denot.toString == "case def unapply: (x$0: u)Option[Int]")
    }
  )

  targeted(
    """
    object v {
      new Object().<<toString>>
      List(1).<<toString>>
    }
    """, { implicit db => (objectToString, listToString) =>
      assert(objectToString.symbol.denot.isJavaDefined)
      assert(!listToString.symbol.denot.isJavaDefined)
    }
  )

}
