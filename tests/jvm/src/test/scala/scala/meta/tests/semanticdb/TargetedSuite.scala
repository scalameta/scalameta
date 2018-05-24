package scala.meta.tests
package semanticdb

import scala.meta._
import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.scalac._
import scala.meta.internal.semanticdb3._
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import Compat._

// Contributing tips:
// - Create another suite like YYY.scala that extends SemanticdbSuite,
//   add YYY.scala to your .gitignore, and run `> ~testsJVM/testOnly *YYY`.
//   That should give you a tight edit/run/debug cycle.
// - On test failure, the obtained output is printed to the console for
//   easy copy-paste to replace the current expected output.
// - Try to follow the alphabetical order of the enclosing object, at the time
//   of this writing the latest object is `object ad`, so the next object should
//   be `object ae`.
// - glhf, and if you have any questions don't hesitate to ask in the gitter channel :)
class TargetedSuite extends SemanticdbSuite() {
  occurrences(
    """
    |object A {
    |  def main(args: Array[String]): Unit = {
    |    val list = List(1, 2, 3)
    |    println(list)
    |  }
    |}
  """.trim.stripMargin,
    """|[0:7..0:8): A <= _empty_.A.
       |[1:6..1:10): main <= _empty_.A.main(Array).
       |[1:11..1:15): args <= _empty_.A.main(Array).(args)
       |[1:17..1:22): Array => scala.Array#
       |[1:23..1:29): String => scala.Predef.String#
       |[1:33..1:37): Unit => scala.Unit#
       |[2:8..2:12): list <= local0
       |[2:15..2:19): List => scala.collection.immutable.List.
       |[3:4..3:11): println => scala.Predef.println(Any).
       |[3:12..3:16): list => local0
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
      assert(second === "_empty_.B.")
    }
  )

  occurrences(
    """
    |import _root_.scala.List
    |
    |class C {
    |  _root_.scala.List
    |}
  """.trim.stripMargin,
    """|[0:7..0:13): _root_ => _root_.
       |[0:14..0:19): scala => scala.
       |[0:20..0:24): List => scala.package.List#
       |[0:20..0:24): List => scala.package.List().
       |[2:6..2:7): C <= _empty_.C#
       |[2:8..2:8):  <= _empty_.C#`<init>`().
       |[3:2..3:8): _root_ => _root_.
       |[3:9..3:14): scala => scala.
       |[3:15..3:19): List => scala.collection.immutable.
    """.trim.stripMargin
  )

  targeted(
    // curried function application with named args, #648
    """
      |object D {
      |  def bar(children: Int)(x: Int) = children + x
      |  <<bar>>(children = 4)(3)
      |}
    """.trim.stripMargin, { (_, second) =>
      assert(second === "_empty_.D.bar(Int,Int).")
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
    """.trim.stripMargin, { (_, copy, age) =>
      assert(copy === "e.User#copy(String,Int).")
      assert(age === "e.User#copy(String,Int).(age)")
    }
  )

  // TODO: Disabled under Scala 2.11 because of:
  // https://github.com/scalameta/scalameta/issues/1328.
  if (scala.util.Properties.versionNumberString.startsWith("2.12")) {
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
      """|F.package. => package object package
       |f. => package f
       |f.C1# => class C1.{+17 decls}
       |  extends AnyRef
       |f.C1#T1# => abstract type T1: >: Nothing <: Int
       |  Nothing => scala.Nothing#
       |  Int => scala.Int#
       |f.C1#T2# => type T2: >: Int <: Int
       |  Int => scala.Int#
       |f.C1#`<init>`(). => ctor <init>: (): C1
       |  C1 => f.C1#
       |f.C1#`<init>`(Int,Int,Int). => primary ctor <init>: (p1: Int, val p2: Int, var p3: Int): C1
       |  p1 => f.C1#`<init>`(Int,Int,Int).(p1)
       |  Int => scala.Int#
       |  p2 => f.C1#`<init>`(Int,Int,Int).(p2)
       |  p3 => f.C1#`<init>`(Int,Int,Int).(p3)
       |  C1 => f.C1#
       |f.C1#`<init>`(Int,Int,Int).(p1) => param p1: Int
       |  Int => scala.Int#
       |f.C1#`<init>`(Int,Int,Int).(p2) => val param p2: Int
       |  Int => scala.Int#
       |f.C1#`<init>`(Int,Int,Int).(p3) => var param p3: Int
       |  Int => scala.Int#
       |f.C1#`f2_=`(Nothing). => var method f2_=: (x$1: Nothing): Unit
       |  x$1 => f.C1#`f2_=`(Nothing).(x$1)
       |  Nothing => scala.Nothing#
       |  Unit => scala.Unit#
       |f.C1#`f2_=`(Nothing).(x$1) => synthetic param x$1: Nothing
       |  Nothing => scala.Nothing#
       |f.C1#`p3_=`(Int). => var method p3_=: (x$1: Int): Unit
       |  x$1 => f.C1#`p3_=`(Int).(x$1)
       |  Int => scala.Int#
       |  Unit => scala.Unit#
       |f.C1#`p3_=`(Int).(x$1) => synthetic param x$1: Int
       |  Int => scala.Int#
       |f.C1#f1(). => val method f1: : Nothing
       |  Nothing => scala.Nothing#
       |f.C1#f1. => private[this] val field f1: Nothing
       |  Nothing => scala.Nothing#
       |f.C1#f1.l1. => val local l1: Nothing
       |  Nothing => scala.Nothing#
       |f.C1#f1.l2. => var local l2: Nothing
       |  Nothing => scala.Nothing#
       |f.C1#f2(). => var method f2: : Nothing
       |  Nothing => scala.Nothing#
       |f.C1#f2. => private[this] var field f2: Nothing
       |  Nothing => scala.Nothing#
       |f.C1#m1(Int). => method m1: [T >: Nothing <: Any] => (x: Int): Int
       |  T => f.C1#m1(Int).[T]
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |  x => f.C1#m1(Int).(x)
       |  Int => scala.Int#
       |f.C1#m1(Int).(x) => param x: Int
       |  Int => scala.Int#
       |f.C1#m1(Int).[T] => typeparam T: >: Nothing <: Any
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |f.C1#m2(). => macro m2: : Nothing
       |  Nothing => scala.Nothing#
       |f.C1#p1. => private[this] val field p1: Int
       |  Int => scala.Int#
       |f.C1#p2(). => val method p2: : Int
       |  Int => scala.Int#
       |f.C1#p2. => private[this] val field p2: Int
       |  Int => scala.Int#
       |f.C1#p3(). => var method p3: : Int
       |  Int => scala.Int#
       |f.C1#p3. => private[this] var field p3: Int
       |  Int => scala.Int#
       |f.C2# => abstract class C2.{+3 decls}
       |  extends AnyRef
       |f.C2#`<init>`(). => primary ctor <init>: (): C2
       |  C2 => f.C2#
       |f.C2#m3(). => abstract method m3: : Int
       |  Int => scala.Int#
       |f.C2#m4(). => final method m4: : Nothing
       |  Nothing => scala.Nothing#
       |f.C3# => sealed class C3.{+3 decls}
       |  extends C2
       |f.C3#`<init>`(). => primary ctor <init>: (): C3
       |  C3 => f.C3#
       |f.C3#m3(). => method m3: : Int
       |  Int => scala.Int#
       |f.C3#toString(). => method toString: (): String
       |  String => java.lang.String#
       |f.M. => final object M
       |f.M.C1# => case class C1.{+10 decls}
       |  extends AnyRef
       |  extends Product
       |  extends Serializable
       |f.M.C1#`<init>`(). => primary ctor <init>: (): C1
       |  C1 => f.M.C1#
       |f.M.C1#canEqual(Any). => synthetic method canEqual: (x$1: Any): Boolean
       |  x$1 => f.M.C1#canEqual(Any).(x$1)
       |  Any => scala.Any#
       |  Boolean => scala.Boolean#
       |f.M.C1#canEqual(Any).(x$1) => synthetic param x$1: Any
       |  Any => scala.Any#
       |f.M.C1#copy(). => synthetic method copy: (): C1
       |  C1 => f.M.C1#
       |f.M.C1#equals(Any). => synthetic method equals: (x$1: Any): Boolean
       |  x$1 => f.M.C1#equals(Any).(x$1)
       |  Any => scala.Any#
       |  Boolean => scala.Boolean#
       |f.M.C1#equals(Any).(x$1) => synthetic param x$1: Any
       |  Any => scala.Any#
       |f.M.C1#hashCode(). => synthetic method hashCode: (): Int
       |  Int => scala.Int#
       |f.M.C1#productArity(). => synthetic method productArity: : Int
       |  Int => scala.Int#
       |f.M.C1#productElement(Int). => synthetic method productElement: (x$1: Int): Any
       |  x$1 => f.M.C1#productElement(Int).(x$1)
       |  Int => scala.Int#
       |  Any => scala.Any#
       |f.M.C1#productElement(Int).(x$1) => synthetic param x$1: Int
       |  Int => scala.Int#
       |f.M.C1#productIterator(). => synthetic method productIterator: : Iterator[Any]
       |  Iterator => scala.collection.Iterator#
       |  Any => scala.Any#
       |f.M.C1#productPrefix(). => synthetic method productPrefix: : String
       |  String => java.lang.String#
       |f.M.C1#toString(). => synthetic method toString: (): String
       |  String => java.lang.String#
       |f.M.C1. => final synthetic object C1
       |f.M.C1.apply(). => case synthetic method apply: (): C1
       |  C1 => f.M.C1#
       |f.M.C1.readResolve(). => private synthetic method readResolve: (): Object
       |  Object => java.lang.Object#
       |f.M.C1.toString(). => final synthetic method toString: (): String
       |  String => java.lang.String#
       |f.M.C1.unapply(C1). => case synthetic method unapply: (x$0: C1): Boolean
       |  x$0 => f.M.C1.unapply(C1).(x$0)
       |  C1 => f.M.C1#
       |  Boolean => scala.Boolean#
       |f.M.C1.unapply(C1).(x$0) => synthetic param x$0: C1
       |  C1 => f.M.C1#
       |f.M.C2# => class C2[+T >: Nothing <: Any, -U >: Nothing <: Any].{+1 decls}
       |  extends AnyRef
       |f.M.C2#[T] => covariant typeparam T: >: Nothing <: Any
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |f.M.C2#[U] => contravariant typeparam U: >: Nothing <: Any
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |f.M.C2#`<init>`(). => primary ctor <init>: (): C2[T, U]
       |  C2 => f.M.C2#
       |  T => f.M.C2#[T]
       |  U => f.M.C2#[U]
       |f.M.i1(). => implicit method i1: : Nothing
       |  Nothing => scala.Nothing#
       |f.M.l1(). => lazy val field l1: Nothing
       |  Nothing => scala.Nothing#
       |f.T# => trait T.{+10 decls}
       |  extends AnyRef
       |f.T#$init$(). => primary ctor $init$: (): Unit
       |  Unit => scala.Unit#
       |f.T#`f4_=`(Nothing). => protected var method f4_=: (x$1: Nothing): Unit
       |  x$1 => f.T#`f4_=`(Nothing).(x$1)
       |  Nothing => scala.Nothing#
       |  Unit => scala.Unit#
       |f.T#`f4_=`(Nothing).(x$1) => synthetic param x$1: Nothing
       |  Nothing => scala.Nothing#
       |f.T#`f5_=`(Nothing). => protected[this] var method f5_=: (x$1: Nothing): Unit
       |  x$1 => f.T#`f5_=`(Nothing).(x$1)
       |  Nothing => scala.Nothing#
       |  Unit => scala.Unit#
       |f.T#`f5_=`(Nothing).(x$1) => synthetic param x$1: Nothing
       |  Nothing => scala.Nothing#
       |f.T#`f6_=`(Nothing). => protected[f] var method f6_=: (x$1: Nothing): Unit
       |  x$1 => f.T#`f6_=`(Nothing).(x$1)
       |  Nothing => scala.Nothing#
       |  Unit => scala.Unit#
       |f.T#`f6_=`(Nothing).(x$1) => synthetic param x$1: Nothing
       |  Nothing => scala.Nothing#
       |f.T#f1(). => private val method f1: : Nothing
       |  Nothing => scala.Nothing#
       |f.T#f2(). => private[this] val method f2: : Nothing
       |  Nothing => scala.Nothing#
       |f.T#f3(). => private[f] val method f3: : Nothing
       |  Nothing => scala.Nothing#
       |f.T#f4(). => protected var method f4: : Nothing
       |  Nothing => scala.Nothing#
       |f.T#f5(). => protected[this] var method f5: : Nothing
       |  Nothing => scala.Nothing#
       |f.T#f6(). => protected[f] var method f6: : Nothing
       |  Nothing => scala.Nothing#
       |scala. => package scala
       |scala.Int# => abstract final class Int.{+111 decls}
       |  extends AnyVal
       |scala.Predef.`???`(). => method ???: : Nothing
       |  Nothing => scala.Nothing#
       |scala.language. => final object language
       |scala.language.experimental. => final object experimental
       |scala.language.experimental.macros(). => implicit lazy val field macros: macros
       |  macros => scala.languageFeature.experimental.macros#
     """.trim.stripMargin
    )
  }

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
    """|i. => package i
       |i.B# => trait B.{+2 decls}
       |  extends AnyRef
       |i.B#X# => abstract type X: >: Nothing <: Any
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |i.B#x(). => abstract method x: : B.this.X
       |  B => i.B#
       |  X => i.B#X#
       |i.D# => class D.{+3 decls}
       |  extends AnyRef
       |  extends B
       |i.D#X# => type X: >: HashSet[Int] <: HashSet[Int]
       |  HashSet => scala.collection.mutable.HashSet#
       |  Int => scala.Int#
       |i.D#`<init>`(). => primary ctor <init>: (): D
       |  D => i.D#
       |i.D#x(). => method x: : HashSet[Int]
       |  HashSet => scala.collection.mutable.HashSet#
       |  Int => scala.Int#
       |i.E# => class E.{+3 decls}
       |  extends AnyRef
       |  extends B
       |i.E#X# => type X: >: ListBuffer[Int] <: ListBuffer[Int]
       |  ListBuffer => scala.collection.mutable.ListBuffer#
       |  Int => scala.Int#
       |i.E#`<init>`(). => primary ctor <init>: (): E
       |  E => i.E#
       |i.E#x(). => method x: : ListBuffer[Int]
       |  ListBuffer => scala.collection.mutable.ListBuffer#
       |  Int => scala.Int#
       |i.a. => final object a
       |i.a.foo(B). => method foo: (b: B): b.X
       |  b => i.a.foo(B).(b)
       |  B => i.B#
       |  X => i.B#X#
       |i.a.foo(B).(b) => implicit param b: B
       |  B => i.B#
       |i.a.x(). => val method x: : ListBuffer[Int]
       |  ListBuffer => scala.collection.mutable.ListBuffer#
       |  Int => scala.Int#
       |i.a.x. => private[this] val field x: ListBuffer[Int]
       |  ListBuffer => scala.collection.mutable.ListBuffer#
       |  Int => scala.Int#
       |i.a.y(). => val method y: : HashSet[Int]
       |  HashSet => scala.collection.mutable.HashSet#
       |  Int => scala.Int#
       |i.a.y. => private[this] val field y: HashSet[Int]
       |  HashSet => scala.collection.mutable.HashSet#
       |  Int => scala.Int#
       |java.lang.Object#`<init>`(). => ctor <init>: (): Object
       |  Object => java.lang.Object#
       |local0 => val local result: b.X
       |  b => i.a.foo(B).(b)
       |  X => i.B#X#
       |scala. => package scala
       |scala.Int# => abstract final class Int.{+111 decls}
       |  extends AnyVal
       |scala.collection. => package collection
       |scala.collection.generic.GenericCompanion#empty(). => method empty: [A: <?>] => : CC[A]
       |  A => scala.collection.generic.GenericCompanion#empty().[A]
       |  CC => scala.collection.generic.GenericCompanion#[CC]
       |scala.collection.mutable. => package mutable
       |scala.collection.mutable.HashSet# => @SerialVersionUID class HashSet[A: <?>].{+18 decls}
       |  extends AbstractSet[A]
       |  extends Set[A]
       |  extends GenericSetTemplate[A, HashSet]
       |  extends SetLike[A, HashSet[A]]
       |  extends FlatHashTable[A]
       |  extends CustomParallelizable[A, ParHashSet[A]]
       |  extends Serializable
       |scala.collection.mutable.HashSet. => final object HashSet
       |scala.collection.mutable.HashSet.empty(). => method empty: [A: <?>] => : HashSet[A]
       |  A => scala.collection.mutable.HashSet.empty().[A]
       |  HashSet => scala.collection.mutable.HashSet#
       |scala.collection.mutable.ListBuffer# => @SerialVersionUID final class ListBuffer[A: <?>].{+43 decls}
       |  extends AbstractBuffer[A]
       |  extends Buffer[A]
       |  extends GenericTraversableTemplate[A, ListBuffer]
       |  extends BufferLike[A, ListBuffer[A]]
       |  extends ReusableBuilder[A, List[A]]
       |  extends SeqForwarder[A]
       |  extends Serializable
       |scala.collection.mutable.ListBuffer. => final object ListBuffer
    """.stripMargin.trim
      .replaceAllLiterally(
        ListBufferDeclsInString,
        ListBufferDeclsActual
      )
      .replaceAllLiterally(
        ReusableBuilderInString,
        ReusableBuilderActual
      )
  )

  occurrences(
    s"""
       |package k
       |object tup {
       |  val foo = (a: (Int, Boolean)) => 1
       |  foo(2, true)
       |  foo.apply(2, true)
       |}
    """.stripMargin,
    """|[1:8..1:9): k <= k.
       |[2:7..2:10): tup <= k.tup.
       |[3:6..3:9): foo <= k.tup.foo().
       |[3:13..3:14): a <= k.tup.foo.$anonfun.(a)
       |[3:17..3:20): Int => scala.Int#
       |[3:22..3:29): Boolean => scala.Boolean#
       |[4:2..4:5): foo => k.tup.foo().
       |[5:2..5:5): foo => k.tup.foo().
       |[5:6..5:11): apply => scala.Function1#apply(T1).
    """.stripMargin.trim
  )

  diagnostics(
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
    """|[1:48..1:51)[warning] Unused import
       |[1:53..1:56)[warning] Unused import
       |[2:24..2:25)[warning] Unused import
       |[2:56..2:62)[warning] Unused import
       |[3:39..3:46)[warning] Unused import
    """.stripMargin.trim
  )

  occurrences(
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
    """|[1:8..1:9): m <= m.
       |[2:6..2:7): C <= m.C#
       |[2:7..2:7):  <= m.C#`<init>`(Int).
       |[2:8..2:9): x <= m.C#x.
       |[2:11..2:14): Int => scala.Int#
       |[3:6..3:10): this <= m.C#`<init>`().
       |[3:19..3:19):  => m.C#`<init>`(Int).
       |[6:7..6:8): M <= m.M.
       |[7:6..7:8): c0 <= m.M.c0().
       |[7:15..7:16): C => m.C#
       |[7:16..7:16):  => m.C#`<init>`().
       |[8:6..8:8): c1 <= m.M.c1().
       |[8:15..8:16): C => m.C#
       |[8:16..8:16):  => m.C#`<init>`(Int).
    """.stripMargin.trim
  )

  occurrences(
    // See https://github.com/scalameta/scalameta/issues/977
    """|object n {
       |  val Name = "name:(.*)".r
       |  val x #:: xs = Stream(1, 2);
       |  val Name(name) = "name:foo"
       |  1 #:: 2 #:: Stream.empty
       |}""".stripMargin,
    """|[0:7..0:8): n <= _empty_.n.
       |[1:6..1:10): Name <= _empty_.n.Name().
       |[1:25..1:26): r => scala.collection.immutable.StringLike#r().
       |[2:6..2:7): x <= _empty_.n.x$1.x.
       |[2:8..2:11): #:: => scala.package.`#::`().
       |[2:12..2:14): xs <= _empty_.n.x$1.xs.
       |[2:17..2:23): Stream => scala.package.Stream().
       |[3:6..3:10): Name => _empty_.n.Name().
       |[3:11..3:15): name <= _empty_.n.name.name.
       |[4:4..4:7): #:: => scala.collection.immutable.Stream.ConsWrapper#`#::`(B).
       |[4:10..4:13): #:: => scala.collection.immutable.Stream.ConsWrapper#`#::`(B).
       |[4:14..4:20): Stream => scala.package.Stream().
       |[4:21..4:26): empty => scala.collection.immutable.Stream.empty().
       |""".stripMargin.replaceAllLiterally(ConsWrapperInString, ConsWrapperActual)
  )

  symbols(
    """object o {
      |  List.newBuilder[Int].result
      |  List(1).head
      |}""".stripMargin,
    """|_empty_.o. => final object o
       |scala.Int# => abstract final class Int.{+111 decls}
       |  extends AnyVal
       |scala.collection.IterableLike#head(). => method head: : A
       |  A => scala.collection.IterableLike#[A]
       |scala.collection.immutable.List. => final object List
       |scala.collection.immutable.List.newBuilder(). => method newBuilder: [A: <?>] => : Builder[A, List[A]]
       |  A => scala.collection.immutable.List.newBuilder().[A]
       |  Builder => scala.collection.mutable.Builder#
       |  List => scala.collection.immutable.List#
       |scala.collection.mutable.Builder#result(). => abstract method result: (): To
       |  To => scala.collection.mutable.Builder#[To]
    """.stripMargin.trim
  )

  occurrences(
    """|object p {
       |  val lst = 1 #:: 2 #:: Stream.empty
       |  lst + "foo"
       |}
    """.stripMargin,
    """|[0:7..0:8): p <= _empty_.p.
       |[1:6..1:9): lst <= _empty_.p.lst().
       |[1:14..1:17): #:: => scala.collection.immutable.Stream.ConsWrapper#`#::`(B).
       |[1:20..1:23): #:: => scala.collection.immutable.Stream.ConsWrapper#`#::`(B).
       |[1:24..1:30): Stream => scala.package.Stream().
       |[1:31..1:36): empty => scala.collection.immutable.Stream.empty().
       |[2:2..2:5): lst => _empty_.p.lst().
       |[2:6..2:7): + => scala.Predef.any2stringadd#`+`(String).
       |""".stripMargin.replaceAllLiterally(ConsWrapperInString, ConsWrapperActual)
  )

  diagnostics(
    // See https://github.com/scalameta/scalameta/issues/899
    """import scala.io._
      |object t""".stripMargin,
    "[0:16..0:17)[warning] Unused import"
  )

  targeted(
    // See https://github.com/scalameta/scalameta/issues/830
    "case class u(a: Int); object ya { u.<<unapply>>(u(2)) }", { (db, first) =>
      val denotation = db.symbols.find(_.symbol == first).get
      assert(first == "_empty_.u.unapply(u).")
    }
  )

  targeted(
    """
    object v {
      new Object().<<toString>>
      List(1).<<toString>>
    }
    """, { (db, objectToString, listToString) =>
      val denotation1 = db.symbols.find(_.symbol == objectToString).get
      val denotation2 = db.symbols.find(_.symbol == listToString).get
      assert(denotation1.language.isJava)
      assert(!denotation2.language.isJava)
    }
  )

  occurrences(
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
    """|[1:7..1:12): scala => scala.
       |[1:13..1:17): meta => scala.meta.
       |[2:7..2:10): org => org.
       |[2:11..2:20): scalatest => org.scalatest.
       |[3:7..3:8): x <= _empty_.x.
       |[3:17..3:25): FunSuite => org.scalatest.FunSuite#
       |[3:26..3:26):  => org.scalatest.FunSuite#`<init>`().
       |[4:6..4:7): x <= _empty_.x.x().
       |[4:10..4:11): q => scala.meta.internal.quasiquotes.Unlift.
       |[5:6..5:7): y <= _empty_.x.y().
       |[5:10..5:11): q => scala.meta.internal.quasiquotes.Unlift.
       |[6:6..6:7): z <= _empty_.x.z().
       |[6:10..6:11): q => scala.meta.internal.quasiquotes.Unlift.
       |[6:13..6:14): x => _empty_.x.x().
       |[6:18..6:19): y => _empty_.x.y().
       |[7:6..7:7): k <= _empty_.x.k().
       |[7:10..7:20): sourcecode => sourcecode.
       |[7:21..7:25): Name => sourcecode.Name.
       |[7:26..7:34): generate => sourcecode.Name.generate().
       |[8:2..8:8): assert => org.scalatest.Assertions#assert(Boolean,Prettifier,Position).
       |[8:9..8:10): x => _empty_.x.x().
       |[8:11..8:16): value => scala.meta.Term.Name#value().
       |[8:17..8:19): == => java.lang.Object#`==`(Any).
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
    """|_empty_.y. => final object y
       |_empty_.y.Path# => class Path.{+6 decls}
       |  extends AnyRef
       |_empty_.y.Path#B# => class B.{+2 decls}
       |  extends AnyRef
       |_empty_.y.Path#B#C# => class C.{+1 decls}
       |  extends AnyRef
       |_empty_.y.Path#B#C#`<init>`(). => primary ctor <init>: (): B.this.C
       |  B => _empty_.y.Path#B#
       |  C => _empty_.y.Path#B#C#
       |_empty_.y.Path#B#`<init>`(). => primary ctor <init>: (): Path.this.B
       |  Path => _empty_.y.Path#
       |  B => _empty_.y.Path#B#
       |_empty_.y.Path#`<init>`(). => primary ctor <init>: (): Path
       |  Path => _empty_.y.Path#
       |_empty_.y.Path#x(). => val method x: : Path.this.B
       |  Path => _empty_.y.Path#
       |  B => _empty_.y.Path#B#
       |_empty_.y.Path#x. => private[this] val field x: Path.this.B
       |  Path => _empty_.y.Path#
       |  B => _empty_.y.Path#B#
       |_empty_.y.Path#y(). => val method y: : Path.this.x.C
       |  Path => _empty_.y.Path#
       |  x => _empty_.y.Path#x().
       |  C => _empty_.y.Path#B#C#
       |_empty_.y.Path#y. => private[this] val field y: Path.this.x.C
       |  Path => _empty_.y.Path#
       |  x => _empty_.y.Path#x().
       |  C => _empty_.y.Path#B#C#
       |_empty_.y.b(). => implicit val method b: : Path#B
       |  Path => _empty_.y.Path#
       |  B => _empty_.y.Path#B#
       |_empty_.y.b. => private[this] val field b: Path#B
       |  Path => _empty_.y.Path#
       |  B => _empty_.y.Path#B#
    """.stripMargin
  )

  symbols(
    """
      |object z {
      |  val x = z
      |}
    """.stripMargin,
    """|_empty_.z. => final object z
       |_empty_.z.x(). => val method x: : z.type
       |  z => _empty_.z.
       |_empty_.z.x. => private[this] val field x: z.type
       |  z => _empty_.z.
    """.stripMargin
  )

  symbols(
    """
      |class aa {
      |  val x = this
      |  val y: aa.this.type = this
      |}
    """.stripMargin,
    """|_empty_.aa# => class aa.{+5 decls}
       |  extends AnyRef
       |_empty_.aa#`<init>`(). => primary ctor <init>: (): aa
       |  aa => _empty_.aa#
       |_empty_.aa#x(). => val method x: : aa
       |  aa => _empty_.aa#
       |_empty_.aa#x. => private[this] val field x: aa
       |  aa => _empty_.aa#
       |_empty_.aa#y(). => val method y: : aa.this.type
       |  aa => _empty_.aa#
       |_empty_.aa#y. => private[this] val field y: aa.this.type
       |  aa => _empty_.aa#
    """.stripMargin
  )

  symbols(
    """
      |object `ab ab`
    """.stripMargin,
    """|_empty_.`ab ab`. => final object ab ab
    """.stripMargin
  )

  symbols(
    """
      |object ac {
      |  val x = Int.MaxValue
      |  val y: Class[_] = ???
      |}
    """.stripMargin,
    """|_empty_.ac. => final object ac
       |_empty_.ac.x(). => val method x: : Int
       |  Int => scala.Int#
       |_empty_.ac.x. => private[this] val field x: Int
       |  Int => scala.Int#
       |_empty_.ac.y(). => val method y: : Class[_$1] forSome { type _$1 >: Nothing <: Any }
       |  Class => scala.Predef.Class#
       |  _$1 => _empty_.ac.y._$1#
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |_empty_.ac.y. => private[this] val field y: Class[_$1] forSome { type _$1 >: Nothing <: Any }
       |  Class => scala.Predef.Class#
       |  _$1 => _empty_.ac.y._$1#
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |_empty_.ac.y._$1# => abstract synthetic type _$1: >: Nothing <: Any
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |scala.Int. => final object Int
       |scala.Int.MaxValue(). => final val method MaxValue: : 2147483647
       |scala.Int.MaxValue. => private[this] final val field MaxValue: 2147483647
       |scala.Predef.Class# => type Class: [T: <?>] => >: Class[T] <: Class[T]
       |  T => scala.Predef.Class#[T]
       |  Class => java.lang.Class#
       |scala.Predef.`???`(). => method ???: : Nothing
       |  Nothing => scala.Nothing#
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
    """|_empty_.ad. => final object ad
       |_empty_.ad.$anon#y(). => abstract val method y: : Any
       |  Any => scala.Any#
       |_empty_.ad.Bar# => class Bar.{+1 decls}
       |  extends AnyRef
       |_empty_.ad.Bar#`<init>`(). => primary ctor <init>: (): Bar
       |  Bar => _empty_.ad.Bar#
       |_empty_.ad.Foo# => trait Foo
       |  extends AnyRef
       |_empty_.ad.k(). => val method k: : AnyRef with Foo { val method y: Any }
       |  AnyRef => scala.AnyRef#
       |  Foo => _empty_.ad.Foo#
       |  y => _empty_.ad.$anon#y().
       |  Any => scala.Any#
       |_empty_.ad.k. => private[this] val field k: AnyRef with Foo { val method y: Any }
       |  AnyRef => scala.AnyRef#
       |  Foo => _empty_.ad.Foo#
       |  y => _empty_.ad.$anon#y().
       |  Any => scala.Any#
       |_empty_.ad.x(). => val method x: : AnyRef with Foo { val method y: Int; method z[T >: Nothing <: Any] => (e: T): T }
       |  AnyRef => scala.AnyRef#
       |  Foo => _empty_.ad.Foo#
       |  y => _empty_.ad.x.$anon#y().
       |  Int => scala.Int#
       |  z => _empty_.ad.x.$anon#z(T).
       |  T => _empty_.ad.x.$anon#z(T).[T]
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |  e => _empty_.ad.x.$anon#z(T).(e)
       |_empty_.ad.x. => private[this] val field x: AnyRef with Foo { val method y: Int; method z[T >: Nothing <: Any] => (e: T): T }
       |  AnyRef => scala.AnyRef#
       |  Foo => _empty_.ad.Foo#
       |  y => _empty_.ad.x.$anon#y().
       |  Int => scala.Int#
       |  z => _empty_.ad.x.$anon#z(T).
       |  T => _empty_.ad.x.$anon#z(T).[T]
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |  e => _empty_.ad.x.$anon#z(T).(e)
       |_empty_.ad.x.$anon#y(). => val method y: : Int
       |  Int => scala.Int#
       |_empty_.ad.x.$anon#y. => private[this] val field y: Int
       |  Int => scala.Int#
       |_empty_.ad.x.$anon#z(T). => method z: [T >: Nothing <: Any] => (e: T): T
       |  T => _empty_.ad.x.$anon#z(T).[T]
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |  e => _empty_.ad.x.$anon#z(T).(e)
       |_empty_.ad.x.$anon#z(T).(e) => param e: T
       |  T => _empty_.ad.x.$anon#z(T).[T]
       |_empty_.ad.x.$anon#z(T).[T] => typeparam T: >: Nothing <: Any
       |  Nothing => scala.Nothing#
       |  Any => scala.Any#
       |_empty_.ad.z(). => val method z: : AnyRef with Foo { val method y: Any }
       |  AnyRef => scala.AnyRef#
       |  Foo => _empty_.ad.Foo#
       |  y => _empty_.ad.$anon#y().
       |  Any => scala.Any#
       |_empty_.ad.z. => private[this] val field z: AnyRef with Foo { val method y: Any }
       |  AnyRef => scala.AnyRef#
       |  Foo => _empty_.ad.Foo#
       |  y => _empty_.ad.$anon#y().
       |  Any => scala.Any#
       |_empty_.ad.zz(). => val method zz: : Bar { val method y: Int }
       |  Bar => _empty_.ad.Bar#
       |  y => _empty_.ad.zz.$anon#y().
       |  Int => scala.Int#
       |_empty_.ad.zz. => private[this] val field zz: Bar { val method y: Int }
       |  Bar => _empty_.ad.Bar#
       |  y => _empty_.ad.zz.$anon#y().
       |  Int => scala.Int#
       |_empty_.ad.zz.$anon#y(). => val method y: : Int
       |  Int => scala.Int#
       |_empty_.ad.zz.$anon#y. => private[this] val field y: Int
       |  Int => scala.Int#
       |java.lang.Object#`<init>`(). => ctor <init>: (): Object
       |  Object => java.lang.Object#
       |scala.Any# => abstract class Any.{+9 decls}
       |scala.AnyRef# => type AnyRef: >: Object <: Object
       |  Object => java.lang.Object#
       |scala.Int# => abstract final class Int.{+111 decls}
       |  extends AnyVal
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

  occurrences(
    """
      |object ag {
      | for (x <- 1 to 10; y <- 0 until 10) println(x -> x)
      | for (i <- 1 to 10; j <- 0 until 10) yield (i, j)
      | for (i <- 1 to 10; j <- 0 until 10 if i % 2 == 0) yield (i, j)
      |}
    """.trim.stripMargin,
    """|[0:7..0:9): ag <= _empty_.ag.
       |[1:6..1:7): x <= local0
       |[1:13..1:15): to => scala.runtime.RichInt#to(Int).
       |[1:20..1:21): y <= local1
       |[1:27..1:32): until => scala.runtime.RichInt#until(Int).
       |[1:37..1:44): println => scala.Predef.println(Any).
       |[1:45..1:46): x => local0
       |[1:47..1:49): -> => scala.Predef.ArrowAssoc#`->`(B).
       |[1:50..1:51): x => local0
       |[2:6..2:7): i <= local2
       |[2:13..2:15): to => scala.runtime.RichInt#to(Int).
       |[2:20..2:21): j <= local3
       |[2:27..2:32): until => scala.runtime.RichInt#until(Int).
       |[2:44..2:45): i => local2
       |[2:47..2:48): j => local3
       |[3:6..3:7): i <= local4
       |[3:13..3:15): to => scala.runtime.RichInt#to(Int).
       |[3:20..3:21): j <= local5
       |[3:27..3:32): until => scala.runtime.RichInt#until(Int).
       |[3:39..3:40): i => local4
       |[3:41..3:42): % => scala.Int#`%`(Int).
       |[3:45..3:47): == => scala.Int#`==`(Int).
       |[3:58..3:59): i => local4
       |[3:61..3:62): j => local6
    """.trim.stripMargin
  )

  targeted(
    """package ak
      |trait Foo
      |object Foo {
      |  new <<Foo>> {}
      |}
    """.stripMargin, { (_, Foo) =>
      assertNoDiff(Foo, "ak.Foo#")
    }
  )

  targeted(
    """
      |package an
      |object M1 {
      |  class C
      |}
      |object M2 {
      |  class C
      |}
      |object M {
      |  def foo(c: M1.C) = ???
      |  def foo(c: M2.C) = ???
      |}
      |object U {
      |  M.<<foo>>(new M1.C)
      |  M.<<foo>>(new M2.C)
      |}
    """.trim.stripMargin, { (_, foo1, foo2) =>
      assert(foo1 === "an.M.foo(C).")
      assert(foo2 === "an.M.foo(C+1).")
    }
  )

  targeted(
    """
      |package a
      |case class <<Foo>>(b: Foo)
    """.stripMargin, { (db, FooTypeString) =>
      val fooType = Symbol(FooTypeString)
      val Symbol.Global(qual, Signature.Type(foo)) = fooType
      val companion = Symbol.Global(qual, Signature.Term(foo)).syntax
      val objectDenot = db.symbols.find(_.symbol == companion).get
      assert(objectDenot.kind.isObject)
      assert(!objectDenot.has(p.CASE))
      val classDenot = db.symbols.find(_.symbol == fooType.syntax).get
      assert(classDenot.kind.isClass)
      assert(classDenot.has(p.CASE))
      val decls = classDenot.tpe.get.classInfoType.get.declarations
      assert(decls.nonEmpty)
      decls.foreach { decl =>
        val declDenot = db.symbols.find(_.symbol == decl)
        assert(declDenot.isDefined, decl)
      }
    }
  )

  targeted(
    """
      |object a {
      |  for {
      |    i <- List(1, 2)
      |    <<j>> <- List(3, 4)
      |  } yield j
      |}
      """.stripMargin, { (db, j) =>
      val denot = db.symbols.find(_.symbol == j).get
      assert(denot.kind.isLocal)
    }
  )

  targeted(
    """
      |package ao
      |case class <<Foo>>(i: Int)
    """.stripMargin, { (db, foo) =>
      val syntheticSymbols = db.symbols.filter(_.has(p.SYNTHETIC))
      val obtainedSymbols = syntheticSymbols.map(_.symbol).sorted.mkString("\n")
      val expectedSymbols =
        """
          |ao.Foo#canEqual(Any).
          |ao.Foo#canEqual(Any).(x$1)
          |ao.Foo#copy$default$1().
          |ao.Foo#copy(Int).
          |ao.Foo#equals(Any).
          |ao.Foo#equals(Any).(x$1)
          |ao.Foo#hashCode().
          |ao.Foo#productArity().
          |ao.Foo#productElement(Int).
          |ao.Foo#productElement(Int).(x$1)
          |ao.Foo#productIterator().
          |ao.Foo#productPrefix().
          |ao.Foo#toString().
          |ao.Foo.
          |ao.Foo.apply(Int).
          |ao.Foo.readResolve().
          |ao.Foo.toString().
          |ao.Foo.unapply(Foo).
          |ao.Foo.unapply(Foo).(x$0)
        """.stripMargin.trim
      assertNoDiff(obtainedSymbols,  expectedSymbols)
    }
  )

  targeted(
    """
      |package ap
      |case class Foo() {
      |  override def <<equals>>(that: Any): Boolean = false
      |}
    """.stripMargin, { (db, equals) =>
      val equalsInfo = db.symbols.find(_.symbol == equals).get
      assert(!equalsInfo.has(p.SYNTHETIC))
    }
  )
}

object Compat {
  import scala.reflect.runtime.universe._

  val ConsWrapperInString = "Stream.ConsWrapper#`#::`(B)."
  lazy val ConsWrapperActual: String = {
    val consWrapper = typeOf[scala.collection.immutable.Stream.ConsWrapper[_]]
    val streamCons = consWrapper.decl(TermName("#::").encodedName)
    val List(List(param)) = streamCons.info.paramLists
    val name = param.info.toString
    s"Stream.ConsWrapper#`#::`($name)."
  }

  val ListBufferDeclsInString = "+43 decls"
  lazy val ListBufferDeclsActual: String = {
    val consWrapper = typeOf[scala.collection.mutable.ListBuffer[_]]
    val decls = consWrapper.decls.size
    s"+$decls decls"
  }

  val ReusableBuilderInString = "extends ReusableBuilder[A, List[A]]"
  lazy val ReusableBuilderActual: String = {
    if (scala.util.Properties.versionNumberString.startsWith("2.11")) {
      "extends Builder[A, List[A]]"
    } else {
      ReusableBuilderInString
    }
  }

}
