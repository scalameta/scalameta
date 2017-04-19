package scala.meta.tests
package scalahost
package v1

import scala.meta._

class DatabaseSuite extends OnlineMirrorSuite {
  database(
    """
    |object First {
    |  def main(args: Array[String]): Unit = {
    |    val list = List(1, 2, 3)
    |    println(list)
    |  }
    |}
  """.trim.stripMargin,
    """
    |[7..12): First => _empty_.First.
    |[21..25): main => _empty_.First.main([Ljava/lang/String;)V.
    |[26..30): args => _empty_.First.main([Ljava/lang/String;)V.(args)
    |[32..37): Array => _root_.scala.Array#
    |[38..44): String => _root_.scala.Predef.String#
    |[48..52): Unit => _root_.scala.Unit#
    |[65..69): list => <...>@61..85
    |[72..76): List => _root_.scala.collection.immutable.List.apply(Lscala/collection/Seq;)Lscala/collection/immutable/List;.
    |[90..97): println => _root_.scala.Predef.println(Ljava/lang/Object;)V.
    |[98..102): list => <...>@61..85
    |
    |Denotations:
    |<...>@61..85 => VAL
    |_empty_.First. => FINAL | OBJECT
    |_empty_.First.main([Ljava/lang/String;)V. => DEF
    |_empty_.First.main([Ljava/lang/String;)V.(args) => TERMPARAM
  """.trim.stripMargin
  )

  targeted(
    """
    |object <<Second>> {
    |  def doSomething = {
    |    42
    |  }
    |}
  """.trim.stripMargin,
    second => {
      assert(second.symbol === Symbol("_empty_.Second."))
    }
  )

  targeted(
    """
    |object Third {
    |  val x1: scala.<<Int>> = ???
    |  val x2: <<Int>> = ???
    |  locally {
    |    type Int = String
    |    val x3: <<Int>> = ???
    |  }
    |}
  """.trim.stripMargin,
    (intname1, int2, int3) => {
      val int1 = intname1.parent.get.asInstanceOf[Type.Select]
      assert(int1 === int2)
      assert(int1 =!= int3)
    }
  )

  database(
    """
    |import _root_.scala.List
    |
    |class C {
    |  _root_.scala.List
    |}
  """.trim.stripMargin,
    """
    |[0..24): import _root_.scala.List => Warning Unused import
    |[7..13): _root_ => _root_.
    |[14..19): scala => _root_.scala.
    |[20..24): List => _root_.scala.package.List.;_root_.scala.package.List#
    |[32..33): C => _empty_.C#
    |[38..44): _root_ => _root_.
    |[45..50): scala => _root_.scala.
    |[51..55): List => _root_.scala.collection.immutable.
    |
    |Denotations:
    |_empty_.C# => CLASS
    |_empty_.C#`<init>`()V. => PRIMARYCTOR
  """.trim.stripMargin
  )

  targeted(
    // curried function application with named args, #648
    """
      |object Foo {
      |  def bar(children: Int)(x: Int) = children + x
      |  <<bar>>(children = 4)(3)
      |}
    """.trim.stripMargin,
    second => {
      assert(second.symbol === Symbol("_empty_.Foo.bar(II)I."))
    }
  )

  targeted(
    """
      |case class User(name: String, age: Int)
      |object Library {
      |  val u: User = ???
      |  u.<<copy>>(<<age>> = 43)
      |}
    """.trim.stripMargin,
    (copy, age) => {
      assert(copy.symbol === Symbol("_empty_.User#copy(Ljava/lang/String;I)LUser;."))
      assert(age.symbol === Symbol("_empty_.User#copy(Ljava/lang/String;I)LUser;.(age)"))
    }
  )

  denotations(
    """
      |import scala.language.experimental.macros
      |
      |package foo {
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
      |    private[foo] val f3 = ???
      |    protected var f4 = ???
      |    protected[this] var f5 = ???
      |    protected[foo] var f6 = ???
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
      |package object foo {
      |}
  """.trim.stripMargin,
    """
      |_root_.foo. => PACKAGE
      |_root_.foo.C1# => CLASS
      |_root_.foo.C1#(p1) => TERMPARAM
      |_root_.foo.C1#(p2) => VAL | TERMPARAM
      |_root_.foo.C1#(p3) => VAR | TERMPARAM
      |_root_.foo.C1#T1# => ABSTRACT | TYPE
      |_root_.foo.C1#T2# => TYPE
      |_root_.foo.C1#`<init>`()V. => SECONDARYCTOR
      |_root_.foo.C1#`<init>`(III)V. => PRIMARYCTOR
      |_root_.foo.C1#f1. => VAL
      |_root_.foo.C1#f1.l1. => VAL
      |_root_.foo.C1#f1.l2. => VAR
      |_root_.foo.C1#f2. => VAR
      |_root_.foo.C1#m1(I)I. => DEF
      |_root_.foo.C1#m1(I)I.(x) => TERMPARAM
      |_root_.foo.C1#m1(I)I.T# => TYPEPARAM
      |_root_.foo.C1#m2()Lscala/Nothing;. => MACRO
      |_root_.foo.C2# => ABSTRACT | CLASS
      |_root_.foo.C2#`<init>`()V. => PRIMARYCTOR
      |_root_.foo.C2#m3()I. => ABSTRACT | DEF
      |_root_.foo.C2#m4()Lscala/Nothing;. => FINAL | DEF
      |_root_.foo.C3# => SEALED | CLASS
      |_root_.foo.C3#`<init>`()V. => PRIMARYCTOR
      |_root_.foo.C3#m3()I. => DEF
      |_root_.foo.C3#toString()Ljava/lang/String;. => DEF
      |_root_.foo.M. => FINAL | OBJECT
      |_root_.foo.M.C1# => CASE | CLASS
      |_root_.foo.M.C1#`<init>`()V. => PRIMARYCTOR
      |_root_.foo.M.C2# => CLASS
      |_root_.foo.M.C2#[T] => COVARIANT | TYPEPARAM
      |_root_.foo.M.C2#[U] => CONTRAVARIANT | TYPEPARAM
      |_root_.foo.M.C2#`<init>`()V. => PRIMARYCTOR
      |_root_.foo.M.i1()Lscala/Nothing;. => IMPLICIT | DEF
      |_root_.foo.M.l1. => LAZY | VAL
      |_root_.foo.T# => TRAIT
      |_root_.foo.T#f1. => PRIVATE | VAL
      |_root_.foo.T#f2. => PRIVATE | VAL
      |_root_.foo.T#f3. => PRIVATE | VAL
      |_root_.foo.T#f4. => PROTECTED | VAR
      |_root_.foo.T#f5. => PROTECTED | VAR
      |_root_.foo.T#f6. => PROTECTED | VAR
      |_root_.foo.package. => PACKAGEOBJECT
  """.trim.stripMargin
  )
}
