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
    |[65..69): list => file:<...>@61..85
    |[72..76): List => _root_.scala.collection.immutable.List.apply(Lscala/collection/Seq;)Lscala/collection/immutable/List;.
    |[90..97): println => _root_.scala.Predef.println(Ljava/lang/Object;)V.
    |[98..102): list => file:<...>@61..85
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

  targeted("""
    |object Fourth {
    |  val x: <<Int>> = ???
    |}
  """.trim.stripMargin,
           (int) => {
             assert(int === t"Int")
           })

  database(
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
    |[38..44): _root_ => _root_.
    |[45..50): scala => _root_.scala.
    |[51..55): List => _root_.scala.collection.immutable.
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
}
