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
  """.trim.stripMargin)

  targeted("""
    |object <<Second>> {
    |  def doSomething = {
    |    42
    |  }
    |}
  """.trim.stripMargin,
           second => {
             assert(second.symbol === Symbol("_empty_.Second."))
           })

  targeted("""
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
           })

  targeted("""
    |object Fourth {
    |  val x: <<Int>> = ???
    |}
  """.trim.stripMargin,
           (int) => {
             assert(int === t"Int")
           })
}
