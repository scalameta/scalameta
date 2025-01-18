package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}

class PrintSuite extends PrintSuiteBase {

  checkType("java/io/ByteArrayOutputStream#buf.", "Array[Byte]")
  checkType("scala/Predef.ArrowAssoc#`->`().", "Tuple2[A, B]")

  checkConstant(s.UnitConstant(), "()")
  checkConstant(s.LongConstant(64), "64L")
  checkConstant(s.CharConstant('a'.toInt), "'a'")
  checkConstant(s.StringConstant("a"), "\"a\"")

  checkSignature("scala/Predef.assert(+1).", """(assertion: Boolean, message: => Any): Unit""")
  checkSignature("scala/Predef.ArrowAssoc#`->`().", "[B](y: B): Tuple2[A, B]")

  checkInfo(
    "scala/Predef.assert(+1).",
    """scala/Predef.assert(+1). => @inline @elidable final method assert(assertion: Boolean, message: => Any): Unit"""
  )
  checkInfo("scala/Any#", """scala/Any# => abstract class Any { +10 decls }""")
  checkInfo(
    "java/util/Collections#singletonList().",
    """java/util/Collections#singletonList(). => static method singletonList[T](param0: T): List[T]"""
  )

  checkSynthetics(
    "List(1).map(_ + 2)",
    """|[2:0..2:11): List(1).map => *[Int, List[Int]]
       |[2:0..2:18): List(1).map(_ + 2) => *(List.canBuildFrom[Int])
       |[2:0..2:4): List => *.apply[Int]
       |""".stripMargin
  )

  checkTrees(
    "List(1).map(_ + 2)",
    """|orig(List(1).map)[Int, List[Int]]
       |orig(List(1).map(_ + 2))(List.canBuildFrom[Int])
       |orig(List).apply[Int]
       |""".stripMargin
  )

  checkSynthetics(
    """|object LegacySyntheticsTest {
       |  def m1(xs: Set[Int]): List[Int] =
       |    xs.to
       |}
       |""".stripMargin,
    """|[4:4..4:9): xs.to => *(List.canBuildFrom[Int])
       |""".stripMargin
  )

}
