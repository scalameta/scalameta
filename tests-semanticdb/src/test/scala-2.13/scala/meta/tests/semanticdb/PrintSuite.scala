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
    ScalaVersion.getExpected(
      Seq(
        ScalaVersion.Full("2.13.9") ->
          """scala/Predef.assert(+1). => @inline @elidable final method assert(assertion: Boolean, message: => Any): Unit""",
        ScalaVersion.Full("2.13.10") ->
          """scala/Predef.assert(+1). => @inline @elidable final method assert(assertion: Boolean, message: => Any): Unit""",
        ScalaVersion.Full("2.13.11") ->
          """scala/Predef.assert(+1). => @inline @elidable final method assert(assertion: Boolean, message: => Any): Unit"""
      ),
      """scala/Predef.assert(+1). => @elidable @inline final method assert(assertion: Boolean, message: => Any): Unit"""
    )
  )
  checkInfo("scala/Any#", """scala/Any# => abstract class Any { +10 decls }""")
  checkInfo(
    "java/util/Collections#singletonList().",
    """java/util/Collections#singletonList(). => static method singletonList[T](param0: T): List[T]"""
  )

  checkSynthetics(
    "List(1).map(_ + 2)",
    """|[2:0..2:11): List(1).map => *[Int]
       |[2:0..2:4): List => *.apply[Int]
       |""".stripMargin
  )

  checkTrees(
    "List(1).map(_ + 2)",
    """|orig(List(1).map)[Int]
       |orig(List).apply[Int]
       |""".stripMargin
  )
}
