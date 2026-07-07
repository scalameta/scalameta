package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}
import scala.meta.tests.semanticdb.ScalaVersion.Scala212

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
    // the compilers emit the two annotations in opposite order
    ScalaVersion.getExpected(
      Seq(
        Scala212 ->
          """scala/Predef.assert(+1). => @inline @elidable final method assert(assertion: Boolean, message: => Any): Unit""",
      ),
      """scala/Predef.assert(+1). => @elidable @inline final method assert(assertion: Boolean, message: => Any): Unit""",
    ),
  )
  checkInfo("scala/Any#", """scala/Any# => abstract class Any { +10 decls }""")
  checkInfo(
    "java/util/Collections#singletonList().",
    """java/util/Collections#singletonList(). => static method singletonList[T](param0: T): List[T]""",
  )

  // 2.12's CanBuildFrom-based collections desugar `map` with an extra builder argument
  checkSynthetics(
    "List(1).map(_ + 2)",
    ScalaVersion.getExpected(
      Seq(
        Scala212 ->
          """|[2:0..2:11): List(1).map => *[Int, List[Int]]
             |[2:0..2:18): List(1).map(_ + 2) => *(List.canBuildFrom[Int])
             |[2:0..2:4): List => *.apply[Int]
             |""".stripMargin,
      ),
      """|[2:0..2:11): List(1).map => *[Int]
         |[2:0..2:4): List => *.apply[Int]
         |""".stripMargin,
    ),
  )

  checkTrees(
    "List(1).map(_ + 2)",
    ScalaVersion.getExpected(
      Seq(
        Scala212 ->
          """|orig(List(1).map)[Int, List[Int]]
             |orig(List(1).map(_ + 2))(List.canBuildFrom[Int])
             |orig(List).apply[Int]
             |""".stripMargin,
      ),
      """|orig(List(1).map)[Int]
         |orig(List).apply[Int]
         |""".stripMargin,
    ),
  )

  // parameterless `to` exists only in 2.12's CanBuildFrom collections; the snippet does not
  // compile on 2.13, so the whole test is version-gated rather than expectation-switched
  ScalaVersion.doIf("PrintSuite legacy synthetics", ScalaVersion.is212)(checkSynthetics(
    """|object LegacySyntheticsTest {
       |  def m1(xs: Set[Int]): List[Int] =
       |    xs.to
       |}
       |""".stripMargin,
    """|[4:4..4:9): xs.to => *(List.canBuildFrom[Int])
       |[4:4..4:9): xs.to => *[List]
       |""".stripMargin,
  ))

}
