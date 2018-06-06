package scala.meta.tests.semanticdb

class TargetedSuite2_12 extends SemanticdbSuite {

  diagnostics(
    """
      |package l2
      |class A {
      |  private val a = 1
      |}
      |object B {
      |  private val a = 1
      |}
      |package object C {
      |  private val a = 1
      |}
      |trait D {
      |  private val a = 1
      |}
    """.stripMargin.trim,
    """|[2:14..2:14) [warning] private val a in class A is never used
       |[5:14..5:14) [warning] private val a in object B is never used
       |[8:14..8:14) [warning] private val a in package object C is never used
       |[11:2..11:19) [warning] private val a in trait D is never used
    """.stripMargin.trim
  )

}
