package scala.meta.tests.parsers.dotty

import scala.meta._

class CtrlExprJVMSuite extends BaseDottySuite {

  test("metals#6552: huge nested `if`") {
    val nestedNum = 100 // was previously failing around 25-30 deep
    def generateIfs(num: Int, ident: String): String =
      if (num <= 1) "if (1 >= 10) {}"
      else s"""|if (1 >= 10) {
               |$ident  ${generateIfs(num - 1, ident + "  ")}
               |$ident}""".stripMargin

    val code = s"""|object Test {
                   |  def apply(): Unit = {
                   |    ${generateIfs(nestedNum, "    ")}
                   |  }
                   |}
                   |""".stripMargin

    def generateIfsTree(num: Int): Term.If = {
      val inner = if (num <= 1) Nil else List(generateIfsTree(num - 1))
      Term.If(Term.ApplyInfix(lit(1), tname(">="), Nil, List(lit(10))), blk(inner), Lit.Unit(), Nil)
    }
    val tree = Source(List(Defn.Object(
      Nil,
      tname("Test"),
      tpl(Defn.Def(
        Nil,
        tname("apply"),
        Nil,
        List(Nil),
        Some(pname("Unit")),
        blk(generateIfsTree(nestedNum))
      ))
    )))

    runTestAssert[Source](code)(tree)
  }

}
