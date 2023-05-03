package scala.meta.tests.parsers

import scala.meta._

class FooSuite extends ParseSuite {

  test("match without braces") {
    val code =
      """|class A1 {
         |  def a2(a3: A4): A5 = {
         |    a3 match {
         |      case Some(_) =>
         |        case class A6(a7: A8)
         |
         |        object A9
         |    }
         |  }
         |}
      """.stripMargin
    val output =
      """|if (!other) if (member) () else overrideError() else checkOverrideDeprecated()
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(
        Term.ApplyUnary(tname("!"), tname("other")),
        Term.If(tname("member"), Lit.Unit(), Term.Apply(tname("overrideError"), Nil), Nil),
        Term.Apply(tname("checkOverrideDeprecated"), Nil),
        Nil
      )
    )
  }

}
