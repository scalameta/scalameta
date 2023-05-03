package scala.meta.tests.parsers.dotty

import scala.meta._

class FooSuite extends BaseDottySuite {

  test("match without braces") {
    val code =
      """|for { project <- projects
         |      (source, id) <- project.sources.zipWithIndex } yield source 
         |""".stripMargin
    val output =
      "def f = if (x.exists(x => x == 10)) println(\"Yes\") else println(\"No\")"
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
