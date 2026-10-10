package scala.meta.tests.parsers.dotty

import scala.meta._

class TemplateSuite extends BaseDottySuite {

  test("blank-after-template") {
    val code =
      """|
         |trait B extends F
         |  [Int]
         |""".stripMargin
    val code2 =
      """|
         |trait B extends F
         |  [
         |    Int]
         |""".stripMargin
    val code3 =
      """|
         |trait B extends F
         |[Int]
         |""".stripMargin

    val tree = Defn.Trait(
      Nil,
      pname("B"),
      Type.ParamClause(Nil),
      EmptyCtor(),
      Template(
        None,
        List(Init(
          Type.Apply(Type.Name("F"), Type.ArgClause(List(Type.Name("Int")))),
          Name.Anonymous(),
          Seq.empty[Term.ArgClause],
        )),
        Template.Body(None, Nil),
        Nil,
      ),
    )
    checkStat(code, "trait B extends F[Int]")(tree)
    checkStat(code2, "trait B extends F[Int]")(tree)
    runTestError[Source](code3, "error: illegal start of definition `[`")
  }

  test("tuple statement after a def declaration, blank line between") {
    val code =
      """|trait A {
         |  def f: Int
         |
         |  (a, b)
         |}
         |""".stripMargin
    runTestError[Stat](code, "`=` expected but `(` found")
  }

  test("tuple statement after a def declaration") {
    val code =
      """|trait A {
         |  def f: Int
         |  (a, b)
         |}
         |""".stripMargin
    runTestError[Stat](code, "`=` expected but `(` found")
  }

  test("tuple statement after a val declaration") {
    val code =
      """|trait A {
         |  val f: Int
         |  (a, b)
         |}
         |""".stripMargin
    runTestError[Stat](code, "`;` expected but `(` found")
  }

}
