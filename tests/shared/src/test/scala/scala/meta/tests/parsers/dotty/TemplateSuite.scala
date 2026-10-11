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
    val layout =
      """|trait A {
         |  def f: Int
         |
         |  (a, b)
         |}
         |""".stripMargin
    val tree = Defn.Trait(
      Nil,
      pname("A"),
      Nil,
      ctor,
      tpl(Decl.Def(Nil, "f", Nil, Nil, "Int"), Term.Tuple(List(tname("a"), tname("b")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("tuple statement after a def declaration") {
    val code =
      """|trait A {
         |  def f: Int
         |  (a, b)
         |}
         |""".stripMargin
    val layout =
      """|trait A {
         |  def f: Int
         |
         |  (a, b)
         |}
         |""".stripMargin
    val tree = Defn.Trait(
      Nil,
      pname("A"),
      Nil,
      ctor,
      tpl(Decl.Def(Nil, "f", Nil, Nil, "Int"), Term.Tuple(List(tname("a"), tname("b")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("tuple statement after a val declaration") {
    val code =
      """|trait A {
         |  val f: Int
         |  (a, b)
         |}
         |""".stripMargin
    val layout =
      """|trait A {
         |  val f: Int
         |
         |  (a, b)
         |}
         |""".stripMargin
    val tree = Defn.Trait(
      Nil,
      pname("A"),
      Nil,
      ctor,
      tpl(Decl.Val(Nil, List(patvar("f")), pname("Int")), Term.Tuple(List(tname("a"), tname("b")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
