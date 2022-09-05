package scala.meta.tests.parsers.dotty

import scala.meta._, Type._

class AndOrTypesSuite extends BaseDottySuite {

  /**
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/new-types/intersection-types.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/new-types/union-types.html]]
   */
  test("view bounds not allowed") {
    intercept[ParseException] {
      dialects.Scala3("{ def foo[T <% Int](t: T) = ??? }").parse[Term].get
    }
  }

  test("A with B") {
    runTestAssert[Type]("A with B", None)(
      With(Type.Name("A"), Type.Name("B"))
    )
  }

  test("A & B & C") {
    runTestAssert[Type]("A & B & C")(
      ApplyInfix(ApplyInfix(pname("A"), pname("&"), pname("B")), pname("&"), pname("C"))
    )
  }

  test("A & B") {
    runTestAssert[Type]("A & B")(
      ApplyInfix(pname("A"), pname("&"), pname("B"))
    )
  }

  test("A | B") {
    runTestAssert[Type]("A | B")(
      ApplyInfix(pname("A"), pname("|"), pname("B"))
    )
  }

  test("ortype-example") {
    runTestAssert[Stat]("def help(id: UserName | Password): Unit")(
      Decl.Def(
        Nil,
        tname("help"),
        List(
          Clause.TermClause(List(
            Term.Param(
              Nil,
              tname("id"),
              Some(ApplyInfix(pname("UserName"), pname("|"), pname("Password"))),
              None
            )
          ))
        ),
        pname("Unit")
      )
    )

    runTestAssert[Stat]("val either: Password | UserName")(
      Decl.Val(
        Nil,
        List(Pat.Var(tname("either"))),
        ApplyInfix(pname("Password"), pname("|"), pname("UserName"))
      )
    )
  }

  test("andtype-example") {
    runTestAssert[Stat]("val x: Reset & Ord[Int]")(
      Decl.Val(
        Nil,
        List(Pat.Var(tname("x"))),
        ApplyInfix(pname("Reset"), pname("&"), Type.Apply(pname("Ord"), List(pname("Int"))))
      )
    )
    runTestAssert[Stat]("def fx(a: List[A & B]): Unit")(
      Decl.Def(
        Nil,
        tname("fx"),
        List(
          Clause.TermClause(List(
            Term.Param(
              Nil,
              tname("a"),
              Some(Type.Apply(pname("List"), List(ApplyInfix(pname("A"), pname("&"), pname("B"))))),
              None
            )
          ))
        ),
        pname("Unit")
      )
    )
  }
}
