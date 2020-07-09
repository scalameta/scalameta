package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers.ParseSuite

import scala.meta._, Type._

class AndOrTypesSuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => templStat(code)(dialects.Dotty)
  implicit val parseType: String => Type = code => tpe(code)(dialects.Dotty)

  /**
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/new-types/intersection-types.html
   *  https://dotty.epfl.ch/docs/reference/new-types/union-types.html
   */
  test("view bounds not allowed") {
    intercept[ParseException] {
      dialects.Dotty("{ def foo[T <% Int](t: T) = ??? }").parse[Term].get
    }
  }

  test("A with B") {
    runTestAssert[Type]("A with B", None)(
      And(Type.Name("A"), Type.Name("B"))
    )
  }

  test("A & B & C") {
    runTestAssert[Type]("A & B & C")(
      And(And(Type.Name("A"), Type.Name("B")), Type.Name("C"))
    )
  }

  test("A & B") {
    runTestAssert[Type]("A & B")(
      And(Type.Name("A"), Type.Name("B"))
    )
  }

  test("A | B") {
    runTestAssert[Type]("A | B")(
      Or(Type.Name("A"), Type.Name("B"))
    )
  }

  test("ortype-example") {
    runTestAssert[Stat]("def help(id: UserName | Password): Unit")(
      Decl.Def(
        Nil,
        tname("help"),
        Nil,
        List(
          List(
            Term.Param(Nil, tname("id"), Some(Type.Or(pname("UserName"), pname("Password"))), None)
          )
        ),
        pname("Unit")
      )
    )

    runTestAssert[Stat]("val either: Password | UserName")(
      Decl.Val(Nil, List(Pat.Var(tname("either"))), Type.Or(pname("Password"), pname("UserName")))
    )
  }

  test("andtype-example") {
    runTestAssert[Stat]("val x: Reset & Ord[Int]")(
      Decl.Val(
        Nil,
        List(Pat.Var(tname("x"))),
        Type.And(pname("Reset"), Type.Apply(Type.Name("Ord"), List(pname("Int"))))
      )
    )
    runTestAssert[Stat]("def fx(a: List[A & B]): Unit")(
      Decl.Def(
        Nil,
        tname("fx"),
        Nil,
        List(
          List(
            Term.Param(
              Nil,
              tname("a"),
              Some(Type.Apply(pname("List"), List(Type.And(Type.Name("A"), pname("B"))))),
              None
            )
          )
        ),
        pname("Unit")
      )
    )
  }
}
