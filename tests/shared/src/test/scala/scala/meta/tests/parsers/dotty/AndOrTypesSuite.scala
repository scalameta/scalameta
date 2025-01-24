package scala.meta.tests.parsers.dotty

import scala.meta._

class AndOrTypesSuite extends BaseDottySuite {
  import Type._

  test("A with B")(runTestAssert[Type]("A with B", None)(With(pname("A"), pname("B"))))

  test("A & B & C")(runTestAssert[Type]("A & B & C")(
    ApplyInfix(ApplyInfix(pname("A"), pname("&"), pname("B")), pname("&"), pname("C"))
  ))

  test("A & B")(runTestAssert[Type]("A & B")(ApplyInfix(pname("A"), pname("&"), pname("B"))))

  test("A | B")(runTestAssert[Type]("A | B")(ApplyInfix(pname("A"), pname("|"), pname("B"))))

  test("ortype-example") {
    runTestAssert[Stat]("def help(id: UserName | Password): Unit")(Decl.Def(
      Nil,
      tname("help"),
      Nil,
      List(List(tparam("id", ApplyInfix(pname("UserName"), pname("|"), pname("Password"))))),
      pname("Unit")
    ))

    runTestAssert[Stat]("val either: Password | UserName")(
      Decl
        .Val(Nil, List(patvar("either")), ApplyInfix(pname("Password"), pname("|"), pname("UserName")))
    )
  }

  test("andtype-example") {
    runTestAssert[Stat]("val x: Reset & Ord[Int]")(
      Decl.Val(Nil, List(patvar("x")), ApplyInfix(pname("Reset"), pname("&"), papply("Ord", "Int")))
    )
    runTestAssert[Stat]("def fx(a: List[A & B]): Unit")(Decl.Def(
      Nil,
      tname("fx"),
      Nil,
      List(List(tparam(Nil, "a", papply("List", pinfix("A", "&", "B"))))),
      pname("Unit")
    ))
  }

  test("#3119") {
    val layout = "object A { type AllTraits = Trait1 & Trait2 & Trait3 & Trait4 & Trait5 }"
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Type(
        Nil,
        pname("AllTraits"),
        Nil,
        pinfix(
          pinfix(
            pinfix(pinfix("Trait1", "&", pname("Trait2")), "&", pname("Trait3")),
            "&",
            pname("Trait4")
          ),
          "&",
          pname("Trait5")
        ),
        noBounds
      ))
    )
    runTestAssert[Stat](
      """|object A:
         |  type AllTraits = Trait1 & Trait2 & Trait3
         |    & Trait4 & Trait5
         |""".stripMargin,
      Some(layout)
    )(tree)
  }

}
