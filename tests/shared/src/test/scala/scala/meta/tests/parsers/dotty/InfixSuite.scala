package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class InfixSuite extends BaseDottySuite {

  test("simple-modifier") {
    runTestAssert[Stat](
      """|infix def a(param: Int) = param
         |""".stripMargin
    )(
      Defn.Def(
        List(Mod.Infix()),
        tname("a"),
        Nil,
        List(List(Term.Param(Nil, tname("param"), Some(pname("Int")), None))),
        None,
        tname("param")
      )
    )
  }
  test("infix-type-complex") {
    runTestAssert[Stat](
      """|class A:
         |  infix type or[X, Y]
         |  infix def x(a : Int): String or Int = 1
         |""".stripMargin,
      assertLayout = Some(
        """|class A {
           |  infix type or[X, Y]
           |  infix def x(a: Int): String or Int = 1
           |}
           |""".stripMargin
      )
    )(
      Defn.Class(
        Nil,
        pname("A"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Decl.Type(
              List(Mod.Infix()),
              pname("or"),
              List(
                Type.Param(Nil, pname("X"), Nil, Type.Bounds(None, None), Nil, Nil),
                Type.Param(Nil, pname("Y"), Nil, Type.Bounds(None, None), Nil, Nil)
              ),
              Type.Bounds(None, None)
            ),
            Defn.Def(
              List(Mod.Infix()),
              tname("x"),
              Nil,
              List(List(Term.Param(Nil, tname("a"), Some(pname("Int")), None))),
              Some(Type.ApplyInfix(pname("String"), pname("or"), pname("Int"))),
              int(1)
            )
          )
        )
      )
    )

  }

  test("infix-class") {
    runTestAssert[Stat]("infix class A[B, C]")(
      Defn.Class(
        List(Mod.Infix()),
        pname("A"),
        List(
          pparam("B"),
          pparam("C")
        ),
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )
  }

  test("infix-trait") {
    runTestAssert[Stat]("infix trait A[B, C]")(
      Defn.Trait(
        List(Mod.Infix()),
        pname("A"),
        List(
          pparam("B"),
          pparam("C")
        ),
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )
  }

  test("infix-identifier") {
    runTestAssert[Stat]("infix def infix(infix: infix): infix = new infix {}")(
      Defn.Def(
        List(Mod.Infix()),
        tname("infix"),
        Nil,
        List(List(Term.Param(Nil, tname("infix"), Some(pname("infix")), None))),
        Some(pname("infix")),
        Term.NewAnonymous(
          Template(Nil, List(Init(pname("infix"), Name(""), Nil)), Self(Name(""), None), Nil)
        )
      )
    )
  }

  test("extension-method") {
    runTestAssert[Stat]("extension (i: Int) infix def zero(other: Int): Int = 0")(
      Defn.ExtensionGroup(
        Nil,
        List(List(Term.Param(Nil, tname("i"), Some(pname("Int")), None))),
        Defn.Def(
          List(Mod.Infix()),
          tname("zero"),
          Nil,
          List(List(Term.Param(Nil, tname("other"), Some(pname("Int")), None))),
          Some(pname("Int")),
          int(0)
        )
      )
    )
  }

}
