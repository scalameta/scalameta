package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.Type.Apply
import scala.meta.Type.Placeholder

class InfixSuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)
  implicit val parseType: String => Type = code => tpe(code)(dialects.Dotty)
  implicit val parseSource: String => Source = code => source(code)(dialects.Dotty)

  val parseTempl: String => Stat = code => templStat(code)(dialects.Dotty)

  test("simple-modifier") {

    runTestAssert[Stat](
      """|infix def a(param: Int) = param
         |""".stripMargin
    )(
      Defn.Def(
        List(Mod.Infix()),
        Term.Name("a"),
        Nil,
        List(List(Term.Param(Nil, Term.Name("param"), Some(Type.Name("Int")), None))),
        None,
        Term.Name("param")
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
        Type.Name("A"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Decl.Type(
              List(Mod.Infix()),
              Type.Name("or"),
              List(
                Type.Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, None), Nil, Nil),
                Type.Param(Nil, Type.Name("Y"), Nil, Type.Bounds(None, None), Nil, Nil)
              ),
              Type.Bounds(None, None)
            ),
            Defn.Def(
              List(Mod.Infix()),
              Term.Name("x"),
              Nil,
              List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None))),
              Some(Type.ApplyInfix(Type.Name("String"), Type.Name("or"), Type.Name("Int"))),
              Lit.Int(1)
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
        Type.Name("A"),
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
        Type.Name("A"),
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
        Term.Name("infix"),
        Nil,
        List(List(Term.Param(Nil, Term.Name("infix"), Some(Type.Name("infix")), None))),
        Some(Type.Name("infix")),
        Term.NewAnonymous(
          Template(Nil, List(Init(Type.Name("infix"), Name(""), Nil)), Self(Name(""), None), Nil)
        )
      )
    )
  }

  test("extension-method") {
    runTestAssert[Stat]("extension (i: Int) infix def zero(other: Int): Int = 0")(
      Defn.ExtensionGroup(
        Term.Param(Nil, Term.Name("i"), Some(Type.Name("Int")), None),
        Nil,
        Nil,
        Defn.Def(
          List(Mod.Infix()),
          Term.Name("zero"),
          Nil,
          List(List(Term.Param(Nil, Term.Name("other"), Some(Type.Name("Int")), None))),
          Some(Type.Name("Int")),
          Lit.Int(0)
        )
      )
    )
  }

}
