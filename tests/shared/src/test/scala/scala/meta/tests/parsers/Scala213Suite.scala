package scala.meta.tests
package parsers

import scala.meta._

class Scala213Suite extends ParseSuite {
  private def runAssert(
      code: String
  )(expected: Tree)(implicit d: Dialect, loc: munit.Location): Unit = {
    assertTree(templStat(code)(d))(expected)
  }

  import dialects.Scala213

  checkOK("def foo(implicit x: => Int) = 1")
  checkOK("def foo(implicit y: Int, x: => Int) = 1")

  test("literal-types") {
    // https://docs.scala-lang.org/sips/42.type.html
    runAssert("val a: 42 = 42")(
      Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), Some(Lit.Int(42)), Lit.Int(42))
    )

    runAssert("val a: -2 = -2")(
      Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), Some(Lit.Int(-2)), Lit.Int(-2))
    )

    runAssert("def foo(a: 3.14159f): .1d")(
      Decl.Def(
        Nil,
        Term.Name("foo"),
        Nil,
        List(List(Term.Param(Nil, Term.Name("a"), Some(Lit.Float("3.14159f")), None))),
        Lit.Double(".1d")
      )
    )

    runAssert("def foo(x: 'a'): Option['z']")(
      Decl.Def(
        Nil,
        Term.Name("foo"),
        Nil,
        List(List(Term.Param(Nil, Term.Name("x"), Some(Lit.Char('a')), None))),
        Type.Apply(Type.Name("Option"), List(Lit.Char('z')))
      )
    )

    runAssert("def bar[T <: 1](t: T): T = t")(
      Defn.Def(
        Nil,
        Term.Name("bar"),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, Some(Lit.Int(1))), Nil, Nil)),
        List(List(Term.Param(Nil, Term.Name("t"), Some(Type.Name("T")), None))),
        Some(Type.Name("T")),
        Term.Name("t")
      )
    )
  }

  test("identifier-types") {
    // "x" is a class with def =>>(x: Int): Int, in dotty ==> is keyword and it will be error here
    runAssert("val c = x =>> 3")(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("c"))),
        None,
        Term.ApplyInfix(Term.Name("x"), Term.Name("=>>"), Nil, List(Lit.Int(3)))
      )
    )

    runAssert("val given = 3")(
      Defn.Val(Nil, List(Pat.Var(Term.Name("given"))), None, Lit.Int(3))
    )

    runAssert("val extension = 3")(
      Defn.Val(Nil, List(Pat.Var(Term.Name("extension"))), None, Lit.Int(3))
    )

    runAssert("val enum = 3")(
      Defn.Val(Nil, List(Pat.Var(Term.Name("enum"))), None, Lit.Int(3))
    )
  }

  test("try with any expr") {
    runAssert("try (1 + 2).toString")(
      Term.Try(
        Term.Select(
          Term.ApplyInfix(Lit.Int(1), Term.Name("+"), Nil, List(Lit.Int(2))),
          Term.Name("toString")
        ),
        Nil,
        None
      )
    )
    runAssert("try { 1 + 2 }.toString")(
      Term.Try(
        Term.Select(
          Term.Block(List(Term.ApplyInfix(Lit.Int(1), Term.Name("+"), Nil, List(Lit.Int(2))))),
          Term.Name("toString")
        ),
        Nil,
        None
      )
    )
    runAssert("try (1 :: Nil) map fn")(
      Term.Try(
        Term.ApplyInfix(
          Term.ApplyInfix(Lit.Int(1), Term.Name("::"), Nil, List(Term.Name("Nil"))),
          Term.Name("map"),
          Nil,
          List(Term.Name("fn"))
        ),
        Nil,
        None
      )
    )
    runAssert("try (true, false)")(
      Term.Try(Term.Tuple(List(Lit.Boolean(true), Lit.Boolean(false))), Nil, None)
    )
    runAssert("try ()")(Term.Try(Lit.Unit(), Nil, None))
  }

  test("infix-at-line-start") {
    implicit val Scala213 = dialects.Scala213Source3
    runAssert(
      """|val x = "hello"
         |  ++ "world"
         |""".stripMargin
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("x"))),
        None,
        Term.ApplyInfix(Lit.String("hello"), Term.Name("++"), Nil, List(Lit.String("world")))
      )
    )
  }

  test("issue-2880") {
    implicit val Scala213 = dialects.Scala213Source3
    runAssert(
      """|Flow {
         |    b.add()
         |
         |    input_< ~> filtering ~> removeItems.in0
         |}
         |""".stripMargin
    )(
      Term.Apply(
        Term.Name("Flow"),
        Term.Block(
          List(
            Term.Apply(Term.Select(Term.Name("b"), Term.Name("add")), Nil),
            Term.ApplyInfix(
              Term.ApplyInfix(
                Term.Name("input_<"),
                Term.Name("~>"),
                Nil,
                List(Term.Name("filtering"))
              ),
              Term.Name("~>"),
              Nil,
              List(Term.Select(Term.Name("removeItems"), Term.Name("in0")))
            )
          )
        ) :: Nil
      )
    )
  }

}
