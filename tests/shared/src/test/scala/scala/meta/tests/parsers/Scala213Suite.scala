package scala.meta.tests
package parsers

import scala.meta._

class Scala213Suite extends ParseSuite {
  private def runAssert(code: String)(
      expected: Tree
  )(implicit d: Dialect, loc: munit.Location): Unit = assertTree(templStat(code)(d))(expected)

  import dialects.Scala213

  checkOK("def foo(implicit x: => Int) = 1")
  checkOK("def foo(implicit y: Int, x: => Int) = 1")

  test("literal-types") {
    // https://docs.scala-lang.org/sips/42.type.html
    runAssert("val a: 42 = 42")(Defn.Val(Nil, List(Pat.Var(tname("a"))), Some(int(42)), int(42)))

    runAssert("val a: -2 = -2")(Defn.Val(Nil, List(Pat.Var(tname("a"))), Some(int(-2)), int(-2)))

    runAssert("def foo(a: 3.14159f): .1d")(Decl.Def(
      Nil,
      tname("foo"),
      Nil,
      List(List(tparam("a", Lit.Float("3.14159f")))),
      Lit.Double("0.1d")
    ))

    runAssert("def foo(x: 'a'): Option['z']")(Decl.Def(
      Nil,
      tname("foo"),
      Nil,
      List(List(tparam("x", Lit.Char('a')))),
      Type.Apply(pname("Option"), List(Lit.Char('z')))
    ))

    runAssert("def bar[T <: 1](t: T): T = t")(Defn.Def(
      Nil,
      tname("bar"),
      List(pparam("T", hiBound(int(1)))),
      List(List(tparam("t", "T"))),
      Some(pname("T")),
      tname("t")
    ))
  }

  test("identifier-types") {
    // "x" is a class with def =>>(x: Int): Int, in dotty ==> is keyword and it will be error here
    runAssert("val c = x =>> 3")(Defn.Val(
      Nil,
      List(Pat.Var(tname("c"))),
      None,
      Term.ApplyInfix(tname("x"), tname("=>>"), Nil, List(int(3)))
    ))

    runAssert("val given = 3")(Defn.Val(Nil, List(Pat.Var(tname("given"))), None, int(3)))

    runAssert("val extension = 3")(Defn.Val(Nil, List(Pat.Var(tname("extension"))), None, int(3)))

    runAssert("val enum = 3")(Defn.Val(Nil, List(Pat.Var(tname("enum"))), None, int(3)))
  }

  test("try with any expr") {
    runAssert("try (1 + 2).toString")(Term.Try(
      Term.Select(Term.ApplyInfix(int(1), tname("+"), Nil, List(int(2))), tname("toString")),
      Nil,
      None
    ))
    runAssert("try { 1 + 2 }.toString")(Term.Try(
      Term.Select(
        Term.Block(List(Term.ApplyInfix(int(1), tname("+"), Nil, List(int(2))))),
        tname("toString")
      ),
      Nil,
      None
    ))
    runAssert("try (1 :: Nil) map fn")(Term.Try(
      Term.ApplyInfix(
        Term.ApplyInfix(int(1), tname("::"), Nil, List(tname("Nil"))),
        tname("map"),
        Nil,
        List(tname("fn"))
      ),
      Nil,
      None
    ))
    runAssert("try (true, false)")(Term.Try(Term.Tuple(List(bool(true), bool(false))), Nil, None))
    runAssert("try ()")(Term.Try(Lit.Unit(), Nil, None))
  }

  test("infix-at-line-start") {
    implicit val Scala213 = dialects.Scala213Source3
    runAssert(
      """|val x = "hello"
         |  ++ "world"
         |""".stripMargin
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("x"))),
      None,
      Term.ApplyInfix(str("hello"), tname("++"), Nil, List(str("world")))
    ))
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
    )(Term.Apply(
      tname("Flow"),
      Term.Block(List(
        Term.Apply(Term.Select(tname("b"), tname("add")), Nil),
        Term.ApplyInfix(
          Term.ApplyInfix(tname("input_<"), tname("~>"), Nil, List(tname("filtering"))),
          tname("~>"),
          Nil,
          List(Term.Select(tname("removeItems"), tname("in0")))
        )
      )) :: Nil
    ))
  }

}
