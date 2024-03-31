package scala.meta.tests.parsers.dotty

import scala.meta._

class InterleavedDefnSuite extends BaseDottySuite {

  import dialects.Scala3Future

  override protected val dialect: Dialect = Scala3Future

  test("def x = 2") {
    checkTree(templStat("def x = 2"))(Defn.Def(Nil, tname("x"), Nil, None, int(2)))
  }

  test("def x[A <: B] = 2") {
    checkTree(templStat("def x[A <: B] = 2")) {
      Defn.Def(Nil, tname("x"), pparam(Nil, "A", hiBound("B")) :: Nil, Nil, None, int(2))
    }
  }

  test("def x[A: B] = 2") {
    checkTree(templStat("def x[A: B] = 2")) {
      Defn.Def(
        Nil,
        tname("x"),
        pparam(Nil, "A", vb = Nil, cb = pname("B") :: Nil) :: Nil,
        Nil,
        None,
        int(2)
      )
    }
  }

  test("def f(a: Int)(implicit b: Int) = a + b") {
    checkTree(templStat("def f(a: Int)(implicit b: Int) = a + b")) {
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(tparam("a", "Int") :: Nil, tparam(Mod.Implicit() :: Nil, "b", "Int") :: Nil),
        None,
        Term.ApplyInfix(tname("a"), tname("+"), Nil, tname("b") :: Nil)
      )
    }
  }

  test("def f(x: Int) = macro impl") {
    checkTree(templStat("def f(x: Int) = macro impl")) {
      Defn.Macro(Nil, tname("f"), Nil, List(tparam(List(), "x", "Int") :: Nil), None, tname("impl"))
    }
  }

  test("def f(x: Int): Int = macro impl") {
    checkTree(templStat("def f(x: Int): Int = macro impl")) {
      Defn.Macro(
        Nil,
        tname("f"),
        Nil,
        List(tparam(List(), "x", "Int") :: Nil),
        Some(pname("Int")),
        tname("impl")
      )
    }
  }

  test("braces-in-functions") {
    val defn = templStat(
      """|def f = { (n: Int) =>
         |  {
         |    for {
         |      _ <- scala.util.Success(123)
         |    } yield 42
         |  }.recover(???)
         |}""".stripMargin
    )
    checkTree(defn)(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      None,
      Term.Block(List(Term.Function(
        List(tparam("n", "Int")),
        Term.Apply(
          Term.Select(
            Term.Block(List(Term.ForYield(
              List(Enumerator.Generator(
                Pat.Wildcard(),
                Term.Apply(
                  Term.Select(Term.Select(tname("scala"), tname("util")), tname("Success")),
                  List(int(123))
                )
              )),
              int(42)
            ))),
            tname("recover")
          ),
          List(tname("???"))
        )
      )))
    ))
  }

  test("def f[A][B]: A = ???") {
    runTestError[Stat](
      "def f[A][B]: A = ???",
      """|error: `=` expected but `[` found
         |def f[A][B]: A = ???
         |        ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B]: B = ???") {
    runTestAssert[Stat]("def f[A](a: A, as: A*)[B]: B = ???") {
      Defn.Def(
        Nil,
        tname("f"),
        List(
          Member.ParamClauseGroup(
            Type.ParamClause(pparam("A") :: Nil),
            Term
              .ParamClause(List(tparam("a", "A"), tparam("as", Type.Repeated(pname("A")))), None) ::
              Nil
          ),
          Member.ParamClauseGroup(Type.ParamClause(pparam("B") :: Nil), Nil)
        ),
        Some(pname("B")),
        tname("???")
      )
    }
  }

  test("def f[A](implicit a: A)[B](implicit b: B): B = ???") {
    runTestError[Stat](
      "def f[A](implicit a: A)[B](implicit b: B): B = ???",
      """|error: `=` expected but `[` found
         |def f[A](implicit a: A)[B](implicit b: B): B = ???
         |                       ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B = ???") {
    runTestAssert[Stat]("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B = ???") {
      Defn.Def(
        Nil,
        tname("f"),
        List(
          Member.ParamClauseGroup(
            Type.ParamClause(pparam("A") :: Nil),
            Term
              .ParamClause(List(tparam("a", "A"), tparam("as", Type.Repeated(pname("A")))), None) ::
              Nil
          ),
          Member.ParamClauseGroup(
            Type.ParamClause(pparam("B") :: Nil),
            Term
              .ParamClause(List(tparam("b", "B"), tparam("bs", Type.Repeated(pname("B")))), None) ::
              Nil
          ),
          Member.ParamClauseGroup(
            Type.ParamClause(pparam("C") :: Nil),
            Term.ParamClause(List(tparam(List(Mod.Implicit()), "c", "C")), Some(Mod.Implicit())) ::
              Nil
          )
        ),
        Some(pname("B")),
        tname("???")
      )
    }
  }

}
