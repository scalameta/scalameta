package scala.meta.tests.parsers.dotty

import scala.meta._

class InterleavedDefnSuite extends BaseDottySuite {

  import dialects.Scala3Future

  protected override val dialect: Dialect = Scala3Future

  test("def x = 2") {
    checkTree(templStat("def x = 2")) {
      Defn.Def(Nil, Term.Name("x"), Nil, Nil, None, Lit.Int(2))
    }
  }

  test("def x[A <: B] = 2") {
    checkTree(templStat("def x[A <: B] = 2")) {
      Defn.Def(
        Nil,
        Term.Name("x"),
        Type.Param(
          Nil,
          Type.Name("A"),
          Type.ParamClause(Nil),
          Type.Bounds(None, Some(Type.Name("B"))),
          Nil,
          Nil
        ) :: Nil,
        Nil,
        None,
        Lit.Int(2)
      )
    }
  }

  test("def x[A: B] = 2") {
    checkTree(templStat("def x[A: B] = 2")) {
      Defn.Def(
        Nil,
        Term.Name("x"),
        Type.Param(
          Nil,
          Type.Name("A"),
          Type.ParamClause(Nil),
          Type.Bounds(None, None),
          Nil,
          Type.Name("B") :: Nil
        ) :: Nil,
        Nil,
        None,
        Lit.Int(2)
      )
    }
  }

  test("def f(a: Int)(implicit b: Int) = a + b") {
    checkTree(templStat("def f(a: Int)(implicit b: Int) = a + b")) {
      Defn.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(
          Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None) :: Nil,
          Term.Param(Mod.Implicit() :: Nil, Term.Name("b"), Some(Type.Name("Int")), None) :: Nil
        ),
        None,
        Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Term.Name("b") :: Nil)
      )
    }
  }

  test("def f(x: Int) = macro impl") {
    checkTree(templStat("def f(x: Int) = macro impl")) {
      Defn.Macro(
        Nil,
        Term.Name("f"),
        Nil,
        List(Term.Param(List(), Term.Name("x"), Some(Type.Name("Int")), None) :: Nil),
        None,
        Term.Name("impl")
      )
    }
  }

  test("def f(x: Int): Int = macro impl") {
    checkTree(templStat("def f(x: Int): Int = macro impl")) {
      Defn.Macro(
        Nil,
        Term.Name("f"),
        Nil,
        List(Term.Param(List(), Term.Name("x"), Some(Type.Name("Int")), None) :: Nil),
        Some(Type.Name("Int")),
        Term.Name("impl")
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
    checkTree(defn)(
      Defn.Def(
        Nil,
        Term.Name("f"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Term.Function(
              List(Term.Param(Nil, Term.Name("n"), Some(Type.Name("Int")), None)),
              Term.Apply(
                Term.Select(
                  Term.Block(
                    List(
                      Term.ForYield(
                        List(
                          Enumerator.Generator(
                            Pat.Wildcard(),
                            Term.Apply(
                              Term.Select(
                                Term.Select(Term.Name("scala"), Term.Name("util")),
                                Term.Name("Success")
                              ),
                              List(Lit.Int(123))
                            )
                          )
                        ),
                        Lit.Int(42)
                      )
                    )
                  ),
                  Term.Name("recover")
                ),
                List(Term.Name("???"))
              )
            )
          )
        )
      )
    )
  }

  test("def f[A][B]: A = ???") {
    runTestError[Stat](
      "def f[A][B]: A = ???",
      """|error: = expected but [ found
         |def f[A][B]: A = ???
         |        ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B]: B = ???") {
    runTestError[Stat](
      "def f[A](a: A, as: A*)[B]: B = ???",
      """|error: = expected but [ found
         |def f[A](a: A, as: A*)[B]: B = ???
         |                      ^""".stripMargin
    )
  }

  test("def f[A](implicit a: A)[B](implicit b: B): B = ???") {
    runTestError[Stat](
      "def f[A](implicit a: A)[B](implicit b: B): B = ???",
      """|error: = expected but [ found
         |def f[A](implicit a: A)[B](implicit b: B): B = ???
         |                       ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B = ???") {
    runTestError[Stat](
      "def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B = ???",
      """|error: = expected but [ found
         |def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B = ???
         |                      ^""".stripMargin
    )
  }

}
