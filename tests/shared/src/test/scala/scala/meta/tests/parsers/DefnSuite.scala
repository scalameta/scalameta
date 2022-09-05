package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.dialects.Scala213

class DefnSuite extends ParseSuite {
  test("val x = 2") {
    val Defn.Val(Nil, Pat.Var(Term.Name("x")) :: Nil, None, Lit(2)) = templStat("val x = 2")
  }

  test("var x = 2") {
    val Defn.Var(Nil, Pat.Var(Term.Name("x")) :: Nil, None, Some(Lit(2))) = templStat("var x = 2")
  }

  test("val x, y = 2") {
    val Defn.Val(Nil, Pat.Var(Term.Name("x")) :: Pat.Var(Term.Name("y")) :: Nil, None, Lit(2)) =
      templStat("val x, y = 2")
  }

  test("val x: Int = 2") {
    val Defn.Val(Nil, Pat.Var(Term.Name("x")) :: Nil, Some(Type.Name("Int")), Lit(2)) =
      templStat("val x: Int = 2")
  }

  test("val `x`: Int = 2") {
    val Defn.Val(Nil, Pat.Var(Term.Name("x")) :: Nil, Some(Type.Name("Int")), Lit(2)) =
      templStat("val `x`: Int = 2")
  }

  test("val f: Int => String = _.toString") {
    val Defn.Val(
      Nil,
      Pat.Var(Term.Name("f")) :: Nil,
      Some(Type.Function(Type.Name("Int") :: Nil, Type.Name("String"))),
      Term.AnonymousFunction(Term.Select(Term.Placeholder(), Term.Name("toString")))
    ) =
      templStat("val f: Int => String = _.toString")
  }

  test("var f: Int => String = _.toString") {
    val Defn.Var(
      Nil,
      Pat.Var(Term.Name("f")) :: Nil,
      Some(Type.Function(Type.Name("Int") :: Nil, Type.Name("String"))),
      Some(Term.AnonymousFunction(Term.Select(Term.Placeholder(), Term.Name("toString"))))
    ) =
      templStat("var f: Int => String = _.toString")
  }

  test("var x: Int = _") {
    val Defn.Var(Nil, Pat.Var(Term.Name("x")) :: Nil, Some(Type.Name("Int")), None) =
      templStat("var x: Int = _")
  }

  test("var x = _ is not allowed") {
    intercept[parsers.ParseException] {
      templStat("var x = _")
    }
  }

  test("val x: Int = _ is not allowed") {
    intercept[parsers.ParseException] {
      templStat("val x: Int = _")
    }
  }

  test("val (x: Int) = 2") {
    val Defn.Val(Nil, Pat.Typed(Pat.Var(Term.Name("x")), Type.Name("Int")) :: Nil, None, Lit(2)) =
      templStat("val (x: Int) = 2")
  }

  test("type A = B") {
    val Defn.Type(Nil, Type.Name("A"), Nil, Type.Name("B")) =
      templStat("type A = B")
  }

  test("type F[T] = List[T]") {
    val Defn.Type(
      Nil,
      Type.Name("F"),
      Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil,
      Type.Apply(Type.Name("List"), Type.Name("T") :: Nil)
    ) = templStat("type F[T] = List[T]")
  }

  test("def x = 2") {
    val Defn.Def(Nil, Term.Name("x"), Nil, Nil, None, Lit(2)) = templStat("def x = 2")
  }

  test("def x[A <: B] = 2") {
    val Defn.Def(
      Nil,
      Term.Name("x"),
      Type.Param(Nil, Type.Name("A"), Nil, Type.Bounds(None, Some(Type.Name("B"))), Nil, Nil)
        :: Nil,
      Nil,
      None,
      Lit(2)
    ) = templStat("def x[A <: B] = 2")
  }

  test("def x[A <% B] = 2") {
    val Defn.Def(
      Nil,
      Term.Name("x"),
      Type.Param(Nil, Type.Name("A"), Nil, Type.Bounds(None, None), Type.Name("B") :: Nil, Nil)
        :: Nil,
      Nil,
      None,
      Lit(2)
    ) = templStat("def x[A <% B] = 2")
  }

  test("def x[A: B] = 2") {
    val Defn.Def(
      Nil,
      Term.Name("x"),
      Type.Param(Nil, Type.Name("A"), Nil, Type.Bounds(None, None), Nil, Type.Name("B") :: Nil)
        :: Nil,
      Nil,
      None,
      Lit(2)
    ) = templStat("def x[A: B] = 2")
  }

  test("def f(a: Int)(implicit b: Int) = a + b") {
    val Defn.Def(
      Nil,
      Term.Name("f"),
      Nil,
      (Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None) :: Nil) ::
        (Term.Param(Mod.Implicit() :: Nil, Term.Name("b"), Some(Type.Name("Int")), None) :: Nil)
        :: Nil,
      None,
      Term.ApplyInfix(Term.Name("a"), Term.Name("+"), Nil, Term.Name("b") :: Nil)
    ) =
      templStat("def f(a: Int)(implicit b: Int) = a + b")
  }

  test("def proc { return 42 }") {
    val Defn.Def(
      Nil,
      Term.Name("proc"),
      Nil,
      Nil,
      Some(Type.Name("Unit")),
      Term.Block((ret @ Term.Return(Lit(42))) :: Nil)
    ) =
      templStat("def proc { return 42 }")
  }

  test("def f(x: Int) = macro impl") {
    val Defn.Macro(
      Nil,
      Term.Name("f"),
      Nil,
      (Term.Param(List(), Term.Name(x), Some(Type.Name("Int")), None) :: Nil) :: Nil,
      None,
      Term.Name("impl")
    ) = templStat("def f(x: Int) = macro impl")
  }

  test("def f(x: Int): Int = macro impl") {
    val Defn.Macro(
      Nil,
      Term.Name("f"),
      Nil,
      (Term.Param(List(), Term.Name(x), Some(Type.Name("Int")), None) :: Nil) :: Nil,
      Some(Type.Name("Int")),
      Term.Name("impl")
    ) = templStat("def f(x: Int): Int = macro impl")
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
    assertTree(defn)(
      Defn
        .Def(
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

  test("braces-in-if-cond") {
    val defn = templStat(
      """|if (cond) { expr }.select else { expr } + { expr }
         |""".stripMargin
    )
    assertTree(defn)(
      Term.If(
        Term.Name("cond"),
        Term.Select(Term.Block(List(Term.Name("expr"))), Term.Name("select")),
        Term.ApplyInfix(
          Term.Block(List(Term.Name("expr"))),
          Term.Name("+"),
          Nil,
          List(Term.Block(List(Term.Name("expr"))))
        ),
        Nil
      )
    )
  }

  test("braces-in-try-expr") {
    val defn = templStat(
      """|try {expr}.select finally {expr}.select 
         |""".stripMargin
    )
    assertTree(defn)(
      Term.Try(
        Term.Select(Term.Block(List(Term.Name("expr"))), Term.Name("select")),
        Nil,
        Some(Term.Select(Term.Block(List(Term.Name("expr"))), Term.Name("select")))
      )
    )
  }

  test("braces-in-while-expr") {
    val defn = templStat(
      """|while (cond) {expr}.select
         |""".stripMargin
    )
    assertTree(defn)(
      Term.While(
        Term.Name("cond"),
        Term.Select(Term.Block(List(Term.Name("expr"))), Term.Name("select"))
      )
    )
  }

  test("braces-in-for-expr") {
    val defn = templStat(
      """|for (i <- list) {expr}.select
         |""".stripMargin
    )
    assertTree(defn)(
      Term.For(
        List(Enumerator.Generator(Pat.Var(Term.Name("i")), Term.Name("list"))),
        Term.Select(Term.Block(List(Term.Name("expr"))), Term.Name("select"))
      )
    )
  }

  test("inline is not allowed") {
    intercept[parsers.ParseException] {
      blockStat("inline def x = 42")
    }
  }

  test("infix is not allowed") {
    intercept[parsers.ParseException] {
      blockStat("infix def x = 42")
    }
  }

  test("val with infix and no break") {
    val defn = templStat(
      """|val foo :: bar :: baz :: Nil =
         |  qux
         |""".stripMargin
    )
    assertTree(defn) {
      Defn.Val(
        Nil,
        Pat.ExtractInfix(
          Pat.Var(Term.Name("foo")),
          Term.Name("::"),
          Pat.ExtractInfix(
            Pat.Var(Term.Name("bar")),
            Term.Name("::"),
            Pat.ExtractInfix(
              Pat.Var(Term.Name("baz")),
              Term.Name("::"),
              List(Term.Name("Nil"))
            ) :: Nil
          ) :: Nil
        ) :: Nil,
        None,
        Term.Name("qux")
      )
    }
  }

  test("val with infix and break") {
    val defn = templStat(
      """|val foo :: bar ::
         |  baz :: Nil = qux
         |""".stripMargin
    )
    assertTree(defn) {
      Defn.Val(
        Nil,
        Pat.ExtractInfix(
          Pat.Var(Term.Name("foo")),
          Term.Name("::"),
          Pat.ExtractInfix(
            Pat.Var(Term.Name("bar")),
            Term.Name("::"),
            Pat.ExtractInfix(
              Pat.Var(Term.Name("baz")),
              Term.Name("::"),
              List(Term.Name("Nil"))
            ) :: Nil
          ) :: Nil
        ) :: Nil,
        None,
        Term.Name("qux")
      )
    }
  }

}
