package scala.meta.tests
package parsers

import scala.meta._

class DefnSuite extends ParseSuite {

  implicit val dialect: Dialect = dialects.Scala213

  test("val x = 2") {
    assertTree(templStat("val x = 2"))(Defn.Val(Nil, Pat.Var(tname("x")) :: Nil, None, int(2)))
  }

  test("var x = 2") {
    assertTree(templStat("var x = 2"))(Defn.Var(Nil, Pat.Var(tname("x")) :: Nil, None, Some(int(2))))
  }

  test("val x, y = 2") {
    assertTree(templStat("val x, y = 2"))(
      Defn.Val(Nil, Pat.Var(tname("x")) :: Pat.Var(tname("y")) :: Nil, None, int(2))
    )
  }

  test("val x: Int = 2") {
    assertTree(templStat("val x: Int = 2"))(
      Defn.Val(Nil, Pat.Var(tname("x")) :: Nil, Some(pname("Int")), int(2))
    )
  }

  test("val `x`: Int = 2") {
    assertTree(templStat("val `x`: Int = 2"))(
      Defn.Val(Nil, Pat.Var(tname("x")) :: Nil, Some(pname("Int")), int(2))
    )
  }

  test("val f: Int => String = _.toString") {
    assertTree(templStat("val f: Int => String = _.toString"))(Defn.Val(
      Nil,
      Pat.Var(tname("f")) :: Nil,
      Some(Type.Function(pname("Int") :: Nil, pname("String"))),
      Term.AnonymousFunction(Term.Select(Term.Placeholder(), tname("toString")))
    ))
  }

  test("var f: Int => String = _.toString") {
    assertTree(templStat("var f: Int => String = _.toString"))(Defn.Var(
      Nil,
      Pat.Var(tname("f")) :: Nil,
      Some(Type.Function(pname("Int") :: Nil, pname("String"))),
      Some(Term.AnonymousFunction(Term.Select(Term.Placeholder(), tname("toString"))))
    ))
  }

  test("var x: Int = _") {
    assertTree(templStat("var x: Int = _"))(
      Defn.Var(Nil, Pat.Var(tname("x")) :: Nil, Some(pname("Int")), None)
    )
  }

  test("var x = _ is not allowed")(intercept[parsers.ParseException](templStat("var x = _")))

  test("val x: Int = _ is not allowed") {
    intercept[parsers.ParseException](templStat("val x: Int = _"))
  }

  test("val (x: Int) = 2") {
    assertTree(templStat("val (x: Int) = 2"))(
      Defn.Val(Nil, Pat.Typed(Pat.Var(tname("x")), pname("Int")) :: Nil, None, int(2))
    )
  }

  test("type A = B") {
    assertTree(templStat("type A = B")) {
      Defn.Type(Nil, pname("A"), Type.ParamClause(Nil), pname("B"))
    }
  }

  test("type F[T] = List[T]") {
    assertTree(templStat("type F[T] = List[T]")) {
      Defn.Type(Nil, pname("F"), pparam("T") :: Nil, Type.Apply(pname("List"), pname("T") :: Nil))
    }
  }

  test("def x = 2") {
    assertTree(templStat("def x = 2"))(Defn.Def(Nil, tname("x"), Nil, Nil, None, int(2)))
  }

  test("def x[A <: B] = 2") {
    assertTree(templStat("def x[A <: B] = 2")) {
      Defn.Def(Nil, tname("x"), pparam("A", hiBound("B")) :: Nil, Nil, None, int(2))
    }
  }

  test("def x[A <% B] = 2") {
    assertTree(templStat("def x[A <% B] = 2")) {
      Defn.Def(
        Nil,
        tname("x"),
        pparam(Nil, "A", noBounds, vb = pname("B") :: Nil) :: Nil,
        Nil,
        None,
        int(2)
      )
    }
  }

  test("def x[A: B] = 2") {
    assertTree(templStat("def x[A: B] = 2")) {
      Defn.Def(Nil, tname("x"), pparam(Nil, "A", cb = pname("B") :: Nil) :: Nil, Nil, None, int(2))
    }
  }

  test("def f(a: Int)(implicit b: Int) = a + b") {
    assertTree(templStat("def f(a: Int)(implicit b: Int) = a + b")) {
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        (tparam("a", "Int") :: Nil) :: (tparam(Mod.Implicit() :: Nil, "b", "Int") :: Nil) :: Nil,
        None,
        Term.ApplyInfix(tname("a"), tname("+"), Nil, tname("b") :: Nil)
      )
    }
  }

  test("def proc { return 42 }") {
    assertTree(templStat("def proc { return 42 }")) {
      Defn.Def(
        Nil,
        tname("proc"),
        Nil,
        Nil,
        Some(pname("Unit")),
        Term.Block(Term.Return(int(42)) :: Nil)
      )
    }
  }

  test("def f(x: Int) = macro impl") {
    assertTree(templStat("def f(x: Int) = macro impl")) {
      Defn.Macro(
        Nil,
        tname("f"),
        Type.ParamClause(Nil),
        (tparam("x", "Int") :: Nil) :: Nil,
        None,
        tname("impl")
      )
    }
  }

  test("def f(x: Int): Int = macro impl") {
    assertTree(templStat("def f(x: Int): Int = macro impl")) {
      Defn.Macro(
        Nil,
        tname("f"),
        Nil,
        (tparam("x", "Int") :: Nil) :: Nil,
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
    assertTree(defn)(Defn.Def(
      Nil,
      tname("f"),
      Type.ParamClause(Nil),
      Nil,
      None,
      Term.Block(
        Term.Function(
          List(tparam("n", "Int")),
          Term.Apply(
            Term.Select(
              Term.Block(
                Term.ForYield(
                  Enumerator.Generator(
                    Pat.Wildcard(),
                    Term.Apply(
                      Term.Select(Term.Select(tname("scala"), tname("util")), tname("Success")),
                      List(int(123))
                    )
                  ) :: Nil,
                  int(42)
                ) :: Nil
              ),
              tname("recover")
            ),
            List(tname("???"))
          )
        ) :: Nil
      )
    ))
  }

  test("braces-in-if-cond") {
    val defn = templStat(
      """|if (cond) { expr }.select else { expr } + { expr }
         |""".stripMargin
    )
    assertTree(defn)(Term.If(
      tname("cond"),
      Term.Select(Term.Block(List(tname("expr"))), tname("select")),
      Term.ApplyInfix(
        Term.Block(List(tname("expr"))),
        tname("+"),
        Nil,
        List(Term.Block(List(tname("expr"))))
      ),
      Nil
    ))
  }

  test("braces-in-try-expr") {
    val defn = templStat(
      """|try {expr}.select finally {expr}.select 
         |""".stripMargin
    )
    assertTree(defn)(Term.Try(
      Term.Select(Term.Block(List(tname("expr"))), tname("select")),
      Nil,
      Some(Term.Select(Term.Block(List(tname("expr"))), tname("select")))
    ))
  }

  test("braces-in-while-expr") {
    val defn = templStat(
      """|while (cond) {expr}.select
         |""".stripMargin
    )
    assertTree(defn)(
      Term.While(tname("cond"), Term.Select(Term.Block(List(tname("expr"))), tname("select")))
    )
  }

  test("braces-in-for-expr") {
    val defn = templStat(
      """|for (i <- list) {expr}.select
         |""".stripMargin
    )
    assertTree(defn)(Term.For(
      List(Enumerator.Generator(Pat.Var(tname("i")), tname("list"))),
      Term.Select(Term.Block(List(tname("expr"))), tname("select"))
    ))
  }

  test("inline is not allowed") {
    intercept[parsers.ParseException](blockStat("inline def x = 42"))
  }

  test("infix is not allowed")(intercept[parsers.ParseException](blockStat("infix def x = 42")))

  test("#3210") {
    val code = """|a3 match {
                  |  case Some(_) =>
                  |    case class A6(a7: A8)
                  |
                  |    object A9
                  |}
                  |""".stripMargin
    val layout = """|a3 match {
                    |  case Some(_) =>
                    |    case class A6(a7: A8)
                    |    object A9
                    |}
                    |""".stripMargin
    val tree = Term.Match(
      tname("a3"),
      Case(
        Pat.Extract(tname("Some"), List(Pat.Wildcard())),
        None,
        blk(
          Defn.Class(List(Mod.Case()), pname("A6"), Nil, ctorp(tparam("a7", "A8")), tplNoBody()),
          Defn.Object(Nil, tname("A9"), tplNoBody())
        )
      ) :: Nil,
      Nil
    )
    runTestAssert[Stat](code, assertLayout = Some(layout))(tree)
  }

  test("#3571 scala213") {
    val code = """|new A {
                  |  def b: C =
                  |    ???
                  |}
                  |""".stripMargin
    val layout = """|new A { def b: C = ??? }
                    |""".stripMargin
    val tree = Term.NewAnonymous(tpl(
      List(Init(pname("A"), anon, Nil)),
      List(Defn.Def(Nil, tname("b"), Nil, Some(pname("C")), tname("???")))
    ))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3605 scala213") {
    val code = """|new A {
                  |  def b: C =
                  |    ???
                  |
                  |}
                  |""".stripMargin
    val layout = """|new A { def b: C = ??? }
                    |""".stripMargin
    val tree = Term.NewAnonymous(tpl(
      List(Init(pname("A"), anon, Nil)),
      List(Defn.Def(Nil, tname("b"), Nil, Some(pname("C")), tname("???")))
    ))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3617 scala213") {
    val codeWithoutBlank = """|object Test {
                              |  def bar =
                              |    `f-oo`
                              |}
                              |""".stripMargin
    val codeWithBlank = """|object Test {
                           |  def bar =
                           |    `f-oo`
                           |
                           |}
                           |""".stripMargin
    val layout = "object Test { def bar = `f-oo` }"
    val tree = Defn
      .Object(Nil, tname("Test"), tpl(Defn.Def(Nil, tname("bar"), Nil, None, tname("f-oo"))))
    runTestAssert[Stat](codeWithoutBlank, layout)(tree)
    runTestAssert[Stat](codeWithBlank, layout)(tree)
  }

  test("#3571 scala213source3") {
    implicit val dialect = dialects.Scala213Source3
    val code = """|new A {
                  |  def b: C =
                  |    ???
                  |}
                  |""".stripMargin
    val layout = """|new A { def b: C = ??? }
                    |""".stripMargin
    val tree = Term.NewAnonymous(tpl(
      List(Init(pname("A"), anon, Nil)),
      List(Defn.Def(Nil, tname("b"), Nil, Some(pname("C")), tname("???")))
    ))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3605 scala213source3") {
    implicit val dialect = dialects.Scala213Source3
    val code = """|new A {
                  |  def b: C =
                  |    ???
                  |
                  |}
                  |""".stripMargin
    val layout = """|new A { def b: C = ??? }
                    |""".stripMargin
    val tree = Term.NewAnonymous(tpl(
      List(Init(pname("A"), anon, Nil)),
      List(Defn.Def(Nil, tname("b"), Nil, Some(pname("C")), tname("???")))
    ))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3617 scala213source3") {
    implicit val dialect = dialects.Scala213Source3
    val codeWithoutBlank = """|object Test {
                              |  def bar =
                              |    `f-oo`
                              |}
                              |""".stripMargin
    val codeWithBlank = """|object Test {
                           |  def bar =
                           |    `f-oo`
                           |
                           |}
                           |""".stripMargin
    val layout = "object Test { def bar = `f-oo` }"
    val tree = Defn
      .Object(Nil, tname("Test"), tpl(Defn.Def(Nil, tname("bar"), Nil, None, tname("f-oo"))))
    runTestAssert[Stat](codeWithoutBlank, layout)(tree)
    runTestAssert[Stat](codeWithBlank, layout)(tree)
  }

  test("final trait") {
    val code = "final trait Singleton extends Any"
    val tree = Defn.Trait(List(Mod.Final()), pname("Singleton"), Nil, ctor, tplNoBody(init("Any")))
    runTestAssert[Stat](code)(tree)
  }

  test("#4133 intellij ScalaImportTypeFix.scala") {
    val code =
      """|private def hasApplyMethod(`class`: PsiClass): Boolean = `class` match {
         |  case `object`: ScObject => `object`.allFunctionsByName(ScFunction.CommonNames.Apply).nonEmpty
         |  case `class` @ ScClass(`type`) => isCaseOrInScala3File(`class`) // SCL-19992, SCL-21187
         |  case _ => false
         |} 
         |""".stripMargin
    val layout = """|private def hasApplyMethod(`class`: PsiClass): Boolean = `class` match {
                    |  case `object`: ScObject =>
                    |    `object`.allFunctionsByName(ScFunction.CommonNames.Apply).nonEmpty
                    |  case `class` @ ScClass(`type`) =>
                    |    isCaseOrInScala3File(`class`)
                    |  case _ =>
                    |    false
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      List(Mod.Private(anon)),
      tname("hasApplyMethod"),
      Nil,
      List(List(tparam("class", "PsiClass"))),
      Some(pname("Boolean")),
      Term.Match(
        tname("class"),
        List(
          Case(
            Pat.Typed(Pat.Var(tname("object")), pname("ScObject")),
            None,
            Term.Select(
              Term.Apply(
                Term.Select(tname("object"), tname("allFunctionsByName")),
                List(
                  Term
                    .Select(Term.Select(tname("ScFunction"), tname("CommonNames")), tname("Apply"))
                )
              ),
              tname("nonEmpty")
            )
          ),
          Case(
            Pat.Bind(Pat.Var(tname("class")), Pat.Extract(tname("ScClass"), List(tname("type")))),
            None,
            Term.Apply(tname("isCaseOrInScala3File"), List(tname("class")))
          ),
          Case(Pat.Wildcard(), None, lit(false))
        ),
        Nil
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
