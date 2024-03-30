package scala.meta.tests.parsers.dotty

import scala.meta._

class InterleavedDeclSuite extends BaseDottySuite {

  import dialects.Scala3Future

  override protected val dialect: Dialect = Scala3Future

  test("def f: Unit") {
    checkTree(templStat("def f: Unit"))(Decl.Def(Nil, tname("f"), Nil, pname("Unit")))
  }

  test("def f(x: Int): Unit") {
    checkTree(templStat("def f(x: Int): Unit")) {
      Decl.Def(Nil, tname("f"), Nil, List(tparam("x", "Int") :: Nil), pname("Unit"))
    }
  }

  test("def f(x: Int*): Unit") {
    checkTree(templStat("def f(x: Int*): Unit")) {
      Decl.Def(
        Nil,
        tname("f"),
        Nil,
        List(tparam("x", Type.Repeated(pname("Int"))) :: Nil),
        pname("Unit")
      )
    }
  }

  test("def f(x: => Int): Unit") {
    checkTree(templStat("def f(x: => Int): Unit")) {
      Decl
        .Def(Nil, tname("f"), Nil, List(tparam("x", Type.ByName(pname("Int"))) :: Nil), pname("Unit"))
    }
  }

  test("def f(implicit x: Int): Unit") {
    checkTree(templStat("def f(implicit x: Int): Unit")) {
      Decl.Def(
        Nil,
        tname("f"),
        Nil,
        List(tparam(Mod.Implicit() :: Nil, "x", "Int") :: Nil),
        pname("Unit")
      )
    }
  }

  test("def f: X")(checkTree(templStat("def f: X"))(Decl.Def(Nil, tname("f"), Nil, pname("X"))))

  test("def f[T]: T") {
    checkTree(templStat("def f[T]: T")) {
      Decl.Def(Nil, tname("f"), pparam("T") :: Nil, Nil, pname("T"))
    }
  }

  test("def f[A][B]: A") {
    runTestError[Stat](
      "def f[A][B]: A",
      """|error: `=` expected but `[` found
         |def f[A][B]: A
         |        ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B]: B") {
    runTestAssert[Stat]("def f[A](a: A, as: A*)[B]: B") {
      Decl.Def(
        Nil,
        tname("f"),
        Member.ParamClauseGroup(
          Type.ParamClause(pparam("A") :: Nil),
          Term.ParamClause(List(tparam("a", "A"), tparam("as", Type.Repeated(pname("A")))), None) ::
            Nil
        ) :: Member.ParamClauseGroup(Type.ParamClause(pparam("B") :: Nil), Nil) :: Nil,
        pname("B")
      )
    }
  }

  test("def f[A](implicit a: A)[B](implicit b: B): B") {
    runTestError[Stat](
      "def f[A](implicit a: A)[B](implicit b: B): B",
      """|error: `=` expected but `[` found
         |def f[A](implicit a: A)[B](implicit b: B): B
         |                       ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B") {
    runTestAssert[Stat]("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B") {
      Decl.Def(
        Nil,
        tname("f"),
        Member.ParamClauseGroup(
          Type.ParamClause(pparam("A") :: Nil),
          Term.ParamClause(List(tparam("a", "A"), tparam("as", Type.Repeated(pname("A")))), None) ::
            Nil
        ) :: Member.ParamClauseGroup(
          Type.ParamClause(pparam("B") :: Nil),
          Term.ParamClause(List(tparam("b", "B"), tparam("bs", Type.Repeated(pname("B")))), None) ::
            Nil
        ) :: Member.ParamClauseGroup(
          Type.ParamClause(pparam("C") :: Nil),
          Term.ParamClause(List(tparam(List(Mod.Implicit()), "c", "C")), Some(Mod.Implicit())) ::
            Nil
        ) :: Nil,
        pname("B")
      )
    }
  }

  test("single ..., without tparams") {
    val q"..$mods def $name(...$paramss): $tpe" = q"def f(x: Int)(y: Int): Unit"
    val paramsExpected = Term.ParamClause(tparam("x", "Int") :: Nil, None) ::
      Term.ParamClause(tparam("y", "Int") :: Nil, None) :: Nil

    checkTreesWithSyntax(paramss: _*)("(x: Int)", "(y: Int)")(paramsExpected: _*)
    assertTree(q"..$mods def $name(...$paramss): $tpe")(
      Decl.Def(Nil, tname("f"), Member.ParamClauseGroup(Nil, paramsExpected) :: Nil, pname("Unit"))
    )
  }

  test("single ..., with tparams") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpe" = q"def f[A, B](x: Int)(y: Int): Unit"
    val tparamsExpected = Type.ParamClause(List(pparam("A"), pparam("B")))
    val paramsExpected = Term.ParamClause(tparam("x", "Int") :: Nil, None) ::
      Term.ParamClause(tparam("y", "Int") :: Nil, None) :: Nil

    checkTree(tparams, "[A, B]")(tparamsExpected)
    checkTreesWithSyntax(paramss: _*)("(x: Int)", "(y: Int)")(paramsExpected: _*)
    assertTree(q"..$mods def $name[..$tparams](...$paramss): $tpe")(Decl.Def(
      Nil,
      tname("f"),
      Member.ParamClauseGroup(tparamsExpected, paramsExpected) :: Nil,
      pname("Unit")
    ))
  }

  test("single ...., with tparams") {
    val q"..$mods def $name(....$paramss): $tpe" = q"def f(x: Int)[A](y: Int)[B]: Unit"
    val pcGroups = Member
      .ParamClauseGroup(Nil, Term.ParamClause(List(tparam("x", "Int")), None) :: Nil) ::
      Member.ParamClauseGroup(
        Type.ParamClause(pparam("A") :: Nil),
        Term.ParamClause(List(tparam("y", "Int")), None) :: Nil
      ) :: Member.ParamClauseGroup(Type.ParamClause(pparam("B") :: Nil), Nil) :: Nil

    checkTreesWithSyntax(paramss: _*)("(x: Int)", "[A](y: Int)", "[B]")(pcGroups: _*)
    assertTree(q"..$mods def $name(....$paramss): $tpe")(
      Decl.Def(Nil, tname("f"), pcGroups, pname("Unit"))
    )
  }

}
