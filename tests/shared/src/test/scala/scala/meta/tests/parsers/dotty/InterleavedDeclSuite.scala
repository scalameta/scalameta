package scala.meta.tests.parsers.dotty

import scala.meta._

class InterleavedDeclSuite extends BaseDottySuite {

  import dialects.Scala3Future

  protected override val dialect: Dialect = Scala3Future

  test("def f: Unit") {
    checkTree(templStat("def f: Unit")) {
      Decl.Def(Nil, Term.Name("f"), Nil, Nil, Type.Name("Unit"))
    }
  }

  test("def f(x: Int): Unit") {
    checkTree(templStat("def f(x: Int): Unit")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(
          Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil
        ),
        Type.Name("Unit")
      )
    }
  }

  test("def f(x: Int*): Unit") {
    checkTree(templStat("def f(x: Int*): Unit")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(
          Term.Param(Nil, Term.Name("x"), Some(Type.Repeated(Type.Name("Int"))), None) :: Nil
        ),
        Type.Name("Unit")
      )
    }
  }

  test("def f(x: => Int): Unit") {
    checkTree(templStat("def f(x: => Int): Unit")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(
          Term.Param(Nil, Term.Name("x"), Some(Type.ByName(Type.Name("Int"))), None) :: Nil
        ),
        Type.Name("Unit")
      )
    }
  }

  test("def f(implicit x: Int): Unit") {
    checkTree(templStat("def f(implicit x: Int): Unit")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(
          Term.Param(Mod.Implicit() :: Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil
        ),
        Type.Name("Unit")
      )
    }
  }

  test("def f: X") {
    checkTree(templStat("def f: X")) {
      Decl.Def(Nil, Term.Name("f"), Nil, Nil, Type.Name("X"))
    }
  }

  test("def f[T]: T") {
    checkTree(templStat("def f[T]: T")) {
      Decl.Def(
        Nil,
        Term.Name("f"),
        Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil,
        Nil,
        Type.Name("T")
      )
    }
  }

  test("def f[A][B]: A") {
    runTestError[Stat](
      "def f[A][B]: A",
      """|error: = expected but [ found
         |def f[A][B]: A
         |        ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B]: B") {
    runTestError[Stat](
      "def f[A](a: A, as: A*)[B]: B",
      """|error: = expected but [ found
         |def f[A](a: A, as: A*)[B]: B
         |                      ^""".stripMargin
    )
  }

  test("def f[A](implicit a: A)[B](implicit b: B): B") {
    runTestError[Stat](
      "def f[A](implicit a: A)[B](implicit b: B): B",
      """|error: = expected but [ found
         |def f[A](implicit a: A)[B](implicit b: B): B
         |                       ^""".stripMargin
    )
  }

  test("def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B") {
    runTestError[Stat](
      "def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B",
      """|error: = expected but [ found
         |def f[A](a: A, as: A*)[B](b: B, bs: B*)[C](implicit c: C): B
         |                      ^""".stripMargin
    )
  }

  test("single ..., without tparams") {
    val q"..$mods def $name(...$paramss): $tpe" = q"def f(x: Int)(y: Int): Unit"
    val paramsExpected =
      Term.ParamClause(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil,
        None
      ) :: Term.ParamClause(
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None) :: Nil,
        None
      ) :: Nil

    checkTreesWithSyntax(paramss: _*)("(x: Int)", "(y: Int)")(paramsExpected: _*)
    assertTree(q"..$mods def $name(...$paramss): $tpe")(
      Decl.Def(
        Nil,
        Term.Name("f"),
        Nil,
        paramsExpected.map(_.values),
        Type.Name("Unit")
      )
    )
  }

  test("single ..., with tparams") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpe" = q"def f[A, B](x: Int)(y: Int): Unit"
    val tparamsExpected = Type.ParamClause(
      List(
        Type.Param(Nil, Type.Name("A"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil),
        Type.Param(Nil, Type.Name("B"), Type.ParamClause(Nil), Type.Bounds(None, None), Nil, Nil)
      )
    )
    val paramsExpected =
      Term.ParamClause(
        Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None) :: Nil,
        None
      ) :: Term.ParamClause(
        Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None) :: Nil,
        None
      ) :: Nil

    checkTree(tparams, "[A, B]")(tparamsExpected)
    checkTreesWithSyntax(paramss: _*)("(x: Int)", "(y: Int)")(paramsExpected: _*)
    assertTree(q"..$mods def $name[..$tparams](...$paramss): $tpe")(
      Decl.Def(
        Nil,
        Term.Name("f"),
        tparamsExpected.values,
        paramsExpected.map(_.values),
        Type.Name("Unit")
      )
    )
  }

}
