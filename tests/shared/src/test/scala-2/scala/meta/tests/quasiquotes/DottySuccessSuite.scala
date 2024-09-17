package scala.meta.tests

package quasiquotes

import scala.meta._

class DottySuccessSuite extends TreeSuiteBase {

  test("1 t\"(..tpes) => tpe [param clause]\"") {
    import dialects.Scala3Future
    val t"(..$tpes) => $tpe" = t"(x: X, y: Y) => Z"

    val tpeX = Type.TypedParam(Type.Name("x"), Type.Name("X"))
    val tpeY = Type.TypedParam(Type.Name("y"), Type.Name("Y"))
    checkTree(tpes, "(x: X, y: Y)")(Type.FuncParamClause(List(tpeX, tpeY)))
    checkTree(tpe, "Z")(Type.Name("Z"))

    checkTree(t"(..${tpes.values}) => $tpe", "(x: X, y: Y) => Z")(
      Type.Function(tpes, tpe)
    )
  }

  private final val cparam = tparam("c", "Circle")

  test("extension quasiquotes: triple, no tparams") {
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams](...$paramss) { ..$stats }" =
      q"""extension (c: Circle)(using Context, x: Int)(using y: String, File) {
                def crc: Int = 2
              }"""

    assertEquals(tparams.length, 0)

    assertEquals(paramss.length, 3)
    assertTrees(paramss(0): _*)(cparam)
    assertTrees(paramss(1): _*)(
      Term.Param(List(Mod.Using()), Name(""), Some(Type.Name("Context")), None),
      Term.Param(List(Mod.Using()), Term.Name("x"), Some(Type.Name("Int")), None)
    )
    assertTrees(paramss(2): _*)(
      Term.Param(List(Mod.Using()), Term.Name("y"), Some(Type.Name("String")), None),
      Term.Param(List(Mod.Using()), Name(""), Some(Type.Name("File")), None)
    )

    assertTrees(stats: _*)(
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2))
    )
  }

  test("extension quasiquotes: triple, with tparams") {
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams](...$paramss) { ..$stats }" =
      q"""extension [A, B](c: Circle)(using Context, x: Int)(using y: String, File) {
                def crc: Int = 2
              }"""

    assertTrees(tparams: _*)(pparam("A"), pparam("B"))

    assertEquals(paramss.length, 3)
    assertTrees(paramss(0): _*)(cparam)
    assertTrees(paramss(1): _*)(
      Term.Param(List(Mod.Using()), Name(""), Some(Type.Name("Context")), None),
      Term.Param(List(Mod.Using()), Term.Name("x"), Some(Type.Name("Int")), None)
    )
    assertTrees(paramss(2): _*)(
      Term.Param(List(Mod.Using()), Term.Name("y"), Some(Type.Name("String")), None),
      Term.Param(List(Mod.Using()), Name(""), Some(Type.Name("File")), None)
    )

    assertTrees(stats: _*)(
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2))
    )
  }

  test("extension quasiquotes: double, no tparams") {
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams](..$params) { ..$stats }" =
      q"""extension (c: Circle) {
              def crc: Int = 2
            }"""

    assertEquals(tparams.length, 0)

    assertTrees(params: _*)(cparam)

    assertTrees(stats: _*)(
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2))
    )
  }

  test("extension quasiquotes: double, with tparams") {
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams](..$params) { ..$stats }" =
      q"""extension [A](c: Circle) {
              def crc: Int = 2
            }"""

    assertTrees(tparams: _*)(pparam("A"))

    assertTrees(params: _*)(cparam)

    assertTrees(stats: _*)(
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2))
    )
  }

  test("extension quasiquotes: single, no tparams") {
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams]($param) { ..$stats }" =
      q"""extension (c: Circle) {
              def crb: Int = 1
              def crc: Int = 2
            }"""

    assertEquals(tparams.length, 0)

    assertTree(param)(cparam)

    assertTrees(stats: _*)(
      Defn.Def(Nil, Term.Name("crb"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(1)),
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2))
    )
  }

  test("extension quasiquotes: single, with tparams") {
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams]($param) { ..$stats }" =
      q"""extension [A, B, C](c: Circle) {
              def crb: Int = 1
              def crc: Int = 2
            }"""

    assertTrees(tparams: _*)(pparam("A"), pparam("B"), pparam("C"))

    assertTree(param)(cparam)

    assertTrees(stats: _*)(
      Defn.Def(Nil, Term.Name("crb"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(1)),
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2))
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
        Member.ParamClauseGroup(Nil, paramsExpected) :: Nil,
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
        Member.ParamClauseGroup(tparamsExpected, paramsExpected) :: Nil,
        Type.Name("Unit")
      )
    )
  }

  test("single ...., with tparams") {
    import dialects.Scala3
    val q"..$mods def $name(....$paramss): $tpe" = q"def f(x: Int)[A](y: Int)[B]: Unit"
    val pcGroups = Member.ParamClauseGroup(
      Nil,
      Term.ParamClause(
        List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)),
        None
      ) :: Nil
    ) :: Member.ParamClauseGroup(
      Type.ParamClause(
        Type.Param(Nil, Type.Name("A"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil
      ),
      Term.ParamClause(
        List(Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None)),
        None
      ) :: Nil
    ) :: Member.ParamClauseGroup(
      Type.ParamClause(
        Type.Param(Nil, Type.Name("B"), Nil, Type.Bounds(None, None), Nil, Nil) :: Nil
      ),
      Nil
    ) :: Nil

    checkTreesWithSyntax(paramss: _*)("(x: Int)", "[A](y: Int)", "[B]")(pcGroups: _*)
    assertTree(q"..$mods def $name(....$paramss): $tpe")(
      Decl.Def(Nil, Term.Name("f"), pcGroups, Type.Name("Unit"))
    )
  }

}
