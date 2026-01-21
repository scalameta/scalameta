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

    checkTree(t"(..${tpes.values}) => $tpe", "(x: X, y: Y) => Z")(pfunc(tpes, tpe))
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
      tparam(List(Mod.Using()), "", "Context"),
      tparam(List(Mod.Using()), "x", "Int")
    )
    assertTrees(paramss(2): _*)(
      tparam(List(Mod.Using()), "y", "String"),
      tparam(List(Mod.Using()), "", "File")
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
      tparam(List(Mod.Using()), "", "Context"),
      tparam(List(Mod.Using()), "x", "Int")
    )
    assertTrees(paramss(2): _*)(
      tparam(List(Mod.Using()), "y", "String"),
      tparam(List(Mod.Using()), "", "File")
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
    val paramsExpected = tpc(tparam("x", "Int")) :: tpc(tparam("y", "Int")) :: Nil

    checkTreesWithSyntax(paramss: _*)("(x: Int)", "(y: Int)")(paramsExpected: _*)
    assertTree(q"..$mods def $name(...$paramss): $tpe")(
      Decl.Def(Nil, Term.Name("f"), pcg(paramsExpected: _*) :: Nil, Type.Name("Unit"))
    )
  }

  test("single ..., with tparams") {
    val q"..$mods def $name[..$tparams](...$paramss): $tpe" = q"def f[A, B](x: Int)(y: Int): Unit"
    val tparamsExpected = ppc(pparam("A"), pparam("B"))
    val paramsExpected = tpc(tparam("x", "Int")) :: tpc(tparam("y", "Int")) :: Nil

    checkTree(tparams, "[A, B]")(tparamsExpected)
    checkTreesWithSyntax(paramss: _*)("(x: Int)", "(y: Int)")(paramsExpected: _*)
    assertTree(q"..$mods def $name[..$tparams](...$paramss): $tpe")(
      Decl
        .Def(Nil, Term.Name("f"), pcg(tparamsExpected, paramsExpected: _*) :: Nil, Type.Name("Unit"))
    )
  }

  test("single ...., with tparams") {
    import dialects.Scala3
    val q"..$mods def $name(....$paramss): $tpe" = q"def f(x: Int)[A](y: Int)[B]: Unit"
    val pcGroups = pcg(List(tparam("x", "Int"))) ::
      pcg(List(pparam("A")), List(tparam("y", "Int"))) :: pcg(List(pparam("B"))) :: Nil

    checkTreesWithSyntax(paramss: _*)("(x: Int)", "[A](y: Int)", "[B]")(pcGroups: _*)
    assertTree(q"..$mods def $name(....$paramss): $tpe")(
      Decl.Def(Nil, Term.Name("f"), pcGroups, Type.Name("Unit"))
    )
  }

  test("#4434 quasiquote in braces") {
    import dialects.Scala3
    val fooTypes = Seq(q"Foo", q"Bar")
    val quoted: Tree = q"""${fooTypes(0)}; "any message""""

    assertTokensAsStructureLines(
      quoted.tokens,
      """|BOF [0..0)
         |MacroSplice [0..1)
         |LeftBrace [1..2)
         |Ident(fooTypes) [2..10)
         |LeftParen [10..11)
         |Constant.Int(0) [11..12)
         |""".stripMargin
    )
    val pos = quoted.pos
    assertNoDiff(pos.toString, """[0..12) in Input.String("${fooTypes(0)}; "any message"")""")
    assertNoDiff(pos.text, """${fooTypes(0""")
    assertPositions(
      quoted,
      """|<stats1>Lit.String (</stats1> [10:(:11)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true
    )

    val syntax =
      """|{
         |  Foo
         |  (
         |}
         |""".stripMargin
    assertNoDiff(quoted.text, syntax)
    assertNoDiff(quoted.syntax, syntax)
    assertNoDiff(quoted.reprint, syntax)
    assertTree(quoted)(blk(tname("Foo"), lit("any message")))
  }

}
