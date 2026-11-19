package scala.meta.tests

package quasiquotes

import scala.meta._

import scala.annotation.nowarn

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
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
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
      tparam(List(Mod.Using()), "x", "Int"),
    )
    assertTrees(paramss(2): _*)(
      tparam(List(Mod.Using()), "y", "String"),
      tparam(List(Mod.Using()), "", "File"),
    )

    assertTrees(stats: _*)(
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2)),
    )
  }

  test("extension quasiquotes: triple, with tparams") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
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
      tparam(List(Mod.Using()), "x", "Int"),
    )
    assertTrees(paramss(2): _*)(
      tparam(List(Mod.Using()), "y", "String"),
      tparam(List(Mod.Using()), "", "File"),
    )

    assertTrees(stats: _*)(
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2)),
    )
  }

  test("extension quasiquotes: double, no tparams") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams](..$params) { ..$stats }" =
      q"""extension (c: Circle) {
              def crc: Int = 2
            }"""

    assertEquals(tparams.length, 0)

    assertTrees(params: _*)(cparam)

    assertTrees(stats: _*)(
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2)),
    )
  }

  test("extension quasiquotes: double, with tparams") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams](..$params) { ..$stats }" =
      q"""extension [A](c: Circle) {
              def crc: Int = 2
            }"""

    assertTrees(tparams: _*)(pparam("A"))

    assertTrees(params: _*)(cparam)

    assertTrees(stats: _*)(
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2)),
    )
  }

  test("extension quasiquotes: single, no tparams") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
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
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2)),
    )
  }

  test("extension quasiquotes: single, with tparams") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
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
      Defn.Def(Nil, Term.Name("crc"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2)),
    )
  }

  test("single ..., without tparams") {
    val q"..$mods def $name(...$paramss): $tpe" = q"def f(x: Int)(y: Int): Unit"
    val paramsExpected = tpc(tparam("x", "Int")) :: tpc(tparam("y", "Int")) :: Nil

    checkTreesWithSyntax(paramss: _*)("(x: Int)", "(y: Int)")(paramsExpected: _*)
    assertTree(q"..$mods def $name(...$paramss): $tpe")(
      Decl.Def(Nil, Term.Name("f"), pcg(paramsExpected: _*) :: Nil, Type.Name("Unit")),
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
        .Def(Nil, Term.Name("f"), pcg(tparamsExpected, paramsExpected: _*) :: Nil, Type.Name("Unit")),
    )
  }

  test("single ...., with tparams") {
    import dialects.Scala3
    val q"..$mods def $name(....$paramss): $tpe" = q"def f(x: Int)[A](y: Int)[B]: Unit"
    val pcGroups = pcg(List(tparam("x", "Int"))) ::
      pcg(List(pparam("A")), List(tparam("y", "Int"))) :: pcg(List(pparam("B"))) :: Nil

    checkTreesWithSyntax(paramss: _*)("(x: Int)", "[A](y: Int)", "[B]")(pcGroups: _*)
    assertTree(q"..$mods def $name(....$paramss): $tpe")(
      Decl.Def(Nil, Term.Name("f"), pcGroups, Type.Name("Unit")),
    )
  }

  test("#4434 quasiquote in braces") {
    import dialects.Scala3
    val fooTypes = Seq(q"Foo", q"Bar")
    val quoted: Tree = q"""${fooTypes(0)}; "any message""""

    assertTokensAsStructureLines(
      quoted.tokens,
      """|BOF [0..0)
         |Ident({$}{fooTypes(0)}) [0..18)
         |Semicolon [18..19)
         |Space [19..20)
         |Constant.String(any message) [20..33)
         |EOF [33..33)
         |""".stripMargin,
    )
    val pos = quoted.pos
    assertNoDiff(pos.toString, """[0,33) in str(`{$}{fooTypes(0)}`; "any message")""")
    assertNoDiff(pos.text, """`{$}{fooTypes(0)}`; "any message"""")
    assertPositions(
      quoted,
      """|<stats1>Lit.String "any message"</stats1> [20:"any message":33)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true,
    )

    val syntax =
      """|{
         |  Foo
         |  "any message"
         |}
         |""".stripMargin
    assertNoDiff(quoted.text, syntax)
    assertNoDiff(quoted.syntax, syntax)
    assertNoDiff(quoted.reprint, syntax)
    assertTree(quoted)(blk(tname("Foo"), lit("any message")))
  }

  test("#4481: quasiquote with ellipsis") {
    import dialects.Scala3
    val terms = List(q"Foo")
    val quoted: Tree = q"""..$terms; "any message""""

    assertTokensAsStructureLines(
      quoted.tokens,
      """|BOF [0..0)
         |Comment( .. ) [0..8)
         |Ident({$}terms) [8..18)
         |Semicolon [18..19)
         |Space [19..20)
         |Constant.String(any message) [20..33)
         |EOF [33..33)
         |""".stripMargin,
    )
    val pos = quoted.pos
    assertNoDiff(pos.toString, """[0,33) in str(/* .. */`{$}terms`; "any message")""")
    assertNoDiff(pos.text, """/* .. */`{$}terms`; "any message"""")
    assertPositions(
      quoted,
      """|<stats1>Lit.String "any message"</stats1> [20:"any message":33)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true,
    )

    val syntax =
      """|{
         |  Foo
         |  "any message"
         |}
         |""".stripMargin
    assertNoDiff(quoted.text, syntax)
    assertNoDiff(quoted.syntax, syntax)
    assertNoDiff(quoted.reprint, syntax)
    assertTree(quoted)(blk(tname("Foo"), lit("any message")))
  }

  test("SIP-75 colon: lambda with spliced params") {
    import dialects.Scala3
    val ps = List(tparam("a"), tparam("b"))
    val layout =
      """|xs.map {
         |  (a, b) => x
         |}""".stripMargin
    checkTree(q"xs.map: ..$ps => x", layout)(
      tapply(tselect("xs", "map"), blk(tfunc(tparam("a"), tparam("b"))(tname("x")))),
    )
  }

  private val unquoted = q""""x""""

  test("comments: an unquote and a line break in a comment on its own line, a statement after it") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val b = tname("b").toBuilder.begComment(Seq("/* x\n  */")).result()
    assertTree(
      q"""def f =
  a
  /* $unquoted
  */ b""",
    )(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))
  }

  test("comments: a line break and an unquote in a comment on its own line, a statement after it") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val b = tname("b").toBuilder.begComment(Seq("/*\n x */")).result()
    assertTree(
      q"""def f =
  a
  /*
  $unquoted */ b""",
    )(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))
  }

  test("comments: an unquote and a line break in a comment on its own line, a leading infix operator after it") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val plus = tname("+").toBuilder.begComment(Seq("/* x\n  */")).result()
    assertTree(
      q"""def f =
  a
  /* $unquoted
  */ + b""",
    )(Defn.Def(Nil, tname("f"), Nil, None, tinfix(tname("a"), plus, tname("b"))))
  }

  test("comments: an unquote in a one-line comment after a statement, an outdent after it") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val f = Defn.Def(Nil, tname("f"), Nil, None, tname("a")).toBuilder.endComment(Seq("/* x */"))
      .result()
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(
      q"""object O:
  def f =
    a /* $unquoted */
  def g = 1""",
    )(Defn.Object(Nil, tname("O"), tpl(f, g)))
  }

  test(
    "comments: an unquote at the end of a one-line comment after a statement, an outdent after it",
  ) {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val f = Defn.Def(Nil, tname("f"), Nil, None, tname("a")).toBuilder.endComment(Seq("/* y x*/"))
      .result()
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(
      q"""object O:
  def f =
    a /* y $unquoted*/
  def g = 1""",
    )(Defn.Object(Nil, tname("O"), tpl(f, g)))
  }

  test("comments: an unquote in a one-line comment after a leading infix operator, its operand after it") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    // without the unquote: `a + b`
    val plus = tname("+").toBuilder.endComment(Seq("/* x */")).result()
    assertTree(
      q"""def f =
  a
  + /* $unquoted */ b""",
    )(Defn.Def(Nil, tname("f"), Nil, None, tinfix(tname("a"), plus, tname("b"))))
  }

  test("comments: an unquote in a one-line comment after a leading infix operator, its operand on the next line") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    // without the unquote: `a + b`
    val plus = tname("+").toBuilder.endComment(Seq("/* x */")).result()
    assertTree(
      q"""def f =
  a
  + /* $unquoted */
    b""",
    )(Defn.Def(Nil, tname("f"), Nil, None, tinfix(tname("a"), plus, tname("b"))))
  }

  test(
    "comments: an unquote in a one-line comment on its own line after the last statement of a body",
  ) {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    // without the unquote: `def f = { a }`, with the comment attached to `a`
    val a = tname("a").toBuilder.endComment(Seq("/* x */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(a))
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(
      q"""object O:
  def f =
    a
    /* $unquoted */
  def g = 1""",
    )(Defn.Object(Nil, tname("O"), tpl(f, g)))
  }

  test(
    "comments: a line break and an unquote in a comment opened after the last statement of a body",
  ) {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val f = Defn.Def(Nil, tname("f"), Nil, None, tname("a")).toBuilder
      .endComment(Seq("/* foo\n    bar x */")).result()
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(
      q"""object O:
  def f =
    a /* foo
    bar $unquoted */
  def g = 1""",
    )(Defn.Object(Nil, tname("O"), tpl(f, g)))
  }

  test("comments: an unquote in a one-line comment on its own line before the only method of an extension") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    // without the unquote: the method is in a block
    val f = Defn.Def(Nil, tname("f"), Nil, None, int(1)).toBuilder
      .begComment(detachedComments("/* x */")).result()
    val group = Member
      .ParamClauseGroup(Type.ParamClause(Nil), List(Term.ParamClause(List(tparam("y", "Int")))))
    assertTree(
      q"""extension (y: Int)
  /* $unquoted */
  def f = 1""",
    )(Defn.ExtensionGroup(Some(group), blk(f)))
  }

  test("comments: an unquote in a one-line comment on its own line before `then`") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    // without the unquote: the `then` body is a block with the comment attached
    val thenp = blk(tname("a")).toBuilder.begComment(detachedComments("/* x */")).result()
    assertTree(
      q"""def f =
  if c
  /* $unquoted */
  then
    a
  else b""",
    )(Defn.Def(Nil, tname("f"), Nil, None, tif(tname("c"), thenp, tname("b"))))
  }

  test("comments: an unquote in a one-line comment before `then` on its line") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    // without the unquote: the `then` body is a block with the comment attached
    val thenp = blk(tname("a")).toBuilder.begComment(detachedComments("/* x */")).result()
    assertTree(
      q"""def f =
  if c
  /* $unquoted */ then
    a
  else b""",
    )(Defn.Def(Nil, tname("f"), Nil, None, tif(tname("c"), thenp, tname("b"))))
  }

  test("comments: an unquote on the last line of a comment on lines of its own before `then`") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    // without the unquote: the `then` body is a block with the comment attached
    val thenp = blk(tname("a")).toBuilder.begComment(detachedComments("/* foo\n   * x */")).result()
    assertTree(
      q"""def f =
  if c
  /* foo
   * $unquoted */
  then
    a
  else b""",
    )(Defn.Def(Nil, tname("f"), Nil, None, tif(tname("c"), thenp, tname("b"))))
  }

  test("comments: an unquote in a one-line comment after the last statement of a case") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val quoted =
      q"""y match
  case 1 => a /* $unquoted */"""
    assertPositions(
      quoted,
      """|<casesBlock>Term.CasesBlock {
         |  case 1 => a /* x */
         |}</casesBlock> [10:case 1 => a:21)
         |<cases0>Case case 1 => a /* x */
         |</cases0> [10:case 1 => a:21)
         |<body>Term.Name a /* x */
         |</body> [20:a:21)
         |""".stripMargin,
      showPosition = true,
      showFieldName = true,
    )
  }

  test("comments: an unquote and a line break in a comment opened after a statement, a statement after it") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val a = tname("a").toBuilder.endComment(Seq("/* x\n  */")).result()
    assertTree(
      q"""def f =
  a /* $unquoted
  */ b""",
    )(Defn.Def(Nil, tname("f"), Nil, None, blk(a, tname("b"))))
  }

  test("comments: an unquote at the start of a line in a comment opened after a statement, a statement after it") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val a = tname("a").toBuilder.endComment(Seq("/* foo\nx */")).result()
    assertTree(
      q"""a /* foo
$unquoted */ b""",
    )(blk(a, tname("b")))
  }

  test("comments: an unquote and a line break in a comment opened after an end marker, a definition after it") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val f = Defn.Def(Nil, tname("f"), Nil, None, tname("a"))
    val end = Term.EndMarker(tname("f")).toBuilder.endComment(Seq("/* x\n  */")).result()
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(
      q"""object O:
  def f =
    a
  end f /* $unquoted
  */ def g = 1""",
    )(Defn.Object(Nil, tname("O"), tpl(f, end, g)))
  }

  test("comments: an unquote at the start of a line in a comment opened after an end marker, a definition after it") {
    val code =
      """|def f =
         |  a
         |end f /* foo
         |$x */ def g = 1""".stripMargin
    val parsed = dialects.Scala3.unquoteTerm(multiline = true)(code).parse[Stat]
    assertEquals(
      parsed.toEither.left.map(_.message).map(_.children.map(_.productPrefix)),
      Right(List("Defn.Def", "Term.EndMarker", "Defn.Def")),
    )
  }

  test("comments: a quasiquote with an unquote at the start of a line in a comment opened after an end marker, a definition after it") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val f = Defn.Def(Nil, tname("f"), Nil, None, tname("a"))
    val end = Term.EndMarker(tname("f")).toBuilder.endComment(Seq("/* foo\nx */")).result()
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(
      q"""def f =
  a
end f /* foo
$unquoted */ def g = 1""",
    )(blk(f, end, g))
  }

  test("comments: an unquote whose value holds a newline, in a comment between a function and its argument") {
    @nowarn("msg=never used|unused") // hides the implicit dialect of the suite
    val dialect: Dialect = null
    import dialects.Scala3
    val content = Lit.String("foo\nbar")
    val tree = q"f /* $content */ (x)"
    val f = tname("f").toBuilder.endComment(Seq("/* foo\nbar */")).result()
    assertTree(tree)(Term.Apply(f, Term.ArgClause(List(tname("x")))))
    val printed = tree.reprint
    assertNoDiff(
      printed,
      """|f /* foo
         |bar */ (x)""".stripMargin,
    )
    assertTree(s"{\n$printed\n}".parse[Term].get)(blk(f, tname("x")))
  }

}
