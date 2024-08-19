package scala.meta.tests.parsers.dotty

import scala.meta._

class ExtensionMethodsSuite extends BaseDottySuite {

  implicit def parseBlock(code: String, dialect: Dialect): Stat = {
    implicit val implicitDialect: Dialect = dialect
    blockStat(code)
  }

  private final val cparam = tparam("c", "Circle")
  private final val cparamss: List[List[Term.Param]] = List(List(cparam))

  /**
   * For checking examples in repl declare:
   * {{{
   *  case class Circle(x: Int)
   * }}}
   *
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/contextual/extension-methods.html]]
   */

  test("simple-method") {
    runTestAssert[Stat]("extension (c: Circle) def crc: Int = 2")(Defn.ExtensionGroup(
      Nil,
      cparamss,
      Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
    ))
  }

  test("modifier-method") {
    runTestAssert[Stat]("extension (c: Circle) private def crc: Int = 2")(Defn.ExtensionGroup(
      Nil,
      cparamss,
      Defn.Def(List(Mod.Private(anon)), tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
    ))
  }

  test("simple-method-indent") {
    val code = """|extension (c: Circle)
                  |  def crc: Int = 2
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("extension (c: Circle) def crc: Int = 2"))(
      Defn.ExtensionGroup(
        Nil,
        cparamss,
        Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
      )
    )
  }

  test("modifier-method-indent") {
    val code = """|extension (c: Circle)
                  |  private def crc: Int = 2
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("extension (c: Circle) private def crc: Int = 2"))(
      Defn.ExtensionGroup(
        Nil,
        List(List(tparam("c", "Circle"))),
        Defn.Def(List(Mod.Private(anon)), tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
      )
    )
  }

  test("multiple-methods-indent") {
    val code = """|extension (c: Circle)
                  |  def cra: Int = 2
                  |  def crb: String = "3"
                  |  def crc: Boolean = 4
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(Defn.ExtensionGroup(
      Nil,
      List(List(tparam("c", "Circle"))),
      Term.Block(List(
        Defn.Def(Nil, tname("cra"), Nil, Nil, Some(pname("Int")), int(2)),
        Defn.Def(Nil, tname("crb"), Nil, Nil, Some(pname("String")), str("3")),
        Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Boolean")), int(4))
      ))
    ))
  }

  test("simple-method-braces") {
    val code = """|extension (c: Circle) {
                  |  def crc: Int = 2
                  |}
                  |""".stripMargin
    runTestAssert[Stat](
      code,
      assertLayout = Some(
        """|extension (c: Circle) {
           |  def crc: Int = 2
           |}
           |""".stripMargin
      )
    )(Defn.ExtensionGroup(
      Nil,
      List(List(tparam("c", "Circle"))),
      Term.Block(List(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))))
    ))
  }

  test("extension-using-single") {
    val code = """|extension (c: Circle)(using Context, x: Int) {
                  |  def crc: Int = 2
                  |}
                  |""".stripMargin
    val output = """|extension (c: Circle)(using Context, x: Int) {
                    |  def crc: Int = 2
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.ExtensionGroup(
      Nil,
      List(
        List(tparam("c", "Circle")),
        List(tparam(List(Mod.Using()), "", "Context"), tparam(List(Mod.Using()), "x", "Int"))
      ),
      Term.Block(List(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))))
    ))
  }

  test("extension-using-newline") {
    val code = """|extension (c: Circle)
                  |  (using Context, x: Int) 
                  |{
                  |  def crc: Int = 2
                  |}
                  |""".stripMargin
    val output = """|extension (c: Circle)(using Context, x: Int) {
                    |  def crc: Int = 2
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.ExtensionGroup(
      Nil,
      List(
        List(tparam("c", "Circle")),
        List(tparam(List(Mod.Using()), "", "Context"), tparam(List(Mod.Using()), "x", "Int"))
      ),
      Term.Block(List(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))))
    ))
  }

  test("extension-using-multi") {
    val code = """|extension (c: Circle)(using Context, x: Int)(using y: String, File) {
                  |  def crc: Int = 2
                  |}
                  |""".stripMargin
    val output = """|extension (c: Circle)(using Context, x: Int)(using y: String, File) {
                    |  def crc: Int = 2
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.ExtensionGroup(
      Nil,
      List(
        List(tparam("c", "Circle")),
        List(tparam(List(Mod.Using()), "", "Context"), tparam(List(Mod.Using()), "x", "Int")),
        List(tparam(List(Mod.Using()), "y", "String"), tparam(List(Mod.Using()), "", "File"))
      ),
      Term.Block(List(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))))
    ))
  }

  test("extension-additional-comment") {
    runTestAssert[Stat](
      """|extension (a: Int)
         |
         |
         |    /** */
         |
         |
         |    def double = a * 2
         |    
         |    /** */
         |""".stripMargin,
      """|extension (a: Int) {
         |  def double = a * 2
         |}""".stripMargin
    )(Defn.ExtensionGroup(
      Nil,
      List(List(tparam("a", "Int"))),
      blk(Defn.Def(
        Nil,
        tname("double"),
        Nil,
        Nil,
        None,
        Term.ApplyInfix(tname("a"), tname("*"), Nil, List(int(2)))
      ))
    ))
  }

  // Scala 3 doesn't allow for methods named `extension` to be invoked
  // https://github.com/lampepfl/dotty/issues/10076
  test("extension-named-method") {
    runTestError[Stat](
      """|object A{
         |  def extension(a : Int) = a + 2
         |  extension(2)
         |}""".stripMargin,
      "`identifier` expected but `integer constant` found"
    )

    runTestAssert[Stat](
      """|object A {
         |  def extension(a: Int) = a + 2
         |  `extension`(2)
         |}""".stripMargin
    )(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Def(
          Nil,
          tname("extension"),
          Nil,
          List(List(tparam("a", "Int"))),
          None,
          Term.ApplyInfix(tname("a"), tname("+"), Nil, List(int(2)))
        ),
        Term.Apply(tname("extension"), List(int(2)))
      )
    ))
  }

  test("extension-named-method") {
    runTestAssert[Stat]("extension + 3")(
      Term.ApplyInfix(tname("extension"), tname("+"), Nil, List(int(3)))
    )

    runTestAssert[Stat]("def extension(x: extension): extension = x")(Defn.Def(
      Nil,
      tname("extension"),
      Nil,
      List(List(tparam("x", "extension"))),
      Some(pname("extension")),
      tname("x")
    ))

    runTestAssert[Stat]("extension.extension(3)")(
      Term.Apply(Term.Select(tname("extension"), tname("extension")), List(int(3)))
    )
  }

  test("method-type-params") {
    runTestAssert[Stat]("extension [T](xs: List[T]) def sumBy[U](t: T): U = ???")(
      Defn.ExtensionGroup(
        List(pparam("T")),
        List(List(tparam("xs", Type.Apply(pname("List"), List(pname("T")))))),
        Defn.Def(
          Nil,
          tname("sumBy"),
          List(pparam("U")),
          List(List(tparam("t", "T"))),
          Some(pname("U")),
          tname("???")
        )
      )
    )
  }

  test("method-using-before") {
    runTestAssert[Stat]("extension (using a: Int)(b: Int) def hello = a + b")(Defn.ExtensionGroup(
      Nil,
      List(List(tparam(List(Mod.Using()), "a", "Int")), List(tparam("b", "Int"))),
      Defn.Def(
        Nil,
        tname("hello"),
        Nil,
        Nil,
        None,
        Term.ApplyInfix(tname("a"), tname("+"), Nil, List(tname("b")))
      )
    ))
  }

  test("method-multi-using") {
    runTestAssert[Stat](
      """|extension 
         |  (
         |    using a: Int
         |  )
         |  (
         |    b: Int
         |  )
         |  (
         |    using c: String
         |  ) 
         |    def hello = a + b + c.toInt""".stripMargin,
      assertLayout =
        Some("extension (using a: Int)(b: Int)(using c: String) def hello = a + b + c.toInt")
    )(Defn.ExtensionGroup(
      Nil,
      List(
        List(tparam(List(Mod.Using()), "a", "Int")),
        List(tparam("b", "Int")),
        List(tparam(List(Mod.Using()), "c", "String"))
      ),
      Defn.Def(
        Nil,
        tname("hello"),
        Nil,
        Nil,
        None,
        Term.ApplyInfix(
          Term.ApplyInfix(tname("a"), tname("+"), Nil, List(tname("b"))),
          tname("+"),
          Nil,
          List(Term.Select(tname("c"), tname("toInt")))
        )
      )
    ))
  }

  test("private-export") {
    runTestAssert[Stat](
      """|extension (i: Int)
         |  private def richInt = RichInt(i)
         |  export richInt.*
         |""".stripMargin,
      assertLayout = Some(
        """|extension (i: Int) {
           |  private def richInt = RichInt(i)
           |  export richInt.*
           |}
           |""".stripMargin
      )
    )(Defn.ExtensionGroup(
      Nil,
      List(List(tparam("i", "Int"))),
      Term.Block(List(
        Defn.Def(
          List(Mod.Private(anon)),
          tname("richInt"),
          Nil,
          Nil,
          None,
          Term.Apply(tname("RichInt"), List(tname("i")))
        ),
        Export(List(Importer(tname("richInt"), List(Importee.Wildcard()))))
      ))
    ))
  }

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

    assertTrees(stats: _*)(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2)))
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

    assertTrees(stats: _*)(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2)))
  }

  test("extension quasiquotes: double, no tparams") {
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams](..$params) { ..$stats }" = q"""extension (c: Circle) {
              def crc: Int = 2
            }"""

    assertEquals(tparams.length, 0)

    assertTrees(params: _*)(cparam)

    assertTrees(stats: _*)(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2)))
  }

  test("extension quasiquotes: double, with tparams") {
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams](..$params) { ..$stats }" = q"""extension [A](c: Circle) {
              def crc: Int = 2
            }"""

    assertTrees(tparams: _*)(pparam("A"))

    assertTrees(params: _*)(cparam)

    assertTrees(stats: _*)(Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2)))
  }

  test("extension quasiquotes: single, no tparams") {
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams]($param) { ..$stats }" = q"""extension (c: Circle) {
              def crb: Int = 1
              def crc: Int = 2
            }"""

    assertEquals(tparams.length, 0)

    assertTree(param)(cparam)

    assertTrees(stats: _*)(
      Defn.Def(Nil, tname("crb"), Nil, Nil, Some(pname("Int")), int(1)),
      Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
    )
  }

  test("extension quasiquotes: single, with tparams") {
    val dialect: Dialect = null
    import dialects.Scala3

    val q"extension [..$tparams]($param) { ..$stats }" = q"""extension [A, B, C](c: Circle) {
              def crb: Int = 1
              def crc: Int = 2
            }"""

    assertTrees(tparams: _*)(pparam("A"), pparam("B"), pparam("C"))

    assertTree(param)(cparam)

    assertTrees(stats: _*)(
      Defn.Def(Nil, tname("crb"), Nil, Nil, Some(pname("Int")), int(1)),
      Defn.Def(Nil, tname("crc"), Nil, Nil, Some(pname("Int")), int(2))
    )
  }

  test("#3215 1") {
    runTestAssert[Stat](
      """|  extension (x: X)
         |
         |    def foo: Foo =
         |      getFoo
         |    end foo
         |
         |    def bar: Bar =
         |      getBar
         |    end bar
         |""".stripMargin,
      assertLayout = Some(
        """|extension (x: X) {
           |  def foo: Foo = getFoo
           |  end foo
           |  def bar: Bar = getBar
           |  end bar
           |}
           |""".stripMargin
      )
    )(Defn.ExtensionGroup(
      Nil,
      List(List(tparam("x", "X"))),
      Term.Block(List(
        Defn.Def(Nil, tname("foo"), Nil, Some(pname("Foo")), tname("getFoo")),
        Term.EndMarker(tname("foo")),
        Defn.Def(Nil, tname("bar"), Nil, Some(pname("Bar")), tname("getBar")),
        Term.EndMarker(tname("bar"))
      ))
    ))
  }

  test("#3215 2") {
    runTestAssert[Stat](
      """|object MtagsEnrichments extends ScalametaCommonEnrichments:
         |  extension (x: X)
         |
         |    def foo: Foo =
         |      getFoo
         |    end foo
         |
         |    def bar: Bar =
         |      getBar
         |    end bar
         |""".stripMargin,
      assertLayout = Some(
        """|object MtagsEnrichments extends ScalametaCommonEnrichments {
           |  extension (x: X) {
           |    def foo: Foo = getFoo
           |    end foo
           |    def bar: Bar = getBar
           |    end bar
           |  }
           |}
           |""".stripMargin
      )
    )(Defn.Object(
      Nil,
      tname("MtagsEnrichments"),
      tpl(
        List(init("ScalametaCommonEnrichments")),
        Defn.ExtensionGroup(
          Nil,
          List(List(tparam("x", "X"))),
          Term.Block(List(
            Defn.Def(Nil, tname("foo"), Nil, Some(pname("Foo")), tname("getFoo")),
            Term.EndMarker(tname("foo")),
            Defn.Def(Nil, tname("bar"), Nil, Some(pname("Bar")), tname("getBar")),
            Term.EndMarker(tname("bar"))
          ))
        ) :: Nil
      )
    ))
  }

  test("#3215 3") {
    runTestAssert[Stat](
      """|object MtagsEnrichments extends ScalametaCommonEnrichments {
         |  extension (x: X)
         |
         |    def foo: Foo =
         |      getFoo
         |    end foo
         |
         |    def bar: Bar =
         |      getBar
         |    end bar
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|object MtagsEnrichments extends ScalametaCommonEnrichments {
           |  extension (x: X) {
           |    def foo: Foo = getFoo
           |    end foo
           |    def bar: Bar = getBar
           |    end bar
           |  }
           |}
           |""".stripMargin
      )
    )(Defn.Object(
      Nil,
      tname("MtagsEnrichments"),
      tpl(
        List(init("ScalametaCommonEnrichments")),
        Defn.ExtensionGroup(
          Nil,
          List(List(tparam("x", "X"))),
          Term.Block(List(
            Defn.Def(Nil, tname("foo"), Nil, Some(pname("Foo")), tname("getFoo")),
            Term.EndMarker(tname("foo")),
            Defn.Def(Nil, tname("bar"), Nil, Some(pname("Bar")), tname("getBar")),
            Term.EndMarker(tname("bar"))
          ))
        ) :: Nil
      )
    ))
  }

  test("#3231 1") {
    runTestAssert[Stat](
      """|object A {
         |  self =>
         |  extension (x: X)
         |    @annoFoo
         |    def foo: Foo =
         |      getFoo
         |
         |    @annoBar
         |    def bar: Bar =
         |      getBar
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|object A { self =>
           |  extension (x: X) {
           |    @annoFoo def foo: Foo = getFoo
           |    @annoBar def bar: Bar = getBar
           |  }
           |}
           |""".stripMargin
      )
    )(Defn.Object(
      Nil,
      tname("A"),
      Template(
        Nil,
        Nil,
        self("self"),
        Defn.ExtensionGroup(
          Nil,
          List(List(tparam("x", "X"))),
          Term.Block(List(
            Defn.Def(
              List(Mod.Annot(Init(pname("annoFoo"), anon, emptyArgClause))),
              tname("foo"),
              Nil,
              Some(pname("Foo")),
              tname("getFoo")
            ),
            Defn.Def(
              List(Mod.Annot(Init(pname("annoBar"), anon, emptyArgClause))),
              tname("bar"),
              Nil,
              Some(pname("Bar")),
              tname("getBar")
            )
          ))
        ) :: Nil,
        Nil
      )
    ))
  }

  test("#3231 2") {
    runTestAssert[Stat](
      """|object A {
         |  self =>
         |  extension (x: X)
         |    private def foo: Foo =
         |      getFoo
         |
         |    protected def bar: Bar =
         |      getBar
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|object A { self =>
           |  extension (x: X) {
           |    private def foo: Foo = getFoo
           |    protected def bar: Bar = getBar
           |  }
           |}
           |""".stripMargin
      )
    )(Defn.Object(
      Nil,
      tname("A"),
      Template(
        Nil,
        Nil,
        self("self"),
        Defn.ExtensionGroup(
          Nil,
          List(List(tparam("x", "X"))),
          Term.Block(List(
            Defn
              .Def(List(Mod.Private(anon)), tname("foo"), Nil, Some(pname("Foo")), tname("getFoo")),
            Defn
              .Def(List(Mod.Protected(anon)), tname("bar"), Nil, Some(pname("Bar")), tname("getBar"))
          ))
        ) :: Nil,
        Nil
      )
    ))
  }

}
