package scala.meta.tests.parsers.dotty

import scala.meta._

class GivenUsingSuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = blockStat(_)

  /**
   * For checking examples in repl declare:
   * {{{
   *  trait Ord[T] { def f(): Int }
   * }}}
   *
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/contextual/givens.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/contextual/using-clauses.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/contextual/given-imports.html]]
   */
  // ---------------------------------
  // GIVEN
  // ---------------------------------

  def defone(mods: List[Mod]): Defn.Def = Defn
    .Def(mods, tname("f"), Nil, List(Nil), Some(pname("Int")), int(1))
  val defone: Defn.Def = defone(Nil)

  test("given-named")(runTestAssert[Stat]("given intOrd: Ord[Int] with { def f(): Int = 1 }")(
    Defn.Given(Nil, tname("intOrd"), Nil, Nil, tpl(List(init(papply("Ord", "Int"))), List(defone)))
  ))

  test("given-named-newline")(
    runTestAssert[Stat]("given intOrd: Ord[Int] with \n{ def f(): Int = 1 }", assertLayout = None)(
      Defn.Given(Nil, tname("intOrd"), Nil, Nil, tpl(List(init(papply("Ord", "Int"))), List(defone)))
    )
  )

  test("given-anonymous")(runTestAssert[Stat]("given Ord[Int] with { def f(): Int = 1 }")(
    Defn.Given(Nil, anon, Nil, Nil, tpl(List(init(papply("Ord", "Int"))), List(defone)))
  ))

  test("given-multiple-inheritance") {
    val expectedGiven = Defn.Given(
      Nil,
      tname("intM"),
      Nil,
      Nil,
      tpl(
        List(init(papply("Ord", "Int")), init(papply("Eq", "Int"))),
        List(Defn.Def(Nil, tname("fx"), Nil, Nil, None, int(3)))
      )
    )

    runTestAssert[Stat]("given intM: Ord[Int] with Eq[Int] with { def fx = 3 }")(expectedGiven)

    runTestAssert[Stat](
      "given intM: Ord[Int] with\n Eq[Int] with { def fx = 3 }",
      assertLayout = Some("given intM: Ord[Int] with Eq[Int] with { def fx = 3 }")
    )(expectedGiven)
  }

  test("given-override-def")(
    runTestAssert[Stat](
      "given intOrd: Ord[Int] \n with { override def f(): Int = 1 }",
      assertLayout = Some("given intOrd: Ord[Int] with { override def f(): Int = 1 }")
    )(Defn.Given(
      Nil,
      tname("intOrd"),
      Nil,
      Nil,
      tpl(List(init(papply("Ord", "Int"))), List(defone(List(Mod.Override()))))
    ))
  )
  test("given-override-def-colon-newline")(
    runTestAssert[Stat](
      """|given intOrd
         |   : Ord[Int] with {
         |  def fn = ()
         |}
         |""".stripMargin,
      assertLayout = Some("given intOrd: Ord[Int] with { def fn = () }")
    )(Defn.Given(
      Nil,
      tname("intOrd"),
      Nil,
      Nil,
      tpl(List(init(papply("Ord", "Int"))), List(Defn.Def(Nil, tname("fn"), Nil, Nil, None, Lit.Unit())))
    ))
  )

  test("given-override-def") {
    runTestAssert[Stat](
      """|  given Facade.SimpleFacade[Json] with
         |    override def jnull = ???
         |    override def jtrue = ???
         |""".stripMargin,
      assertLayout = Some(
        """|given Facade.SimpleFacade[Json] with {
           |  override def jnull = ???
           |  override def jtrue = ???
           |}
           |""".stripMargin
      )
    )(Defn.Given(
      Nil,
      anon,
      Nil,
      Nil,
      tpl(
        init(papply(pselect("Facade", "SimpleFacade"), "Json")) :: Nil,
        List(
          Defn.Def(List(Mod.Override()), tname("jnull"), Nil, Nil, None, tname("???")),
          Defn.Def(List(Mod.Override()), tname("jtrue"), Nil, Nil, None, tname("???"))
        )
      )
    ))
  }

  test("given-self")(runTestAssert[Stat]("given intOrd: Ord[Int] with { current => }")(
    Defn.Given(Nil, tname("intOrd"), Nil, Nil, tpl(List(init(papply("Ord", "Int"))), self("current")))
  ))

  test("given-selftype-error")(runTestError(
    "given intOrd: Ord[Int] with { current: Ord[Int] => }",
    "given cannot have a self type"
  ))

  test("given-generic-named")(
    runTestAssert[Stat]("given listOrd[T]: Ord[List[T]] with { def f(): Int = 1 }")(Defn.Given(
      Nil,
      tname("listOrd"),
      List(pparam("T")),
      Nil,
      tpl(init(papply("Ord", papply("List", "T"))) :: Nil, List(defone))
    ))
  )

  test("given-generic-anonymous")(runTestAssert[Stat]("given Ord[List[T]] with { def f(): Int = 1 }")(
    Defn.Given(Nil, anon, Nil, Nil, tpl(init(papply("Ord", papply("List", "T"))) :: Nil, List(defone)))
  ))

  test("given-generic-anonymous") {
    runTestAssert[Stat](
      """|given noCaller: Caller(???) with
         |  override def computeValue() = ()
         |""".stripMargin,
      assertLayout = Some("given noCaller: Caller(???) with { override def computeValue() = () }")
    )(Defn.Given(
      Nil,
      tname("noCaller"),
      Nil,
      Nil,
      tpl(
        List(init("Caller", List(tname("???")))),
        Defn
          .Def(List(Mod.Override()), tname("computeValue"), Nil, List(List()), None, Lit.Unit()) ::
          Nil
      )
    ))
  }

  test("given-empty-anon")(
    runTestAssert[Stat](
      """|given C(1) with {}
         |""".stripMargin,
      assertLayout = Some("given C(1)")
    )(Defn.Given(Nil, anon, Nil, Nil, tpl(List(init("C", List(int(1)))), Nil)))
  )

  test("given-empty-anon-parens")(
    runTestAssert[Stat](
      """|given C(1)
         |""".stripMargin
    )(Defn.Given(Nil, anon, Nil, Nil, tplNoBody(init("C", List(int(1))))))
  )

  test("given-empty-anon-empty-parens")(
    runTestAssert[Stat](
      """|given C()
         |""".stripMargin
    )(Defn.Given(Nil, anon, Nil, Nil, tplNoBody(init("C", Nil))))
  )

  test("given-empty-anon-no-parens")(
    runTestAssert[Stat](
      """|given C with {}
         |""".stripMargin
    )(Defn.Given(Nil, anon, Nil, Nil, tpl(List(init("C")), Nil)))
  )

  test("given-empty")(
    runTestAssert[Stat](
      """|given t1[T]: E[T]("low") with {}
         |""".stripMargin,
      assertLayout = Some(
        """|given t1[T]: E[T]("low")
           |""".stripMargin
      )
    )(Defn.Given(
      Nil,
      tname("t1"),
      List(pparam("T")),
      Nil,
      tpl(init(papply("E", "T"), List(str("low"))) :: Nil, Nil)
    ))
  )

  test("given-extension") {
    val code =
      """|given x: AnyRef with
         |      extension (y: String)
         |         def f (y: Int): Boolean = false
         |""".stripMargin
    val expected = "given x: AnyRef with { extension (y: String) def f(y: Int): Boolean = false }"
    runTestAssert[Stat](code, assertLayout = Some(expected))(Defn.Given(
      Nil,
      tname("x"),
      Nil,
      Nil,
      tpl(
        List(init("AnyRef")),
        Defn.ExtensionGroup(
          Nil,
          List(List(tparam("y", "String"))),
          Defn.Def(
            Nil,
            tname("f"),
            Nil,
            List(List(tparam("y", "Int"))),
            Some(pname("Boolean")),
            bool(false)
          )
        ) :: Nil
      )
    ))
  }

  test("given-inline")(
    runTestAssert[Stat]("inline given intOrd: Ord[Int] with { def f(): Int = 1 }")(Defn.Given(
      List(Mod.Inline()),
      tname("intOrd"),
      Nil,
      Nil,
      tpl(
        List(init(papply("Ord", "Int"))),
        List(Defn.Def(Nil, tname("f"), Nil, List(List()), Some(pname("Int")), int(1)))
      )
    ))
  )

  test("given-with-import") {
    runTestAssert[Stat](
      code =
        """|given intOrd: Ord[Int] with
           |  import math.max as maxF
           |  def f(): Int = 1
           |""".stripMargin,
      assertLayout = Some(
        """|given intOrd: Ord[Int] with {
           |  import math.max as maxF
           |  def f(): Int = 1
           |}""".stripMargin
      )
    )(Defn.Given(
      Nil,
      tname("intOrd"),
      Nil,
      Nil,
      tpl(
        List(init(papply("Ord", "Int"))),
        List(
          Import(List(Importer(tname("math"), List(Importee.Rename(Name("max"), Name("maxF")))))),
          Defn.Def(Nil, tname("f"), Nil, List(List()), Some(pname("Int")), int(1))
        )
      )
    ))
  }

  test("given-with-export") {
    runTestAssert[Stat](
      code =
        """|given intOrd: Ord[Int] with
           |  export math.max
           |  def f(): Int = 1
           |""".stripMargin,
      assertLayout = Some(
        """|given intOrd: Ord[Int] with {
           |  export math.max
           |  def f(): Int = 1
           |}""".stripMargin
      )
    )(Defn.Given(
      Nil,
      tname("intOrd"),
      Nil,
      Nil,
      tpl(
        List(init(papply("Ord", "Int"))),
        List(
          Export(List(Importer(tname("math"), List(Importee.Name(Name("max")))))),
          Defn.Def(Nil, tname("f"), Nil, List(List()), Some(pname("Int")), int(1))
        )
      )
    ))
  }

  test("given-with-empty-refinement") {
    runTestAssert[Stat](
      code =
        """|given {} with
           |  extension [T](t: T) def hello = ""
           |""".stripMargin,
      assertLayout = Some("""given {} with { extension [T](t: T) def hello = "" }""")
    )(Defn.Given(
      Nil,
      anon,
      Nil,
      Nil,
      tpl(
        List(init(Type.Refine(None, Nil))),
        Defn.ExtensionGroup(
          List(pparam("T")),
          List(List(tparam("t", "T"))),
          Defn.Def(Nil, tname("hello"), Nil, Nil, None, str(""))
        ) :: Nil
      )
    ))
  }

  // ---------------------------------
  // GIVEN ALIAS
  // ---------------------------------

  test("given-alias-named")(
    runTestAssert[Stat]("given global: Option[Int] = Some(3)")(Defn.GivenAlias(
      Nil,
      tname("global"),
      Nil,
      Nil,
      papply("Option", "Int"),
      tapply(tname("Some"), int(3))
    ))
  )

  test("given-alias-anonymous")(runTestAssert[Stat]("given Option[Int] = Some(3)")(
    Defn.GivenAlias(Nil, anon, Nil, Nil, papply("Option", "Int"), tapply(tname("Some"), int(3)))
  ))

  test("given-alias-anon-or")(runTestAssert[Stat]("given (Cancelable & Movable) = ???")(
    Defn.GivenAlias(Nil, anon, Nil, Nil, pinfix("Cancelable", "&", pname("Movable")), tname("???"))
  ))

  test("given-alias-block")(
    runTestAssert[Stat](
      "given global: Option[Int] = { def f(): Int = 1; Some(3) }",
      assertLayout = None
    )(Defn.GivenAlias(
      Nil,
      tname("global"),
      Nil,
      Nil,
      papply("Option", "Int"),
      blk(defone, tapply(tname("Some"), int(3)))
    ))
  )

  test("given-alias-using-named")(
    runTestAssert[Stat]("given ordInt(using ord: Ord[Int]): Ord[List[Int]] = ???")(Defn.GivenAlias(
      Nil,
      tname("ordInt"),
      Nil,
      List(List(tparam(List(Mod.Using()), "ord", papply("Ord", "Int")))),
      papply("Ord", papply("List", "Int")),
      tname("???")
    ))
  )

  test("given-alias-using-anonymous")(
    runTestAssert[Stat]("given (using ord: Ord[Int]): Ord[List[Int]] = ???")(Defn.GivenAlias(
      Nil,
      anon,
      Nil,
      List(List(tparam(List(Mod.Using()), "ord", papply("Ord", "Int")))),
      papply("Ord", papply("List", "Int")),
      tname("???")
    ))
  )

  test("given-alias-inline")(runTestAssert[Stat]("inline given intOrd: Ord[Int] = ???")(
    Defn.GivenAlias(List(Mod.Inline()), tname("intOrd"), Nil, Nil, papply("Ord", "Int"), tname("???"))
  ))
  test("given-alias-type-apply") {
    runTestAssert[Stat](
      """|object AppliedName:
         |  given Conversion[AppliedName, Expr.Apply[Id]] = _.toExpr
         |""".stripMargin,
      assertLayout =
        Some("object AppliedName { given Conversion[AppliedName, Expr.Apply[Id]] = _.toExpr }")
    )(Defn.Object(
      Nil,
      tname("AppliedName"),
      tpl(Defn.GivenAlias(
        Nil,
        anon,
        Nil,
        Nil,
        papply("Conversion", "AppliedName", papply(pselect("Expr", "Apply"), "Id")),
        Term.AnonymousFunction(tselect(Term.Placeholder(), "toExpr"))
      ))
    ))
  }

  // ---------------------------------
  // Abstract given
  // ---------------------------------

  test("abstract-given")(runTestAssert[Stat]("given intOrd: Ord[Int]")(
    Decl.Given(Nil, tname("intOrd"), Nil, Nil, papply("Ord", "Int"))
  ))

  test("abstract-given-inline")(runTestAssert[Stat]("inline given intOrd: Ord[Int]")(
    Decl.Given(List(Mod.Inline()), tname("intOrd"), Nil, Nil, papply("Ord", "Int"))
  ))

  test("abstract-given-depend")(
    runTestAssert[Stat]("given setOrd[T](using ord: Ord[T]): Ord[Set[T]]")(Decl.Given(
      Nil,
      tname("setOrd"),
      List(pparam("T")),
      List(List(tparam(List(Mod.Using()), "ord", papply("Ord", "T")))),
      papply("Ord", papply("Set", "T"))
    ))
  )

  test("abstract-given-anonymous") {
    runTestError("given Ord[Int]", "abstract givens cannot be anonymous")
    runTestError("given [T](using ord: Ord[T]): Ord[Set[T]]", "abstract givens cannot be anonymous")
    runTestError("given (using Ord[String]): Ord[Int]", "abstract givens cannot be anonymous")
  }

  test("abstract-given-with") {
    runTestError("given (using Ord[String]): Ord[Int] with Eq[Int]", "expected 'with' <body>")
    runTestError("given ordered(using Ord[String]): Ord[Int] with Eq[Int]", "expected 'with' <body>")
  }

  test("given without sig and type in parens looking like a param clause") {
    val code =
      """|given (using[String]) with Eq[Int] with {
         |  def foo = ???
         |}
         |""".stripMargin
    val layout = "given using[String] with Eq[Int] with { def foo = ??? }"
    val tree = Defn.Given(
      Nil,
      anon,
      None,
      tpl(
        List(init(papply("using", "String")), init(papply("Eq", "Int"))),
        List(Defn.Def(Nil, "foo", Nil, None, "???"))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // ---------------------------------
  // USING
  // ---------------------------------

  test("using-named")(runTestAssert[Stat]("def f(a: Int)(using ord: UInt): Int = ???")(Defn.Def(
    Nil,
    tname("f"),
    Nil,
    List(List(tparam("a", "Int")), List(tparamUsing("ord", "UInt"))),
    Some(pname("Int")),
    tname("???")
  )))

  test("using-anonymous")(runTestAssert[Stat]("def f(a: Int)(using UInt): Int = ???")(Defn.Def(
    Nil,
    tname("f"),
    Nil,
    List(List(tparam("a", "Int")), List(tparamUsing("", "UInt"))),
    Some(pname("Int")),
    tname("???")
  )))

  test("using-multiple-parens") {
    runTestAssert[Stat]("def f(a: Int)(using ui: UInt)(using us: UString): Boolean = ???")(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      List(
        List(tparam("a", "Int")),
        List(tparamUsing("ui", "UInt")),
        List(tparamUsing("us", "UString"))
      ),
      Some(pname("Boolean")),
      tname("???")
    ))
    runTestAssert[Stat]("def f(a: Int)(using UInt)(using UString): Boolean = ???")(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      List(List(tparam("a", "Int")), List(tparamUsing("", "UInt")), List(tparamUsing("", "UString"))),
      Some(pname("Boolean")),
      tname("???")
    ))
  }

  test("using-many-single-paren") {
    val paramByName = tparam(List(Mod.Using()), "us", Type.ByName(pname("UString")))
    runTestAssert[Stat]("def f(a: Int)(using ui: UInt, us: => UString): Boolean = ???")(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      List(List(tparam("a", "Int")), List(tparamUsing("ui", "UInt"), paramByName)),
      Some(pname("Boolean")),
      tname("???")
    ))
    runTestAssert[Stat]("def f(a: Int)(using UInt, UString): Boolean = ???")(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      List(List(tparam("a", "Int")), List(tparamUsing("", "UInt"), tparamUsing("", "UString"))),
      Some(pname("Boolean")),
      tname("???")
    ))
  }

  test("using-complex")(
    runTestAssert[Stat]("def f(x: String)(using Int)(y: String)(using b: Int): Unit = ???")(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      List(
        List(tparam("x", "String")),
        List(tparamUsing("", "Int")),
        List(tparam("y", "String")),
        List(tparamUsing("b", "Int"))
      ),
      Some(pname("Unit")),
      tname("???")
    ))
  )

  test("using-lambda-method-parameter") {
    val output =
      """|LazyBody {
         |  (using ctx: Context) => 3
         |}
         |""".stripMargin
    runTestAssert[Stat]("LazyBody { (using ctx: Context) => 3 }", assertLayout = Some(output))(
      tapply(tname("LazyBody"), blk(tfunc(tparam(List(Mod.Using()), "ctx", "Context"))(int(3))))
    )

  }

  test("using-named-with-by-name-parameter") {
    val usingByName = tparam(List(Mod.Using()), "a", Type.ByName(pname("Int")))
    runTestAssert[Stat]("def f(using a: => Int): Unit = ???")(
      Defn.Def(Nil, tname("f"), Nil, List(List(usingByName)), Some(pname("Unit")), tname("???"))
    )
  }

  test("using-call-site")(runTestAssert[Stat]("val a = f()(using a)(using 3, true)")(Defn.Val(
    Nil,
    List(patvar("a")),
    None,
    tapplyUsing(tapplyUsing(tapply(tname("f")), tname("a")), int(3), bool(true))
  )))

  test("using-anonymous-method") {
    runTestAssert[Stat]("val fun = (using ctx: Context) => ctx.open")(Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using()), "ctx", "Context"))(tselect("ctx", "open"))
    ))

    runTestAssert[Stat]("val fun = (using ctx) => ctx.open")(Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using()), "ctx"))(tselect("ctx", "open"))
    ))

    runTestAssert[Stat]("val fun = (using _: Context) => ctx.open")(Defn.Val(
      Nil,
      List(patvar("fun")),
      None,
      tfunc(tparam(List(Mod.Using()), "_", "Context"))(tselect("ctx", "open"))
    ))
  }

  test("given-pat")(
    runTestAssert[Stat](
      """|pair match {
         |  case (ctx @ given Context, y) =>
         |}
         |""".stripMargin
    )(tmatch(
      tname("pair"),
      Case(
        Pat.Tuple(List(Pat.Bind(patvar("ctx"), Pat.Given(pname("Context"))), patvar("y"))),
        None,
        blk()
      )
    ))
  )
  test("given-pat-new") {
    runTestAssert[Stat](
      """|pair match {
         | case ctx @ given Context => new Provider
         | case ctx @ given (Context => String) => new Provider
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|pair match {
           |  case ctx @ given Context =>
           |    new Provider
           |  case ctx @ given (Context => String) =>
           |    new Provider
           |}
           |""".stripMargin
      )
    )(tmatch(
      tname("pair"),
      Case(Pat.Bind(patvar("ctx"), Pat.Given(pname("Context"))), None, Term.New(init("Provider"))),
      Case(
        Pat.Bind(patvar("ctx"), Pat.Given(pfunc(pname("Context"))(pname("String")))),
        None,
        Term.New(init("Provider"))
      )
    ))
  }

  test("given-pat-for")(
    runTestAssert[Stat](
      """|for given Context <- applicationContexts do a
         |""".stripMargin,
      assertLayout = Some("for (given Context <- applicationContexts) a")
    )(Term.For(
      List(Enumerator.Generator(Pat.Given(pname("Context")), tname("applicationContexts"))),
      tname("a")
    ))
  )

  // ---------------------------------
  // GIVEN IMPORT
  // ---------------------------------

  test("import-given") {

    runTestAssert[Stat]("import File.given")(Import(
      List(Importer(tname("File"), List(Importee.GivenAll())))
    ))

    runTestAssert[Stat]("import File.{ *, given }")(Import(
      List(Importer(tname("File"), List(Importee.Wildcard(), Importee.GivenAll())))
    ))

    runTestAssert[Stat]("import File.{ given, * }")(Import(
      List(Importer(tname("File"), List(Importee.GivenAll(), Importee.Wildcard())))
    ))

    runTestAssert[Stat]("import File.{ given TC }", assertLayout = Some("import File.given TC"))(
      Import(List(Importer(tname("File"), List(Importee.Given(pname("TC"))))))
    )

    runTestAssert[Stat]("import File.{ given TC, given AC, * }")(Import(List(Importer(
      tname("File"),
      List(Importee.Given(pname("TC")), Importee.Given(pname("AC")), Importee.Wildcard())
    ))))

    runTestAssert[Stat]("import Instances.{ im, given Ordering[?] }")(Import(List(Importer(
      tname("Instances"),
      List(Importee.Name(Name("im")), Importee.Given(papply("Ordering", pwildcard)))
    ))))
  }

  test("lazy given") {
    val code = "lazy given foo: Foo = ???"
    val tree = Defn.GivenAlias(List(Mod.Lazy()), tname("foo"), None, pname("Foo"), tname("???"))
    runTestAssert[Stat](code)(tree)
  }

  test("scalafmt #4764") {
    val code =
      """|object a:
         |  def foo =
         |    bar:
         |      case given String =>
         |  end foo
         |""".stripMargin
    val layout =
      """|object a {
         |  def foo = bar {
         |    case given String =>
         |      end foo
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      "a",
      tpl(Defn.Def(
        Nil,
        "foo",
        Nil,
        None,
        tapply("bar", Term.PartialFunction(List(Case(Pat.Given("String"), None, Term.EndMarker("foo")))))
      ))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
