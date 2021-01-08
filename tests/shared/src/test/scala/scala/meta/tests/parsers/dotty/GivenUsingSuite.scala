package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class GivenUsingSuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)

  /**
   * For checking examples in repl declare:
   *  trait Ord[T] { def f(): Int }
   *
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/contextual/givens.html
   *  https://dotty.epfl.ch/docs/reference/contextual/using-clauses.html
   *  https://dotty.epfl.ch/docs/reference/contextual/given-imports.html
   */
  // ---------------------------------
  // GIVEN
  // ---------------------------------

  def defone(mods: List[Mod]): Defn.Def =
    Defn.Def(mods, tname("f"), Nil, List(Nil), Some(pname("Int")), int(1))
  val defone: Defn.Def = defone(Nil)

  test("given-named") {
    runTestAssert[Stat]("given intOrd: Ord[Int] with { def f(): Int = 1 }")(
      Defn.Given(
        Nil,
        Type.Name("intOrd"),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Name(""), None),
          List(defone),
          Nil
        )
      )
    )
  }

  test("given-named-newline") {
    runTestAssert[Stat]("given intOrd: Ord[Int] with \n{ def f(): Int = 1 }", assertLayout = None)(
      Defn.Given(
        Nil,
        Type.Name("intOrd"),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Name(""), None),
          List(defone),
          Nil
        )
      )
    )
  }

  test("given-anonymous") {
    runTestAssert[Stat]("given Ord[Int] with { def f(): Int = 1 }")(
      Defn.Given(
        Nil,
        Name(""),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Name(""), None),
          List(defone),
          Nil
        )
      )
    )
  }

  test("given-multiple-inheritance") {
    val expectedGiven = Defn.Given(
      Nil,
      Type.Name("intM"),
      Nil,
      Nil,
      Template(
        Nil,
        List(
          Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil),
          Init(Type.Apply(Type.Name("Eq"), List(Type.Name("Int"))), Name(""), Nil)
        ),
        Self(Name(""), None),
        List(Defn.Def(Nil, Term.Name("fx"), Nil, Nil, None, Lit.Int(3))),
        Nil
      )
    )

    runTestAssert[Stat]("given intM: Ord[Int] with Eq[Int] with { def fx = 3 }")(expectedGiven)

    runTestAssert[Stat](
      "given intM: Ord[Int] with\n Eq[Int] with { def fx = 3 }",
      assertLayout = Some("given intM: Ord[Int] with Eq[Int] with { def fx = 3 }")
    )(
      expectedGiven
    )
  }

  test("given-override-def") {
    runTestAssert[Stat](
      "given intOrd: Ord[Int] \n with { override def f(): Int = 1 }",
      assertLayout = Some("given intOrd: Ord[Int] with { override def f(): Int = 1 }")
    )(
      Defn.Given(
        Nil,
        Type.Name("intOrd"),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Name(""), None),
          List(defone(List(Mod.Override()))),
          Nil
        )
      )
    )
  }
  test("given-override-def-colon-newline") {
    runTestAssert[Stat](
      """|given intOrd
         |   : Ord[Int] with {
         |  def fn = ()
         |}
         |""".stripMargin,
      assertLayout = Some("given intOrd: Ord[Int] with { def fn = () }")
    )(
      Defn.Given(
        Nil,
        Type.Name("intOrd"),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Name(""), None),
          List(Defn.Def(Nil, Term.Name("fn"), Nil, Nil, None, Lit.Unit())),
          Nil
        )
      )
    )
  }

  test("given-self") {
    runTestAssert[Stat]("given intOrd: Ord[Int] with { current => }")(
      Defn.Given(
        Nil,
        Type.Name("intOrd"),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Term.Name("current"), None),
          Nil,
          Nil
        )
      )
    )
  }

  test("given-selftype-error") {
    runTestError(
      "given intOrd: Ord[Int] with { current: Ord[Int] => }",
      "given cannot have a self type"
    )
  }

  test("given-no-block") {
    runTestAssert[Stat]("given intOrd: Ord[Int]")(
      Defn.Given(
        Nil,
        Type.Name("intOrd"),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Name(""), None),
          Nil,
          Nil
        )
      )
    )
  }

  test("given-anonymous-no-block") {
    runTestAssert[Stat]("given Ord[Int]")(
      Defn.Given(
        Nil,
        Name(""),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Name(""), None),
          Nil,
          Nil
        )
      )
    )
  }

  test("given-generic-named") {
    runTestAssert[Stat]("given listOrd[T]: Ord[List[T]] with { def f(): Int = 1 }")(
      Defn.Given(
        Nil,
        Type.Name("listOrd"),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Nil,
        Template(
          Nil,
          List(
            Init(
              Type
                .Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("List"), List(Type.Name("T"))))),
              Name(""),
              Nil
            )
          ),
          Self(Name(""), None),
          List(defone),
          Nil
        )
      )
    )
  }

  test("given-generic-anonymous") {
    runTestAssert[Stat]("given Ord[List[T]] with { def f(): Int = 1 }")(
      Defn.Given(
        Nil,
        Name(""),
        Nil,
        Nil,
        Template(
          Nil,
          List(
            Init(
              Type
                .Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("List"), List(Type.Name("T"))))),
              Name(""),
              Nil
            )
          ),
          Self(Name(""), None),
          List(defone),
          Nil
        )
      )
    )
  }

  test("given-depend-given-named") {
    runTestAssert[Stat]("given setOrd[T](using ord: Ord[T]): Ord[Set[T]]")(
      Defn.Given(
        Nil,
        Type.Name("setOrd"),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              Term.Name("ord"),
              Some(Type.Apply(Type.Name("Ord"), List(Type.Name("T")))),
              None
            )
          )
        ),
        Template(
          Nil,
          List(
            Init(
              Type
                .Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("Set"), List(Type.Name("T"))))),
              Name(""),
              Nil
            )
          ),
          Self(Name(""), None),
          Nil,
          Nil
        )
      )
    )
  }

  test("given-depend-given-anonymous") {
    runTestAssert[Stat]("given [T](using ord: Ord[T]): Ord[Set[T]]")(
      Defn.Given(
        Nil,
        Name(""),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              Term.Name("ord"),
              Some(Type.Apply(Type.Name("Ord"), List(Type.Name("T")))),
              None
            )
          )
        ),
        Template(
          Nil,
          List(
            Init(
              Type
                .Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("Set"), List(Type.Name("T"))))),
              Name(""),
              Nil
            )
          ),
          Self(Name(""), None),
          Nil,
          Nil
        )
      )
    )
  }

  test("given-depend-given-anonymous-using") {
    runTestAssert[Stat]("given (using Ord[String]): Ord[Int]")(
      Defn.Given(
        Nil,
        Name(""),
        Nil,
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              Name(""),
              Some(Type.Apply(Type.Name("Ord"), List(Type.Name("String")))),
              None
            )
          )
        ),
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Name(""), None),
          Nil,
          Nil
        )
      )
    )
  }

  test("given-inline") {
    runTestAssert[Stat]("inline given intOrd: Ord[Int] with { def f(): Int = 1 }")(
      Defn.Given(
        List(Mod.Inline()),
        Type.Name("intOrd"),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Name(""), None),
          List(
            Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))
          ),
          Nil
        )
      )
    )
  }

  // ---------------------------------
  // GIVEN ALIAS
  // ---------------------------------

  test("given-alias-named") {
    runTestAssert[Stat]("given global: Option[Int] = Some(3)")(
      Defn.GivenAlias(
        Nil,
        pname("global"),
        Nil,
        Nil,
        Type.Apply(pname("Option"), List(pname("Int"))),
        Term.Apply(tname("Some"), List(int(3)))
      )
    )
  }

  test("given-alias-anonymous") {
    runTestAssert[Stat]("given Option[Int] = Some(3)")(
      Defn.GivenAlias(
        Nil,
        anon,
        Nil,
        Nil,
        Type.Apply(pname("Option"), List(pname("Int"))),
        Term.Apply(tname("Some"), List(int(3)))
      )
    )
  }

  test("given-alias-block") {
    runTestAssert[Stat](
      "given global: Option[Int] = { def f(): Int = 1; Some(3) }",
      assertLayout = None
    )(
      Defn.GivenAlias(
        Nil,
        pname("global"),
        Nil,
        Nil,
        Type.Apply(pname("Option"), List(pname("Int"))),
        Term.Block(List(defone, Term.Apply(tname("Some"), List(int(3)))))
      )
    )
  }

  test("given-alias-using-named") {
    runTestAssert[Stat]("given ordInt(using ord: Ord[Int]): Ord[List[Int]] = ???")(
      Defn.GivenAlias(
        Nil,
        pname("ordInt"),
        Nil,
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              tname("ord"),
              Some(Type.Apply(pname("Ord"), List(pname("Int")))),
              None
            )
          )
        ),
        Type.Apply(pname("Ord"), List(Type.Apply(pname("List"), List(pname("Int"))))),
        tname("???")
      )
    )
  }

  test("given-alias-using-anonymous") {
    runTestAssert[Stat]("given (using ord: Ord[Int]): Ord[List[Int]] = ???")(
      Defn.GivenAlias(
        Nil,
        anon,
        Nil,
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              tname("ord"),
              Some(Type.Apply(pname("Ord"), List(pname("Int")))),
              None
            )
          )
        ),
        Type.Apply(pname("Ord"), List(Type.Apply(pname("List"), List(pname("Int"))))),
        tname("???")
      )
    )
  }

  test("given-alias-inline") {
    runTestAssert[Stat]("inline given intOrd: Ord[Int] = ???")(
      Defn.GivenAlias(
        List(Mod.Inline()),
        Type.Name("intOrd"),
        Nil,
        Nil,
        Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        Term.Name("???")
      )
    )
  }

  // ---------------------------------
  // USING
  // ---------------------------------

  test("using-named") {
    runTestAssert[Stat]("def f(a: Int)(using ord: UInt): Int = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(List(tparam("a", "Int")), List(tparamUsing("ord", "UInt"))),
        Some(pname("Int")),
        tname("???")
      )
    )
  }

  test("using-anonymous") {
    runTestAssert[Stat]("def f(a: Int)(using UInt): Int = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(List(tparam("a", "Int")), List(tparamUsing("", "UInt"))),
        Some(pname("Int")),
        tname("???")
      )
    )
  }

  test("using-multiple-parens") {
    runTestAssert[Stat]("def f(a: Int)(using ui: UInt)(using us: UString): Boolean = ???")(
      Defn.Def(
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
      )
    )
    runTestAssert[Stat]("def f(a: Int)(using UInt)(using UString): Boolean = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(
          List(tparam("a", "Int")),
          List(tparamUsing("", "UInt")),
          List(tparamUsing("", "UString"))
        ),
        Some(pname("Boolean")),
        tname("???")
      )
    )
  }

  test("using-many-single-paren") {
    val paramByName =
      Term.Param(List(Mod.Using()), tname("us"), Some(Type.ByName(pname("UString"))), None)
    runTestAssert[Stat]("def f(a: Int)(using ui: UInt, us: => UString): Boolean = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(List(tparam("a", "Int")), List(tparamUsing("ui", "UInt"), paramByName)),
        Some(pname("Boolean")),
        tname("???")
      )
    )
    runTestAssert[Stat]("def f(a: Int)(using UInt, UString): Boolean = ???")(
      Defn.Def(
        Nil,
        tname("f"),
        Nil,
        List(List(tparam("a", "Int")), List(tparamUsing("", "UInt"), tparamUsing("", "UString"))),
        Some(pname("Boolean")),
        tname("???")
      )
    )
  }

  test("using-complex") {
    runTestAssert[Stat]("def f(x: String)(using Int)(y: String)(using b: Int): Unit = ???")(
      Defn.Def(
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
      )
    )
  }

  test("using-lambda-method-parameter") {
    val output = """|LazyBody {
                    |  (using ctx: Context) => 3
                    |}
                    |""".stripMargin
    runTestAssert[Stat]("LazyBody { (using ctx: Context) => 3 }", assertLayout = Some(output))(
      Term.Apply(
        Term.Name("LazyBody"),
        List(
          Term.Block(
            List(
              Term.Function(
                List(
                  Term.Param(List(Mod.Using()), Term.Name("ctx"), Some(Type.Name("Context")), None)
                ),
                Lit.Int(3)
              )
            )
          )
        )
      )
    )

  }

  test("using-named-with-by-name-parameter") {
    val usingByName =
      Term.Param(List(Mod.Using()), tname("a"), Some(Type.ByName(pname("Int"))), None)
    runTestAssert[Stat]("def f(using a: => Int): Unit = ???")(
      Defn.Def(Nil, tname("f"), Nil, List(List(usingByName)), Some(pname("Unit")), tname("???"))
    )
  }

  test("using-call-site") {
    runTestAssert[Stat]("val a = f()(using a)(using 3, true)")(
      Defn.Val(
        Nil,
        List(Pat.Var(tname("a"))),
        None,
        Term.ApplyUsing(
          Term.ApplyUsing(Term.Apply(tname("f"), Nil), List(tname("a"))),
          List(int(3), Lit.Boolean(true))
        )
      )
    )
  }

  test("using-anonymous-method") {
    runTestAssert[Stat]("val fun = (using ctx: Context) => ctx.open")(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("fun"))),
        None,
        Term.Function(
          List(Term.Param(List(Mod.Using()), Term.Name("ctx"), Some(Type.Name("Context")), None)),
          Term.Select(Term.Name("ctx"), Term.Name("open"))
        )
      )
    )

    runTestAssert[Stat]("val fun = (using ctx) => ctx.open")(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("fun"))),
        None,
        Term.Function(
          List(Term.Param(List(Mod.Using()), Term.Name("ctx"), None, None)),
          Term.Select(Term.Name("ctx"), Term.Name("open"))
        )
      )
    )

    runTestAssert[Stat]("val fun = (using _: Context) => ctx.open")(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("fun"))),
        None,
        Term.Function(
          List(Term.Param(List(Mod.Using()), Name(""), Some(Type.Name("Context")), None)),
          Term.Select(Term.Name("ctx"), Term.Name("open"))
        )
      )
    )
  }

  test("given-pat") {
    runTestAssert[Stat](
      """|pair match {
         |  case (ctx @ given Context, y) =>
         |}
         |""".stripMargin
    )(
      Term.Match(
        Term.Name("pair"),
        List(
          Case(
            Pat.Tuple(
              List(
                Pat.Bind(Pat.Var(Term.Name("ctx")), Pat.Given(Type.Name("Context"))),
                Pat.Var(Term.Name("y"))
              )
            ),
            None,
            Term.Block(Nil)
          )
        )
      )
    )
  }

  test("given-pat-for") {
    runTestAssert[Stat](
      """|for given Context <- applicationContexts do a
         |""".stripMargin,
      assertLayout = Some(
        "for (given Context <- applicationContexts) a"
      )
    )(
      Term.For(
        List(
          Enumerator.Generator(Pat.Given(Type.Name("Context")), Term.Name("applicationContexts"))
        ),
        Term.Name("a")
      )
    )
  }

  // ---------------------------------
  // GIVEN IMPORT
  // ---------------------------------

  test("import-given") {

    runTestAssert[Stat]("import File.given")(
      Import(List(Importer(Term.Name("File"), List(Importee.GivenAll()))))
    )

    runTestAssert[Stat]("import File.{ _, given }")(
      Import(List(Importer(Term.Name("File"), List(Importee.Wildcard(), Importee.GivenAll()))))
    )

    runTestAssert[Stat]("import File.{ given, _ }")(
      Import(List(Importer(Term.Name("File"), List(Importee.GivenAll(), Importee.Wildcard()))))
    )

    runTestAssert[Stat]("import File.{ given TC }")(
      Import(
        List(
          Importer(
            Term.Name("File"),
            List(
              Importee.Given(
                Type.Name("TC")
              )
            )
          )
        )
      )
    )

    runTestAssert[Stat]("import File.{ given TC, given AC, _ }")(
      Import(
        List(
          Importer(
            Term.Name("File"),
            List(
              Importee.Given(Type.Name("TC")),
              Importee.Given(Type.Name("AC")),
              Importee.Wildcard()
            )
          )
        )
      )
    )

    runTestAssert[Stat]("import Instances.{ im, given Ordering[?] }")(
      Import(
        List(
          Importer(
            Term.Name("Instances"),
            List(
              Importee.Name(Name("im")),
              Importee.Given(
                Type.Apply(Type.Name("Ordering"), List(Type.Placeholder(Type.Bounds(None, None))))
              )
            )
          )
        )
      )
    )
  }
}
