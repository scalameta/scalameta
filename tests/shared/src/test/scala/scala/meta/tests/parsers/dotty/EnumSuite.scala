package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.tests.tokenizers.TokenizerSuite

class EnumSuite extends BaseDottySuite {

  /**
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/enums/enums.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/enums/adts.html]]
   */
  // ---------------------------------
  // ENUM
  // ---------------------------------

  val RGCase = Defn.RepeatedEnumCase(Nil, List(tname("R"), tname("G")))

  test("enum") {
    runTestAssert[Stat]("enum Color { case R, G }")(
      Defn.Enum(Nil, pname("Color"), Nil, ctor, tpl(List(RGCase)))
    )
  }

  test("enum-private-case") {
    runTestAssert[Stat](
      "enum Color { private case R; protected case G }",
      assertLayout = Some(
        """|enum Color {
           |  private case R
           |  protected case G
           |}
           |""".stripMargin
      )
    )(
      Defn.Enum(
        Nil,
        Type.Name("Color"),
        Nil,
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.EnumCase(
              List(Mod.Private(Name(""))),
              Term.Name("R"),
              Nil,
              EmptyCtor(),
              Nil
            ),
            Defn.EnumCase(
              List(Mod.Protected(Name(""))),
              Term.Name("G"),
              Nil,
              EmptyCtor(),
              Nil
            )
          ),
          Nil
        )
      )
    )
  }

  test("enum-wrong-soft") {
    runTestError[Stat](
      """|enum Color:
         | open case R, G """.stripMargin,
      "error: Only access modifiers allowed on enum case"
    )
  }

  test("enum-parametrized") {
    runTestAssert[Stat]("enum C(i: Int) { case R, G }")(
      Defn.Enum(Nil, pname("C"), Nil, ctorp(List(tparam("i", "Int"))), tpl(List(RGCase)))
    )
  }

  test("enum-extends") {
    runTestAssert[Stat]("enum C extends T with R { case R, G }")(
      Defn.Enum(
        Nil,
        pname("C"),
        Nil,
        ctor,
        Template(Nil, List(init("T"), init("R")), slf, List(RGCase))
      )
    )
  }

  test("enum-generics") {
    runTestAssert[Stat]("enum C[X, Y] { case R, G }")(
      Defn.Enum(Nil, pname("C"), List(pparam("X"), pparam("Y")), ctor, tpl(List(RGCase)))
    )
  }

  test("enum-no-case-allowed") {
    runTestAssert[Stat]("enum Color { }", assertLayout = None)(
      Defn.Enum(
        Nil,
        Type.Name("Color"),
        Nil,
        EmptyCtor(),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )
    runTestAssert[Stat]("enum Color { val PI = 314 }")(
      Defn.Enum(
        Nil,
        Type.Name("Color"),
        Nil,
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Defn.Val(Nil, List(Pat.Var(Term.Name("PI"))), None, Lit.Int(314)))
        )
      )
    )
  }

  test("enum-single-case") {
    runTestAssert[Stat]("enum Color { case R extends Color }")(
      Defn.Enum(
        Nil,
        pname("Color"),
        Nil,
        ctor,
        tpl(
          List(
            Defn.EnumCase(Nil, tname("R"), Nil, ctor, List(init("Color")))
          )
        )
      )
    )
  }

  test("enum-other-stat") {
    runTestAssert[Stat]("enum C { val PI=3; def r: Int = 4; case R }", assertLayout = None)(
      Defn.Enum(
        Nil,
        pname("C"),
        Nil,
        ctor,
        tpl(
          List(
            Defn.Val(Nil, List(Pat.Var(tname("PI"))), None, int(3)),
            Defn.Def(Nil, tname("r"), Nil, Nil, Some(pname("Int")), int(4)),
            Defn.EnumCase(Nil, tname("R"), Nil, ctor, Nil)
          )
        )
      )
    )
  }

  test("enum-parse-option") {
    val tparam =
      Type.Param(List(Mod.Covariant()), pname("T"), Nil, Type.Bounds(None, None), Nil, Nil)
    val xparam = List(Term.Param(Nil, tname("x"), Some(pname("T")), None))
    def ext(tp: String) = Init(Type.Apply(pname("Option"), List(pname(tp))), anon, emptyArgClause)

    {
      val opt = """
                  |enum Option[+T] {
                  |  case Some(x: T)
                  |  case None
                  |}
      """.stripMargin
      runTestAssert[Stat](opt)(
        Defn.Enum(
          Nil,
          pname("Option"),
          List(tparam),
          ctor,
          tpl(
            List(
              Defn.EnumCase(Nil, tname("Some"), Nil, ctorp(xparam), Nil),
              Defn.EnumCase(Nil, tname("None"), Nil, ctor, Nil)
            )
          )
        )
      )
    }

    {
      val opt = """
                  |enum Option[+T] {
                  |  case Some(x: T) extends Option[T]
                  |  case None extends Option[Nothing]
                  |}""".stripMargin
      runTestAssert[Stat](opt)(
        Defn.Enum(
          Nil,
          pname("Option"),
          List(tparam),
          ctor,
          tpl(
            List(
              Defn.EnumCase(Nil, tname("Some"), Nil, ctorp(xparam), List(ext("T"))),
              Defn.EnumCase(Nil, tname("None"), Nil, ctor, List(ext("Nothing")))
            )
          )
        )
      )
    }
  }

  test("case-caseother-diff") {
    val code = """
                 |enum X {
                 |  case R
                 |  v match {
                 |    case Unap => 1
                 |    case Unap => 2
                 |  }
                 |  case G
                 |}""".stripMargin
    def unap(i: Int): Case = Case(tname("Unap"), None, int(i))
    runTestAssert[Stat](code)(
      Defn.Enum(
        Nil,
        pname("X"),
        Nil,
        ctor,
        tpl(
          List(
            Defn.EnumCase(Nil, tname("R"), Nil, ctor, Nil),
            Term.Match(tname("v"), List(unap(1), unap(2))),
            Defn.EnumCase(Nil, tname("G"), Nil, ctor, Nil)
          )
        )
      )
    )
  }

  final val rcase = Defn.EnumCase(Nil, tname("R"), Nil, ctor, Nil)
  final val gcase = Defn.EnumCase(Nil, tname("G"), Nil, ctor, Nil)

  test("case-match-interop") {
    val code = """
                 |enum X {
                 |  case R
                 |  v match {
                 |    case 'a' |
                 |         'c' |
                 |         'e' => "OK"
                 |  }
                 |  case G
                 |}""".stripMargin
    val cmatch = Term.Match(
      tname("v"),
      List(
        Case(
          Pat.Alternative(Lit.Char('a'), Pat.Alternative(Lit.Char('c'), Lit.Char('e'))),
          None,
          Lit.String("OK")
        )
      )
    )
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Enum(Nil, pname("X"), Nil, ctor, tpl(List(rcase, cmatch, gcase)))
    )
  }

  test("case-match-if-interop") {
    val code = """
                 |enum X {
                 |  case R
                 |  v match {
                 |    case x
                 |       if x > 3 => "OK"
                 |  }
                 |  case G
                 |}""".stripMargin
    val cmatch = Term.Match(
      tname("v"),
      List(
        Case(
          Pat.Var(tname("x")),
          Some(Term.ApplyInfix(tname("x"), tname(">"), Nil, List(Lit.Int(3)))),
          Lit.String("OK")
        )
      )
    )
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Enum(Nil, pname("X"), Nil, ctor, tpl(List(rcase, cmatch, gcase)))
    )
  }

  test("case-try-interop") {
    val code = """
                 |enum X {
                 |  case R
                 |  try a() catch {
                 |    case 'a' |
                 |         'e' => "Recovered"
                 |    case 'c' => "ERROR"
                 |  }
                 |  case G
                 |}""".stripMargin

    val tryCatch = Term.Try(
      Term.Apply(tname("a"), Nil),
      List(
        Case(Pat.Alternative(Lit.Char('a'), Lit.Char('e')), None, Lit.String("Recovered")),
        Case(Lit.Char('c'), None, Lit.String("ERROR"))
      ),
      None
    )
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Enum(Nil, pname("X"), Nil, ctor, tpl(List(rcase, tryCatch, gcase)))
    )
  }
  // ---------------------------------
  // CASE
  // ---------------------------------

  test("case-repeated") {
    runTestAssert[Stat]("enum E { case A, B, C }")(
      enumWithCase("E", Defn.RepeatedEnumCase(Nil, List(tname("A"), tname("B"), tname("C"))))
    )
  }

  test("case-single") {
    runTestAssert[Stat]("enum E { case A }")(
      enumWithCase("E", Defn.EnumCase(Nil, tname("A"), Nil, ctor, Nil))
    )
    runTestAssert[Stat]("enum E { case A() }")(
      enumWithCase("E", Defn.EnumCase(Nil, tname("A"), Nil, ctorp(), Nil))
    )
  }

  test("case-arguments") {
    runTestAssert[Stat]("enum Option { case Some(x: Int) }")(
      enumWithCase(
        "Option",
        Defn.EnumCase(Nil, tname("Some"), Nil, ctorp(List(tparam("x", "Int"))), Nil)
      )
    )
  }

  test("case-annotation") {
    runTestAssert[Stat]("enum Option { @deprecated case Some(x: Int) }")(
      enumWithCase(
        "Option",
        Defn.EnumCase(
          List(Mod.Annot(Init(Type.Name("deprecated"), Name(""), emptyArgClause))),
          tname("Some"),
          Nil,
          ctorp(List(tparam("x", "Int"))),
          Nil
        )
      )
    )
  }

  test("case-generic") {
    val generic = Type.Param(Nil, pname("X"), Nil, Type.Bounds(None, None), Nil, List(pname("Ord")))
    runTestAssert[Stat]("enum E { case A[X: Ord]() }")(
      enumWithCase("E", Defn.EnumCase(Nil, tname("A"), List(generic), ctorp(), Nil))
    )
    runTestAssert[Stat]("enum E { case A[X: Ord] }")(
      enumWithCase("E", Defn.EnumCase(Nil, tname("A"), List(generic), ctor, Nil))
    )
  }

  test("case-extends") {
    val init = Init(Type.Apply(pname("Option"), List(pname("Nothing"))), anon, emptyArgClause)
    runTestAssert[Stat]("enum Option { case None extends Option[Nothing] }")(
      enumWithCase("Option", Defn.EnumCase(Nil, tname("None"), Nil, ctor, List(init)))
    )
  }

  test("case-extends-argument") {
    val init = Init(pname("Color"), anon, List(List(Lit.Int(65280))))
    runTestAssert[Stat]("enum Color { case Red extends Color(65280) }")(
      enumWithCase(
        "Color",
        Defn.EnumCase(
          Nil,
          Term.Name("Red"),
          Nil,
          EmptyCtor(),
          List(Init(Type.Name("Color"), Name(""), List(List(Lit.Int(65280)))))
        )
      )
    )
  }

  test("case-extends-argument-generic") {
    val init = Init(pname("Color"), anon, List(List(Lit.Int(65280))))
    runTestAssert[Stat]("enum Color { case Red[T](a: Int) extends Color(65280) }")(
      enumWithCase(
        "Color",
        Defn.EnumCase(
          Nil,
          Term.Name("Red"),
          List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
          Ctor.Primary(
            Nil,
            Name(""),
            List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None)))
          ),
          List(Init(Type.Name("Color"), Name(""), List(List(Lit.Int(65280)))))
        )
      )
    )
  }

  test("enum-case-toplevel") {
    val init = Init(pname("Color"), anon, List(List(Lit.Int(65280))))
    runTestError[Stat](
      """|class A { 
         |  case Red[T](a: Int) extends Color(65280) 
         |}""".stripMargin,
      "Enum cases are only allowed in enums"
    )
  }

  test("enum-colon") {
    val code =
      """|enum A:
         |  case B, C
      """.stripMargin
    val expected = "enum A { case B, C }"
    runTestAssert[Stat](code, assertLayout = Some(expected))(
      enumWithCase(
        "A",
        Defn.RepeatedEnumCase(Nil, List(Term.Name("B"), Term.Name("C")))
      )
    )
  }

  test("enum-annotated") {
    val code =
      """|@annot
         |enum A:
         |  case B, C
      """.stripMargin
    val expected = "@annot enum A { case B, C }"
    runTestAssert[Stat](code, assertLayout = Some(expected))(
      Defn.Enum(
        List(Mod.Annot(Init(Type.Name("annot"), Name(""), emptyArgClause))),
        Type.Name("A"),
        Nil,
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Defn.RepeatedEnumCase(Nil, List(Term.Name("B"), Term.Name("C")))),
          Nil
        )
      )
    )
  }

  test("enum-self-braces") {
    val code =
      """|enum A { self =>
         |  case B(v: Int)
         |  case C(v: String)
         |}
      """.stripMargin
    runTestAssert[Stat](code)(
      Defn.Enum(
        Nil,
        Type.Name("A"),
        Nil,
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          Self(Term.Name("self"), None),
          List(
            Defn.EnumCase(
              Nil,
              Term.Name("B"),
              Nil,
              Ctor.Primary(
                Nil,
                Name(""),
                List(List(Term.Param(Nil, Term.Name("v"), Some(Type.Name("Int")), None)))
              ),
              Nil
            ),
            Defn.EnumCase(
              Nil,
              Term.Name("C"),
              Nil,
              Ctor.Primary(
                Nil,
                Name(""),
                List(List(Term.Param(Nil, Term.Name("v"), Some(Type.Name("String")), None)))
              ),
              Nil
            )
          ),
          Nil
        )
      )
    )
  }

  test("enum-self-indented") {
    val code =
      """|enum A:
         |  self =>
         |    case B(v: Int)
         |    case C(v: String)
         |  def fx: Int = 4
      """.stripMargin
    val expected =
      """|enum A { self =>
         |  case B(v: Int)
         |  case C(v: String)
         |  def fx: Int = 4
         |}""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(expected))(
      Defn.Enum(
        Nil,
        Type.Name("A"),
        Nil,
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          Self(Term.Name("self"), None),
          List(
            Defn.EnumCase(
              Nil,
              Term.Name("B"),
              Nil,
              Ctor.Primary(
                Nil,
                Name(""),
                List(List(Term.Param(Nil, Term.Name("v"), Some(Type.Name("Int")), None)))
              ),
              Nil
            ),
            Defn.EnumCase(
              Nil,
              Term.Name("C"),
              Nil,
              Ctor.Primary(
                Nil,
                Name(""),
                List(List(Term.Param(Nil, Term.Name("v"), Some(Type.Name("String")), None)))
              ),
              Nil
            ),
            Defn.Def(Nil, Term.Name("fx"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(4))
          ),
          Nil
        )
      )
    )
  }

  test("enum-colon-sep-after") {
    val code =
      """|object Main :
         |  enum A {
         |    case B, C
         |  }
         |  object X:
         |    val x = "x"
      """.stripMargin
    val expected =
      """|object Main {
         |  enum A { case B, C }
         |  object X { val x = "x" }
         |}
      """.stripMargin
    runTestAssert[Stat](code, assertLayout = Some(expected))(
      Defn.Object(
        Nil,
        Term.Name("Main"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            enumWithCase("A", Defn.RepeatedEnumCase(Nil, List(Term.Name("B"), Term.Name("C")))),
            Defn.Object(
              Nil,
              Term.Name("X"),
              Template(
                Nil,
                Nil,
                Self(Name(""), None),
                List(
                  Defn.Val(Nil, List(Pat.Var(Term.Name("x"))), None, Lit.String("x"))
                ),
                Nil
              )
            )
          ),
          Nil
        )
      )
    )
  }

  test("enum-inside-indented") {
    val code = """|object Main:
                  |  enum A:
                  |    kind =>
                  |
                  |    case B, C
                  |
                  |  end A
                  |""".stripMargin
    val output =
      """|object Main {
         |  enum A { kind => case B, C }
         |  end A
         |}
      """.stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Object(
        Nil,
        Term.Name("Main"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Enum(
              Nil,
              Type.Name("A"),
              Nil,
              EmptyCtor(),
              Template(
                Nil,
                Nil,
                Self(Term.Name("kind"), None),
                List(
                  Defn.RepeatedEnumCase(Nil, List(Term.Name("B"), Term.Name("C")))
                ),
                Nil
              )
            ),
            Term.EndMarker(Term.Name("A"))
          ),
          Nil
        )
      )
    )
  }

  test("enum-arrow-toplevel") {
    val code =
      """|enum T2Enum:
         |  case Hmm
         |  val a = () =>  
         |    fx()
         |    gx()
    """.stripMargin
    val expected =
      """|enum T2Enum {
         |  case Hmm
         |  val a = () => {
         |    fx()
         |    gx()
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(expected))(
      Defn.Enum(
        Nil,
        Type.Name("T2Enum"),
        Nil,
        EmptyCtor(),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.EnumCase(Nil, Term.Name("Hmm"), Nil, EmptyCtor(), Nil),
            Defn.Val(
              Nil,
              List(Pat.Var(Term.Name("a"))),
              None,
              Term.Function(
                Nil,
                Term.Block(List(Term.Apply(Term.Name("fx"), Nil), Term.Apply(Term.Name("gx"), Nil)))
              )
            )
          ),
          Nil
        )
      )
    )
  }

  private def enumWithCase(name: String, enumCase: Stat) = Defn.Enum(
    Nil,
    Type.Name(name),
    Nil,
    EmptyCtor(),
    Template(
      Nil,
      Nil,
      Self(Name(""), None),
      List(
        enumCase
      ),
      Nil
    )
  )
}
