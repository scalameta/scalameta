package scala.meta.tests.parsers.dotty

import scala.meta._

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

  test("enum")(runTestAssert[Stat]("enum Color { case R, G }")(
    Defn.Enum(Nil, pname("Color"), Nil, ctor, tpl(RGCase))
  ))

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
    )(Defn.Enum(
      Nil,
      pname("Color"),
      Nil,
      EmptyCtor(),
      tpl(
        Defn.EnumCase(List(Mod.Private(anon)), tname("R"), Nil, EmptyCtor(), Nil),
        Defn.EnumCase(List(Mod.Protected(anon)), tname("G"), Nil, EmptyCtor(), Nil)
      )
    ))
  }

  test("enum-wrong-soft")(runTestError[Stat](
    """|enum Color:
       | open case R, G """.stripMargin,
    "error: Only access modifiers allowed on enum case"
  ))

  test("enum-parametrized")(runTestAssert[Stat]("enum C(i: Int) { case R, G }")(
    Defn.Enum(Nil, pname("C"), Nil, ctorp(tparam("i", "Int")), tpl(RGCase))
  ))

  test("enum-extends")(runTestAssert[Stat]("enum C extends T with R { case R, G }")(
    Defn.Enum(Nil, pname("C"), Nil, ctor, tpl(List(init("T"), init("R")), List(RGCase)))
  ))

  test("enum-generics")(runTestAssert[Stat]("enum C[X, Y] { case R, G }")(
    Defn.Enum(Nil, pname("C"), List(pparam("X"), pparam("Y")), ctor, tpl(RGCase))
  ))

  test("enum-no-case-allowed") {
    runTestAssert[Stat]("enum Color { }", assertLayout = None)(
      Defn.Enum(Nil, pname("Color"), Nil, EmptyCtor(), EmptyTemplate())
    )
    runTestAssert[Stat]("enum Color { val PI = 314 }")(Defn.Enum(
      Nil,
      pname("Color"),
      Nil,
      EmptyCtor(),
      tpl(Defn.Val(Nil, List(patvar("PI")), None, int(314)))
    ))
  }

  test("enum-single-case")(runTestAssert[Stat]("enum Color { case R extends Color }")(Defn.Enum(
    Nil,
    pname("Color"),
    Nil,
    ctor,
    tpl(Defn.EnumCase(Nil, tname("R"), Nil, ctor, List(init("Color"))))
  )))

  test("enum-other-stat")(
    runTestAssert[Stat]("enum C { val PI=3; def r: Int = 4; case R }", assertLayout = None)(
      Defn.Enum(
        Nil,
        pname("C"),
        Nil,
        ctor,
        tpl(
          Defn.Val(Nil, List(patvar("PI")), None, int(3)),
          Defn.Def(Nil, tname("r"), Nil, Nil, Some(pname("Int")), int(4)),
          Defn.EnumCase(Nil, tname("R"), Nil, ctor, Nil)
        )
      )
    )
  )

  test("enum-parse-option") {
    val typeParam = pparam(List(Mod.Covariant()), "T")
    val xparam = List(tparam("x", "T"))
    def ext(tp: String) = init(papply("Option", tp))

    {
      val opt =
        """|
           |enum Option[+T] {
           |  case Some(x: T)
           |  case None
           |}
           |""".stripMargin
      runTestAssert[Stat](opt)(Defn.Enum(
        Nil,
        pname("Option"),
        List(typeParam),
        ctor,
        tpl(
          Defn.EnumCase(Nil, tname("Some"), Nil, ctorp(xparam), Nil),
          Defn.EnumCase(Nil, tname("None"), Nil, ctor, Nil)
        )
      ))
    }

    {
      val opt =
        """
          |enum Option[+T] {
          |  case Some(x: T) extends Option[T]
          |  case None extends Option[Nothing]
          |}""".stripMargin
      runTestAssert[Stat](opt)(Defn.Enum(
        Nil,
        pname("Option"),
        List(typeParam),
        ctor,
        tpl(
          Defn.EnumCase(Nil, tname("Some"), Nil, ctorp(xparam), List(ext("T"))),
          Defn.EnumCase(Nil, tname("None"), Nil, ctor, List(ext("Nothing")))
        )
      ))
    }
  }

  test("case-caseother-diff") {
    val code =
      """
        |enum X {
        |  case R
        |  v match {
        |    case Unap => 1
        |    case Unap => 2
        |  }
        |  case G
        |}""".stripMargin
    def unap(i: Int): Case = Case(tname("Unap"), None, int(i))
    runTestAssert[Stat](code)(Defn.Enum(
      Nil,
      pname("X"),
      Nil,
      ctor,
      tpl(
        Defn.EnumCase(Nil, tname("R"), Nil, ctor, Nil),
        tmatch(tname("v"), unap(1), unap(2)),
        Defn.EnumCase(Nil, tname("G"), Nil, ctor, Nil)
      )
    ))
  }

  final val rcase = Defn.EnumCase(Nil, tname("R"), Nil, ctor, Nil)
  final val gcase = Defn.EnumCase(Nil, tname("G"), Nil, ctor, Nil)

  test("case-match-interop") {
    val code =
      """
        |enum X {
        |  case R
        |  v match {
        |    case 'a' |
        |         'c' |
        |         'e' => "OK"
        |  }
        |  case G
        |}""".stripMargin
    val cmatch = tmatch(
      tname("v"),
      Case(Pat.Alternative(lit('a'), Pat.Alternative(lit('c'), lit('e'))), None, str("OK"))
    )
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Enum(Nil, pname("X"), Nil, ctor, tpl(rcase, cmatch, gcase))
    )
  }

  test("case-match-if-interop") {
    val code =
      """
        |enum X {
        |  case R
        |  v match {
        |    case x
        |       if x > 3 => "OK"
        |  }
        |  case G
        |}""".stripMargin
    val cmatch = Term
      .Match(tname("v"), List(Case(patvar("x"), Some(tinfix(tname("x"), ">", int(3))), str("OK"))))
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Enum(Nil, pname("X"), Nil, ctor, tpl(rcase, cmatch, gcase))
    )
  }

  test("case-try-interop") {
    val code =
      """
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
      tapply(tname("a")),
      List(
        Case(Pat.Alternative(lit('a'), lit('e')), None, str("Recovered")),
        Case(lit('c'), None, str("ERROR"))
      ),
      None
    )
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Enum(Nil, pname("X"), Nil, ctor, tpl(rcase, tryCatch, gcase))
    )
  }
  // ---------------------------------
  // CASE
  // ---------------------------------

  test("case-repeated")(runTestAssert[Stat]("enum E { case A, B, C }")(
    enumWithCase("E", Defn.RepeatedEnumCase(Nil, List(tname("A"), tname("B"), tname("C"))))
  ))

  test("case-single") {
    runTestAssert[Stat]("enum E { case A }")(
      enumWithCase("E", Defn.EnumCase(Nil, tname("A"), Nil, ctor, Nil))
    )
    runTestAssert[Stat]("enum E { case A() }")(
      enumWithCase("E", Defn.EnumCase(Nil, tname("A"), Nil, ctorp(Nil), Nil))
    )
  }

  test("case-arguments")(runTestAssert[Stat]("enum Option { case Some(x: Int) }")(
    enumWithCase("Option", Defn.EnumCase(Nil, tname("Some"), Nil, ctorp(tparam("x", "Int")), Nil))
  ))

  test("case-annotation")(
    runTestAssert[Stat]("enum Option { @deprecated case Some(x: Int) }")(enumWithCase(
      "Option",
      Defn.EnumCase(
        List(Mod.Annot(init("deprecated"))),
        tname("Some"),
        Nil,
        ctorp(tparam("x", "Int")),
        Nil
      )
    ))
  )

  test("case-generic") {
    val generic = pparam("X", bounds(cb = List(pname("Ord"))))
    runTestAssert[Stat]("enum E { case A[X: Ord]() }")(
      enumWithCase("E", Defn.EnumCase(Nil, tname("A"), List(generic), ctorp(Nil), Nil))
    )
    runTestAssert[Stat]("enum E { case A[X: Ord] }")(
      enumWithCase("E", Defn.EnumCase(Nil, tname("A"), List(generic), ctor, Nil))
    )
  }

  test("case-extends")(
    runTestAssert[Stat]("enum Option { case None extends Option[Nothing] }")(enumWithCase(
      "Option",
      Defn.EnumCase(Nil, tname("None"), Nil, ctor, List(init(papply("Option", "Nothing"))))
    ))
  )

  test("case-extends-argument")(
    runTestAssert[Stat]("enum Color { case Red extends Color(65280) }")(enumWithCase(
      "Color",
      Defn.EnumCase(Nil, tname("Red"), Nil, EmptyCtor(), List(init("Color", List(int(65280)))))
    ))
  )

  test("case-extends-argument-generic")(
    runTestAssert[Stat]("enum Color { case Red[T](a: Int) extends Color(65280) }")(enumWithCase(
      "Color",
      Defn.EnumCase(
        Nil,
        tname("Red"),
        List(pparam("T")),
        ctorp(List(tparam("a", "Int"))),
        List(init("Color", List(int(65280))))
      )
    ))
  )

  test("enum-case-toplevel")(runTestError[Stat](
    """|class A { 
       |  case Red[T](a: Int) extends Color(65280) 
       |}""".stripMargin,
    "Enum cases are only allowed in enums"
  ))

  test("enum-colon") {
    val code =
      """|enum A:
         |  case B, C
         |""".stripMargin
    val expected = "enum A { case B, C }"
    runTestAssert[Stat](code, assertLayout = Some(expected))(
      enumWithCase("A", Defn.RepeatedEnumCase(Nil, List(tname("B"), tname("C"))))
    )
  }

  test("enum-annotated") {
    val code =
      """|@annot
         |enum A:
         |  case B, C
         |""".stripMargin
    val expected = "@annot enum A { case B, C }"
    runTestAssert[Stat](code, assertLayout = Some(expected))(Defn.Enum(
      List(Mod.Annot(init("annot"))),
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(Defn.RepeatedEnumCase(Nil, List(tname("B"), tname("C"))))
    ))
  }

  test("enum-self-braces") {
    val code =
      """|enum A { self =>
         |  case B(v: Int)
         |  case C(v: String)
         |}
         |""".stripMargin
    runTestAssert[Stat](code)(Defn.Enum(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(
        self("self"),
        Defn.EnumCase(Nil, tname("B"), Nil, ctorp(List(tparam("v", "Int"))), Nil),
        Defn.EnumCase(Nil, tname("C"), Nil, ctorp(List(tparam("v", "String"))), Nil)
      )
    ))
  }

  test("enum-self-indented") {
    val code =
      """|enum A:
         |  self =>
         |    case B(v: Int)
         |    case C(v: String)
         |  def fx: Int = 4
         |""".stripMargin
    val expected =
      """|enum A { self =>
         |  case B(v: Int)
         |  case C(v: String)
         |  def fx: Int = 4
         |}""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(expected))(Defn.Enum(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(
        self("self"),
        Defn.EnumCase(Nil, tname("B"), Nil, ctorp(tparam("v", "Int")), Nil),
        Defn.EnumCase(Nil, tname("C"), Nil, ctorp(tparam("v", "String")), Nil),
        Defn.Def(Nil, tname("fx"), Nil, Nil, Some(pname("Int")), int(4))
      )
    ))
  }

  test("enum-colon-sep-after") {
    val code =
      """|object Main :
         |  enum A {
         |    case B, C
         |  }
         |  object X:
         |    val x = "x"
         |""".stripMargin
    val expected =
      """|object Main {
         |  enum A { case B, C }
         |  object X { val x = "x" }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(expected))(Defn.Object(
      Nil,
      tname("Main"),
      tpl(
        enumWithCase("A", Defn.RepeatedEnumCase(Nil, List(tname("B"), tname("C")))),
        Defn.Object(Nil, tname("X"), tpl(Defn.Val(Nil, List(patvar("x")), None, str("x"))))
      )
    ))
  }

  test("enum-inside-indented") {
    val code =
      """|object Main:
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
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Object(
      Nil,
      tname("Main"),
      tpl(
        Defn.Enum(
          Nil,
          pname("A"),
          Nil,
          EmptyCtor(),
          tpl(self("kind"), Defn.RepeatedEnumCase(Nil, List(tname("B"), tname("C"))))
        ),
        Term.EndMarker(tname("A"))
      )
    ))
  }

  test("enum-arrow-toplevel") {
    val code =
      """|enum T2Enum:
         |  case Hmm
         |  val a = () =>  
         |    fx()
         |    gx()
         |""".stripMargin
    val expected =
      """|enum T2Enum {
         |  case Hmm
         |  val a = () => {
         |    fx()
         |    gx()
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(expected))(Defn.Enum(
      Nil,
      pname("T2Enum"),
      Nil,
      EmptyCtor(),
      tpl(
        Defn.EnumCase(Nil, tname("Hmm"), Nil, EmptyCtor(), Nil),
        Defn.Val(Nil, List(patvar("a")), None, tfunc()(blk(tapply(tname("fx")), tapply(tname("gx")))))
      )
    ))
  }

  private def enumWithCase(name: String, enumCase: Stat) = Defn
    .Enum(Nil, pname(name), Nil, EmptyCtor(), tpl(enumCase))
}
