package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.tests.tokenizers.TokenizerSuite

class EnumSuite extends BaseDottySuite {

  implicit val parseStat: String => Stat = code => templStat(code)(dialects.Dotty)

  /**
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/enums/enums.html
   *  https://dotty.epfl.ch/docs/reference/enums/adts.html
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
        Template(Nil, List(init("T"), init("R")), Nil, slf, List(RGCase))
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
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Nil, Self(Name(""), None), Nil)
      )
    )
    runTestAssert[Stat]("enum Color { val PI = 314 }")(
      Defn.Enum(
        Nil,
        Type.Name("Color"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
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
    def ext(tp: String) = Init(Type.Apply(pname("Option"), List(pname(tp))), anon, Nil)

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
    runTestAssert[Stat]("case A, B, C")(
      Defn.RepeatedEnumCase(Nil, List(tname("A"), tname("B"), tname("C")))
    )
  }

  test("case-single") {
    runTestAssert[Stat]("case A")(
      Defn.EnumCase(Nil, tname("A"), Nil, ctor, Nil)
    )
    runTestAssert[Stat]("case A()")(
      Defn.EnumCase(Nil, tname("A"), Nil, ctorp(), Nil)
    )
  }

  test("case-arguments") {
    runTestAssert[Stat]("case Some(x: Int)")(
      Defn.EnumCase(Nil, tname("Some"), Nil, ctorp(List(tparam("x", "Int"))), Nil)
    )
  }

  test("case-annotation") {
    runTestAssert[Stat]("@deprecated case Some(x: Int)")(
      Defn.EnumCase(
        List(Mod.Annot(Init(Type.Name("deprecated"), Name(""), Nil))),
        tname("Some"),
        Nil,
        ctorp(List(tparam("x", "Int"))),
        Nil
      )
    )
  }

  test("case-generic") {
    val generic = Type.Param(Nil, pname("X"), Nil, Type.Bounds(None, None), Nil, List(pname("Ord")))
    runTestAssert[Stat]("case A[X: Ord]()")(
      Defn.EnumCase(Nil, tname("A"), List(generic), ctorp(), Nil)
    )
    runTestAssert[Stat]("case A[X: Ord]")(
      Defn.EnumCase(Nil, tname("A"), List(generic), ctor, Nil)
    )
  }

  test("case-extends") {
    val init = Init(Type.Apply(pname("Option"), List(pname("Nothing"))), anon, Nil)
    runTestAssert[Stat]("case None extends Option[Nothing]")(
      Defn.EnumCase(Nil, tname("None"), Nil, ctor, List(init))
    )
  }

  test("case-extends-argument") {
    val init = Init(pname("Color"), anon, List(List(Lit.Int(65280))))
    runTestAssert[Stat]("case Red extends Color(65280)")(
      Defn.EnumCase(Nil, tname("Red"), Nil, ctor, List(init))
    )
  }

  test("case-extends-argument-generic") {
    val init = Init(pname("Color"), anon, List(List(Lit.Int(65280))))
    runTestAssert[Stat]("case Red[T](a: Int) extends Color(65280)")(
      Defn
        .EnumCase(Nil, tname("Red"), List(pparam("T")), ctorp(List(tparam("a", "Int"))), List(init))
    )
  }
}
