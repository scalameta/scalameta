package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.tests.tokenizers.TokenizerSuite

class EnumSuite extends BaseDottySuite {

  implicit val parseStat: String => Stat = code => templStat(code)

  /**
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/enums/enums.html
   *  https://dotty.epfl.ch/docs/reference/enums/adts.html
   */
  // ---------------------------------
  // ENUM
  // ---------------------------------

  val RGCase = Enum.RepeatedCase(Nil, List(tname("R"), tname("G")))

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
        Template(Nil, List(init("T"), init("R")), slf, List(RGCase))
      )
    )
  }

  test("enum-generics") {
    runTestAssert[Stat]("enum C[X,Y] { case R, G }")(
      Defn.Enum(Nil, pname("C"), List(pparam("X"), pparam("Y")), ctor, tpl(List(RGCase)))
    )
  }

  test("enum-min-one-case") {
    runTestError("enum Color { }", "Enumerations must contain at least one case")
    runTestError("enum Color { val PI=3.14 }", "Enumerations must contain at least one case")
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
            Enum.Case(Nil, tname("R"), Nil, ctor, List(init("Color")))
          )
        )
      )
    )
  }

  test("enum-other-stat") {
    runTestAssert[Stat]("enum C { val PI=3; def r: Int = 4; case R }")(
      Defn.Enum(
        Nil,
        pname("C"),
        Nil,
        ctor,
        tpl(
          List(
            Defn.Val(Nil, List(Pat.Var(tname("PI"))), None, int(3)),
            Defn.Def(Nil, tname("r"), Nil, Nil, Some(pname("Int")), int(4)),
            Enum.Case(Nil, tname("R"), Nil, ctor, Nil)
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
      enum Option[+T] {
        case Some(x: T) 
        case None
      }
      """
      runTestAssert[Stat](opt)(
        Defn.Enum(
          Nil,
          pname("Option"),
          List(tparam),
          ctor,
          tpl(
            List(
              Enum.Case(Nil, tname("Some"), Nil, ctorp(xparam), Nil),
              Enum.Case(Nil, tname("None"), Nil, ctor, Nil)
            )
          )
        )
      )
    }

    {
      val opt = """
                  |enum Option[+T] {
                  |  case Some(x: T) extends Option[T]
                  |  case None       extends Option[Nothing]
                  |}""".stripMargin
      runTestAssert[Stat](opt)(
        Defn.Enum(
          Nil,
          pname("Option"),
          List(tparam),
          ctor,
          tpl(
            List(
              Enum.Case(Nil, tname("Some"), Nil, ctorp(xparam), List(ext("T"))),
              Enum.Case(Nil, tname("None"), Nil, ctor, List(ext("Nothing")))
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
                 |    case Unap => 5
                 |  }
                 |  case G
                 |}
    """.stripMargin
    runTestAssert[Stat](code)(
      Defn.Enum(
        Nil,
        pname("X"),
        Nil,
        ctor,
        tpl(
          List(
            Enum.Case(Nil, tname("R"), Nil, ctor, List()),
            Term.Match(tname("v"), List(Case(tname("Unap"), None, int(5)))),
            Enum.Case(Nil, tname("G"), Nil, ctor, Nil)
          )
        )
      )
    )
  }

  // ---------------------------------
  // CASE
  // ---------------------------------

  test("case-repeated") {
    runTestAssert[Stat]("case A, B, C")(
      Enum.RepeatedCase(Nil, List(tname("A"), tname("B"), tname("C")))
    )
  }

  test("case-single") {
    runTestAssert[Stat]("case A")(
      Enum.Case(Nil, tname("A"), Nil, ctor, Nil)
    )
    runTestAssert[Stat]("case A()")(
      Enum.Case(Nil, tname("A"), Nil, ctorp(), Nil)
    )
  }

  test("case-arguments") {
    runTestAssert[Stat]("case Some(x: Int)")(
      Enum.Case(Nil, tname("Some"), Nil, ctorp(List(tparam("x", "Int"))), Nil)
    )
  }

  test("case-generic") {
    val generic = Type.Param(Nil, pname("X"), Nil, Type.Bounds(None, None), Nil, List(pname("Ord")))
    runTestAssert[Stat]("case A[X : Ord]()")(
      Enum.Case(Nil, tname("A"), List(generic), ctorp(), Nil)
    )
    runTestAssert[Stat]("case A[X : Ord]")(
      Enum.Case(Nil, tname("A"), List(generic), ctor, Nil)
    )
  }

  test("case-extends") {
    val init = Init(Type.Apply(pname("Option"), List(pname("Nothing"))), anon, Nil)
    runTestAssert[Stat]("case None extends Option[Nothing]")(
      Enum.Case(Nil, tname("None"), Nil, ctor, List(init))
    )
  }

  test("case-extends-argument") {
    val init = Init(pname("Color"), anon, List(List(Lit.Int(0xFF00))))
    runTestAssert[Stat]("case Red extends Color(0xFF00)")(
      Enum.Case(Nil, tname("Red"), Nil, ctor, List(init))
    )
  }

  test("case-extends-argument-generic") {
    val init = Init(pname("Color"), anon, List(List(Lit.Int(0xFF00))))
    runTestAssert[Stat]("case Red[T](a: Int) extends Color(0xFF00)")(
      Enum.Case(Nil, tname("Red"), List(pparam("T")), ctorp(List(tparam("a", "Int"))), List(init))
    )
  }
}
