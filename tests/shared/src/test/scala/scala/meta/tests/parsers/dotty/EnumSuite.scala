package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.tests.tokenizers.TokenizerSuite

class EnumSuite extends ParseSuite {
  
  implicit val dialect: Dialect = scala.meta.dialects.Dotty

  /**
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/enums/enums.html
   *  https://dotty.epfl.ch/docs/reference/enums/adts.html
   */

  // ---------------------------------
  // ENUM
  // ---------------------------------

  val insideCase = Enum.RepeatedCase(Nil, List(Term.Name("R"), Term.Name("G")))
  test("enum") {
    runTestAssert("enum Color { case R, G }")(
      Defn.Enum(Nil, Type.Name("Color"), Nil, ctor, Template(Nil, Nil, slf, List(insideCase)))
    )
  }
   
  test("enum-parametrized") {
    val enumArg = Term.Param(Nil, Term.Name("i"), Some(Type.Name("Int")), None)
    runTestAssert("enum C(i: Int) { case R, G }")(
      Defn.Enum(Nil, Type.Name("C"), Nil, ctor.copy(paramss=List(List(enumArg))), Template(Nil, Nil, slf, List(insideCase)))
    )
  }

  test("enum-extends") {
    val inits = List(Init(Type.Name("T"), anon, Nil), Init(Type.Name("R"), anon, Nil))
    runTestAssert("enum C extends T with R { case R, G }")(
      Defn.Enum(Nil, Type.Name("C"), Nil, ctor, Template(Nil, inits, slf, List(insideCase)))
    )
  }

  test("enum-generics") {
    def tparam(s: String): Type.Param = Type.Param(Nil, Type.Name(s), Nil, Type.Bounds(None, None), Nil, Nil)
    runTestAssert("enum C[X,Y] { case R, G }")(
      Defn.Enum(Nil, Type.Name("C"), List(tparam("X"), tparam("Y")), ctor, Template(Nil, Nil, slf, List(insideCase)))
    )
  }

  test("enum-min-one-case") {
    runTestError("enum Color { }", "Enumerations must contain at least one case")
    runTestError("enum Color { val PI=3.14 }", "Enumerations must contain at least one case")
  }

  test("enum-single-case") {
    runTestAssert("enum Color { case R extends Color }")(
      Defn.Enum(Nil, Type.Name("Color"), Nil, ctor, Template(Nil, Nil, slf, List(
        Enum.Case(Nil, Term.Name("R"), Nil, ctor, List(Init(Type.Name("Color"), anon, Nil)))
      )))
    )
  }

  // test("enum-other-stat") {
  //   runTestAssert("enum C { val PI=3.14; def r: Double = PI * 4; case R }")(Lit(3))
  // }

  // test("case-caseclass-diff") {
  //   // enum X { case R; case class D(); case T }
  // }

  test("enum-parse-option") {
    val tparam = Type.Param(List(Mod.Covariant()), Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)
    val xparam = List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("T")), None))
    def ext(tp: String) = Init(Type.Apply(Type.Name("Option"), List(Type.Name(tp))), Name.Anonymous(), Nil)

    {
      val opt = """
      enum Option[+T] {
        case Some(x: T) 
        case None
      }
      """
      runTestAssert(opt)(
        Defn.Enum(Nil, Type.Name("Option"), List(tparam), ctor, Template(Nil, Nil, slf, List(
          Enum.Case(Nil, Term.Name("Some"), Nil, Ctor.Primary(Nil, Name(""), List(xparam)), Nil),
          Enum.Case(Nil, Term.Name("None"), Nil, Ctor.Primary(Nil, Name(""), Nil), Nil))
        ))
      )
    }

    {
      val opt = """
      |enum Option[+T] {
      |  case Some(x: T) extends Option[T]
      |  case None       extends Option[Nothing]
      |}""".stripMargin
      runTestAssert(opt)(
        Defn.Enum(Nil, Type.Name("Option"), List(tparam), ctor, Template(Nil, Nil, slf, List(
          Enum.Case(Nil, Term.Name("Some"), Nil, Ctor.Primary(Nil, Name(""), List(xparam)), List(ext("T"))),
          Enum.Case(Nil, Term.Name("None"), Nil, Ctor.Primary(Nil, Name(""), Nil), List(ext("Nothing"))))
        ))
      )
    }
  }

  // ---------------------------------
  // CASE
  // ---------------------------------

  test("case-repeated") {
    runTestAssert("case A, B, C")(
      Enum.RepeatedCase(Nil, List(Term.Name("A"), Term.Name("B"), Term.Name("C")))
    )
  }

  test("case-single") {
    runTestAssert("case A")(
      Enum.Case(Nil, Term.Name("A"), Nil, ctor, Nil)
    )
    runTestAssert("case A()")(
      Enum.Case(Nil, Term.Name("A"), Nil, ctorp, Nil)
    )
  }

  test("case-arguments") {
    runTestAssert("case Some(x: Int)")(
      Enum.Case(Nil, Term.Name("Some"), Nil,
        Ctor.Primary(Nil, Name.Anonymous(), List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None)))),
        Nil
      )
    )
  }

  test("case-generic") {
    val generic = Type.Param(Nil, Type.Name("X"), Nil, Type.Bounds(None, None), Nil, List(Type.Name("Ord")))
    runTestAssert("case A[X : Ord]()")(
      Enum.Case(Nil, Term.Name("A"), List(generic), ctorp, Nil)
    )
    runTestAssert("case A[X : Ord]")(
      Enum.Case(Nil, Term.Name("A"), List(generic), ctor, Nil)
    )
  }

  test("case-extends") {
    val init = Init(Type.Apply(Type.Name("Option"), List(Type.Name("Nothing"))), anon, Nil)
    runTestAssert("case None extends Option[Nothing]")(
      Enum.Case(Nil, Term.Name("None"), Nil, ctor, List(init))
    )
  }

  test("case-extends-argument") {
    val init = Init(Type.Name("Color"), anon, List(List(Lit.Int(0xFF00))))
    runTestAssert("case Red extends Color(0xFF00)")(
      Enum.Case(Nil, Term.Name("Red"), Nil, ctor, List(init))
    )
  }

  test("case-extends-argument-generic") {
    val init = Init(Type.Name("Color"), anon, List(List(Lit.Int(0xFF00))))
    val tparam = Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)
    val a = Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None)
    runTestAssert("case Red[T](a: Int) extends Color(0xFF00)")(
      Enum.Case(Nil, Term.Name("Red"), List(tparam), Ctor.Primary(Nil, Name(""), List(List(a))),
       List(Init(Type.Name("Color"), anon, List(List(Lit.Int(0xFF00))))))
    )
  }

  final val ctor = Ctor.Primary(Nil, Name.Anonymous(), Nil)
  final val ctorp = Ctor.Primary(Nil, Name.Anonymous(), List(List()))
  final val anon = meta.Name.Anonymous()
  val slf = meta.Self(anon, None)

  private def runTestAssert(code: String)(expected: Stat) {
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val obtained: Stat = templStat(code)
    try {
      assertEquals(obtained, expected)
    } catch {
      case e: Throwable =>
        println(s"Generated stat: \n ${obtained.structure}")
        throw e
    }
  }

  private def runTestError(code: String, expected: String) {
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val error = intercept[ParseException] {
      val result = templStat(code)
      println(s"Statement ${code} should not parse! Got result ${result.structure}")
    }
    if (!error.getMessage().contains(expected)) {
      println(s"Expected [${error.getMessage}] to contain [${expected}].")
    }
    assert(error.getMessage.contains(expected))
  }

}