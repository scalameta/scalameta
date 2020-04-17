package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class EnumSuite extends ParseSuite {
  
  implicit val dialect: Dialect = scala.meta.dialects.Dotty

  /**
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/enums/enums.html
   *  https://dotty.epfl.ch/docs/reference/enums/adts.html
   * 
   */

  // ---------------------------------
  // ENUM
  // ---------------------------------

  // test("enum") {
  //   runTestAssert("enum Color { case R, G }")(
  //     Defn.Enum(Nil, Type.Name("Color"), Nil, ctor, Template(Nil, Nil, slf, List(
  //       Enum.RepeatedCase(Nil, List(Term.Name("R"), Term.Name("G")))
  //     )))
  //   )
  // }
   
  // test("enum-parametrized") {
  //   runTestAssert("enum C(i: Int) { case A extends C(1); case B extends C(2) }")(Lit(3))
  // }

  // test("enum-other-stat") {
  //   runTestAssert("enum C { val PI=3.14; def r: Double = PI * 4; case R }")(Lit(3))
  // }

  // test("enum-extends") {
  //   runTestAssert("enum C extends T, R { case R }")(Lit(3))
  // }

  // test("enum-generics") {
  //   runTestAssert("enum C[X,Y] extends T, R { case R extends C[T, Int] }")(Lit(3))

  // }

  // test("enum-min-one-case".ignore) {
  //   runTestError("enum Color { }", "Enumerations must contain at least one case")
  //   runTestError("enum Color { val PI=3.14 }", "Enumerations must contain at least one case")
  // }

  // test("case-caseclass-diff") {
  //   // enum X { case R; case class D(); case T }
  // }

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
  }

  test("case-extends") {
    val init = Init(Type.Apply(Type.Name("Option"), List(Type.Name("Nothing"))), meta.Name.Anonymous(), Nil)
    runTestAssert("case None extends Option[Nothing]")(
      Enum.Case(Nil, Term.Name("None"), Nil, ctor, List(init))
    )
  }

  test("case-extends-argument") {
    val init = Init(Type.Name("Color"), meta.Name.Anonymous(), List(List(Lit.Int(0xFF00))))
    runTestAssert("case Red extends Color(0xFF00)")(
      Enum.Case(Nil, Term.Name("Red"), Nil, ctor, List(init))
    )
  }

  final val ctor = Ctor.Primary(Nil, Name.Anonymous(), Nil)
  final val ctorp = Ctor.Primary(Nil, Name.Anonymous(), List(List()))

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