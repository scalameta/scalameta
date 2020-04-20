package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.Type.Apply
import scala.meta.Type.Placeholder

class MinorDottySuite extends ParseSuite {
  
  implicit val dialect: Dialect = scala.meta.dialects.Dotty

  /** 
   * 
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/other-new-features/named-typeargs.html
   *  https://dotty.epfl.ch/docs/reference/other-new-features/open-classes.html
   *  https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html
   *  
   */

  test("named-type-arguments") {
    val namedParam1 = Type.NamedParam(Type.Name("A"), Type.Name("Int"))
    val namedParam2 = Type.NamedParam(Type.Name("B"), Type.Name("List"))
    assertEquals(
      stat("f[A = Int, B = List]()"),
      Term.Apply(Term.ApplyType(Term.Name("f"), List(namedParam1, namedParam2)), Nil): Stat
    )
  }

  test("open-class") {
    val Defn.Class(List(Mod.Open()), Type.Name("A"), _, _, _) = 
      templStat("open class A {}")

    val Defn.Trait(List(Mod.Open()), Type.Name("C"), _, _, _) = 
      templStat("open trait C {}")

    val Defn.Object(List(Mod.Open()), Term.Name("X"), _) = 
      templStat("open object X {}")

  }

  test("open-class-negative-cases") {
    runTestError("final open class A {}", "illegal combination of modifiers: open and final")
    runTestError("open sealed trait C {}", "illegal combination of modifiers: open and sealed for")
    runTestError("open def f(): Int = 3", "error: expected start of definition")
    runTestError("def f(open a: Int): Int = 3", "error")
  }

  test("open-soft-modifier") {
    stat("def open(open: open): open = ???").structure
  }

  def ptype(a: String): Type.Param = Type.Param(Nil, Type.Name(a), Nil, Type.Bounds(None, None), Nil, Nil)
  test("type-lambda") {
    // cannot carry +/- but can carry bounds >: , <:
    runTestAssert("[X, Y] =>> Map[Y, X]")(
      Type.Lambda(List(ptype("X"), ptype("Y")),
        Type.Apply(Type.Name("Map"), List(Type.Name("Y"), Type.Name("X"))))
    ) 
    runTestAssert("[X >: L <: U] =>> R")(
      Type.Lambda(List(Type.Param(Nil, Type.Name("X"), Nil,
        Type.Bounds(Some(Type.Name("L")), Some(Type.Name("U"))), Nil, Nil)),
        Type.Name("R"))
    )
    runTestAssert("[X] =>> (X, X)")(
      Type.Lambda(List(ptype("X")), Type.Tuple(List(Type.Name("X"), Type.Name("X"))))
    )
    runTestAssert("[X] =>> [Y] =>> (X, Y)")(
      Type.Lambda(List(ptype("X")), Type.Lambda(List(ptype("Y")),
        Type.Tuple(List(Type.Name("X"), Type.Name("Y")))))
    )
  }

  private def runTestError(code: String, expected: String) {
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val error = intercept[ParseException] {
      val result = blockStat(code)
      println(s"Statement ${code} should not parse! Got result ${result.structure}")
    }
    if (!error.getMessage().contains(expected)) {
      println(s"Expected [${error.getMessage}] to contain [${expected}].")
    }
    assert(error.getMessage.contains(expected))
  }

  private def runTestAssert(code: String)(expected: meta.Type) {
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val obtained: meta.Type = tpe(code)
    try {
      assertEquals(obtained, expected)
    } catch {
      case e: Throwable =>
        println(s"Generated tpe: \n ${obtained.structure}")
        throw e
    }
  }
}
