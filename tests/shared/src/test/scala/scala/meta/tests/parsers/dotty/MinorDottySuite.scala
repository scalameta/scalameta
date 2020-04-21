package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.Type.Apply
import scala.meta.Type.Placeholder

class MinorDottySuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)
  implicit val parseType: String => Type = code => tpe(code)

  /**
   *
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/other-new-features/named-typeargs.html
   *  https://dotty.epfl.ch/docs/reference/other-new-features/open-classes.html
   *  https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html
   *
   */
  test("named-type-arguments") {
    val namedParam1 = Type.NamedParam(pname("A"), pname("Int"))
    val namedParam2 = Type.NamedParam(pname("B"), pname("List"))
    assertEquals(
      stat("f[A = Int, B = List]()"),
      Term.Apply(Term.ApplyType(tname("f"), List(namedParam1, namedParam2)), Nil): Stat
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
    runTestError[Stat]("final open class A {}", "illegal combination of modifiers: open and final")
    runTestError[Stat](
      "open sealed trait C {}",
      "illegal combination of modifiers: open and sealed for"
    )
    runTestError[Stat]("open def f(): Int = 3", "error: expected start of definition")
    runTestError[Stat]("def f(open a: Int): Int = 3", "error")
  }

  test("open-soft-modifier") {
    stat("def open(open: open): open = ???").structure
  }

  test("type-lambda") {
    // cannot carry +/- but can carry bounds >: , <:
    runTestAssert[Type]("[X, Y] =>> Map[Y, X]")(
      Type.Lambda(
        List(pparam("X"), pparam("Y")),
        Type.Apply(pname("Map"), List(pname("Y"), pname("X")))
      )
    )
    runTestAssert[Type]("[X >: L <: U] =>> R")(
      Type.Lambda(
        List(
          Type
            .Param(Nil, pname("X"), Nil, Type.Bounds(Some(pname("L")), Some(pname("U"))), Nil, Nil)
        ),
        pname("R")
      )
    )
    runTestAssert[Type]("[X] =>> (X, X)")(
      Type.Lambda(List(pparam("X")), Type.Tuple(List(pname("X"), pname("X"))))
    )
    runTestAssert[Type]("[X] =>> [Y] =>> (X, Y)")(
      Type.Lambda(
        List(pparam("X")),
        Type.Lambda(List(pparam("Y")), Type.Tuple(List(pname("X"), pname("Y"))))
      )
    )
  }

  test("literal-types") {
    runTestAssert[Stat]("val a: 42 = 42")(
      Defn.Val(Nil, List(Pat.Var(tname("a"))), Some(int(42)), int(42))
    )
  }

  test("case-classes-empty-plist") {
    templStat("case class A()")
    templStat("case class A @deprecated() ()")
    templStat("case class A private ()")
  }

  test("xml literals") {
    intercept[TokenizeException] { term("<foo>{bar}</foo>") }
  }

}
