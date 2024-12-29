package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.parsers.ParseException

class TypeSuite extends ParseSuite {
  import Type.{Name => _, _}

  private def assertTpe(expr: String)(tree: => Tree)(implicit dialect: Dialect): Unit =
    assertTree(tpe(expr))(tree)

  test("T")(assertTpe("T")(pname("T")))

  test("F[T]")(assertTpe("F[T]")(papply("F", "T")))

  test("F#T")(assertTpe("F#T")(Project("F", "T")))

  test("A \\/ B")(assertTpe("A \\/ B")(pinfix("A", "\\/", "B")))

  test("A * B")(assertTpe("A * B")(pinfix("A", "*", "B")))

  test("A * B + C")(assertTpe("A * B + C")(pinfix(pinfix("A", "*", "B"), "+", "C")))

  test("A + B * C")(assertTpe("A + B * C")(pinfix(pinfix("A", "+", "B"), "*", "C")))

  test("A * B + C / D") {
    assertTpe("A * B + C / D")(pinfix(pinfix(pinfix("A", "*", "B"), "+", "C"), "/", "D"))
  }

  test("f.T")(assertTpe("f.T")(pselect("f", "T")))

  test("f.type")(assertTpe("f.type")(Type.Singleton("f")))

  test("super.T")(assertTpe("super.T")(pselect(Term.Super(anon, anon), "T")))

  test("this.T")(assertTpe("this.T")(pselect(Term.This(anon), "T")))

  test("(A, B)")(assertTpe("(A, B)")(Type.Tuple(pname("A") :: pname("B") :: Nil)))

  test("(A, B) => C")(assertTpe("(A, B) => C")(pfunc("A", "B")("C")))

  test("T @foo")(assertTpe("T @foo")(Type.Annotate("T", Mod.Annot(init("foo")) :: Nil)))

  test("A with B")(assertTpe("A with B")(Type.With("A", "B")))

  test("A & B is not a special type")(assertTpe("A & B")(pinfix("A", "&", "B")))

  test("A with B {}")(assertTpe("A with B {}")(Type.Refine(Some(Type.With("A", "B")), Nil)))

  test("{}")(assertTpe("{}")(Type.Refine(None, Nil)))

  test("A { def x: A; val y: B; type C }") {
    assertTpe("A { def x: Int; val y: B; type C }") {
      Refine(
        Some(pname("A")),
        Decl.Def(Nil, "x", Nil, Nil, "Int") :: Decl.Val(Nil, List(patvar("y")), "B") ::
          Decl.Type(Nil, pname("C"), Nil, noBounds) :: Nil
      )
    }
  }

  test("F[_ >: lo <: hi]") {
    assertTpe("F[_ >: lo <: hi]")(papply("F", Wildcard(bounds("lo", "hi"))))
  }

  test("F[_ >: lo")(assertTpe("F[_ >: lo]")(papply("F", Wildcard(bounds(lo = "lo")))))

  test("F[_ <: hi]")(assertTpe("F[_ <: hi]")(papply("F", Wildcard(bounds(hi = "hi")))))

  test("F[_]")(assertTpe("F[_]")(papply("F", Wildcard(noBounds))))

  test("F[T] forSome { type T }") {
    assertTpe("F[T] forSome { type T }") {
      Existential(papply("F", "T"), Decl.Type(Nil, pname("T"), Nil, noBounds) :: Nil)
    }
  }

  test("a.T forSome { val a: A }") {
    assertTpe("a.T forSome { val a: A }")(
      Existential(pselect("a", "T"), Decl.Val(Nil, patvar("a") :: Nil, "A") :: Nil)
    )
  }

  test("A | B is not a special type")(assertTpe("A | B")(pinfix("A", "|", "B")))

  test("42.type") {
    intercept[ParseException] {
      implicit val dialect = dialects.Scala211
      tpe("42")
    }

    implicit val dialect = dialects.Scala3

    def matchSubStructureTyp3(typ: String, func: PartialFunction[Tree, Unit])(implicit
        loc: munit.Location
    ) = matchSubStructure[Type](typ, func)(parseType, loc)

    assertTpe("42")(int(42))
    assertTpe("-42")(int(-42))
    assertTpe("42L")(lit(42L))
    matchSubStructureTyp3("42.0f", { case Lit(42.0f) => () })
    matchSubStructureTyp3("-42.0f", { case Lit(-42.0f) => () })
    matchSubStructureTyp3("42.0d", { case Lit(42.0d) => () })
    matchSubStructureTyp3("-42.0d", { case Lit(-42.0d) => () })
    assertTpe("\"42\"")(str("42"))
    assertTpe("false")(bool(false))
    assertTpe("true")(bool(true))

    val exceptionScala3 = intercept[ParseException](tpe("() => ()"))
    assertNoDiff(exceptionScala3.shortMessage, "illegal literal type (), use Unit instead")

    val exceptionScala2 = intercept[ParseException](tpe("() => ()"))
    assertNoDiff(exceptionScala2.shortMessage, "illegal literal type (), use Unit instead")

  }

  test("plus-minus-then-underscore-source3") {
    implicit val dialect = dialects.Scala213Source3
    assertTpe("+_ => Int")(pfunc("+_")("Int"))
    assertTpe("Option[- _]")(papply("Option", "-_"))
  }

  test("[scala213] (x: Int, y)") {
    implicit val dialect = dialects.Scala213
    val err = intercept[ParseException](tpe("(x: Int, y)"))
    assertNoDiff(err.shortMessage, "can't mix function type and dependent function type syntaxes")
  }

  test("[scala213] (x: Int, y: Int)(z: String)") {
    implicit val dialect = dialects.Scala213
    val err = intercept[ParseException](tpe("(x: Int, y: Int)(z: String)"))
    assertNoDiff(err.shortMessage, "dependent function types are not supported")
  }

  test("[scala3] (x: Int, y: Int)(z: String)") {
    implicit val dialect = dialects.Scala3
    val err = intercept[ParseException](tpe("(x: Int, y: Int)(z: String)"))
    assertNoDiff(err.shortMessage, "can't have multiple parameter lists in function types")
  }

  test("#3672 [scala213] ***")(runTestAssert[Type]("***")(pname("***")))

}
