package scala.meta.tests
package parsers

import scala.meta._
import scala.meta.parsers.ParseException

class TypeSuite extends ParseSuite {
  import Name.Anonymous
  import Term.{Name => TermName}
  import Type.{Name => TypeName, _}

  private def assertTpe(expr: String)(tree: => Tree)(implicit dialect: Dialect): Unit =
    assertTree(tpe(expr))(tree)

  test("T")(assertTpe("T")(TypeName("T")))

  test("F[T]")(assertTpe("F[T]")(Apply(TypeName("F"), ArgClause(TypeName("T") :: Nil))))

  test("F#T")(assertTpe("F#T")(Project(TypeName("F"), TypeName("T"))))

  test("A \\/ B") {
    assertTpe("A \\/ B")(ApplyInfix(TypeName("A"), TypeName("\\/"), TypeName("B")))
  }

  test("A * B")(assertTpe("A * B")(ApplyInfix(TypeName("A"), TypeName("*"), TypeName("B"))))

  test("A * B + C") {
    assertTpe("A * B + C") {
      Type.ApplyInfix(Type.ApplyInfix(pname("A"), pname("*"), pname("B")), pname("+"), pname("C"))
    }
  }

  test("A + B * C") {
    assertTpe("A + B * C") {
      Type.ApplyInfix(Type.ApplyInfix(pname("A"), pname("+"), pname("B")), pname("*"), pname("C"))
    }
  }

  test("A * B + C / D") {
    assertTpe("A * B + C / D") {
      Type.ApplyInfix(
        Type
          .ApplyInfix(Type.ApplyInfix(pname("A"), pname("*"), pname("B")), pname("+"), pname("C")),
        pname("/"),
        pname("D")
      )
    }
  }

  test("f.T")(assertTpe("f.T")(Type.Select(tname("f"), pname("T"))))

  test("f.type")(assertTpe("f.type")(Type.Singleton(tname("f"))))

  test("super.T") {
    assertTpe("super.T")(Type.Select(Term.Super(Anonymous(), Anonymous()), pname("T")))
  }

  test("this.T")(assertTpe("this.T")(Type.Select(Term.This(Anonymous()), pname("T"))))

  test("(A, B)")(assertTpe("(A, B)")(Type.Tuple(pname("A") :: pname("B") :: Nil)))

  test("(A, B) => C") {
    assertTpe("(A, B) => C")(Type.Function(pname("A") :: pname("B") :: Nil, pname("C")))
  }

  test("T @foo") {
    assertTpe("T @foo") {
      Type.Annotate(pname("T"), Mod.Annot(Init(pname("foo"), anon, emptyArgClause)) :: Nil)
    }
  }

  test("A with B")(assertTpe("A with B")(Type.With(pname("A"), pname("B"))))

  test("A & B is not a special type") {
    assertTpe("A & B")(Type.ApplyInfix(pname("A"), pname("&"), pname("B")))
  }

  test("A with B {}") {
    assertTpe("A with B {}")(Type.Refine(Some(Type.With(pname("A"), pname("B"))), Nil))
  }

  test("{}")(assertTpe("{}")(Type.Refine(None, Nil)))

  test("A { def x: A; val y: B; type C }") {
    assertTpe("A { def x: Int; val y: B; type C }") {
      Refine(
        Some(TypeName("A")),
        Decl.Def(Nil, TermName("x"), Type.ParamClause(Nil), Nil, TypeName("Int")) ::
          Decl.Val(Nil, List(Pat.Var(TermName("y"))), TypeName("B")) ::
          Decl.Type(Nil, TypeName("C"), Type.ParamClause(Nil), Type.Bounds(None, None)) :: Nil
      )
    }
  }

  test("F[_ >: lo <: hi]") {
    assertTpe("F[_ >: lo <: hi]") {
      Apply(TypeName("F"), List(Wildcard(Bounds(Some(TypeName("lo")), Some(TypeName("hi"))))))
    }
  }

  test("F[_ >: lo") {
    assertTpe("F[_ >: lo]") {
      Apply(TypeName("F"), List(Wildcard(Bounds(Some(TypeName("lo")), None))))
    }
  }

  test("F[_ <: hi]") {
    assertTpe("F[_ <: hi]") {
      Apply(TypeName("F"), List(Wildcard(Bounds(None, Some(TypeName("hi"))))))
    }
  }

  test("F[_]")(assertTpe("F[_]")(Apply(TypeName("F"), List(Wildcard(Bounds(None, None))))))

  test("F[T] forSome { type T }") {
    assertTpe("F[T] forSome { type T }") {
      Existential(
        Apply(TypeName("F"), TypeName("T") :: Nil),
        Decl.Type(Nil, TypeName("T"), Type.ParamClause(Nil), Type.Bounds(None, None)) :: Nil
      )
    }
  }

  test("a.T forSome { val a: A }") {
    assertTpe("a.T forSome { val a: A }")(Existential(
      Select(TermName("a"), TypeName("T")),
      Decl.Val(Nil, Pat.Var(TermName("a")) :: Nil, TypeName("A")) :: Nil
    ))
  }

  test("A | B is not a special type") {
    assertTpe("A | B")(ApplyInfix(TypeName("A"), TypeName("|"), TypeName("B")))
  }

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
    assertTpe("42L")(Lit.Long(42L))
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
    assertTpe("+_ => Int")(Type.Function(List(pname("+_")), pname("Int")))
    assertTpe("Option[- _]")(Apply(pname("Option"), ArgClause(List(pname("-_")))))
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
