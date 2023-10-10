package scala.meta.tests
package parsers

import scala.meta._
import Term.{Super, Name => TermName}
import Type.{Name => TypeName, _}
import Name.Anonymous
import scala.meta.parsers.ParseException

class TypeSuite extends ParseSuite {

  private def assertTpe(expr: String)(tree: Tree)(implicit dialect: Dialect): Unit = {
    assertTree(tpe(expr))(tree)
  }

  import scala.meta.dialects.Scala211

  test("T") {
    assertTpe("T")(TypeName("T"))
  }

  test("F[T]") {
    assertTpe("F[T]") {
      Apply(TypeName("F"), ArgClause(TypeName("T") :: Nil))
    }
  }

  test("F#T") {
    assertTpe("F#T")(Project(TypeName("F"), TypeName("T")))
  }

  test("A \\/ B") {
    assertTpe("A \\/ B")(ApplyInfix(TypeName("A"), TypeName("\\/"), TypeName("B")))
  }

  test("A * B") {
    assertTpe("A * B")(ApplyInfix(TypeName("A"), TypeName("*"), TypeName("B")))
  }

  test("A * B + C") {
    assertTpe("A * B + C") {
      Type.ApplyInfix(
        Type.ApplyInfix(Type.Name("A"), Type.Name("*"), Type.Name("B")),
        Type.Name("+"),
        Type.Name("C")
      )
    }
  }

  test("A + B * C") {
    assertTpe("A + B * C") {
      Type.ApplyInfix(
        Type.ApplyInfix(Type.Name("A"), Type.Name("+"), Type.Name("B")),
        Type.Name("*"),
        Type.Name("C")
      )
    }
  }

  test("A * B + C / D") {
    assertTpe("A * B + C / D") {
      Type.ApplyInfix(
        Type.ApplyInfix(
          Type.ApplyInfix(Type.Name("A"), Type.Name("*"), Type.Name("B")),
          Type.Name("+"),
          Type.Name("C")
        ),
        Type.Name("/"),
        Type.Name("D")
      )
    }
  }

  test("f.T") {
    assertTpe("f.T") {
      Type.Select(Term.Name("f"), Type.Name("T"))
    }
  }

  test("f.type") {
    assertTpe("f.type") {
      Type.Singleton(Term.Name("f"))
    }
  }

  test("super.T") {
    assertTpe("super.T") {
      Type.Select(Term.Super(Anonymous(), Anonymous()), Type.Name("T"))
    }
  }

  test("this.T") {
    assertTpe("this.T") {
      Type.Select(Term.This(Anonymous()), Type.Name("T"))
    }
  }

  test("(A, B)") {
    assertTpe("(A, B)") {
      Type.Tuple(Type.Name("A") :: Type.Name("B") :: Nil)
    }
  }

  test("(A, B) => C") {
    assertTpe("(A, B) => C") {
      Type.Function(Type.Name("A") :: Type.Name("B") :: Nil, Type.Name("C"))
    }
  }

  test("T @foo") {
    assertTpe("T @foo") {
      Type.Annotate(
        Type.Name("T"),
        Mod.Annot(Init(Type.Name("foo"), Name.Anonymous(), emptyArgClause)) :: Nil
      )
    }
  }

  test("A with B") {
    assertTpe("A with B") {
      Type.With(Type.Name("A"), Type.Name("B"))
    }
  }

  test("A & B is not a special type") {
    assertTpe("A & B") {
      Type.ApplyInfix(Type.Name("A"), Type.Name("&"), Type.Name("B"))
    }
  }

  test("A with B {}") {
    assertTpe("A with B {}") {
      Type.Refine(Some(Type.With(Type.Name("A"), Type.Name("B"))), Nil)
    }
  }

  test("{}") {
    assertTpe("{}") {
      Type.Refine(None, Nil)
    }
  }

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
      Apply(
        TypeName("F"),
        List(Wildcard(Bounds(Some(TypeName("lo")), Some(TypeName("hi")))))
      )
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

  test("F[_]") {
    assertTpe("F[_]") {
      Apply(TypeName("F"), List(Wildcard(Bounds(None, None))))
    }
  }

  test("F[T] forSome { type T }") {
    assertTpe("F[T] forSome { type T }") {
      Existential(
        Apply(TypeName("F"), TypeName("T") :: Nil),
        Decl.Type(Nil, TypeName("T"), Type.ParamClause(Nil), Type.Bounds(None, None)) :: Nil
      )
    }
  }

  test("a.T forSome { val a: A }") {
    assertTpe("a.T forSome { val a: A }")(
      Existential(
        Select(TermName("a"), TypeName("T")),
        Decl.Val(Nil, Pat.Var(TermName("a")) :: Nil, TypeName("A")) :: Nil
      )
    )
  }

  test("A | B is not a special type") {
    assertTpe("A | B")(ApplyInfix(TypeName("A"), TypeName("|"), TypeName("B")))
  }

  test("42.type") {
    intercept[ParseException] {
      tpe("42")(dialects.Scala211)
    }

    implicit val dialect = dialects.Scala3

    def matchSubStructureTyp3(typ: String, func: PartialFunction[Tree, Unit])(
        implicit loc: munit.Location
    ) = {
      matchSubStructure[Type](typ, func)(
        parseType,
        dialects.Scala3,
        loc
      )
    }

    assertTpe("42")(Lit.Int(42))(dialects.Scala3)
    assertTpe("-42")(Lit.Int(-42))(dialects.Scala3)
    assertTpe("42L")(Lit.Long(42L))(dialects.Scala3)
    matchSubStructureTyp3("42.0f", { case Lit(42.0f) => () })
    matchSubStructureTyp3("-42.0f", { case Lit(-42.0f) => () })
    matchSubStructureTyp3("42.0d", { case Lit(42.0d) => () })
    matchSubStructureTyp3("-42.0d", { case Lit(-42.0d) => () })
    assertTpe("\"42\"")(Lit.String("42"))(dialects.Scala3)
    assertTpe("false")(Lit.Boolean(false))(dialects.Scala3)
    assertTpe("true")(Lit.Boolean(true))(dialects.Scala3)

    val exceptionScala3 = intercept[ParseException] {
      tpe("() => ()")(dialects.Scala3)
    }
    assertNoDiff(exceptionScala3.shortMessage, "illegal literal type (), use Unit instead")

    val exceptionScala2 = intercept[ParseException] {
      tpe("() => ()")(dialects.Scala213)
    }
    assertNoDiff(exceptionScala2.shortMessage, "illegal literal type (), use Unit instead")

  }

  test("plus-minus-then-underscore-source3") {
    assertTpe("+_ => Int")(Type.Function(List(Type.Name("+_")), Type.Name("Int")))(
      dialects.Scala213Source3
    )
    assertTpe("Option[- _]") {
      Apply(Type.Name("Option"), ArgClause(List(Type.Name("-_"))))
    }(dialects.Scala213Source3)
  }

  test("[scala213] (x: Int, y)") {
    val err = intercept[ParseException] {
      tpe("(x: Int, y)")(dialects.Scala213)
    }
    assertNoDiff(err.shortMessage, "can't mix function type and dependent function type syntaxes")
  }

  test("[scala213] (x: Int, y: Int)(z: String)") {
    val err = intercept[ParseException] {
      tpe("(x: Int, y: Int)(z: String)")(dialects.Scala213)
    }
    assertNoDiff(err.shortMessage, "dependent function types are not supported")
  }

  test("[scala3] (x: Int, y: Int)(z: String)") {
    val err = intercept[ParseException] {
      tpe("(x: Int, y: Int)(z: String)")(dialects.Scala3)
    }
    assertNoDiff(err.shortMessage, "can't have multiple parameter lists in function types")
  }

}
