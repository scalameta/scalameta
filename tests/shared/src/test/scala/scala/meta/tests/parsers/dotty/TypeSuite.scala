package scala.meta.tests.parsers.dotty

import scala.meta._
import Term.{Super, Name => TermName}
import Type.{Name => TypeName, _}
import Name.Anonymous
import scala.meta.dialects.Scala3
import scala.meta.parsers.ParseException
import scala.meta.tests.parsers.ParseSuite

class TypeSuite extends ParseSuite {

  private def assertTpe(expr: String)(tree: Tree): Unit = {
    assertEquals(tpe(expr).structure, tree.structure)
  }

  test("T") {
    val TypeName("T") = tpe("T")
  }

  test("F[T]") {
    val Apply(TypeName("F"), TypeName("T") :: Nil) = tpe("F[T]")
  }

  test("F#T") {
    val Project(TypeName("F"), TypeName("T")) = tpe("F#T")
  }

  test("A \\/ B") {
    val ApplyInfix(TypeName("A"), TypeName("\\/"), TypeName("B")) = tpe("A \\/ B")
  }

  test("A * B") {
    val ApplyInfix(TypeName("A"), TypeName("*"), TypeName("B")) = tpe("A * B")
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
        Type.Name("A"),
        Type.Name("+"),
        Type.ApplyInfix(Type.Name("B"), Type.Name("*"), Type.Name("C"))
      )
    }
  }

  test("A * B + C / D") {
    assertTpe("A * B + C / D") {
      Type.ApplyInfix(
        Type.ApplyInfix(Type.Name("A"), Type.Name("*"), Type.Name("B")),
        Type.Name("+"),
        Type.ApplyInfix(Type.Name("C"), Type.Name("/"), Type.Name("D"))
      )
    }
  }

  test("f.T") {
    val Select(TermName("f"), TypeName("T")) = tpe("f.T")
  }

  test("f.type") {
    val Singleton(TermName("f")) = tpe("f.type")
  }

  test("super.T") {
    val Select(Super(Anonymous(), Anonymous()), TypeName("T")) = tpe("super.T")
  }

  test("this.T") {
    val Select(Term.This(Anonymous()), TypeName("T")) = tpe("this.T")
  }

  test("(A, B)") {
    val Tuple(TypeName("A") :: TypeName("B") :: Nil) = tpe("(A, B)")
  }

  test("(A, B) => C") {
    val Function(TypeName("A") :: TypeName("B") :: Nil, TypeName("C")) = tpe("(A, B) => C")
  }

  test("T @foo") {
    val Annotate(TypeName("T"), Mod.Annot(Init(Type.Name("foo"), Name.Anonymous(), Nil)) :: Nil) =
      tpe("T @foo")
  }

  test("A with B") {
    val With(TypeName("A"), TypeName("B")) = tpe("A with B")
  }

  test("A & B is not a special type") {
    val ApplyInfix(TypeName("A"), TypeName("&"), TypeName("B")) = tpe("A & B")
  }

  test("A with B {}") {
    val Refine(Some(With(TypeName("A"), TypeName("B"))), Nil) = tpe("A with B {}")
  }

  test("{}") {
    val Refine(None, Nil) = tpe("{}")
  }

  test("A { def x: A; val y: B; type C }") {
    val Refine(
      Some(TypeName("A")),
      Decl.Def(Nil, TermName("x"), Nil, Nil, TypeName("Int")) ::
        Decl.Val(Nil, List(Pat.Var(TermName("y"))), TypeName("B")) ::
        Decl.Type(Nil, TypeName("C"), Nil, Type.Bounds(None, None)) :: Nil
    ) =
      tpe("A { def x: Int; val y: B; type C }")
  }

  test("F[_ >: lo <: hi]") {
    val Apply(
      TypeName("F"),
      Placeholder(Type.Bounds(Some(TypeName("lo")), Some(TypeName("hi")))) :: Nil
    ) =
      tpe("F[_ >: lo <: hi]")
  }

  test("F[_ >: lo") {
    val Apply(TypeName("F"), Placeholder(Type.Bounds(Some(TypeName("lo")), None)) :: Nil) =
      tpe("F[_ >: lo]")
  }

  test("F[_ <: hi]") {
    val Apply(TypeName("F"), Placeholder(Type.Bounds(None, Some(TypeName("hi")))) :: Nil) =
      tpe("F[_ <: hi]")
  }

  test("F[_]") {
    val Apply(TypeName("F"), Placeholder(Type.Bounds(None, None)) :: Nil) =
      tpe("F[_]")
  }

  test("F[T] forSome { type T }") {
    val Existential(
      Apply(TypeName("F"), TypeName("T") :: Nil),
      Decl.Type(Nil, TypeName("T"), Nil, Type.Bounds(None, None)) :: Nil
    ) =
      tpe("F[T] forSome { type T }")
  }

  test("a.T forSome { val a: A }") {
    val Existential(
      Select(TermName("a"), TypeName("T")),
      Decl.Val(Nil, Pat.Var(TermName("a")) :: Nil, TypeName("A")) :: Nil
    ) =
      tpe("a.T forSome { val a: A }")
  }

  test("A | B is not a special type") {
    val comp @ ApplyInfix(TypeName("A"), TypeName("|"), TypeName("B")) = tpe("A | B")
  }

  test("42.type") {
    intercept[ParseException] {
      tpe("42")(dialects.Scala211)
    }

    val Lit(42) = tpe("42")(dialects.Scala3)
    val Lit(-42) = tpe("-42")(dialects.Scala3)
    val Lit(42L) = tpe("42L")(dialects.Scala3)
    val Lit(42f) = tpe("42f")(dialects.Scala3)
    val Lit(-42f) = tpe("-42f")(dialects.Scala3)
    val Lit(42d) = tpe("42d")(dialects.Scala3)
    val Lit(-42d) = tpe("-42d")(dialects.Scala3)
    val Lit("42") = tpe("\"42\"")(dialects.Scala3)
    val Lit(false) = tpe("false")(dialects.Scala3)
    val Lit(true) = tpe("true")(dialects.Scala3)

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
    val Type.Function(List(Type.Name("+_")), Type.Name("Int")) =
      tpe("+_ => Int")(dialects.Scala213Source3)
    val Type.Apply(Type.Name("Option"), List(Type.Name("-_"))) =
      tpe("Option[- _]")(dialects.Scala213Source3)
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
