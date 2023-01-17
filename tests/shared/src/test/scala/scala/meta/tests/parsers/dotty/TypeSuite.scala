package scala.meta.tests.parsers.dotty

import scala.meta._
import Term.{Super, Name => TermName}
import Type.{Name => TypeName, _}
import Name.Anonymous
import scala.meta.parsers.ParseException
import scala.meta.tests.parsers.ParseSuite

class TypeSuite extends BaseDottySuite {

  private def assertTpe(expr: String)(tree: Tree)(implicit dialect: Dialect): Unit = {
    assertTree(tpe(expr))(tree)
  }

  test("with-type") {
    runTestAssert[Stat](
      """|type A = AnyRef with
         |  type T>: Null
         |""".stripMargin,
      assertLayout = Some("type A = AnyRef { type T >: Null }")
    )(
      Defn.Type(
        Nil,
        Type.Name("A"),
        Nil,
        Type.Refine(
          Some(Type.Name("AnyRef")),
          List(Decl.Type(Nil, Type.Name("T"), Nil, Type.Bounds(Some(Type.Name("Null")), None)))
        ),
        Type.Bounds(None, None)
      )
    )
  }

  test("with-type2") {
    runTestAssert[Stat](
      """|type A = AnyRef with Product with
         |  type T>: Null
         |""".stripMargin,
      assertLayout = Some("type A = AnyRef with Product { type T >: Null }")
    )(
      Defn.Type(
        Nil,
        Type.Name("A"),
        Nil,
        Type.Refine(
          Some(Type.With(Type.Name("AnyRef"), Type.Name("Product"))),
          List(Decl.Type(Nil, Type.Name("T"), Nil, Type.Bounds(Some(Type.Name("Null")), None)))
        ),
        Type.Bounds(None, None)
      )
    )
  }

  test("with-type3") {
    runTestAssert[Stat](
      """|type A = Product with
         |  type T>: Null
         |  with
         |    type D <: Product
         |""".stripMargin,
      assertLayout = Some("type A = Product { type T >: Null { type D <: Product } }")
    )(
      Defn.Type(
        Nil,
        Type.Name("A"),
        Nil,
        Type.Refine(
          Some(Type.Name("Product")),
          List(
            Decl.Type(
              Nil,
              Type.Name("T"),
              Nil,
              Type.Bounds(
                Some(
                  Type.Refine(
                    Some(Type.Name("Null")),
                    List(
                      Decl.Type(
                        Nil,
                        Type.Name("D"),
                        Nil,
                        Type.Bounds(None, Some(Type.Name("Product")))
                      )
                    )
                  )
                ),
                None
              )
            )
          )
        ),
        Type.Bounds(None, None)
      )
    )
  }

  test("coloneol-type3") {
    runTestError[Stat](
      """|type A = Product:
         |  type T>: Null:
         |    type D <: Product
         |""".stripMargin,
      """|error: ; expected but : found
         |type A = Product:
         |                ^""".stripMargin
    )
  }

  test("with-type-error") {
    runTestError[Stat](
      """|type A = Product with
         |  type T>: Null
         | with
         |    type D <: Product
         |""".stripMargin,
      "error: ; expected but with found"
    )
  }

  test("with-indent-error") {

    // latter type should be ignored despite indentation
    runTestAssert[Stat](
      """|type A = Product
         |  type T>: Null
         |""".stripMargin,
      assertLayout = Some("type A = Product")
    )(
      Defn.Type(Nil, Type.Name("A"), Nil, Type.Name("Product"), Type.Bounds(None, None))
    )
  }

  test("with-followed-by-brace-indent") {
    runTestAssert[Stat](
      """|type AA = String with Int with
         |    type T>: Null
         |      {
         |        type T>: Int
         |      }
         |""".stripMargin,
      assertLayout = Some("type AA = String with Int { type T >: Null { type T >: Int } }")
    )(
      Defn.Type(
        Nil,
        Type.Name("AA"),
        Nil,
        Type.Refine(
          Some(Type.With(Type.Name("String"), Type.Name("Int"))),
          List(
            Decl.Type(
              Nil,
              Type.Name("T"),
              Nil,
              Type.Bounds(
                Some(
                  Type.Refine(
                    Some(Type.Name("Null")),
                    List(
                      Decl.Type(Nil, Type.Name("T"), Nil, Type.Bounds(Some(Type.Name("Int")), None))
                    )
                  )
                ),
                None
              )
            )
          )
        ),
        Type.Bounds(None, None)
      )
    )
  }

  test("coloneol-followed-by-brace-indent") {
    runTestError[Stat](
      """|type AA = String with Int:
         |    type T>: Null:
         |        type T>: Int
         |""".stripMargin,
      """|error: ; expected but : found
         |type AA = String with Int:
         |                         ^""".stripMargin
    )
  }

  test("with-followed-by-brace") {
    runTestError[Stat](
      """|{
         |  type AA = String with Int with
         |    type T>: Null
         |  {
         |    type T>: Int
         |  }
         |}
         |""".stripMargin,
      "; expected but { found"
    )
  }

  test("T") {
    val TypeName("T") = tpe("T")
  }

  test("F[T]") {
    assertTpe("F[T]") {
      Apply(TypeName("F"), ArgClause(TypeName("T") :: Nil))
    }
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
    implicit val dialect: Dialect = dialects.Scala31
    val expected =
      Apply(
        TypeName("F"),
        Wildcard(Bounds(Some(TypeName("lo")), Some(TypeName("hi")))) :: Nil
      )
    assertTpe("F[_ >: lo <: hi]") { expected }
    assertTpe("F[? >: lo <: hi]") { expected }
  }

  test("F[_ >: lo") {
    implicit val dialect: Dialect = dialects.Scala31
    val expected =
      Apply(TypeName("F"), Wildcard(Bounds(Some(TypeName("lo")), None)) :: Nil)
    assertTpe("F[_ >: lo]") { expected }
    assertTpe("F[? >: lo]") { expected }
  }

  test("F[_ <: hi]") {
    implicit val dialect: Dialect = dialects.Scala31
    val expected =
      Apply(TypeName("F"), Wildcard(Bounds(None, Some(TypeName("hi")))) :: Nil)
    assertTpe("F[_ <: hi]") { expected }
    assertTpe("F[? <: hi]") { expected }
  }

  test("F[?]") {
    implicit val dialect: Dialect = dialects.Scala31
    val expected =
      Apply(TypeName("F"), List(Wildcard(Bounds(None, None))))
    assertTpe("F[?]") { expected }
    assertTpe("F[_]") { expected }
  }

  test("F[_]") {
    implicit val dialect: Dialect = dialects.Scala3Future
    assertTpe("F[_]") {
      AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(None))))
    }
    assertTpe("F[+_]") {
      AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(Some(Mod.Covariant())))))
    }
    assertTpe("F[-_]") {
      AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(Some(Mod.Contravariant())))))
    }
  }

  test("F[*]") {
    // will be deprecated in later versions
    implicit val dialect: Dialect = dialects.Scala31
    assertTpe("F[*]") {
      AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(None))))
    }
    assertTpe("F[+*]") {
      AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(Some(Mod.Covariant())))))
    }
    assertTpe("F[-*]") {
      AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(Some(Mod.Contravariant())))))
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
