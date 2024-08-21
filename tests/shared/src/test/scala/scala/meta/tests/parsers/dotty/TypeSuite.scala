package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.parsers.ParseException

import munit.Location

class TypeSuite extends BaseDottySuite {
  import Name.Anonymous
  import Term.Super
  import Term.{Name => TermName}
  import Type.{Name => TypeName, _}

  private def assertTpe(expr: String)(tree: Tree)(implicit dialect: Dialect): Unit =
    assertTree(tpe(expr))(tree)

  test("with-type") {
    runTestAssert[Stat](
      """|type A = AnyRef with
         |  type T>: Null
         |""".stripMargin,
      assertLayout = """|type A = AnyRef {
                        |  type T >: Null
                        |}
                        |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("A"),
      Nil,
      Type.Refine(Some(pname("AnyRef")), List(Decl.Type(Nil, pname("T"), Nil, loBound("Null")))),
      noBounds
    ))
  }

  test("with-type2") {
    runTestAssert[Stat](
      """|type A = AnyRef with Product with
         |  type T>: Null
         |""".stripMargin,
      assertLayout = """|type A = AnyRef with Product {
                        |  type T >: Null
                        |}
                        |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("A"),
      Nil,
      Type.Refine(
        Some(Type.With(pname("AnyRef"), pname("Product"))),
        List(Decl.Type(Nil, pname("T"), Nil, loBound("Null")))
      ),
      noBounds
    ))
  }

  test("with-type3") {
    runTestAssert[Stat](
      """|type A = Product with
         |  type T>: Null
         |  with
         |    type D <: Product
         |""".stripMargin,
      assertLayout = """|type A = Product {
                        |  type T >: Null {
                        |    type D <: Product
                        |  }
                        |}
                        |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("A"),
      Nil,
      Type.Refine(
        Some(pname("Product")),
        List(Decl.Type(
          Nil,
          pname("T"),
          Nil,
          Type.Bounds(
            Some(
              Type
                .Refine(Some(pname("Null")), List(Decl.Type(Nil, pname("D"), Nil, hiBound("Product"))))
            ),
            None
          )
        ))
      ),
      noBounds
    ))
  }

  test("coloneol-type3") {
    runTestAssert[Stat](
      """|type A = Product:
         |  type T>: Null:
         |    type D <: Product
         |""".stripMargin,
      assertLayout = """|type A = Product {
                        |  type T >: Null {
                        |    type D <: Product
                        |  }
                        |}
                        |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("A"),
      Nil,
      Type.Refine(
        Some(pname("Product")),
        List(Decl.Type(
          Nil,
          pname("T"),
          Nil,
          Type.Bounds(
            Some(
              Type
                .Refine(Some(pname("Null")), List(Decl.Type(Nil, pname("D"), Nil, hiBound("Product"))))
            ),
            None
          )
        ))
      ),
      noBounds
    ))
  }

  test("with-type-error") {
    runTestError[Stat](
      """|type A = Product with
         |  type T>: Null
         | with
         |    type D <: Product
         |""".stripMargin,
      """|<input>:3: error: illegal start of definition `with`
         | with
         | ^""".stripMargin
    )
  }

  test("with-indent-error") {

    // latter type should be ignored despite indentation
    runTestAssert[Stat](
      """|type A = Product
         |  type T>: Null
         |""".stripMargin,
      assertLayout = Some("type A = Product")
    )(Defn.Type(Nil, pname("A"), Nil, pname("Product"), Type.Bounds(None, None)))
  }

  test("with-followed-by-brace-indent") {
    runTestAssert[Stat](
      """|type AA = String with Int with
         |    type T>: Null
         |      {
         |        type T>: Int
         |      }
         |""".stripMargin,
      assertLayout = """|type AA = String with Int {
                        |  type T >: Null {
                        |    type T >: Int
                        |  }
                        |}
                        |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("AA"),
      Nil,
      Type.Refine(
        Some(Type.With(pname("String"), pname("Int"))),
        Decl.Type(
          Nil,
          pname("T"),
          Nil,
          loBound(Type.Refine(
            Some(pname("Null")),
            Decl.Type(Nil, pname("T"), Nil, loBound(pname("Int"))) :: Nil
          ))
        ) :: Nil
      ),
      noBounds
    ))
  }

  test("coloneol-followed-by-brace-indent") {
    runTestAssert[Stat](
      """|type AA = String with Int:
         |    type T>: Null:
         |        type T>: Int
         |""".stripMargin,
      assertLayout = """|type AA = String with Int {
                        |  type T >: Null {
                        |    type T >: Int
                        |  }
                        |}
                        |""".stripMargin
    )(Defn.Type(
      Nil,
      pname("AA"),
      Nil,
      Type.Refine(
        Some(Type.With(pname("String"), pname("Int"))),
        List(Decl.Type(
          Nil,
          pname("T"),
          Nil,
          Type.Bounds(
            Some(Type.Refine(Some(pname("Null")), List(Decl.Type(Nil, pname("T"), Nil, loBound("Int"))))),
            None
          )
        ))
      ),
      noBounds
    ))
  }

  test("with-followed-by-brace") {
    runTestAssert[Stat](
      """|{
         |  type AA = String with Int with
         |    type T>: Null
         |  {
         |    type T>: Int
         |  }
         |}
         |""".stripMargin,
      Some(
        """|{
           |  type AA = String with Int {
           |    type T >: Null
           |  }
           |  {
           |    type T >: Int
           |  }
           |}
           |""".stripMargin
      )
    )(Term.Block(List(
      Defn.Type(
        Nil,
        pname("AA"),
        Nil,
        Type.Refine(
          Some(Type.With(pname("String"), pname("Int"))),
          Decl.Type(Nil, pname("T"), Nil, loBound("Null")) :: Nil
        ),
        noBounds
      ),
      Term.Block(Decl.Type(Nil, pname("T"), Nil, loBound("Int")) :: Nil)
    )))
  }

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
      Type.ApplyInfix(pname("A"), pname("+"), Type.ApplyInfix(pname("B"), pname("*"), pname("C")))
    }
  }

  test("A * B + C / D") {
    assertTpe("A * B + C / D") {
      Type.ApplyInfix(
        Type.ApplyInfix(pname("A"), pname("*"), pname("B")),
        pname("+"),
        Type.ApplyInfix(pname("C"), pname("/"), pname("D"))
      )
    }
  }

  test("f.T")(assertTpe("f.T")(Select(TermName("f"), TypeName("T"))))

  test("f.type")(assertTpe("f.type")(Singleton(TermName("f"))))

  test("super.T")(assertTpe("super.T")(Select(Super(Anonymous(), Anonymous()), TypeName("T"))))

  test("this.T")(assertTpe("this.T")(Select(Term.This(Anonymous()), TypeName("T"))))

  test("(A, B)")(assertTpe("(A, B)")(Tuple(TypeName("A") :: TypeName("B") :: Nil)))

  test("(A, B) => C") {
    assertTpe("(A, B) => C")(Function(TypeName("A") :: TypeName("B") :: Nil, TypeName("C")))
  }

  test("T @foo") {
    assertTpe("T @foo")(
      Annotate(TypeName("T"), Mod.Annot(Init(pname("foo"), anon, emptyArgClause)) :: Nil)
    )
  }

  test("A with B")(assertTpe("A with B")(With(TypeName("A"), TypeName("B"))))

  test("A & B is not a special type") {
    assertTpe("A & B")(ApplyInfix(TypeName("A"), TypeName("&"), TypeName("B")))
  }

  test("A with B {}") {
    assertTpe("A with B {}")(Refine(Some(With(TypeName("A"), TypeName("B"))), Nil))
  }

  test("{}")(assertTpe("{}")(Refine(None, Nil)))

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
      Apply(TypeName("F"), Wildcard(Bounds(Some(TypeName("lo")), Some(TypeName("hi")))) :: Nil)
    assertTpe("F[_ >: lo <: hi]")(expected)
    assertTpe("F[? >: lo <: hi]")(expected)
  }

  test("F[_ >: lo") {
    implicit val dialect: Dialect = dialects.Scala31
    val expected = Apply(TypeName("F"), Wildcard(Bounds(Some(TypeName("lo")), None)) :: Nil)
    assertTpe("F[_ >: lo]")(expected)
    assertTpe("F[? >: lo]")(expected)
  }

  test("F[_ <: hi]") {
    implicit val dialect: Dialect = dialects.Scala31
    val expected = Apply(TypeName("F"), Wildcard(Bounds(None, Some(TypeName("hi")))) :: Nil)
    assertTpe("F[_ <: hi]")(expected)
    assertTpe("F[? <: hi]")(expected)
  }

  test("F[?]") {
    implicit val dialect: Dialect = dialects.Scala31
    val expected = Apply(TypeName("F"), List(Wildcard(Bounds(None, None))))
    assertTpe("F[?]")(expected)
    assertTpe("F[_]")(expected)
  }

  test("F[_]") {
    implicit val dialect: Dialect = dialects.Scala3Future
    assertTpe("F[_]")(AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(None)))))
    assertTpe("F[+_]") {
      AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(Some(Mod.Covariant())))))
    }
    assertTpe("F[-_]") {
      AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(Some(Mod.Contravariant())))))
    }
    runTestError[Stat](
      "F[`+`_]",
      """|<input>:1: error: `]` expected but `_` found
         |F[`+`_]
         |     ^""".stripMargin
    )
    runTestError[Stat](
      "F[`-`_]",
      """|<input>:1: error: `]` expected but `_` found
         |F[`-`_]
         |     ^""".stripMargin
    )
  }

  test("F[*]") {
    // will be deprecated in later versions
    implicit val dialect: Dialect = dialects.Scala31
    assertTpe("F[*]")(AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(None)))))
    assertTpe("F[+*]") {
      AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(Some(Mod.Covariant())))))
    }
    assertTpe("F[-*]") {
      AnonymousLambda(Apply(TypeName("F"), List(AnonymousParam(Some(Mod.Contravariant())))))
    }
  }

  test("F[`*`]") {
    // will be deprecated in later versions
    implicit val dialect: Dialect = dialects.Scala31
    runTestAssert[Type]("F[`*`]")(Apply(pname("F"), List(pname("*"))))
    runTestAssert[Type]("F[`+*`]")(Apply(pname("F"), List(pname("+*"))))
    runTestAssert[Type]("F[`-*`]")(Apply(pname("F"), List(pname("-*"))))
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
      implicit val dialect: Dialect = dialects.Scala211
      tpe("42")
    }

    assertTpe("42")(int(42))
    assertTpe("-42")(int(-42))
    assertTpe("42L")(Lit.Long(42L))
    matchSubStructure[Type]("42.0f", { case Lit(42.0f) => () })
    matchSubStructure[Type]("-42.0f", { case Lit(-42.0f) => () })
    matchSubStructure[Type]("42.0d", { case Lit(42.0d) => () })
    matchSubStructure[Type]("-42.0d", { case Lit(-42.0d) => () })
    assertTpe("\"42\"")(str("42"))
    assertTpe("true")(bool(true))
    assertTpe("false")(bool(false))

    val exceptionScala3 = intercept[ParseException](tpe("() => ()"))
    assertNoDiff(exceptionScala3.shortMessage, "illegal literal type (), use Unit instead")

    val exceptionScala2 = intercept[ParseException] {
      implicit val dialect: Dialect = dialects.Scala213
      tpe("() => ()")
    }
    assertNoDiff(exceptionScala2.shortMessage, "illegal literal type (), use Unit instead")

  }

  test("plus-minus-then-underscore-source3") {
    implicit val dialect: Dialect = dialects.Scala213Source3
    implicit val parser: String => Type = parseType
    matchSubStructure(
      "+_ => Int",
      { case Type.Function(List(Type.Name("+_")), Type.Name("Int")) => () }
    )
    assertTpe("Option[- _]")(Apply(pname("Option"), ArgClause(List(pname("-_")))))
  }

  test("[scala213] (x: Int, y)") {
    implicit val dialect: Dialect = dialects.Scala213
    val err = intercept[ParseException](tpe("(x: Int, y)"))
    assertNoDiff(err.shortMessage, "can't mix function type and dependent function type syntaxes")
  }

  test("[scala213] (x: Int, y: Int)(z: String)") {
    implicit val dialect: Dialect = dialects.Scala213
    val err = intercept[ParseException](tpe("(x: Int, y: Int)(z: String)"))
    assertNoDiff(err.shortMessage, "dependent function types are not supported")
  }

  test("[scala3] (x: Int, y: Int)(z: String)") {
    implicit val dialect: Dialect = dialects.Scala3
    val err = intercept[ParseException](tpe("(x: Int, y: Int)(z: String)"))
    assertNoDiff(err.shortMessage, "can't have multiple parameter lists in function types")
  }

  test("#3162 [scala30] higher-kinded is not wildcard 1") {
    implicit val dialect: Dialect = dialects.Scala30
    runTestAssert[Stat]("def foo[A <: C[_]] = bar.baz[_, F[_]]")(Defn.Def(
      Nil,
      tname("foo"),
      pparam(
        Nil,
        "A",
        hiBound(Type.AnonymousLambda(Type.Apply(pname("C"), List(Type.AnonymousParam(None)))))
      ) :: Nil,
      Nil,
      None,
      Term.ApplyType(
        Term.Select(tname("bar"), tname("baz")),
        List(
          Type.Wildcard(noBounds),
          Type.AnonymousLambda(Type.Apply(pname("F"), List(Type.AnonymousParam(None))))
        )
      )
    ))
  }

  test("#3162 [scala3+] higher-kinded is not wildcard 1") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowUnderscoreAsTypePlaceholder(true)
    runTestAssert[Stat]("def foo[A <: C[_]] = bar.baz[_, F[_]]")(Defn.Def(
      Nil,
      tname("foo"),
      pparam(
        Nil,
        "A",
        hiBound(Type.AnonymousLambda(Type.Apply(pname("C"), List(Type.AnonymousParam(None)))))
      ) :: Nil,
      Nil,
      None,
      Term.ApplyType(
        Term.Select(tname("bar"), tname("baz")),
        List(
          Type.AnonymousParam(None),
          Type.AnonymousLambda(Type.Apply(pname("F"), List(Type.AnonymousParam(None))))
        )
      )
    ))
  }

  test("#3162 [scala30] higher-kinded is not wildcard 2") {
    implicit val dialect: Dialect = dialects.Scala30
    runTestAssert[Stat]("gr.pure[Resource[F, _]]")(Term.ApplyType(
      Term.Select(tname("gr"), tname("pure")),
      Type.AnonymousLambda(
        Type.Apply(pname("Resource"), List(pname("F"), Type.AnonymousParam(None)))
      ) :: Nil
    ))
  }

  test("#3162 [scala3+] higher-kinded is not wildcard 2") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowUnderscoreAsTypePlaceholder(true)
    runTestAssert[Stat]("gr.pure[Resource[F, _]]")(Term.ApplyType(
      Term.Select(tname("gr"), tname("pure")),
      Type.AnonymousLambda(
        Type.Apply(pname("Resource"), List(pname("F"), Type.AnonymousParam(None)))
      ) :: Nil
    ))
  }

  test("star-dot") {

    runTestAssert[Stat](
      """|
         |given Conversion[*.type, List[*.type]] with
         |  def apply(ast: *.type) = ast :: Nil
         |""".stripMargin,
      Some("given Conversion[*.type, List[*.type]] with { def apply(ast: *.type) = ast :: Nil }")
    )(Defn.Given(
      Nil,
      anon,
      None,
      tpl(
        Init(
          Type.Apply(
            pname("Conversion"),
            List(Type.Singleton(tname("*")), Type.Apply(pname("List"), List(Type.Singleton(tname("*")))))
          ),
          anon,
          emptyArgClause
        ) :: Nil,
        List(Defn.Def(
          Nil,
          tname("apply"),
          Nil,
          List(List(tparam("ast", Type.Singleton(tname("*"))))),
          None,
          Term.ApplyInfix(tname("ast"), tname("::"), Nil, List(tname("Nil")))
        ))
      )
    ))
  }

  test("#3672 [scala3] ***")(runTestAssert[Type]("***")(pname("***")))

}
