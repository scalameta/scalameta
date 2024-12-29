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
          bounds(lo =
            Type
              .Refine(Some(pname("Null")), List(Decl.Type(Nil, pname("D"), Nil, hiBound("Product"))))
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
          bounds(lo =
            Type
              .Refine(Some(pname("Null")), List(Decl.Type(Nil, pname("D"), Nil, hiBound("Product"))))
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
    )(Defn.Type(Nil, pname("A"), Nil, pname("Product"), noBounds))
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
          bounds(lo =
            Type.Refine(Some(pname("Null")), List(Decl.Type(Nil, pname("T"), Nil, loBound("Int"))))
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
    )(blk(
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
      blk(Decl.Type(Nil, pname("T"), Nil, loBound("Int")))
    ))
  }

  test("T")(assertTpe("T")(TypeName("T")))

  test("F[T]")(assertTpe("F[T]")(Apply(TypeName("F"), ArgClause(TypeName("T") :: Nil))))

  test("F#T")(assertTpe("F#T")(Project(TypeName("F"), TypeName("T"))))

  test("A \\/ B") {
    assertTpe("A \\/ B")(ApplyInfix(TypeName("A"), TypeName("\\/"), TypeName("B")))
  }

  test("A * B")(assertTpe("A * B")(ApplyInfix(TypeName("A"), TypeName("*"), TypeName("B"))))

  test("A * B + C") {
    assertTpe("A * B + C")(pinfix(pinfix("A", "*", pname("B")), "+", pname("C")))
  }

  test("A + B * C")(assertTpe("A + B * C")(pinfix("A", "+", pinfix("B", "*", pname("C")))))

  test("A * B + C / D") {
    assertTpe("A * B + C / D") {
      pinfix(pinfix("A", "*", pname("B")), "+", pinfix("C", "/", pname("D")))
    }
  }

  test("f.T")(assertTpe("f.T")(pselect("f", "T")))

  test("f.type")(assertTpe("f.type")(Singleton(TermName("f"))))

  test("super.T")(assertTpe("super.T")(Select(Super(Anonymous(), Anonymous()), TypeName("T"))))

  test("this.T")(assertTpe("this.T")(pselect(Term.This(Anonymous()), "T")))

  test("(A, B)")(assertTpe("(A, B)")(Tuple(TypeName("A") :: TypeName("B") :: Nil)))

  test("(A, B) => C") {
    assertTpe("(A, B) => C")(Function(TypeName("A") :: TypeName("B") :: Nil, TypeName("C")))
  }

  test("T @foo")(assertTpe("T @foo")(Annotate(TypeName("T"), Mod.Annot(init("foo")) :: Nil)))

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
        Decl.Def(Nil, TermName("x"), Nil, Nil, TypeName("Int")) ::
          Decl.Val(Nil, List(patvar("y")), TypeName("B")) ::
          Decl.Type(Nil, TypeName("C"), Nil, noBounds) :: Nil
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
        Decl.Type(Nil, TypeName("T"), Nil, noBounds) :: Nil
      )
    }
  }

  test("a.T forSome { val a: A }") {
    assertTpe("a.T forSome { val a: A }")(
      Existential(pselect("a", "T"), Decl.Val(Nil, patvar("a") :: Nil, TypeName("A")) :: Nil)
    )
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
    assertTpe("42L")(lit(42L))
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
    assertTpe("Option[- _]")(papply(pname("Option"), pname("-_")))
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
      pparam("A", hiBound(Type.AnonymousLambda(papply("C", Type.AnonymousParam(None))))) :: Nil,
      Nil,
      None,
      tapplytype(
        tselect("bar", "baz"),
        pwildcard,
        Type.AnonymousLambda(papply("F", Type.AnonymousParam(None)))
      )
    ))
  }

  test("#3162 [scala3+] higher-kinded is not wildcard 1") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowUnderscoreAsTypePlaceholder(true)
    runTestAssert[Stat]("def foo[A <: C[_]] = bar.baz[_, F[_]]")(Defn.Def(
      Nil,
      tname("foo"),
      pparam("A", hiBound(Type.AnonymousLambda(papply("C", Type.AnonymousParam(None))))) :: Nil,
      Nil,
      None,
      tapplytype(
        tselect("bar", "baz"),
        Type.AnonymousParam(None),
        Type.AnonymousLambda(papply("F", Type.AnonymousParam(None)))
      )
    ))
  }

  test("#3162 [scala30] higher-kinded is not wildcard 2") {
    implicit val dialect: Dialect = dialects.Scala30
    runTestAssert[Stat]("gr.pure[Resource[F, _]]")(tapplytype(
      tselect("gr", "pure"),
      Type.AnonymousLambda(papply("Resource", "F", Type.AnonymousParam(None)))
    ))
  }

  test("#3162 [scala3+] higher-kinded is not wildcard 2") {
    implicit val dialect: Dialect = dialects.Scala3.withAllowUnderscoreAsTypePlaceholder(true)
    runTestAssert[Stat]("gr.pure[Resource[F, _]]")(tapplytype(
      tselect("gr", "pure"),
      Type.AnonymousLambda(papply("Resource", "F", Type.AnonymousParam(None)))
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
        init(
          papply("Conversion", Type.Singleton(tname("*")), papply("List", Type.Singleton(tname("*"))))
        ) :: Nil,
        List(Defn.Def(
          Nil,
          tname("apply"),
          Nil,
          List(List(tparam("ast", Type.Singleton(tname("*"))))),
          None,
          tinfix(tname("ast"), "::", tname("Nil"))
        ))
      )
    ))
  }

  test("#3672 [scala3] ***")(runTestAssert[Type]("***")(pname("***")))

  // https://dotty.epfl.ch/docs/reference/experimental/into-modifier.html

  test("#3995 into type") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def ++ (elems: into IterableOnce[A]): List[A]"
    val layout = "def ++(elems: into IterableOnce[A]): List[A]"
    val tree = Decl.Def(
      Nil,
      tname("++"),
      Nil,
      List(
        List(tparam("elems", Some(Type.FunctionArg(List(Mod.Into()), papply("IterableOnce", "A")))))
      ),
      papply("List", "A")
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3995 into by-name type") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def ++ (elems: => into IterableOnce[A]): List[A]"
    val layout = "def ++(elems: => (into IterableOnce[A])): List[A]"
    val tree = Decl.Def(
      Nil,
      tname("++"),
      Nil,
      List(List(tparam(
        "elems",
        Some(Type.ByName(Type.FunctionArg(List(Mod.Into()), papply("IterableOnce", "A"))))
      ))),
      papply("List", "A")
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3995 into func type") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def flatMap[B](f: into A => IterableOnce[B]): List[B]"
    val tree = Decl.Def(
      Nil,
      tname("flatMap"),
      List(pparam("B")),
      List(List(tparam(
        "f",
        Some(Type.FunctionArg(List(Mod.Into()), pfunc(pname("A"))(papply("IterableOnce", "B"))))
      ))),
      papply("List", "B")
    )
    runTestAssert[Stat](code)(tree)
  }

  test("#3995 into func arg type") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def flatMap[B](f: A => into IterableOnce[B]): List[B]"
    val layout = "def flatMap[B](f: A => (into IterableOnce[B])): List[B]"
    val tree = Decl.Def(
      Nil,
      tname("flatMap"),
      List(pparam("B")),
      List(List(
        tparam("f", Some(pfunc("A")(Type.FunctionArg(List(Mod.Into()), papply("IterableOnce", "B")))))
      )),
      papply("List", "B")
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3995 into vararg type without parens (wrong)") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def concatAll(xss: into IterableOnce[Char]*): List[Char]"
    val error = """|<input>:1: error: `)` expected but `identifier` found
                   |def concatAll(xss: into IterableOnce[Char]*): List[Char]
                   |                                          ^""".stripMargin
    runTestError[Stat](code, error)
  }

  test("#3995 into vararg type with parens (correct)") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def concatAll(xss: (into IterableOnce[Char])*): List[Char]"
    val layout = "def concatAll(xss: (into IterableOnce[Char])*): List[Char]"
    val tree = Decl.Def(
      Nil,
      "concatAll",
      Nil,
      List(List(tparam(
        "xss",
        Some(Type.Repeated(Type.FunctionArg(List(Mod.Into()), papply("IterableOnce", "Char"))))
      ))),
      papply("List", "Char")
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3998") {
    val code = "val xs1 = construct[Coll = List, Elem = Int](1, 2, 3)"
    val tree = Defn.Val(
      Nil,
      List(patvar("xs1")),
      None,
      tapply(
        tapplytype(
          tname("construct"),
          Type.Assign(pname("Coll"), pname("List")),
          Type.Assign(pname("Elem"), pname("Int"))
        ),
        lit(1),
        lit(2),
        lit(3)
      )
    )
    runTestAssert[Stat](code)(tree)
  }

  // https://dotty.epfl.ch/docs/reference/experimental/cc.html

  test("#3996 capture checking: 1") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "class Logger(fs: FileSystem^)"
    val layout = "class Logger(fs: FileSystem^)"
    val tree = Defn.Class(
      Nil,
      pname("Logger"),
      Nil,
      ctorp(tparam("fs", Type.Capturing("FileSystem", Nil))),
      tplNoBody()
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 capture checking: 2") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "val l: Logger^{fs} = Logger(fs)"
    val layout = "val l: Logger^{fs} = Logger(fs)"
    val tree = Defn
      .Val(Nil, List(patvar("l")), Some(Type.Capturing("Logger", List("fs"))), tapply("Logger", "fs"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 capture checking: 3") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def tail: LazyList[A]^{this}"
    val layout = "def tail: LazyList[A]^{this}"
    val tree = Decl
      .Def(Nil, "tail", Nil, Type.Capturing(papply("LazyList", "A"), List(Term.This(anon))))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3996 capture checking: 3") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code = "def p: Pair[Int ->{ct} String, Logger^{fs}] = Pair(x, y)"
    val layout = "def p: Pair[Int ->{ct} String, Logger^{fs}] = Pair(x, y)"
    val tree = Defn.Def(
      Nil,
      "p",
      Nil,
      Some(papply(
        "Pair",
        Type.Capturing(purefunc("Int")("String"), List("ct")),
        Type.Capturing("Logger", List("fs"))
      )),
      tapply("Pair", "x", "y")
    )

    runTestAssert[Stat](code, layout)(tree)
  }

}
