package scala.meta.tests.parsers.dotty

import org.scalameta.invariants.InvariantFailedException
import scala.meta._
import scala.meta.tests.parsers.ParseSuite

class PatSuite extends ParseSuite {
  import Pat._

  private def assertPat(expr: String)(
      tree: Tree
  )(implicit dialect: Dialect, loc: munit.Location): Unit = assertTree(pat(expr))(tree)

  private def assertPatTyp(expr: String)(
      tree: Tree
  )(implicit dialect: Dialect, loc: munit.Location): Unit = assertTree(patternTyp(expr))(tree)

  implicit val dialect: Dialect = dialects.Scala3

  test("_")(assertPat("_")(Wildcard()))

  test("a @ _")(assertPat("a @ _")(Bind(Var(tname("a")), Wildcard())))

  test("a")(assertPat("a")(Var(tname("a"))))

  test("`a`")(assertPat("`a`")(tname("a")))

  test("a: Int")(assertPat("a: Int")(Typed(Var(tname("a")), pname("Int"))))

  test("_: Int")(assertPat("_: Int")(Typed(Wildcard(), pname("Int"))))

  test("_: t")(assertPat("_: t")(Typed(Wildcard(), pname("t"))))

  test("_: F[t]")(assertPat("_: F[t]")(Typed(patwildcard, papply("F", Type.Var("t")))))

  test("_: F[_]") {
    implicit val dialect = dialects.Scala3Future
    assertPat("_: F[_]")(Typed(Wildcard(), papply("F", Type.AnonymousParam(None))))
  }

  test("_: F[_]") {
    implicit val dialect = dialects.Scala31
    assertPat("_: F[_]")(Pat.Typed(patwildcard, papply("F", pwildcard)))
  }

  test("_: F[*]") {
    // might be deprecated later
    implicit val dialect: Dialect = dialects.Scala31
    assertPat("_: F[*]")(Typed(Wildcard(), papply("F", Type.AnonymousParam(None))))
  }

  test("patTyp: t Map u")(assertPatTyp("t Map u")(pinfix("t", "Map", pname("u"))))

  test("patTyp: t & u | v")(
    assertPatTyp("t & u | v")(pinfix(pinfix("t", "&", pname("u")), "|", pname("v")))
  )

  test("patTyp: t * u + v")(
    assertPatTyp("t * u + v")(pinfix(pinfix("t", "*", pname("u")), "+", pname("v")))
  )

  test("patTyp: t * u + v / w")(assertPatTyp("t * u + v / w")(
    pinfix(pinfix("t", "*", pname("u")), "+", pinfix("v", "/", pname("w")))
  ))

  test("patTyp: t + u * v")(assertPatTyp("t + u * v")(pinfix("t", "+", pinfix("u", "*", pname("v")))))

  test("pat: F[t & u | v]()")(assertPat("F[t & u | v]()")(
    Pat.Extract(tapplytype(tname("F"), pinfix(pinfix("t", "&", pname("u")), "|", pname("v"))), Nil)
  ))

  test("_: (t Map u)")(assertPat("_: (t Map u)")(Typed(Wildcard(), pinfix("t", "Map", pname("u")))))

  test("_: T Map U")(intercept[ParseException](pat("_: T Map U")))

  test("_: T forSome { type U }")(intercept[ParseException](pat("_: T forSome { type U }")))

  test("x@(__ : Y)")(assertPat("x@(__ : Y)")(Pat.Bind(patvar("x"), Pat.Typed(patvar("__"), pname("Y")))))

  test("foo(x)")(assertPat("foo(x)")(Extract(tname("foo"), Var(tname("x")) :: Nil)))

  test("foo(_*)")(assertPat("foo(_*)")(Extract(tname("foo"), SeqWildcard() :: Nil)))

  test("foo(x @ _*)")(
    assertPat("foo(x @ _*)")(Extract(tname("foo"), Bind(Var(tname("x")), SeqWildcard()) :: Nil))
  )

  test("a :: b")(assertPat("a :: b")(patinfix(Var(tname("a")), "::", Var(tname("b")))))

  test("a ::[T] ()") {
    // the "a :: ()" case is tested in parsers/PatSuite, with other dialects
    val error =
      """|<input>:1: error: infix patterns cannot have type arguments: not expected `[`
         |a ::[T] ()
         |    ^""".stripMargin
    runTestError[Pat]("a ::[T] ()", error)
  }

  test("1 | 2 | 3") {
    assertPat("1 | 2")(Alternative(int(1), int(2)))
    assertPat("1 | 2 | 3")(Alternative(int(1), Alternative(int(2), int(3))))
    runTestAssert[Pat]("1 `|` 2")(patinfix(lit(1), "|", lit(2)))
  }

  test("()")(assertPat("()")(Lit.Unit()))

  test("(true, false)")(assertPat("(true, false)")(Tuple(bool(true) :: bool(false) :: Nil)))

  test("foo\"bar\"")(assertPat("foo\"bar\"")(Interpolate(tname("foo"), str("bar") :: Nil, Nil)))

  test("foo\"a $b c\"")(assertPat("foo\"a $b c\"")(
    Interpolate(tname("foo"), str("a ") :: str(" c") :: Nil, Var(tname("b")) :: Nil)
  ))

  test("foo\"${b @ foo()}\"")(assertPat("foo\"${b @ foo()}\"")(Interpolate(
    tname("foo"),
    str("") :: str("") :: Nil,
    Bind(Var(tname("b")), Extract(tname("foo"), Nil)) :: Nil
  )))

  test("$_")(assertPat(""" q"x + $_" """)(
    Pat.Interpolate(tname("q"), List(str("x + "), str("")), List(patwildcard))
  ))

  test("#501")(intercept[ParseException](pat("case List(_: BlockExpr, _: MatchExpr, x:_*)  ⇒ false")))

  test("<a>{_*}</a>")(
    assertPat("<a>{_*}</a>")(Pat.Xml(List(str("<a>"), str("</a>")), List(SeqWildcard())))
  )

  test("<a>{ns @ _*}</a>")(assertPat("<a>{ns @ _*}</a>")(
    Pat.Xml(List(str("<a>"), str("</a>")), List(Bind(Var(tname("ns")), SeqWildcard())))
  ))

  test("a: _") {
    val err = intercept[InvariantFailedException](pat("a: _")).getMessage
    assert(err.contains("found that rhs match {"), err)
    assert(err.contains("} is false"), err)
  }

  test("a: _ scala31") {
    implicit val dialect = dialects.Scala31
    val err = intercept[InvariantFailedException](pat("a: _")).getMessage
    assert(err.contains("found that rhs match {"), err)
    assert(err.contains("} is false"), err)
  }

  test("single spread") {
    val code = "case Seq(1, xs*) =>"
    val tree = Case(Pat.Extract(tname("Seq"), List(lit(1), Pat.Repeated("xs"))), None, blk())
    runTestAssert[Case](code)(tree)

    val codeWithTrailingcomma =
      """|case Seq(
         |  1,
         |  xs*,
         |) =>
         |""".stripMargin
    runTestAssert[Case](codeWithTrailingcomma, code)(tree)
  }

  test("SIP-70 multiple spreads") {
    val code = "case Seq(1, xs*, 2, 3) =>"
    val tree =
      Case(Pat.Extract(tname("Seq"), List(lit(1), Pat.Repeated("xs"), lit(2), lit(3))), None, blk())
    runTestAssert[Case](code)(tree)
  }

  test("#4428 pat with given") {
    val code =
      """|withFoo { case foo @ given Foo => 
         |  import foo.* // Works fine if I remove this line
         |  x()
         |}
         |""".stripMargin
    val layout =
      """|withFoo {
         |  case foo @ given Foo =>
         |    import foo.* // Works fine if I remove this line
         |    x()
         |}
         |""".stripMargin
    val tree = tapply(
      "withFoo",
      Term.PartialFunction(List(Case(
        Pat.Bind(patvar("foo"), Pat.Given("Foo")),
        None,
        blk(
          Import.createWithComments(
            List(Importer("foo", List(Importee.Wildcard()))),
            endComment = Seq("// Works fine if I remove this line")
          ),
          tapply("x")
        )
      )))
    )
    runTestAssert[Stat](code, layout)(tree)

    val codeInParens =
      """|withFoo { case foo @ (given Foo) => 
         |  import foo.* // Works fine if I remove this line
         |  x()
         |}
         |""".stripMargin
    runTestAssert[Stat](codeInParens, layout)(tree)
  }

}
