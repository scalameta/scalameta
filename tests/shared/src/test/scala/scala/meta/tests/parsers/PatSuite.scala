package scala.meta.tests
package parsers

import org.scalameta.invariants.InvariantFailedException
import scala.meta._

class PatSuite extends ParseSuite {
  import Pat._

  implicit val dialect: Dialect = dialects.Scala211

  private def assertPat(expr: String)(tree: Tree)(implicit loc: munit.Location): Unit =
    assertTree(pat(expr))(tree)

  private def assertPatTyp(expr: String)(tree: Tree)(implicit loc: munit.Location): Unit =
    assertTree(patternTyp(expr))(tree)

  implicit def caseParser(code: String, dialect: Dialect): Case = super.parseCase(code)(dialect)

  test("_")(assertPat("_")(patwildcard))

  test("a @ _")(assertPat("a @ _")(Bind(Var(tname("a")), patwildcard)))

  test("a")(assertPat("a")(Var(tname("a"))))

  test("`a`")(assertPat("`a`")(tname("a")))

  test("a: _") {
    val err = intercept[InvariantFailedException](pat("a: _")).getMessage
    assert(err.contains("found that rhs match {"), err)
    assert(err.contains("} is false"), err)
  }

  test("a: Int")(assertPat("a: Int")(Typed(Var(tname("a")), pname("Int"))))

  test("_: Int")(assertPat("_: Int")(Typed(patwildcard, pname("Int"))))

  test("_: t")(assertPat("_: t")(Typed(patwildcard, pname("t"))))

  test("_: F[t]")(assertPat("_: F[t]")(Typed(patwildcard, papply("F", Type.Var(pname("t"))))))

  test("_: F[_]")(assertPat("_: F[_]")(Typed(patwildcard, papply(pname("F"), pwildcard))))

  test("patTyp: t Map u")(assertPatTyp("t Map u")(pinfix("t", "Map", pname("u"))))

  test("patTyp: t & u | v")(
    assertPatTyp("t & u | v")(pinfix(pinfix("t", "&", pname("u")), "|", pname("v")))
  )

  test("patTyp: t * u + v")(
    assertPatTyp("t * u + v")(pinfix(pinfix("t", "*", pname("u")), "+", pname("v")))
  )

  test("patTyp: t * u + v / w")(assertPatTyp("t * u + v / w")(
    pinfix(pinfix(pinfix("t", "*", pname("u")), "+", pname("v")), "/", pname("w"))
  ))

  test("patTyp: t + u * v")(
    assertPatTyp("t + u * v")(pinfix(pinfix("t", "+", pname("u")), "*", pname("v")))
  )

  test("pat: F[t & u | v]()")(assertPat("F[t & u | v]()")(
    Pat.Extract(tapplytype(tname("F"), pinfix(pinfix("t", "&", pname("u")), "|", pname("v"))), Nil)
  ))

  test("_: (t Map u)")(assertPat("_: (t Map u)")(Typed(patwildcard, pinfix("t", "Map", pname("u")))))

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
    val error =
      """|<input>:1: error: infix patterns cannot have type arguments: not expected `[`
         |a ::[T] ()
         |    ^""".stripMargin
    runTestError[Pat]("a ::[T] ()", error)
  }

  test("a :: () [scala211]") {
    implicit val dialect: Dialect = dialects.Scala211
    runTestAssert[Pat]("a :: ()")(patinfix("a", "::"))
  }

  test("a :: () [scala212]") {
    implicit val dialect: Dialect = dialects.Scala212
    runTestAssert[Pat]("a :: ()")(patinfix("a", "::"))
  }

  test("a :: () [scala213]") {
    implicit val dialect: Dialect = dialects.Scala213
    runTestAssert[Pat]("a :: ()", "a :: (())")(patinfix("a", "::", lit()))
  }

  test("a :: () [scala3]") {
    implicit val dialect: Dialect = dialects.Scala3
    runTestAssert[Pat]("a :: ()", "a :: (())")(patinfix("a", "::", lit()))
  }

  test("a :: (()) [scala211]") {
    implicit val dialect: Dialect = dialects.Scala211
    runTestAssert[Pat]("a :: (())")(patinfix("a", "::", lit()))
  }

  test("a :: (()) [scala212]") {
    implicit val dialect: Dialect = dialects.Scala212
    runTestAssert[Pat]("a :: (())")(patinfix("a", "::", lit()))
  }

  test("a :: (()) [scala213]") {
    implicit val dialect: Dialect = dialects.Scala213
    runTestAssert[Pat]("a :: (())")(patinfix("a", "::", lit()))
  }

  test("a :: (()) [scala3]") {
    implicit val dialect: Dialect = dialects.Scala3
    runTestAssert[Pat]("a :: (())")(patinfix("a", "::", lit()))
  }

  test("1 | 2 | 3") {
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

  test("(A, B, C)")(assertPat("(A, B, C)")(Pat.Tuple(List(tname("A"), tname("B"), tname("C")))))

  test("((A, B, C))")(assertPat("((A, B, C))")(Pat.Tuple(List(tname("A"), tname("B"), tname("C")))))

  test("(A, B, C) :: ((A, B, C))")(assertPat("(A, B, C) :: ((A, B, C))")(patinfix(
    Pat.Tuple(List(tname("A"), tname("B"), tname("C"))),
    "::",
    Pat.Tuple(List(tname("A"), tname("B"), tname("C")))
  )))

  test("((A, B, C)) :: ((A, B, C))")(assertPat("((A, B, C)) :: ((A, B, C))")(patinfix(
    Pat.Tuple(List(tname("A"), tname("B"), tname("C"))),
    "::",
    Pat.Tuple(List(tname("A"), tname("B"), tname("C")))
  )))

  test("((A, B, C)) :: (A, B, C)")(assertPat("((A, B, C)) :: (A, B, C)")(patinfix(
    Pat.Tuple(List(tname("A"), tname("B"), tname("C"))),
    "::",
    tname("A"),
    tname("B"),
    tname("C")
  )))

  test("case: at top level") {
    val code =
      """|case foo
         |  if true =>
         |  List(bar)
         |""".stripMargin
    checkTree(
      parseCase(code)
    )(Case(patvar("foo"), Some(bool(true)), tapply(tname("List"), tname("bar"))))
  }

  test("case: break before `|`") {
    val code =
      """|case 'a'
         |   | 'A' =>
         |""".stripMargin
    val layout = "case 'a' | 'A' =>"
    val tree = Case(Pat.Alternative(lit('a'), lit('A')), None, blk())
    runTestAssert[Case](code, Some(layout))(tree)
  }

}
