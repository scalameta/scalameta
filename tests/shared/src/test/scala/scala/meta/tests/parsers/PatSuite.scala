package scala.meta.tests
package parsers

import org.scalameta.invariants.InvariantFailedException
import scala.meta._

class PatSuite extends ParseSuite {
  import Pat._

  implicit val dialect: Dialect = dialects.Scala211

  private def assertPat(expr: String)(tree: Tree): Unit = assertTree(pat(expr))(tree)

  private def assertPatTyp(expr: String)(tree: Tree): Unit = assertTree(patternTyp(expr))(tree)

  implicit def caseParser(code: String, dialect: Dialect): Case = super.parseCase(code)(dialect)
  test("_")(assertPat("_")(Wildcard()))

  test("a @ _")(assertPat("a @ _")(Bind(Var(tname("a")), Wildcard())))

  test("a")(assertPat("a")(Var(tname("a"))))

  test("`a`")(assertPat("`a`")(tname("a")))

  test("a: _") {
    val err = intercept[InvariantFailedException](pat("a: _")).getMessage
    assert(err.contains("found that rhs match {"), err)
    assert(err.contains("} is false"), err)
  }

  test("a: Int")(assertPat("a: Int")(Typed(Var(tname("a")), pname("Int"))))

  test("_: Int")(assertPat("_: Int")(Typed(Wildcard(), pname("Int"))))

  test("_: t")(assertPat("_: t")(Typed(Wildcard(), pname("t"))))

  test("_: F[t]") {
    assertPat("_: F[t]") {
      Typed(Wildcard(), Type.Apply(pname("F"), Type.ArgClause(List(Type.Var(pname("t"))))))
    }
  }

  test("_: F[_]") {
    assertPat("_: F[_]") {
      Typed(Wildcard(), Type.Apply(pname("F"), List(Type.Wildcard(Type.Bounds(None, None)))))
    }
  }

  test("patTyp: t Map u") {
    assertPatTyp("t Map u")(Type.ApplyInfix(pname("t"), pname("Map"), pname("u")))
  }

  test("patTyp: t & u | v") {
    assertPatTyp("t & u | v") {
      Type.ApplyInfix(Type.ApplyInfix(pname("t"), pname("&"), pname("u")), pname("|"), pname("v"))
    }
  }

  test("patTyp: t * u + v") {
    assertPatTyp("t * u + v") {
      Type.ApplyInfix(Type.ApplyInfix(pname("t"), pname("*"), pname("u")), pname("+"), pname("v"))
    }
  }

  test("patTyp: t * u + v / w") {
    assertPatTyp("t * u + v / w") {
      Type.ApplyInfix(
        Type
          .ApplyInfix(Type.ApplyInfix(pname("t"), pname("*"), pname("u")), pname("+"), pname("v")),
        pname("/"),
        pname("w")
      )
    }
  }

  test("patTyp: t + u * v") {
    assertPatTyp("t + u * v") {
      Type.ApplyInfix(Type.ApplyInfix(pname("t"), pname("+"), pname("u")), pname("*"), pname("v"))
    }
  }

  test("pat: F[t & u | v]()") {
    assertPat("F[t & u | v]()") {
      Pat.Extract(
        Term.ApplyType(
          tname("F"),
          List(
            Type
              .ApplyInfix(Type.ApplyInfix(pname("t"), pname("&"), pname("u")), pname("|"), pname("v"))
          )
        ),
        Nil
      )
    }
  }

  test("_: (t Map u)") {
    assertPat("_: (t Map u)")(Typed(Wildcard(), Type.ApplyInfix(pname("t"), pname("Map"), pname("u"))))
  }

  test("_: T Map U")(intercept[ParseException](pat("_: T Map U")))

  test("_: T forSome { type U }")(intercept[ParseException](pat("_: T forSome { type U }")))

  test("x@(__ : Y)") {
    assertPat("x@(__ : Y)")(Pat.Bind(Pat.Var(tname("x")), Pat.Typed(Pat.Var(tname("__")), pname("Y"))))
  }

  test("foo(x)")(assertPat("foo(x)")(Extract(tname("foo"), Var(tname("x")) :: Nil)))

  test("foo(_*)")(assertPat("foo(_*)")(Extract(tname("foo"), SeqWildcard() :: Nil)))

  test("foo(x @ _*)") {
    assertPat("foo(x @ _*)")(Extract(tname("foo"), Bind(Var(tname("x")), SeqWildcard()) :: Nil))
  }

  test("a :: b") {
    assertPat("a :: b")(ExtractInfix(Var(tname("a")), tname("::"), Var(tname("b")) :: Nil))
  }

  test("a :: ()") {
    assertPat("a :: ()")(ExtractInfix(Var(tname("a")), tname("::"), Nil))
    val error = """|<input>:1: error: infix patterns cannot have type arguments
                   |a ::[T] ()
                   |    ^""".stripMargin
    runTestError[Pat]("a ::[T] ()", error)
  }

  test("1 | 2 | 3") {
    assertPat("1 | 2 | 3")(Alternative(int(1), Alternative(int(2), int(3))))
    runTestAssert[Pat]("1 `|` 2")(ExtractInfix(lit(1), tname("|"), List(lit(2))))
  }

  test("()")(assertPat("()")(Lit.Unit()))

  test("(true, false)")(assertPat("(true, false)")(Tuple(bool(true) :: bool(false) :: Nil)))

  test("foo\"bar\"")(assertPat("foo\"bar\"")(Interpolate(tname("foo"), str("bar") :: Nil, Nil)))

  test("foo\"a $b c\"") {
    assertPat("foo\"a $b c\"")(
      Interpolate(tname("foo"), str("a ") :: str(" c") :: Nil, Var(tname("b")) :: Nil)
    )
  }

  test("foo\"${b @ foo()}\"") {
    assertPat("foo\"${b @ foo()}\"")(Interpolate(
      tname("foo"),
      str("") :: str("") :: Nil,
      Bind(Var(tname("b")), Extract(tname("foo"), Nil)) :: Nil
    ))
  }

  test("$_") {
    assertPat(""" q"x + $_" """)(
      Pat.Interpolate(tname("q"), List(str("x + "), str("")), List(Pat.Wildcard()))
    )
  }

  test("#501") {
    intercept[ParseException](pat("case List(_: BlockExpr, _: MatchExpr, x:_*)  ⇒ false"))
  }

  test("<a>{_*}</a>") {
    assertPat("<a>{_*}</a>")(Pat.Xml(List(str("<a>"), str("</a>")), List(SeqWildcard())))
  }

  test("<a>{ns @ _*}</a>") {
    assertPat("<a>{ns @ _*}</a>")(
      Pat.Xml(List(str("<a>"), str("</a>")), List(Bind(Var(tname("ns")), SeqWildcard())))
    )
  }

  test("(A, B, C)")(assertPat("(A, B, C)")(Pat.Tuple(List(tname("A"), tname("B"), tname("C")))))

  test("((A, B, C))") {
    assertPat("((A, B, C))")(Pat.Tuple(List(tname("A"), tname("B"), tname("C"))))
  }

  test("(A, B, C) :: ((A, B, C))") {
    assertPat("(A, B, C) :: ((A, B, C))") {
      Pat.ExtractInfix(
        Pat.Tuple(List(tname("A"), tname("B"), tname("C"))),
        tname("::"),
        List(Pat.Tuple(List(tname("A"), tname("B"), tname("C"))))
      )
    }
  }

  test("((A, B, C)) :: ((A, B, C))") {
    assertPat("((A, B, C)) :: ((A, B, C))") {
      Pat.ExtractInfix(
        Pat.Tuple(List(tname("A"), tname("B"), tname("C"))),
        tname("::"),
        List(Pat.Tuple(List(tname("A"), tname("B"), tname("C"))))
      )
    }
  }

  test("((A, B, C)) :: (A, B, C)") {
    assertPat("((A, B, C)) :: (A, B, C)") {
      Pat.ExtractInfix(
        Pat.Tuple(List(tname("A"), tname("B"), tname("C"))),
        tname("::"),
        List(tname("A"), tname("B"), tname("C"))
      )
    }
  }

  test("case: at top level") {
    val code = """|case foo
                  |  if true =>
                  |  List(bar)
                  |""".stripMargin
    checkTree(parseCase(code)) {
      Case(Pat.Var(tname("foo")), Some(bool(true)), Term.Apply(tname("List"), List(tname("bar"))))
    }
  }

  test("case: break before `|`") {
    val code = """|case 'a'
                  |   | 'A' =>
                  |""".stripMargin
    val layout = "case 'a' | 'A' =>"
    val tree = Case(Pat.Alternative(Lit.Char('a'), Lit.Char('A')), None, Term.Block(Nil))
    runTestAssert[Case](code, Some(layout))(tree)
  }

}
