package scala.meta.tests
package parsers

import org.scalameta.invariants.InvariantFailedException

import scala.meta._, Pat._
import scala.meta.dialects.Scala211

class PatSuite extends ParseSuite {

  private def assertPat(expr: String)(tree: Tree): Unit = {
    assertTree(pat(expr))(tree)
  }

  private def assertPatTyp(expr: String)(tree: Tree): Unit = {
    assertTree(patternTyp(expr))(tree)
  }

  implicit def caseParser(code: String, dialect: Dialect): Case = super.parseCase(code)(dialect)
  test("_") {
    assertPat("_")(Wildcard())
  }

  test("a @ _") {
    assertPat("a @ _")(Bind(Var(Term.Name("a")), Wildcard()))
  }

  test("a") {
    assertPat("a")(Var(Term.Name("a")))
  }

  test("`a`") {
    assertPat("`a`")(Term.Name("a"))
  }

  test("a: _") {
    val err = intercept[InvariantFailedException](pat("a: _")).getMessage
    assert(err.contains("found that rhs match {"), err)
    assert(err.contains("} is false"), err)
  }

  test("a: Int") {
    assertPat("a: Int")(Typed(Var(Term.Name("a")), Type.Name("Int")))
  }

  test("_: Int") {
    assertPat("_: Int")(Typed(Wildcard(), Type.Name("Int")))
  }

  test("_: t") {
    assertPat("_: t")(Typed(Wildcard(), Type.Name("t")))
  }

  test("_: F[t]") {
    assertPat("_: F[t]") {
      Typed(Wildcard(), Type.Apply(Type.Name("F"), Type.ArgClause(List(Type.Var(Type.Name("t"))))))
    }
  }

  test("_: F[_]") {
    assertPat("_: F[_]") {
      Typed(
        Wildcard(),
        Type.Apply(Type.Name("F"), List(Type.Wildcard(Type.Bounds(None, None))))
      )
    }
  }

  test("patTyp: t Map u") {
    assertPatTyp("t Map u") {
      Type.ApplyInfix(Type.Name("t"), Type.Name("Map"), Type.Name("u"))
    }
  }

  test("patTyp: t & u | v") {
    assertPatTyp("t & u | v") {
      Type.ApplyInfix(
        Type.ApplyInfix(Type.Name("t"), Type.Name("&"), Type.Name("u")),
        Type.Name("|"),
        Type.Name("v")
      )
    }
  }

  test("patTyp: t * u + v") {
    assertPatTyp("t * u + v") {
      Type.ApplyInfix(
        Type.ApplyInfix(Type.Name("t"), Type.Name("*"), Type.Name("u")),
        Type.Name("+"),
        Type.Name("v")
      )
    }
  }

  test("patTyp: t * u + v / w") {
    assertPatTyp("t * u + v / w") {
      Type.ApplyInfix(
        Type.ApplyInfix(
          Type.ApplyInfix(Type.Name("t"), Type.Name("*"), Type.Name("u")),
          Type.Name("+"),
          Type.Name("v")
        ),
        Type.Name("/"),
        Type.Name("w")
      )
    }
  }

  test("patTyp: t + u * v") {
    assertPatTyp("t + u * v") {
      Type.ApplyInfix(
        Type.ApplyInfix(Type.Name("t"), Type.Name("+"), Type.Name("u")),
        Type.Name("*"),
        Type.Name("v")
      )
    }
  }

  test("pat: F[t & u | v]()") {
    assertPat("F[t & u | v]()") {
      Pat.Extract(
        Term.ApplyType(
          Term.Name("F"),
          List(
            Type.ApplyInfix(
              Type.ApplyInfix(Type.Name("t"), Type.Name("&"), Type.Name("u")),
              Type.Name("|"),
              Type.Name("v")
            )
          )
        ),
        Nil
      )
    }
  }

  test("_: (t Map u)") {
    assertPat("_: (t Map u)")(
      Typed(Wildcard(), Type.ApplyInfix(Type.Name("t"), Type.Name("Map"), Type.Name("u")))
    )
  }

  test("_: T Map U") {
    intercept[ParseException] { pat("_: T Map U") }
  }

  test("_: T forSome { type U }") {
    intercept[ParseException] { pat("_: T forSome { type U }") }
  }

  test("x@(__ : Y)") {
    assertPat("x@(__ : Y)")(
      Pat.Bind(Pat.Var(Term.Name("x")), Pat.Typed(Pat.Var(Term.Name("__")), Type.Name("Y")))
    )
  }

  test("foo(x)") {
    assertPat("foo(x)")(
      Extract(Term.Name("foo"), Var(Term.Name("x")) :: Nil)
    )
  }

  test("foo(_*)") {
    assertPat("foo(_*)")(
      Extract(Term.Name("foo"), SeqWildcard() :: Nil)
    )
  }

  test("foo(x @ _*)") {
    assertPat("foo(x @ _*)")(
      Extract(Term.Name("foo"), Bind(Var(Term.Name("x")), SeqWildcard()) :: Nil)
    )
  }

  test("a :: b") {
    assertPat("a :: b")(
      ExtractInfix(Var(Term.Name("a")), Term.Name("::"), Var(Term.Name("b")) :: Nil)
    )
  }

  test("a :: ()") {
    assertPat("a :: ()")(
      ExtractInfix(Var(Term.Name("a")), Term.Name("::"), Nil)
    )
  }

  test("1 | 2 | 3") {
    assertPat("1 | 2 | 3")(
      Alternative(Lit.Int(1), Alternative(Lit.Int(2), Lit.Int(3)))
    )
  }

  test("()") {
    assertPat("()")(
      Lit.Unit()
    )
  }

  test("(true, false)") {
    assertPat("(true, false)")(
      Tuple(Lit.Boolean(true) :: Lit.Boolean(false) :: Nil)
    )
  }

  test("foo\"bar\"") {
    assertPat("foo\"bar\"")(
      Interpolate(Term.Name("foo"), Lit.String("bar") :: Nil, Nil)
    )
  }

  test("foo\"a $b c\"") {
    assertPat("foo\"a $b c\"")(
      Interpolate(
        Term.Name("foo"),
        Lit.String("a ") :: Lit.String(" c") :: Nil,
        Var(Term.Name("b")) :: Nil
      )
    )
  }

  test("foo\"${b @ foo()}\"") {
    assertPat("foo\"${b @ foo()}\"")(
      Interpolate(
        Term.Name("foo"),
        Lit.String("") :: Lit.String("") :: Nil,
        Bind(Var(Term.Name("b")), Extract(Term.Name("foo"), Nil)) :: Nil
      )
    )
  }

  test("$_") {
    assertPat(""" q"x + $_" """)(
      Pat.Interpolate(
        Term.Name("q"),
        List(Lit.String("x + "), Lit.String("")),
        List(Pat.Wildcard())
      )
    )
  }

  test("#501") {
    intercept[ParseException] { pat("case List(_: BlockExpr, _: MatchExpr, x:_*)  ⇒ false") }
  }

  test("<a>{_*}</a>") {
    val Pat.Xml(List(Lit("<a>"), Lit("</a>")), List(SeqWildcard())) = pat("<a>{_*}</a>")
  }

  test("<a>{ns @ _*}</a>") {
    val Pat.Xml(List(Lit("<a>"), Lit("</a>")), List(Bind(Var(Term.Name("ns")), SeqWildcard()))) =
      pat("<a>{ns @ _*}</a>")
  }

  test("(A, B, C)") {
    assertPat("(A, B, C)") {
      Pat.Tuple(List(Term.Name("A"), Term.Name("B"), Term.Name("C")))
    }
  }

  test("((A, B, C))") {
    assertPat("((A, B, C))") {
      Pat.Tuple(List(Term.Name("A"), Term.Name("B"), Term.Name("C")))
    }
  }

  test("(A, B, C) :: ((A, B, C))") {
    assertPat("(A, B, C) :: ((A, B, C))") {
      Pat.ExtractInfix(
        Pat.Tuple(List(Term.Name("A"), Term.Name("B"), Term.Name("C"))),
        Term.Name("::"),
        List(Pat.Tuple(List(Term.Name("A"), Term.Name("B"), Term.Name("C"))))
      )
    }
  }

  test("((A, B, C)) :: ((A, B, C))") {
    assertPat("((A, B, C)) :: ((A, B, C))") {
      Pat.ExtractInfix(
        Pat.Tuple(List(Term.Name("A"), Term.Name("B"), Term.Name("C"))),
        Term.Name("::"),
        List(Pat.Tuple(List(Term.Name("A"), Term.Name("B"), Term.Name("C"))))
      )
    }
  }

  test("((A, B, C)) :: (A, B, C)") {
    assertPat("((A, B, C)) :: (A, B, C)") {
      Pat.ExtractInfix(
        Pat.Tuple(List(Term.Name("A"), Term.Name("B"), Term.Name("C"))),
        Term.Name("::"),
        List(Term.Name("A"), Term.Name("B"), Term.Name("C"))
      )
    }
  }

  test("case: at top level") {
    val code =
      """|case foo
         |  if true =>
         |  List(bar)
         |""".stripMargin
    checkTree(parseCase(code)) {
      Case(
        Pat.Var(Term.Name("foo")),
        Some(Lit.Boolean(true)),
        Term.Apply(Term.Name("List"), List(Term.Name("bar")))
      )
    }
  }

}
