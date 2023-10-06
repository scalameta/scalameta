package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class InlineSuite extends BaseDottySuite {

  implicit val parseTempl: String => Stat = code => templStat(code)(dialects.Scala3)

  /**
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/metaprogramming/inline.html]]
   */
  test("inline-soft-keyword-pos") {
    // as ident
    runTestAssert[Stat]("def f(inline: String): Unit")(
      Decl.Def(Nil, tname("f"), Nil, List(List(tparam("inline", "String"))), pname("Unit"))
    )

    // as ident
    runTestAssert[Stat]("inline def inline(inline: inline): inline")(
      Decl.Def(
        List(Mod.Inline()),
        tname("inline"),
        Nil,
        List(List(tparam("inline", "inline"))),
        pname("inline")
      )
    )

    // as modifier
    runTestAssert[Stat]("inline def inline(inline param: inline): inline")(
      Decl.Def(
        List(Mod.Inline()),
        tname("inline"),
        Nil,
        List(List(tparamInline("param", "inline"))),
        pname("inline")
      )
    )

    // as ident and modifier
    runTestAssert[Stat]("inline def inline(inline inline: inline): inline")(
      Decl.Def(
        List(Mod.Inline()),
        tname("inline"),
        Nil,
        List(List(tparamInline("inline", "inline"))),
        pname("inline")
      )
    )

    // as ident and modifier
    runTestAssert[Stat]("inline val inline = false")(
      Defn.Val(List(Mod.Inline()), List(Pat.Var(tname("inline"))), None, Lit.Boolean(false))
    )

    // inline for class as ident
    runTestAssert[Stat]("case class A(inline: Int)")(
      Defn.Class(
        List(Mod.Case()),
        Type.Name("A"),
        Nil,
        Ctor.Primary(
          Nil,
          Name(""),
          List(List(Term.Param(Nil, Term.Name("inline"), Some(Type.Name("Int")), None)))
        ),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )
  }

  test("inline-def-object") {
    runTestAssert[Stat](
      "object X { inline def f(inline sc: Str)(inline args: Any*): String = ??? }"
    )(
      Defn.Object(
        Nil,
        tname("X"),
        tpl(
          List(
            Defn.Def(
              List(Mod.Inline()),
              tname("f"),
              Nil,
              List(
                List(tparamInline("sc", "Str")),
                List(
                  Term.Param(
                    List(Mod.Inline()),
                    tname("args"),
                    Some(Type.Repeated(pname("Any"))),
                    None
                  )
                )
              ),
              Some(pname("String")),
              tname("???")
            )
          )
        )
      )
    )
  }
  test("inline-mods-combination") {
    runTestAssert[Stat](
      "object X { inline override protected def f(): Unit = ??? }"
    )(
      Defn.Object(
        Nil,
        tname("X"),
        tpl(
          List(
            Defn.Def(
              List(Mod.Inline(), Mod.Override(), Mod.Protected(Name(""))),
              tname("f"),
              Nil,
              List(List()),
              Some(pname("Unit")),
              tname("???")
            )
          )
        )
      )
    )

    runTestAssert[Stat](
      "object X { final override inline protected def f(): Unit = ??? }"
    )(
      Defn.Object(
        Nil,
        tname("X"),
        tpl(
          List(
            Defn.Def(
              List(Mod.Final(), Mod.Override(), Mod.Inline(), Mod.Protected(Name(""))),
              tname("f"),
              Nil,
              List(List()),
              Some(pname("Unit")),
              tname("???")
            )
          )
        )
      )
    )

    runTestAssert[Stat](
      "case class Y(val inline: String, inline: Int)"
    )(
      Defn.Class(
        List(Mod.Case()),
        Type.Name("Y"),
        Nil,
        Ctor.Primary(
          Nil,
          Name(""),
          List(
            List(
              Term
                .Param(List(Mod.ValParam()), Term.Name("inline"), Some(Type.Name("String")), None),
              tparam("inline", "Int")
            )
          )
        ),
        tpl(Nil)
      )
    )
  }

  test("inline-soft-ident") {

    runTestAssert[Stat](
      "inline"
    )(
      Term.Name("inline")
    )

    runTestAssert[Stat](
      "`inline`()"
    )(
      Term.Apply(Term.Name("inline"), Nil)
    )

    runTestError[Stat](
      "inline()",
      "`inline` must be followed by an `if` or a `match`"
    )

    runTestError[Stat](
      "object X { inline + 3 }",
      "`inline` must be followed by an `if` or a `match`"
    )

    runTestAssert[Stat](
      "object X { `inline` + 3 }"
    )(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Term.ApplyInfix(Term.Name("inline"), Term.Name("+"), Nil, List(Lit.Int(3))))
        )
      )
    )
  }

  test("inline-match") {
    runTestAssert[Stat](
      """|inline def g: Any = inline x match {
         |  case x: String => (x, x) 
         |  case x: Double => x
         |}""".stripMargin,
      assertLayout = None
    )(
      Defn.Def(
        List(Mod.Inline()),
        Term.Name("g"),
        Nil,
        Nil,
        Some(Type.Name("Any")),
        Term.Match(
          Term.Name("x"),
          List(
            Case(
              Pat.Typed(Pat.Var(Term.Name("x")), Type.Name("String")),
              None,
              Term.Tuple(List(Term.Name("x"), Term.Name("x")))
            ),
            Case(Pat.Typed(Pat.Var(Term.Name("x")), Type.Name("Double")), None, Term.Name("x"))
          ),
          List(Mod.Inline())
        )
      )
    )
  }

  test("inline-match-paren") {
    runTestAssert[Stat](
      """|inline def g = inline (x) match {
         |  case x => x
         |}""".stripMargin,
      assertLayout = None
    )(
      Defn.Def(
        List(Mod.Inline()),
        Term.Name("g"),
        Nil,
        Nil,
        None,
        Term.Match(
          Term.Name("x"),
          List(Case(Pat.Var(Term.Name("x")), None, Term.Name("x"))),
          List(Mod.Inline())
        )
      )
    )
  }

  test("inline-match-brace") {
    runTestAssert[Stat](
      """|inline def g = inline {x} match {
         |  case x => x
         |}""".stripMargin,
      assertLayout = None
    )(
      Defn.Def(
        List(Mod.Inline()),
        Term.Name("g"),
        Nil,
        Nil,
        None,
        Term.Match(
          Term.Block(List(Term.Name("x"))),
          List(Case(Pat.Var(Term.Name("x")), None, Term.Name("x"))),
          List(Mod.Inline())
        )
      )
    )
  }

  test("inline-match-block") {
    runTestAssert[Stat](
      """|inline def g = {
         |  inline x match {
         |    case x => x
         |  }
         |}""".stripMargin,
      assertLayout = None
    )(
      Defn.Def(
        List(Mod.Inline()),
        Term.Name("g"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Term.Match(
              Term.Name("x"),
              List(Case(Pat.Var(Term.Name("x")), None, Term.Name("x"))),
              List(Mod.Inline())
            )
          )
        )
      )
    )
  }

  test("inline-match-block-paren") {
    runTestAssert[Stat](
      """|inline def g = {
         |  inline (x) match {
         |    case x => x
         |  }
         |}""".stripMargin,
      assertLayout = None
    )(
      Defn.Def(
        List(Mod.Inline()),
        Term.Name("g"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Term.Match(
              Term.Name("x"),
              List(Case(Pat.Var(Term.Name("x")), None, Term.Name("x"))),
              List(Mod.Inline())
            )
          )
        )
      )
    )
  }

  test("inline-match-block-brace") {
    runTestAssert[Stat](
      """|inline def g = {
         |  inline {x} match {
         |    case x => x
         |  }
         |}""".stripMargin,
      assertLayout = None
    )(
      Defn.Def(
        List(Mod.Inline()),
        Term.Name("g"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Term.Match(
              Term.Block(List(Term.Name("x"))),
              List(Case(Pat.Var(Term.Name("x")), None, Term.Name("x"))),
              List(Mod.Inline())
            )
          )
        )
      )
    )
  }

  test("inline-match-new") {
    runTestAssert[Stat](
      """|inline def g = {
         |  inline new X match {
         |    case x => x
         |  }
         |}""".stripMargin,
      assertLayout = None
    )(
      Defn.Def(
        List(Mod.Inline()),
        Term.Name("g"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Term.Match(
              Term.New(Init(Type.Name("X"), Name(""), emptyArgClause)),
              List(Case(Pat.Var(Term.Name("x")), None, Term.Name("x"))),
              List(Mod.Inline())
            )
          )
        )
      )
    )
  }

  test("inline-if-method") {
    runTestAssert[Stat](
      """|def fn: Unit =
         |    inline if cond then
         |  truep
         |""".stripMargin,
      assertLayout = Some(
        "def fn: Unit = inline if (cond) truep"
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("fn"),
        Nil,
        Nil,
        Some(Type.Name("Unit")),
        Term.If(Term.Name("cond"), Term.Name("truep"), Lit.Unit(), List(Mod.Inline()))
      )
    )

  }

  test("inline-if-method-oneline") {

    runTestAssert[Stat](
      """|def fn: Unit = inline if cond then truep
         |""".stripMargin,
      assertLayout = Some(
        "def fn: Unit = inline if (cond) truep"
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("fn"),
        Nil,
        Nil,
        Some(Type.Name("Unit")),
        Term.If(Term.Name("cond"), Term.Name("truep"), Lit.Unit(), List(Mod.Inline()))
      )
    )

  }

  test("transparent-inline") {
    runTestAssert[Stat](
      """|transparent inline def choose(b: Boolean): A =
         |   if b then new A else new B
         |""".stripMargin,
      assertLayout = Some(
        "transparent inline def choose(b: Boolean): A = if (b) new A else new B"
      )
    )(
      Defn.Def(
        List(Mod.Transparent(), Mod.Inline()),
        Term.Name("choose"),
        Nil,
        List(List(Term.Param(Nil, Term.Name("b"), Some(Type.Name("Boolean")), None))),
        Some(Type.Name("A")),
        Term.If(
          Term.Name("b"),
          Term.New(Init(Type.Name("A"), Name(""), emptyArgClause)),
          Term.New(Init(Type.Name("B"), Name(""), emptyArgClause)),
          Nil
        )
      )
    )
  }

  test("transparent-trait") {
    runTestAssert[Stat](
      "transparent trait S"
    )(
      Defn.Trait(
        List(Mod.Transparent()),
        Type.Name("S"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
      )
    )
  }

  test("transparent-trait-newlines") {
    runTestAssert[Stat](
      """|transparent 
         |
         |trait S""".stripMargin,
      assertLayout = Some("transparent trait S")
    )(
      Defn.Trait(
        List(Mod.Transparent()),
        Type.Name("S"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
      )
    )
  }

  test("transparent-inline-with-constant") {
    runTestAssert[Stat](
      """|transparent inline def f: String =
         |  inline 10 match
         |    case _ =>
         |      inline "foo" match
         |        case x : String => x
         |""".stripMargin,
      assertLayout = Some(
        """|transparent inline def f: String = inline 10 match {
           |  case _ =>
           |    inline "foo" match {
           |      case x: String => x
           |    }
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        List(Mod.Transparent(), Mod.Inline()),
        Term.Name("f"),
        Nil,
        Nil,
        Some(Type.Name("String")),
        Term.Match(
          Lit.Int(10),
          List(
            Case(
              Pat.Wildcard(),
              None,
              Term.Match(
                Lit.String("foo"),
                List(
                  Case(
                    Pat.Typed(Pat.Var(Term.Name("x")), Type.Name("String")),
                    None,
                    Term.Name("x")
                  )
                ),
                List(Mod.Inline())
              )
            )
          ),
          List(Mod.Inline())
        )
      )
    )
  }

  test("transparent-inline-with-this") {
    runTestAssert[Stat](
      """|transparent inline def nat =
         |  inline this match
         |    case Zero    => ()
         |    case Succ(p) => ()
         |      
         |""".stripMargin,
      assertLayout = Some(
        """|transparent inline def nat = inline this match {
           |  case Zero => ()
           |  case Succ(p) => ()
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        List(Mod.Transparent(), Mod.Inline()),
        Term.Name("nat"),
        Nil,
        Nil,
        None,
        Term.Match(
          Term.This(Name("")),
          List(
            Case(Term.Name("Zero"), None, Lit.Unit()),
            Case(Pat.Extract(Term.Name("Succ"), List(Pat.Var(Term.Name("p")))), None, Lit.Unit())
          ),
          List(Mod.Inline())
        )
      )
    )
  }

  test("transparent-inline-with-this") {
    runTestAssert[Stat](
      """|"static meta" - {
         |   implicit inline def qm = ???
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|"static meta" - {
           |  implicit inline def qm = ???
           |}
           |""".stripMargin
      )
    )(
      Term.ApplyInfix(
        Lit.String("static meta"),
        Term.Name("-"),
        Nil,
        List(
          Term.Block(
            List(
              Defn.Def(
                List(Mod.Implicit(), Mod.Inline()),
                Term.Name("qm"),
                Nil,
                Nil,
                None,
                Term.Name("???")
              )
            )
          )
        )
      )
    )
  }
}
