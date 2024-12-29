package scala.meta.tests.parsers.dotty

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
    runTestAssert[Stat]("inline def inline(inline: inline): inline")(Decl.Def(
      List(Mod.Inline()),
      tname("inline"),
      Nil,
      List(List(tparam("inline", "inline"))),
      pname("inline")
    ))

    // as modifier
    runTestAssert[Stat]("inline def inline(inline param: inline): inline")(Decl.Def(
      List(Mod.Inline()),
      tname("inline"),
      Nil,
      List(List(tparamInline("param", "inline"))),
      pname("inline")
    ))

    // as ident and modifier
    runTestAssert[Stat]("inline def inline(inline inline: inline): inline")(Decl.Def(
      List(Mod.Inline()),
      tname("inline"),
      Nil,
      List(List(tparamInline("inline", "inline"))),
      pname("inline")
    ))

    // as ident and modifier
    runTestAssert[Stat]("inline val inline = false")(
      Defn.Val(List(Mod.Inline()), List(patvar("inline")), None, bool(false))
    )

    // inline for class as ident
    runTestAssert[Stat]("case class A(inline: Int)")(
      Defn.Class(List(Mod.Case()), pname("A"), Nil, ctorp(tparam("inline", "Int")), tplNoBody())
    )
  }

  test("inline-def-object") {
    runTestAssert[Stat](
      "object X { inline def f(inline sc: Str)(inline args: Any*): String = ??? }"
    )(Defn.Object(
      Nil,
      tname("X"),
      tpl(Defn.Def(
        List(Mod.Inline()),
        tname("f"),
        Nil,
        List(
          List(tparamInline("sc", "Str")),
          List(tparam(List(Mod.Inline()), "args", Type.Repeated(pname("Any"))))
        ),
        Some(pname("String")),
        tname("???")
      ))
    ))
  }
  test("inline-mods-combination") {
    runTestAssert[Stat]("object X { inline override protected def f(): Unit = ??? }")(Defn.Object(
      Nil,
      tname("X"),
      tpl(Defn.Def(
        List(Mod.Inline(), Mod.Override(), Mod.Protected(anon)),
        tname("f"),
        Nil,
        List(List()),
        Some(pname("Unit")),
        tname("???")
      ))
    ))

    runTestAssert[Stat]("object X { final override inline protected def f(): Unit = ??? }")(
      Defn.Object(
        Nil,
        tname("X"),
        tpl(Defn.Def(
          List(Mod.Final(), Mod.Override(), Mod.Inline(), Mod.Protected(anon)),
          tname("f"),
          Nil,
          List(List()),
          Some(pname("Unit")),
          tname("???")
        ))
      )
    )

    runTestAssert[Stat]("case class Y(val inline: String, inline: Int)")(Defn.Class(
      List(Mod.Case()),
      pname("Y"),
      Nil,
      ctorp(tparam(List(Mod.ValParam()), "inline", "String"), tparam("inline", "Int")),
      tplNoBody()
    ))
  }

  test("inline-soft-ident") {

    runTestAssert[Stat]("inline")(tname("inline"))

    runTestAssert[Stat]("`inline`()")(tapply(tname("inline")))

    runTestError[Stat]("inline()", "`inline` must be followed by an `if` or a `match`")

    runTestError[Stat](
      "object X { inline + 3 }",
      "`inline` must be followed by an `if` or a `match`"
    )

    runTestAssert[Stat]("object X { `inline` + 3 }")(
      Defn.Object(Nil, tname("X"), tpl(tinfix(tname("inline"), "+", int(3))))
    )
  }

  test("inline-match") {
    runTestAssert[Stat](
      """|inline def g: Any = inline x match {
         |  case x: String => (x, x) 
         |  case x: Double => x
         |}""".stripMargin,
      assertLayout = None
    )(Defn.Def(
      List(Mod.Inline()),
      tname("g"),
      Nil,
      Nil,
      Some(pname("Any")),
      tmatch(Mod.Inline())(
        tname("x"),
        Case(Pat.Typed(patvar("x"), pname("String")), None, Term.Tuple(List(tname("x"), tname("x")))),
        Case(Pat.Typed(patvar("x"), pname("Double")), None, tname("x"))
      )
    ))
  }

  test("inline-match-paren") {
    runTestAssert[Stat](
      """|inline def g = inline (x) match {
         |  case x => x
         |}""".stripMargin,
      assertLayout = None
    )(Defn.Def(
      List(Mod.Inline()),
      tname("g"),
      Nil,
      Nil,
      None,
      tmatch(Mod.Inline())(tname("x"), Case(patvar("x"), None, tname("x")))
    ))
  }

  test("inline-match-brace") {
    runTestAssert[Stat](
      """|inline def g = inline {x} match {
         |  case x => x
         |}""".stripMargin,
      assertLayout = None
    )(Defn.Def(
      List(Mod.Inline()),
      tname("g"),
      Nil,
      Nil,
      None,
      tmatch(Mod.Inline())(blk(tname("x")), Case(patvar("x"), None, tname("x")))
    ))
  }

  test("inline-match-block") {
    runTestAssert[Stat](
      """|inline def g = {
         |  inline x match {
         |    case x => x
         |  }
         |}""".stripMargin,
      assertLayout = None
    )(Defn.Def(
      List(Mod.Inline()),
      tname("g"),
      Nil,
      Nil,
      None,
      blk(tmatch(Mod.Inline())(tname("x"), Case(patvar("x"), None, tname("x"))))
    ))
  }

  test("inline-match-block-paren") {
    runTestAssert[Stat](
      """|inline def g = {
         |  inline (x) match {
         |    case x => x
         |  }
         |}""".stripMargin,
      assertLayout = None
    )(Defn.Def(
      List(Mod.Inline()),
      tname("g"),
      Nil,
      Nil,
      None,
      blk(tmatch(Mod.Inline())(tname("x"), Case(patvar("x"), None, tname("x"))))
    ))
  }

  test("inline-match-block-brace") {
    runTestAssert[Stat](
      """|inline def g = {
         |  inline {x} match {
         |    case x => x
         |  }
         |}""".stripMargin,
      assertLayout = None
    )(Defn.Def(
      List(Mod.Inline()),
      tname("g"),
      Nil,
      Nil,
      None,
      blk(tmatch(Mod.Inline())(blk(tname("x")), Case(patvar("x"), None, tname("x"))))
    ))
  }

  test("inline-match-new") {
    runTestAssert[Stat](
      """|inline def g = {
         |  inline new X match {
         |    case x => x
         |  }
         |}""".stripMargin,
      assertLayout = None
    )(Defn.Def(
      List(Mod.Inline()),
      tname("g"),
      Nil,
      Nil,
      None,
      blk(tmatch(Mod.Inline())(Term.New(init("X")), Case(patvar("x"), None, tname("x"))))
    ))
  }

  test("inline-if-method") {
    runTestAssert[Stat](
      """|def fn: Unit =
         |    inline if cond then
         |  truep
         |""".stripMargin,
      assertLayout = Some("def fn: Unit = inline if (cond) truep")
    )(Defn.Def(
      Nil,
      tname("fn"),
      Nil,
      Nil,
      Some(pname("Unit")),
      Term.If(tname("cond"), tname("truep"), Lit.Unit(), List(Mod.Inline()))
    ))

  }

  test("inline-if-method-oneline") {

    runTestAssert[Stat](
      """|def fn: Unit = inline if cond then truep
         |""".stripMargin,
      assertLayout = Some("def fn: Unit = inline if (cond) truep")
    )(Defn.Def(
      Nil,
      tname("fn"),
      Nil,
      Nil,
      Some(pname("Unit")),
      Term.If(tname("cond"), tname("truep"), Lit.Unit(), List(Mod.Inline()))
    ))

  }

  test("transparent-inline") {
    runTestAssert[Stat](
      """|transparent inline def choose(b: Boolean): A =
         |   if b then new A else new B
         |""".stripMargin,
      assertLayout = Some("transparent inline def choose(b: Boolean): A = if (b) new A else new B")
    )(Defn.Def(
      List(Mod.Transparent(), Mod.Inline()),
      tname("choose"),
      Nil,
      List(List(tparam("b", "Boolean"))),
      Some(pname("A")),
      Term.If(tname("b"), Term.New(init("A")), Term.New(init("B")), Nil)
    ))
  }

  test("transparent-trait") {
    runTestAssert[Stat]("transparent trait S")(
      Defn.Trait(List(Mod.Transparent()), pname("S"), Nil, ctor, tplNoBody())
    )
  }

  test("transparent-class") {
    runTestAssert[Stat]("transparent class S")(
      Defn.Class(List(Mod.Transparent()), pname("S"), Nil, ctor, tplNoBody())
    )
  }

  test("transparent-trait-newlines") {
    runTestAssert[Stat](
      """|transparent 
         |
         |trait S""".stripMargin,
      assertLayout = Some("transparent trait S")
    )(Defn.Trait(List(Mod.Transparent()), pname("S"), Nil, ctor, tplNoBody()))
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
    )(Defn.Def(
      List(Mod.Transparent(), Mod.Inline()),
      tname("f"),
      Nil,
      Nil,
      Some(pname("String")),
      tmatch(Mod.Inline())(
        int(10),
        Case(
          patwildcard,
          None,
          tmatch(Mod.Inline())(
            str("foo"),
            Case(Pat.Typed(patvar("x"), pname("String")), None, tname("x"))
          )
        )
      )
    ))
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
    )(Defn.Def(
      List(Mod.Transparent(), Mod.Inline()),
      tname("nat"),
      Nil,
      Nil,
      None,
      tmatch(Mod.Inline())(
        Term.This(anon),
        Case(tname("Zero"), None, Lit.Unit()),
        Case(Pat.Extract(tname("Succ"), List(patvar("p"))), None, Lit.Unit())
      )
    ))
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
    )(tinfix(
      str("static meta"),
      "-",
      blk(Defn.Def(List(Mod.Implicit(), Mod.Inline()), tname("qm"), Nil, Nil, None, tname("???")))
    ))
  }
}
