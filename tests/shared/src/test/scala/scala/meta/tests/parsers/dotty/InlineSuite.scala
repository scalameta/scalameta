package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class InlineSuite extends BaseDottySuite {

  implicit val parseTempl: String => Stat = code => templStat(code)(dialects.Dotty)

  /**
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/metaprogramming/inline.html
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
    // inline is ident declared as val inline = 4
    runTestAssert[Stat](
      "object X { inline + 3 }"
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

  test("inline-if") {
    runTestAssert[Stat](
      "inline if true then a()",
      assertLayout = Some("inline if (true) a()")
    )(
      Term.If(Lit.Boolean(true), Term.Apply(Term.Name("a"), Nil), Lit.Unit(), List(Mod.Inline()))
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
}
