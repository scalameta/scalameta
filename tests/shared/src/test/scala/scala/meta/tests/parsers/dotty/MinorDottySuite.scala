package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.Type.Apply
import scala.meta.Type.Placeholder

class MinorDottySuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)
  implicit val parseType: String => Type = code => tpe(code)(dialects.Dotty)
  implicit val parseSource: String => Source = code => source(code)(dialects.Dotty)

  val parseTempl: String => Stat = code => templStat(code)(dialects.Dotty)

  /**
   *
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/other-new-features/open-classes.html
   *  https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html
   *  https://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html
   *  https://dotty.epfl.ch/docs/reference/metaprogramming/inline.html
   *
   */
  test("open-class") {
    val Defn.Class(List(Mod.Open()), Type.Name("A"), _, _, _) =
      templStat("open class A {}")(dialects.Dotty)

    val Defn.Trait(List(Mod.Open()), Type.Name("C"), _, _, _) =
      templStat("open trait C {}")(dialects.Dotty)

    val Defn.Object(List(Mod.Open()), Term.Name("X"), _) =
      templStat("open object X {}")(dialects.Dotty)

  }

  test("open-class-negative-cases") {
    runTestError[Stat]("final open class A {}", "illegal combination of modifiers: open and final")
    runTestError[Stat](
      "open sealed trait C {}",
      "illegal combination of modifiers: open and sealed for"
    )
    runTestError[Stat]("open def f(): Int = 3", "error: expected start of definition")
    runTestError[Stat]("def f(open a: Int): Int = 3", "error")
  }

  test("open-soft-modifier") {
    stat("def open(open: open): open = ???").structure
  }

  test("type-lambda") {
    // cannot carry +/- but can carry bounds >: , <:
    runTestAssert[Type]("[X, Y] =>> Map[Y, X]")(
      Type.Lambda(
        List(pparam("X"), pparam("Y")),
        Type.Apply(pname("Map"), List(pname("Y"), pname("X")))
      )
    )
    runTestAssert[Type]("[X >: L <: U] =>> R")(
      Type.Lambda(
        List(
          Type
            .Param(Nil, pname("X"), Nil, Type.Bounds(Some(pname("L")), Some(pname("U"))), Nil, Nil)
        ),
        pname("R")
      )
    )
    runTestAssert[Type]("[X] =>> (X, X)")(
      Type.Lambda(List(pparam("X")), Type.Tuple(List(pname("X"), pname("X"))))
    )
    runTestAssert[Type]("[X] =>> [Y] =>> (X, Y)")(
      Type.Lambda(
        List(pparam("X")),
        Type.Lambda(List(pparam("Y")), Type.Tuple(List(pname("X"), pname("Y"))))
      )
    )
  }

  test("case-classes-empty-plist") {
    templStat("case class A()")(dialects.Dotty)
    templStat("case class A @deprecated() ()")(dialects.Dotty)
    templStat("case class A private ()")(dialects.Dotty)
  }

  test("xml-literals") {
    intercept[TokenizeException] { term("<foo>{bar}</foo>")(dialects.Dotty) }
  }

  test("opaque-type-alias") {
    runTestAssert[Stat]("opaque type F = X")(
      Defn.OpaqueTypeAlias(
        List(Mod.Opaque()),
        pname("F"),
        Nil,
        Type.Bounds(None, None),
        pname("X")
      )
    )(parseTempl)
  }

  test("opaque-type-bounded-alias") {
    runTestAssert[Stat]("opaque type F <: A & B = AB")(
      Defn.OpaqueTypeAlias(
        List(Mod.Opaque()),
        pname("F"),
        Nil,
        Type.Bounds(None, Some(Type.And(pname("A"), pname("B")))),
        pname("AB")
      )
    )(parseTempl)
  }

  test("opaque-type-in-object") {
    runTestAssert[Source]("object X { opaque type IArray[+T] = Array }")(
      Source(
        List(
          Defn.Object(
            Nil,
            tname("X"),
            tpl(
              List(
                Defn.OpaqueTypeAlias(
                  List(Mod.Opaque()),
                  pname("IArray"),
                  List(
                    Type.Param(
                      List(Mod.Covariant()),
                      Type.Name("T"),
                      Nil,
                      Type.Bounds(None, None),
                      Nil,
                      Nil
                    )
                  ),
                  Type.Bounds(None, None),
                  pname("Array")
                )
              )
            )
          )
        )
      )
    )(parseSource)
  }

  test("trait-parameters") {
    runTestAssert[Stat]("trait Foo(val foo: Int)(bar: Int)")(
      Defn.Trait(
        Nil,
        pname("Foo"),
        Nil,
        Ctor.Primary(
          Nil,
          anon,
          List(
            List(tparamval("foo", "Int")),
            List(tparam("bar", "Int"))
          )
        ),
        tpl(Nil)
      )
    )
  }

  test("trait-parameters-generic") {
    runTestAssert[Stat]("trait Foo[T](bar: T)")(
      Defn.Trait(Nil, pname("Foo"), List(pparam("T")), ctorp(List(tparam("bar", "T"))), tpl(Nil))
    )
  }

  test("class-parameters-using") {
    runTestAssert[Stat]("trait A(using String)")(
      Defn.Trait(Nil, pname("A"), Nil, ctorp(List(tparamUsing("", "String"))), tpl(Nil))
    )

    runTestAssert[Stat]("class A(using String)")(
      Defn.Class(Nil, pname("A"), Nil, ctorp(List(tparamUsing("", "String"))), tpl(Nil))
    )

    runTestAssert[Stat]("case class A(a: Int)(using b: String)")(
      Defn.Class(
        List(Mod.Case()),
        pname("A"),
        Nil,
        Ctor.Primary(Nil, anon, List(List(tparam("a", "Int")), List(tparamUsing("b", "String")))),
        tpl(Nil)
      )
    )
  }

  test("inline-soft-keyword-pos") {
    // as ident
    runTestAssert[Stat]("def f(inline: String): Unit")(
      Decl.Def(Nil, tname("f"), Nil, List(List(tparam("inline", "String"))), pname("Unit"))
    )(parseTempl)

    // as ident
    runTestAssert[Stat]("inline def inline(inline: inline): inline")(
      Decl.Def(
        List(Mod.Inline()),
        tname("inline"),
        Nil,
        List(List(tparam("inline", "inline"))),
        pname("inline")
      )
    )(parseTempl)

    // as modifier
    runTestAssert[Stat]("inline def inline(inline param: inline): inline")(
      Decl.Def(
        List(Mod.Inline()),
        tname("inline"),
        Nil,
        List(List(tparamInline("param", "inline"))),
        pname("inline")
      )
    )(parseTempl)

    // as ident and modifier
    runTestAssert[Stat]("inline def inline(inline inline: inline): inline")(
      Decl.Def(
        List(Mod.Inline()),
        tname("inline"),
        Nil,
        List(List(tparamInline("inline", "inline"))),
        pname("inline")
      )
    )(parseTempl)

    // as ident and modifier
    runTestAssert[Stat]("inline val inline = false")(
      Defn.Val(List(Mod.Inline()), List(Pat.Var(tname("inline"))), None, Lit.Boolean(false))
    )(parseTempl)

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
    )(parseTempl)
  }

  test("inline-def-object") {
    runTestAssert[Source](
      "object X { inline def f(inline sc: Str)(inline args: Any*): String = ??? }"
    )(
      Source(
        List(
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
      )
    )(parseSource)
  }

  test("inline-mods-combination") {
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
    )(parseTempl)

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
    )(parseTempl)
  }

  // currently parser allows for more than possible.
  test("inline-soft-keyword-neg".ignore) {
    runTestError[Stat](
      "def f(inline p: String): Unit",
      "inline modifier can only be used for parameters of inline methods"
    )(parseTempl)

    runTestError[Stat](
      "class C(inline p: String)",
      "inline modifier can only be used for parameters of inline methods"
    )(parseTempl)
  }
}
