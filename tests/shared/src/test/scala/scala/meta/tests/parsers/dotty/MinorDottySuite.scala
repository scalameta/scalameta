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
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/other-new-features/open-classes.html
   *  https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html
   *  https://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html
   *  https://dotty.epfl.ch/docs/reference/changed-features/wildcards.html
   */
  test("open-class") {
    val Defn.Class(List(Mod.Open()), Type.Name("A"), _, _, _) =
      templStat("open class A {}")(dialects.Dotty)

    val Defn.Trait(List(Mod.Open()), Type.Name("C"), _, _, _) =
      templStat("open trait C {}")(dialects.Dotty)

    val Defn.Trait(List(Mod.Open(), Mod.Private(Name.Anonymous())), Type.Name("C"), _, _, _) =
      templStat("open private trait C {}")(dialects.Dotty)

    val Defn.Object(List(Mod.Open()), Term.Name("X"), _) =
      templStat("open object X {}")(dialects.Dotty)

  }

  test("open-class-negative-cases") {
    runTestError("final open class A {}", "illegal combination of modifiers: open and final")(
      parseTempl
    )
    runTestError(
      "open sealed trait C {}",
      "illegal combination of modifiers: open and sealed"
    )(parseTempl)
    runTestError("open def f(): Int = 3", "`open' modifier can be used only for classes")(
      parseTempl
    )
    runTestError("def f(open a: Int): Int = 3", "error")(parseTempl)
  }

  test("open-soft-modifier") {
    stat("def open(open: open): open = ???").structure
  }

  test("open-identifier") {
    runTestAssert[Stat]("def run(): Unit = { start; open(p); end }", assertLayout = None)(
      Defn.Def(
        Nil,
        Term.Name("run"),
        Nil,
        List(List()),
        Some(Type.Name("Unit")),
        Term.Block(
          List(
            Term.Name("start"),
            Term.Apply(Term.Name("open"), List(Term.Name("p"))),
            Term.Name("end")
          )
        )
      )
    )
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

  test("opaque-type-mix-mods") {
    runTestAssert[Stat]("object X { private opaque type T = List[Int] }")(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.OpaqueTypeAlias(
              List(Mod.Private(Name("")), Mod.Opaque()),
              Type.Name("T"),
              Nil,
              Type.Bounds(None, None),
              Type.Apply(Type.Name("List"), List(Type.Name("Int")))
            )
          )
        )
      )
    )
    runTestAssert[Stat]("object X { opaque private type T = List[Int] }")(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.OpaqueTypeAlias(
              List(Mod.Opaque(), Mod.Private(Name(""))),
              Type.Name("T"),
              Nil,
              Type.Bounds(None, None),
              Type.Apply(Type.Name("List"), List(Type.Name("Int")))
            )
          )
        )
      )
    )
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

  test("trait-extends-coma-separated") {
    runTestAssert[Stat](
      "trait Foo extends A, B, C",
      assertLayout = Some("trait Foo extends A with B with C")
    )(
      Defn.Trait(
        Nil,
        Type.Name("Foo"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, List(init("A"), init("B"), init("C")), Self(Name(""), None), Nil)
      )
    )(parseTempl)

    runTestAssert[Stat](
      "(new A(), new B())"
    )(
      Term.Tuple(
        List(
          Term.New(Init(Type.Name("A"), Name(""), List(List()))),
          Term.New(Init(Type.Name("B"), Name(""), List(List())))
        )
      )
    )(parseTempl)

  }

  test("super-trait") {
    runTestAssert[Stat]("super trait Foo")(
      Defn.Trait(
        List(Mod.Super()),
        Type.Name("Foo"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )(parseTempl)
  }

  test("question-type") {
    runTestAssert[Stat]("val stat: Tree[? >: Untyped]")(
      Decl.Val(
        Nil,
        List(Pat.Var(Term.Name("stat"))),
        Type.Apply(
          Type.Name("Tree"),
          List(Type.Placeholder(Type.Bounds(Some(Type.Name("Untyped")), None)))
        )
      )
    )(parseTempl)

  }

  test("lazy-val-toplevel") {
    runTestAssert[Source]("lazy val x = 3")(
      Source(List(Defn.Val(List(Mod.Lazy()), List(Pat.Var(Term.Name("x"))), None, Lit.Int(3))))
    )
  }

  test("changed-operator-syntax") {
    // https://dotty.epfl.ch/docs/reference/changed-features/operators.html#syntax-change
    runTestAssert[Source]("""|object X {
                             |  println("hello")
                             |  ???
                             |  ??? match {
                             |    case 0 => 1
                             |  }
                             |}
                             |""".stripMargin)(
      Source(
        List(
          Defn.Object(
            Nil,
            Term.Name("X"),
            Template(
              Nil,
              Nil,
              Self(Name(""), None),
              List(
                Term.Apply(Term.Name("println"), List(Lit.String("hello"))),
                Term.Name("???"),
                Term.Match(Term.Name("???"), List(Case(Lit.Int(0), None, Lit.Int(1))))
              )
            )
          )
        )
      )
    )
  }

  test("type.param-with-name.anon") {
    runTestError[Stat]("trait F[_]", "identifier expected")
    runTestError[Stat]("class F[_]", "identifier expected")
    runTestError[Stat]("enum X[T]{ case A[_] extends X[Int] }", "identifier expected")
    runTestError[Stat]("extension [_](x: Int) def inc: Int = x + 1", "identifier expected")
    runTestError[Stat]("given [_](using Ord[T]) as Ord[List[T]]{}", "identifier expected")

    runTestAssert[Stat]("given [T](using Ord[T]) as Ord[List[T]]")(
      Defn.Given(
        Nil,
        Name(""),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(
          List(
            Term.Param(
              List(Mod.Using()),
              Name(""),
              Some(Type.Apply(Type.Name("Ord"), List(Type.Name("T")))),
              None
            )
          )
        ),
        Type.Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("List"), List(Type.Name("T"))))),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )
  }

  test("repeated-byname-class-parameter") {
    runTestAssert[Stat]("class Foo(bars: => Int*)")(
      Defn.Class(
        Nil,
        Type.Name("Foo"),
        Nil,
        Ctor.Primary(
          Nil,
          Name(""),
          List(
            List(
              Term.Param(
                Nil,
                Term.Name("bars"),
                Some(Type.Repeated(Type.ByName(Type.Name("Int")))),
                None
              )
            )
          )
        ),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )

    runTestAssert[Stat]("def fx(x: => Int*): Int = 3")(
      Defn.Def(
        Nil,
        Term.Name("fx"),
        Nil,
        List(
          List(
            Term
              .Param(Nil, Term.Name("x"), Some(Type.Repeated(Type.ByName(Type.Name("Int")))), None)
          )
        ),
        Some(Type.Name("Int")),
        Lit.Int(3)
      )
    )
  }

  test("lazy-abstract-class-value") {
    runTestAssert[Stat]("trait Foo { protected[this] lazy val from: Int }")(
      Defn.Trait(
        Nil,
        Type.Name("Foo"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Decl.Val(
              List(Mod.Protected(Term.This(Name(""))), Mod.Lazy()),
              List(Pat.Var(Term.Name("from"))),
              Type.Name("Int")
            )
          )
        )
      )
    )
    runTestError[Stat](
      "trait Foo { protected[this] lazy var from: Int }",
      "lazy not allowed here. Only vals can be lazy"
    )
  }

}
