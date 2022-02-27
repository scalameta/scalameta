package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.Type.Apply
import scala.meta.Type.Placeholder

class MinorDottySuite extends BaseDottySuite {

  /**
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/other-new-features/open-classes.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/changed-features/wildcards.html]]
   */
  test("open-class") {
    val Defn.Class(List(Mod.Open()), Type.Name("A"), _, _, _) =
      templStat("open class A {}")(dialects.Scala3)

    val Defn.Trait(List(Mod.Open()), Type.Name("C"), _, _, _) =
      templStat("open trait C {}")(dialects.Scala3)

    val Defn.Trait(List(Mod.Open(), Mod.Private(Name.Anonymous())), Type.Name("C"), _, _, _) =
      templStat("open private trait C {}")(dialects.Scala3)

    val Defn.Object(List(Mod.Open()), Term.Name("X"), _) =
      templStat("open object X {}")(dialects.Scala3)

  }

  test("open-class-negative-cases") {
    runTestError[Stat]("final open class A {}", "illegal combination of modifiers: open and final")
    runTestError[Stat](
      "open sealed trait C {}",
      "illegal combination of modifiers: open and sealed"
    )
    runTestError[Stat]("open def f(): Int = 3", "`open' modifier can be used only for classes")
    runTestError[Stat]("def f(open a: Int): Int = 3", "error")
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

  test("case-classes-empty-plist") {
    templStat("case class A()")(dialects.Scala3)
    templStat("case class A @deprecated() ()")(dialects.Scala3)
    templStat("case class A private ()")(dialects.Scala3)
  }

  test("xml-literals") {
    term("<foo>{bar}</foo>")(dialects.Scala3)
  }

  test("opaque-type-alias") {
    runTestAssert[Stat]("opaque type F = X")(
      Defn.Type(
        List(Mod.Opaque()),
        pname("F"),
        Nil,
        pname("X"),
        Type.Bounds(None, None)
      )
    )

  }

  test("opaque-type-bounded-alias") {
    runTestAssert[Stat]("opaque type F <: A & B = AB")(
      Defn.Type(
        List(Mod.Opaque()),
        pname("F"),
        Nil,
        pname("AB"),
        Type.Bounds(None, Some(Type.And(Type.Name("A"), Type.Name("B"))))
      )
    )
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
                Defn.Type(
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
            Defn.Type(
              List(Mod.Private(Name("")), Mod.Opaque()),
              Type.Name("T"),
              Nil,
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
            Defn.Type(
              List(Mod.Opaque(), Mod.Private(Name(""))),
              Type.Name("T"),
              Nil,
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

  test("secondary-trait-constructors") {
    runTestError[Stat](
      "trait Foo{ def this(i: Int) = this() }",
      "Illegal secondary constructor"
    )
  }

  test("secondary-object-constructors") {
    runTestError[Stat](
      "object Foo{ def this(i: Int) = this() }",
      "Illegal secondary constructor"
    )
  }

  test("no-params") {
    runTestError[Stat](
      """|class A
         |class B extends A:
         |  def this = this.f()""".stripMargin,
      "auxiliary constructor needs non-implicit parameter list"
    )
  }

  test("trait-parameters-generic") {
    runTestAssert[Stat]("trait Foo[T](bar: T)")(
      Defn.Trait(Nil, pname("Foo"), List(pparam("T")), ctorp(List(tparam("bar", "T"))), tpl(Nil))
    )
  }

  test("trait-parameters-context-bounds") {
    runTestAssert[Stat]("trait Foo[T: Eq]")(
      Defn.Trait(
        Nil,
        Type.Name("Foo"),
        List(
          Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, List(Type.Name("Eq")))
        ),
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
      )
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
    )

    runTestAssert[Stat](
      "(new A(), new B())"
    )(
      Term.Tuple(
        List(
          Term.New(Init(Type.Name("A"), Name(""), List(List()))),
          Term.New(Init(Type.Name("B"), Name(""), List(List())))
        )
      )
    )

  }

  // Super traits were removed in Scala 3
  test("super-trait") {
    runTestError[Stat]("super trait Foo", ". expected but trait found")
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
    )

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
    runTestError[Stat]("given [_](using Ord[T]): Ord[List[T]]{}", "identifier expected")
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

  test("type-wildcard-questionmark") {
    runTestAssert[Stat]("val x: List[?] = List(1)")(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("x"))),
        Some(Type.Apply(Type.Name("List"), List(Type.Placeholder(Type.Bounds(None, None))))),
        Term.Apply(Term.Name("List"), List(Lit.Int(1)))
      )
    )

    runTestAssert[Stat]("def x(a: List[?]): Unit = ()")(
      Defn.Def(
        Nil,
        Term.Name("x"),
        Nil,
        List(
          List(
            Term.Param(
              Nil,
              Term.Name("a"),
              Some(Type.Apply(Type.Name("List"), List(Type.Placeholder(Type.Bounds(None, None))))),
              None
            )
          )
        ),
        Some(Type.Name("Unit")),
        Lit.Unit()
      )
    )
  }

  test("annotation after modifier") {
    runTestError[Stat](
      "implicit @foo def foo(): Int",
      "Annotations must precede keyword modifiers"
    )

    runTestError[Stat](
      "{ inline @foo def foo(): Int }",
      "; expected but @ found"
    )
  }

  test("unchecked-annotation") {
    runTestAssert[Stat]("val a :: Nil:  @unchecked = args")(
      Defn.Val(
        Nil,
        List(Pat.ExtractInfix(Pat.Var(Term.Name("a")), Term.Name("::"), List(Term.Name("Nil")))),
        Some(
          Type.Annotate(
            Type.AnonymousName(),
            List(Mod.Annot(Init(Type.Name("unchecked"), Name(""), Nil)))
          )
        ),
        Term.Name("args")
      )
    )

    runTestAssert[Stat]("val x:  @annotation.switch = 2")(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("x"))),
        Some(
          Type.Annotate(
            Type.AnonymousName(),
            List(
              Mod.Annot(
                Init(Type.Select(Term.Name("annotation"), Type.Name("switch")), Name(""), Nil)
              )
            )
          )
        ),
        Lit.Int(2)
      )
    )
  }

  val patternBinding = Term.Match(
    Lit.Int(1),
    List(Case(Pat.Bind(Pat.Var(Term.Name("intValue")), Lit.Int(1)), None, Term.Block(Nil)))
  )

  test("comment-after-coloneol") {
    val expected = "trait X { def x(): String }"
    runTestAssert[Stat](
      """trait X: // comment
        |  def x(): String
        |""".stripMargin,
      assertLayout = Some(expected)
    )(
      Defn.Trait(
        Nil,
        Type.Name("X"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Decl.Def(Nil, Term.Name("x"), Nil, List(List()), Type.Name("String")))
        )
      )
    )
  }

  test("opaque-type-indent-definition") {
    val expected = "opaque type LinearSet[Elem] = Set[Elem]"
    runTestAssert[Stat](
      """|opaque type LinearSet[Elem] =
         |  Set[Elem]
         |""".stripMargin,
      assertLayout = Some(expected)
    )(
      Defn.Type(
        List(Mod.Opaque()),
        Type.Name("LinearSet"),
        List(Type.Param(Nil, Type.Name("Elem"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Type.Apply(Type.Name("Set"), List(Type.Name("Elem")))
      )
    )
  }

  test("opaque-soft-keyword") {
    runTestAssert[Stat](
      "enum Kind { case Type(opaque: Boolean, transparent: Boolean) }"
    )(
      Defn.Enum(
        Nil,
        Type.Name("Kind"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.EnumCase(
              Nil,
              Term.Name("Type"),
              Nil,
              Ctor.Primary(
                Nil,
                Name(""),
                List(
                  List(
                    Term.Param(Nil, Term.Name("opaque"), Some(Type.Name("Boolean")), None),
                    Term.Param(Nil, Term.Name("transparent"), Some(Type.Name("Boolean")), None)
                  )
                )
              ),
              Nil
            )
          )
        )
      )
    )
  }

  test("capital-var-pattern-val") {
    runTestAssert[Stat](
      """val Private @ _ = flags()
        |""".stripMargin
    )(
      Defn.Val(
        Nil,
        List(Pat.Bind(Pat.Var(Term.Name("Private")), Pat.Wildcard())),
        None,
        Term.Apply(Term.Name("flags"), Nil)
      )
    )
  }

  test("capital-var-pattern-case") {
    runTestAssert[Stat](
      """|flags() match {
         |  case Pattern @ _ =>
         |}
         |""".stripMargin
    )(
      Term.Match(
        Term.Apply(Term.Name("flags"), Nil),
        List(Case(Pat.Bind(Pat.Var(Term.Name("Pattern")), Pat.Wildcard()), None, Term.Block(Nil)))
      )
    )
  }

  test("catch-end-def") {
    val layout =
      """|object X {
         |  def fx = try action() catch {
         |    case ex =>
         |      err()
         |  }
         |  private abstract class X()
         |}
         |""".stripMargin
    runTestAssert[Stat](
      """|object X {
         |  def fx = try
         |    action()
         |  catch case ex => err()
         |
         |  private abstract class X()
         |}
         |""".stripMargin,
      assertLayout = Some(layout)
    )(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Def(
              Nil,
              Term.Name("fx"),
              Nil,
              Nil,
              None,
              Term.Try(
                Term.Apply(Term.Name("action"), Nil),
                List(Case(Pat.Var(Term.Name("ex")), None, Term.Apply(Term.Name("err"), Nil))),
                None
              )
            ),
            Defn.Class(
              List(Mod.Private(Name("")), Mod.Abstract()),
              Type.Name("X"),
              Nil,
              Ctor.Primary(Nil, Name(""), List(List())),
              Template(Nil, Nil, Self(Name(""), None), Nil)
            )
          )
        )
      )
    )
  }

  test("type-in-block") {
    runTestAssert[Stat](
      """|def hello = {
         |  type T
         |}
         |""".stripMargin
    )(
      Defn.Def(
        Nil,
        Term.Name("hello"),
        Nil,
        Nil,
        None,
        Term.Block(List(Decl.Type(Nil, Type.Name("T"), Nil, Type.Bounds(None, None))))
      )
    )
  }

  test("operator-next-line") {
    runTestAssert[Stat](
      """|val all = "-siteroot" +: "../docs"
         |    +: "-project" +:  Nil""".stripMargin,
      assertLayout = Some("""val all = "-siteroot" +: "../docs" +: "-project" +: Nil""")
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("all"))),
        None,
        Term.ApplyInfix(
          Lit.String("-siteroot"),
          Term.Name("+:"),
          Nil,
          List(
            Term.ApplyInfix(
              Lit.String("../docs"),
              Term.Name("+:"),
              Nil,
              List(
                Term.ApplyInfix(
                  Lit.String("-project"),
                  Term.Name("+:"),
                  Nil,
                  List(Term.Name("Nil"))
                )
              )
            )
          )
        )
      )
    )
  }

  test("operator-next-line-bad-indent") {
    runTestAssert[Stat](
      """|def withClasspath =
         |       "-siteroot" +: "../docs"
         |    +: "-project" +: Nil
         |""".stripMargin,
      assertLayout = Some("""def withClasspath = "-siteroot" +: "../docs" +: "-project" +: Nil""")
    )(
      Defn.Def(
        Nil,
        Term.Name("withClasspath"),
        Nil,
        Nil,
        None,
        Term.ApplyInfix(
          Lit.String("-siteroot"),
          Term.Name("+:"),
          Nil,
          List(
            Term.ApplyInfix(
              Lit.String("../docs"),
              Term.Name("+:"),
              Nil,
              List(
                Term.ApplyInfix(
                  Lit.String("-project"),
                  Term.Name("+:"),
                  Nil,
                  List(Term.Name("Nil"))
                )
              )
            )
          )
        )
      )
    )
  }

  test("colon-extractor") {
    runTestAssert[Stat](
      """|a match {case List(xs: _*) => }
         |""".stripMargin,
      assertLayout = Some(
        """|a match {
           |  case List(xs @ _*) =>
           |}
           |""".stripMargin
      )
    )(
      Term.Match(
        Term.Name("a"),
        List(
          Case(
            Pat.Extract(
              Term.Name("List"),
              List(Pat.Bind(Pat.Var(Term.Name("xs")), Pat.SeqWildcard()))
            ),
            None,
            Term.Block(Nil)
          )
        )
      )
    )
  }

  test("at-extractor") {
    runTestAssert[Stat](
      """|a match {case List(xs@ _*) => }
         |""".stripMargin,
      assertLayout = Some(
        """|a match {
           |  case List(xs @ _*) =>
           |}
           |""".stripMargin
      )
    )(
      Term.Match(
        Term.Name("a"),
        List(
          Case(
            Pat.Extract(
              Term.Name("List"),
              List(Pat.Bind(Pat.Var(Term.Name("xs")), Pat.SeqWildcard()))
            ),
            None,
            Term.Block(Nil)
          )
        )
      )
    )
  }

  test("vararg-wildcard-postfix-star") {
    runTestAssert[Stat](
      "val lst = List(0, arr*)"
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("lst"))),
        None,
        Term.Apply(Term.Name("List"), List(Lit.Int(0), Term.Repeated(Term.Name("arr"))))
      )
    )
  }

  test("non-vararg-infix-star") {
    runTestAssert[Stat](
      "val lst = List(0, a * b)"
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("lst"))),
        None,
        Term.Apply(
          Term.Name("List"),
          List(
            Lit.Int(0),
            Term.ApplyInfix(Term.Name("a"), Term.Name("*"), Nil, List(Term.Name("b")))
          )
        )
      )
    )
  }

  test("vararg-wildcard-postfix-start-pat") {
    runTestAssert[Stat](
      """|a match {case List(xs*) => }
         |""".stripMargin,
      assertLayout = Some(
        """|a match {
           |  case List(xs*) =>
           |}
           |""".stripMargin
      )
    )(
      Term.Match(
        Term.Name("a"),
        List(
          Case(
            Pat.Extract(Term.Name("List"), List(Pat.Repeated(Term.Name("xs")))),
            None,
            Term.Block(Nil)
          )
        ),
        Nil
      )
    )
  }

  test("empty-case-class") {
    val error = "case classes must have a parameter list"
    runTestError[Stat]("case class A", error)
    runTestError[Stat]("case class A[T]", error)
    runTestError[Stat]("case class A[T] private", error)
  }

  test("trailing-coma") {
    runTestAssert[Stat](
      """|case class A(
         |  x: X,
         |)""".stripMargin,
      assertLayout = Some(
        """|case class A(x: X)
           |""".stripMargin
      )
    )(
      Defn.Class(
        List(Mod.Case()),
        Type.Name("A"),
        Nil,
        Ctor.Primary(
          Nil,
          Name(""),
          List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("X")), None)))
        ),
        Template(Nil, Nil, Self(Name(""), None), Nil)
      )
    )
  }

  test("complex-interpolation") {
    runTestAssert[Stat](
      """|val base =
         |  ""
         |  ++ s""
         |""".stripMargin,
      assertLayout = Some(
        """|val base = "" ++ (s"")
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("base"))),
        None,
        Term.ApplyInfix(
          Lit.String(""),
          Term.Name("++"),
          Nil,
          List(Term.Interpolate(Term.Name("s"), List(Lit.String("")), Nil))
        )
      )
    )
  }

  test("tuple-pattern") {
    runTestAssert[Stat](
      """|def f(t: (String, String)): String =
         |  t match
         |    case (m, _): (String, String) => m
         |""".stripMargin,
      assertLayout = Some(
        """|def f(t: (String, String)): String = t match {
           |  case (m, _): (String, String) => m
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("f"),
        Nil,
        List(
          List(
            Term.Param(
              Nil,
              Term.Name("t"),
              Some(Type.Tuple(List(Type.Name("String"), Type.Name("String")))),
              None
            )
          )
        ),
        Some(Type.Name("String")),
        Term.Match(
          Term.Name("t"),
          List(
            Case(
              Pat.Typed(
                Pat.Tuple(List(Pat.Var(Term.Name("m")), Pat.Wildcard())),
                Type.Tuple(List(Type.Name("String"), Type.Name("String")))
              ),
              None,
              Term.Name("m")
            )
          ),
          Nil
        )
      )
    )
  }

  test("regex-pattern") {
    runTestAssert[Stat](
      """|s match
         |  case re(v): String => v.toDouble
         |  case other => o
         |""".stripMargin,
      assertLayout = Some(
        """|s match {
           |  case re(v): String =>
           |    v.toDouble
           |  case other =>
           |    o
           |}
           |""".stripMargin
      )
    )(
      Term.Match(
        Term.Name("s"),
        List(
          Case(
            Pat.Typed(
              Pat.Extract(Term.Name("re"), List(Pat.Var(Term.Name("v")))),
              Type.Name("String")
            ),
            None,
            Term.Select(Term.Name("v"), Term.Name("toDouble"))
          ),
          Case(Pat.Var(Term.Name("other")), None, Term.Name("o"))
        ),
        Nil
      )
    )
  }

  test("multi-pattern") {
    runTestAssert[Stat](
      """|val x * y = v
         |""".stripMargin
    )(
      Defn.Val(
        Nil,
        List(
          Pat.ExtractInfix(Pat.Var(Term.Name("x")), Term.Name("*"), List(Pat.Var(Term.Name("y"))))
        ),
        None,
        Term.Name("v")
      )
    )
  }

  test("typed-typed-pattern") {
    runTestAssert[Stat](
      """|s match
         |  case (v: String): String => v.toDouble
         |  case other => o
         |""".stripMargin,
      assertLayout = Some(
        """|s match {
           |  case (v: String): String =>
           |    v.toDouble
           |  case other =>
           |    o
           |}
           |""".stripMargin
      )
    )(
      Term.Match(
        Term.Name("s"),
        List(
          Case(
            Pat.Typed(Pat.Typed(Pat.Var(Term.Name("v")), Type.Name("String")), Type.Name("String")),
            None,
            Term.Select(Term.Name("v"), Term.Name("toDouble"))
          ),
          Case(Pat.Var(Term.Name("other")), None, Term.Name("o"))
        ),
        Nil
      )
    )
  }

  test("procedure-syntax") {
    runTestError[Stat](
      """|def hello(){
         |  println("Hello!")
         |}""".stripMargin,
      "Procedure syntax is not supported. Convert procedure `hello` to method by adding `: Unit =`"
    )
  }

  test("do-while") {
    runTestError[Stat](
      """|def hello() = {
         |  do {
         |    i+= 1
         |  } while (i < 10)
         |}""".stripMargin,
      "error: do {...} while (...) syntax is no longer supported"
    )
  }

  test("partial-function-function") {
    runTestAssert[Stat](
      """|val f : String => PartialFunction[String, Int] = s =>
         |    case "Hello" =>
         |        5
         |    case "Goodbye" =>
         |        0
         |""".stripMargin,
      assertLayout = Some(
        """|val f: String => PartialFunction[String, Int] = s => {
           |  case "Hello" => 5
           |  case "Goodbye" => 0
           |}
           |""".stripMargin
      )
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("f"))),
        Some(
          Type.Function(
            List(Type.Name("String")),
            Type.Apply(Type.Name("PartialFunction"), List(Type.Name("String"), Type.Name("Int")))
          )
        ),
        Term.Function(
          List(Term.Param(Nil, Term.Name("s"), None, None)),
          Term.PartialFunction(
            List(
              Case(Lit.String("Hello"), None, Lit.Int(5)),
              Case(Lit.String("Goodbye"), None, Lit.Int(0))
            )
          )
        )
      )
    )
  }

  test("underscore-placeholder") {
    runTestAssert[Stat](
      """|object A:
         |  type X = +_ => Int
         |  type Y = -_ => Int
         |  type Z = _ => Int
         |""".stripMargin,
      assertLayout = Some(
        """|object A {
           |  type X = ? => Int
           |  type Y = ? => Int
           |  type Z = ? => Int
           |}
           |""".stripMargin
      )
    )(
      Defn.Object(
        Nil,
        Term.Name("A"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Type(
              Nil,
              Type.Name("X"),
              Nil,
              Type.Function(List(Type.Placeholder(Type.Bounds(None, None))), Type.Name("Int")),
              Type.Bounds(None, None)
            ),
            Defn.Type(
              Nil,
              Type.Name("Y"),
              Nil,
              Type.Function(List(Type.Placeholder(Type.Bounds(None, None))), Type.Name("Int")),
              Type.Bounds(None, None)
            ),
            Defn.Type(
              Nil,
              Type.Name("Z"),
              Nil,
              Type.Function(List(Type.Placeholder(Type.Bounds(None, None))), Type.Name("Int")),
              Type.Bounds(None, None)
            )
          ),
          Nil
        )
      )
    )
  }

  test("type-param-last") {
    runTestAssert[Stat](
      """|def b2 = "".foo2(using foo)[Any]
         |""".stripMargin,
      assertLayout = Some(
        """|def b2 = "".foo2(using foo)[Any]
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("b2"),
        Nil,
        Nil,
        None,
        Term.ApplyType(
          Term.ApplyUsing(Term.Select(Lit.String(""), Term.Name("foo2")), List(Term.Name("foo"))),
          List(Type.Name("Any"))
        )
      )
    )
  }

  test("refinements") {
    runTestAssert[Stat](
      """|val x: (C { type U = T } { type T = String }) # U 
         |""".stripMargin,
      assertLayout = Some(
        """|val x: ((C { type U = T }) { type T = String })#U
           |""".stripMargin
      )
    )(
      Decl.Val(
        Nil,
        List(Pat.Var(Term.Name("x"))),
        Type.Project(
          Type.Refine(
            Some(
              Type.Refine(
                Some(Type.Name("C")),
                List(Defn.Type(Nil, Type.Name("U"), Nil, Type.Name("T"), Type.Bounds(None, None)))
              )
            ),
            List(Defn.Type(Nil, Type.Name("T"), Nil, Type.Name("String"), Type.Bounds(None, None)))
          ),
          Type.Name("U")
        )
      )
    )
  }

  test("issue-2506") {
    runTestAssert[Stat](
      """|??? match {
         |  case x2: ([V] => () => Int) => ???
         |}
         |""".stripMargin
    )(
      Term.Match(
        Term.Name("???"),
        List(
          Case(
            Pat.Typed(
              Pat.Var(Term.Name("x2")),
              Type.PolyFunction(
                List(Type.Param(Nil, Type.Name("V"), Nil, Type.Bounds(None, None), Nil, Nil)),
                Type.Function(Nil, Type.Name("Int"))
              )
            ),
            None,
            Term.Name("???")
          )
        ),
        Nil
      )
    )
  }

  test("issue-2567") {
    runTestAssert[Source](
      """|import _root_com.olegych.scastie.api.runtime._
         |
         |object Playground extends ScastieApp {
         |  List(1,2,3).map { (using i: Int) => i }
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|import _root_com.olegych.scastie.api.runtime.*
           |object Playground extends ScastieApp {
           |  List(1, 2, 3).map {
           |    (using i: Int) => i
           |  }
           |}
           |""".stripMargin
      )
    )(
      Source(
        List(
          Import(
            List(
              Importer(
                Term.Select(
                  Term.Select(
                    Term.Select(
                      Term.Select(Term.Name("_root_com"), Term.Name("olegych")),
                      Term.Name("scastie")
                    ),
                    Term.Name("api")
                  ),
                  Term.Name("runtime")
                ),
                List(Importee.Wildcard())
              )
            )
          ),
          Defn.Object(
            Nil,
            Term.Name("Playground"),
            Template(
              Nil,
              List(Init(Type.Name("ScastieApp"), Name(""), Nil)),
              Self(Name(""), None),
              List(
                Term.Apply(
                  Term.Select(
                    Term.Apply(Term.Name("List"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3))),
                    Term.Name("map")
                  ),
                  List(
                    Term.Block(
                      List(
                        Term.Function(
                          List(
                            Term.Param(
                              List(Mod.Using()),
                              Term.Name("i"),
                              Some(Type.Name("Int")),
                              None
                            )
                          ),
                          Term.Name("i")
                        )
                      )
                    )
                  )
                )
              ),
              Nil
            )
          )
        )
      )
    )
  }

}
