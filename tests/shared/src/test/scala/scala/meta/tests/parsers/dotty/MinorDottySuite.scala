package scala.meta.tests.parsers.dotty

import scala.meta._

class MinorDottySuite extends BaseDottySuite {

  /**
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/other-new-features/open-classes.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/other-new-features/trait-parameters.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/changed-features/wildcards.html]]
   */
  test("open-class") {
    matchSubStructure[Stat](
      "open class A {}",
      { case Defn.Class(List(Mod.Open()), Type.Name("A"), _, _, _) => () }
    )
    matchSubStructure[Stat](
      "open trait C {}",
      { case Defn.Trait(List(Mod.Open()), Type.Name("C"), _, _, _) => () }
    )

    matchSubStructure[Stat](
      "open private trait C {}",
      { case Defn.Trait(List(Mod.Open(), Mod.Private(anon)), Type.Name("C"), _, _, _) => () }
    )

    matchSubStructure[Stat](
      "open object X {}",
      { case Defn.Object(List(Mod.Open()), Term.Name("X"), _) => () }
    )

  }

  test("open-class-negative-cases")(runTestError[Stat]("def f(open a: Int): Int = 3", "error"))

  test("open-soft-modifier")(stat("def open(open: open): open = ???").structure)

  test("open-soft-modifier-case") {
    runTestAssert[Source](
      """|case class OHLCPrice(
         |    val open: Int
         |) {
         |  val price = OHLCPrice(1)
         |  val price1 = price.open
         |  val price2 = 1
         |}""".stripMargin,
      assertLayout = Some(
        """|case class OHLCPrice(val open: Int) {
           |  val price = OHLCPrice(1)
           |  val price1 = price.open
           |  val price2 = 1
           |}
           |""".stripMargin
      )
    )(Source(List(Defn.Class(
      List(Mod.Case()),
      pname("OHLCPrice"),
      Nil,
      Ctor.Primary(Nil, anon, List(List(tparam(List(Mod.ValParam()), "open", "Int")))),
      tpl(
        Defn
          .Val(Nil, List(Pat.Var(tname("price"))), None, Term.Apply(tname("OHLCPrice"), List(int(1)))),
        Defn
          .Val(Nil, List(Pat.Var(tname("price1"))), None, Term.Select(tname("price"), tname("open"))),
        Defn.Val(Nil, List(Pat.Var(tname("price2"))), None, int(1))
      )
    ))))
  }

  test("open-identifier") {
    runTestAssert[Stat]("def run(): Unit = { start; open(p); end }", assertLayout = None)(Defn.Def(
      Nil,
      tname("run"),
      Nil,
      List(List()),
      Some(pname("Unit")),
      Term.Block(List(tname("start"), Term.Apply(tname("open"), List(tname("p"))), tname("end")))
    ))
  }

  test("case-classes-empty-plist") {
    templStat("case class A()")(dialects.Scala3)
    templStat("case class A @deprecated() ()")(dialects.Scala3)
    templStat("case class A private ()")(dialects.Scala3)
  }

  test("xml-literals")(term("<foo>{bar}</foo>")(dialects.Scala3))

  test("opaque-type-alias") {
    runTestAssert[Stat]("opaque type F = X")(
      Defn.Type(List(Mod.Opaque()), pname("F"), Nil, pname("X"), noBounds)
    )

  }

  test("opaque-type-bounded-alias") {
    runTestAssert[Stat]("opaque type F <: A & B = AB")(Defn.Type(
      List(Mod.Opaque()),
      pname("F"),
      Nil,
      pname("AB"),
      Type.Bounds(None, Some(Type.ApplyInfix(pname("A"), pname("&"), pname("B"))))
    ))
  }

  test("opaque-type-bounded-alias-with-quasiquotes") {
    val dialect: Dialect = null // overrides implicit
    import dialects.Scala3
    runTestAssert[Stat]("opaque type Foo <: String = String")(
      Defn.Type(List(Mod.Opaque()), pname("Foo"), Nil, pname("String"), hiBound("String"))
    )
  }

  test("opaque-type-in-object") {
    runTestAssert[Source]("object X { opaque type IArray[+T] = Array }")(Source(List(Defn.Object(
      Nil,
      tname("X"),
      tpl(Defn.Type(
        List(Mod.Opaque()),
        pname("IArray"),
        List(pparam(List(Mod.Covariant()), "T")),
        pname("Array")
      ))
    ))))
  }

  test("opaque-type-mix-mods") {
    runTestAssert[Stat]("object X { private opaque type T = List[Int] }")(Defn.Object(
      Nil,
      tname("X"),
      tpl(Defn.Type(
        List(Mod.Private(anon), Mod.Opaque()),
        pname("T"),
        Nil,
        Type.Apply(pname("List"), List(pname("Int")))
      ))
    ))
    runTestAssert[Stat]("object X { opaque private type T = List[Int] }")(Defn.Object(
      Nil,
      tname("X"),
      tpl(Defn.Type(
        List(Mod.Opaque(), Mod.Private(anon)),
        pname("T"),
        Nil,
        Type.Apply(pname("List"), List(pname("Int")))
      ))
    ))
  }

  test("trait-parameters") {
    runTestAssert[Stat]("trait Foo(val foo: Int)(bar: Int)")(Defn.Trait(
      Nil,
      pname("Foo"),
      Nil,
      ctorp(List(tparamval("foo", "Int")), List(tparam("bar", "Int"))),
      tplNoBody()
    ))
  }

  test("secondary-trait-constructors") {
    runTestError[Stat]("trait Foo{ def this(i: Int) = this() }", "Illegal secondary constructor")
  }

  test("secondary-object-constructors") {
    runTestError[Stat]("object Foo{ def this(i: Int) = this() }", "Illegal secondary constructor")
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
      Defn.Trait(Nil, pname("Foo"), List(pparam("T")), ctorp(tparam("bar", "T")), tplNoBody())
    )
  }

  test("trait-parameters-context-bounds") {
    runTestAssert[Stat]("trait Foo[T: Eq]")(Defn.Trait(
      Nil,
      pname("Foo"),
      List(pparam(Nil, "T", vb = Nil, cb = List(pname("Eq")))),
      ctor,
      tplNoBody()
    ))
  }

  test("class-parameters-using") {
    runTestAssert[Stat]("trait A(using String)")(
      Defn.Trait(Nil, pname("A"), Nil, ctorp(tparamUsing("", "String")), tplNoBody())
    )

    runTestAssert[Stat]("class A(using String)")(
      Defn.Class(Nil, pname("A"), Nil, ctorp(tparamUsing("", "String")), tplNoBody())
    )

    runTestAssert[Stat]("case class A(a: Int)(using b: String)")(Defn.Class(
      List(Mod.Case()),
      pname("A"),
      Nil,
      ctorp(List(tparam("a", "Int")), List(tparamUsing("b", "String"))),
      tplNoBody()
    ))
  }

  test("trait-extends-coma-separated") {
    runTestAssert[Stat](
      "trait Foo extends A, B, C",
      assertLayout = Some("trait Foo extends A with B with C")
    )(Defn.Trait(Nil, pname("Foo"), Nil, ctor, tplNoBody(init("A"), init("B"), init("C"))))
  }

  test("(new A(), new B())") {
    runTestAssert[Stat]("(new A(), new B())")(Term.Tuple(List(
      Term.New(Init(pname("A"), anon, List(List()))),
      Term.New(Init(pname("B"), anon, List(List())))
    )))
  }

  test("new A(using b)(c)(using d, e)") {
    runTestAssert[Stat]("new A(using b)(c)(using d, e)", Some("new A(using b)(c)(using d, e)"))(
      Term.New(Init(
        pname("A"),
        anon,
        List(
          Term.ArgClause(List(tname("b")), Some(Mod.Using())),
          Term.ArgClause(List(tname("c")), None),
          Term.ArgClause(List(tname("d"), tname("e")), Some(Mod.Using()))
        )
      ))
    )
  }

  test("class A extends B(using b)(c)(using d, e)") {
    runTestAssert[Stat](
      "class A extends B(using b)(c)(using d, e)",
      Some("class A extends B(using b)(c)(using d, e)")
    )(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tplNoBody(init(
        "B",
        Term.ArgClause(List(tname("b")), Some(Mod.Using())),
        Term.ArgClause(List(tname("c")), None),
        Term.ArgClause(List(tname("d"), tname("e")), Some(Mod.Using()))
      ))
    ))
  }

  // Super traits were removed in Scala 3
  test("super-trait")(runTestError[Stat]("super trait Foo", "`.` expected but `trait` found"))

  test("question-type") {
    runTestAssert[Stat]("val stat: Tree[? >: Untyped]")(Decl.Val(
      Nil,
      List(Pat.Var(tname("stat"))),
      Type.Apply(pname("Tree"), List(Type.Wildcard(loBound("Untyped"))))
    ))
  }

  test("question-type-like") {
    val treeWithoutBounds = Decl
      .Val(Nil, List(Pat.Var(tname("stat"))), Type.Apply(pname("Tree"), List(pname("?"))))
    runTestAssert[Stat]("val stat: Tree[`?`]")(treeWithoutBounds)
    val errorWithBounds = """|<input>:1: error: `]` expected but `>:` found
                             |val stat: Tree[`?` >: Untyped]
                             |                   ^""".stripMargin
    runTestError[Stat]("val stat: Tree[`?` >: Untyped]", errorWithBounds)
  }

  test("lazy-val-toplevel") {
    runTestAssert[Source]("lazy val x = 3")(Source(
      List(Defn.Val(List(Mod.Lazy()), List(Pat.Var(tname("x"))), None, int(3)))
    ))
  }

  test("changed-operator-syntax") {
    // https://dotty.epfl.ch/docs/reference/changed-features/operators.html#syntax-change
    runTestAssert[Source](
      """|object X {
         |  println("hello")
         |  ???
         |  ??? match {
         |    case 0 => 1
         |  }
         |}
         |""".stripMargin,
      Some(
        """|object X {
           |  println("hello")
           |  ???
           |  ??? match {
           |    case 0 => 1
           |  }
           |}
           |""".stripMargin
      )
    )(Source(
      Defn.Object(
        Nil,
        tname("X"),
        tpl(
          Term.Apply(tname("println"), List(str("hello"))),
          tname("???"),
          Term.Match(tname("???"), List(Case(int(0), None, int(1))), Nil)
        )
      ) :: Nil
    ))
  }

  test("type.param-with-name.anon") {
    runTestError[Stat]("trait F[_]", "identifier expected")
    runTestError[Stat]("class F[_]", "identifier expected")
    runTestError[Stat]("enum X[T]{ case A[_] extends X[Int] }", "identifier expected")
    runTestError[Stat]("extension [_](x: Int) def inc: Int = x + 1", "identifier expected")
    runTestError[Stat](
      "given [_](using Ord[T]): Ord[List[T]]{}",
      "`identifier` expected but `[` found"
    )
  }

  test("repeated-byname-class-parameter") {
    runTestAssert[Stat]("class Foo(bars: => Int*)")(Defn.Class(
      Nil,
      pname("Foo"),
      Nil,
      ctorp(tparam("bars", Type.Repeated(Type.ByName(pname("Int"))))),
      tplNoBody()
    ))

    runTestAssert[Stat]("def fx(x: => Int*): Int = 3")(Defn.Def(
      Nil,
      tname("fx"),
      Nil,
      List(List(tparam("x", Type.Repeated(Type.ByName(pname("Int")))))),
      Some(pname("Int")),
      int(3)
    ))
  }

  test("repeated-like-class-parameter") {
    val error = """|<input>:1: error: `identifier` expected but `)` found
                   |class Foo(bars: Int`*`)
                   |                      ^""".stripMargin
    runTestError[Stat]("class Foo(bars: Int`*`)", error)
  }

  test("lazy-abstract-class-value") {
    runTestAssert[Stat]("trait Foo { protected[this] lazy val from: Int }")(Defn.Trait(
      Nil,
      pname("Foo"),
      Nil,
      EmptyCtor(),
      tpl(Decl.Val(
        List(Mod.Protected(Term.This(anon)), Mod.Lazy()),
        List(Pat.Var(tname("from"))),
        pname("Int")
      ))
    ))
  }

  test("type-wildcard-questionmark") {
    runTestAssert[Stat]("val x: List[?] = List(1)")(Defn.Val(
      Nil,
      List(Pat.Var(tname("x"))),
      Some(Type.Apply(pname("List"), List(Type.Wildcard(Type.Bounds(None, None))))),
      Term.Apply(tname("List"), List(int(1)))
    ))

    runTestAssert[Stat]("def x(a: List[?]): Unit = ()")(Defn.Def(
      Nil,
      tname("x"),
      Nil,
      List(List(
        tparam(Nil, "a", Type.Apply(pname("List"), List(Type.Wildcard(Type.Bounds(None, None)))))
      )),
      Some(pname("Unit")),
      Lit.Unit()
    ))
  }

  test("annotation after modifier") {
    runTestError[Stat]("implicit @foo def foo(): Int", "Annotations must precede keyword modifiers")

    runTestError[Stat]("{ inline @foo def foo(): Int }", "`;` expected but `@` found")
  }

  test("unchecked-annotation") {
    runTestAssert[Stat]("val a :: Nil:  @unchecked = args")(Defn.Val(
      Nil,
      List(Pat.ExtractInfix(Pat.Var(tname("a")), tname("::"), List(tname("Nil")))),
      Some(Type.Annotate(
        Type.AnonymousName(),
        List(Mod.Annot(Init(pname("unchecked"), anon, emptyArgClause)))
      )),
      tname("args")
    ))

    runTestAssert[Stat]("val x:  @annotation.switch = 2")(Defn.Val(
      Nil,
      List(Pat.Var(tname("x"))),
      Some(Type.Annotate(
        Type.AnonymousName(),
        List(Mod.Annot(Init(Type.Select(tname("annotation"), pname("switch")), anon, emptyArgClause)))
      )),
      int(2)
    ))
  }

  val patternBinding = Term
    .Match(int(1), List(Case(Pat.Bind(Pat.Var(tname("intValue")), int(1)), None, Term.Block(Nil))))

  test("comment-after-coloneol") {
    val expected = "trait X { def x(): String }"
    runTestAssert[Stat](
      """trait X: // comment
        |  def x(): String
        |""".stripMargin,
      assertLayout = Some(expected)
    )(Defn.Trait(
      Nil,
      pname("X"),
      Nil,
      EmptyCtor(),
      tpl(Decl.Def(Nil, tname("x"), Nil, List(List()), pname("String")))
    ))
  }

  test("opaque-type-indent-definition") {
    val expected = "opaque type LinearSet[Elem] = Set[Elem]"
    runTestAssert[Stat](
      """|opaque type LinearSet[Elem] =
         |  Set[Elem]
         |""".stripMargin,
      assertLayout = Some(expected)
    )(Defn.Type(
      List(Mod.Opaque()),
      pname("LinearSet"),
      List(pparam("Elem")),
      Type.Apply(pname("Set"), List(pname("Elem")))
    ))
  }

  test("opaque-soft-keyword") {
    runTestAssert[Stat]("enum Kind { case Type(opaque: Boolean, transparent: Boolean) }")(Defn.Enum(
      Nil,
      pname("Kind"),
      Nil,
      EmptyCtor(),
      tpl(Defn.EnumCase(
        Nil,
        tname("Type"),
        Nil,
        Ctor
          .Primary(Nil, anon, List(List(tparam("opaque", "Boolean"), tparam("transparent", "Boolean")))),
        Nil
      ))
    ))
  }

  test("capital-var-pattern-val") {
    runTestAssert[Stat](
      """val Private @ _ = flags()
        |""".stripMargin
    )(Defn.Val(
      Nil,
      List(Pat.Bind(Pat.Var(tname("Private")), Pat.Wildcard())),
      None,
      Term.Apply(tname("flags"), Nil)
    ))
  }

  test("capital-var-pattern-case") {
    runTestAssert[Stat](
      """|flags() match {
         |  case Pattern @ _ =>
         |}
         |""".stripMargin
    )(Term.Match(
      Term.Apply(tname("flags"), Nil),
      List(Case(Pat.Bind(Pat.Var(tname("Pattern")), Pat.Wildcard()), None, Term.Block(Nil)))
    ))
  }

  test("catch-end-def") {
    val layout = """|object X {
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
    )(Defn.Object(
      Nil,
      tname("X"),
      tpl(
        Defn.Def(
          Nil,
          tname("fx"),
          Nil,
          Nil,
          None,
          Term.Try(
            Term.Apply(tname("action"), Nil),
            List(Case(Pat.Var(tname("ex")), None, Term.Apply(tname("err"), Nil))),
            None
          )
        ),
        Defn.Class(List(Mod.Private(anon), Mod.Abstract()), pname("X"), Nil, ctorp(), tplNoBody())
      )
    ))
  }

  test("type-in-block") {
    runTestAssert[Stat](
      """|def hello = {
         |  type T
         |}
         |""".stripMargin
    )(Defn.Def(
      Nil,
      tname("hello"),
      Nil,
      Nil,
      None,
      Term.Block(List(Decl.Type(Nil, pname("T"), Nil, Type.Bounds(None, None))))
    ))
  }

  test("operator-next-line") {
    runTestAssert[Stat](
      """|val all = "-siteroot" +: "../docs"
         |    +: "-project" +:  Nil""".stripMargin,
      assertLayout = Some("""val all = "-siteroot" +: "../docs" +: "-project" +: Nil""")
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("all"))),
      None,
      Term.ApplyInfix(
        str("-siteroot"),
        tname("+:"),
        Nil,
        List(Term.ApplyInfix(
          str("../docs"),
          tname("+:"),
          Nil,
          List(Term.ApplyInfix(str("-project"), tname("+:"), Nil, List(tname("Nil"))))
        ))
      )
    ))
  }

  test("operator-next-line-bad-indent") {
    runTestAssert[Stat](
      """|def withClasspath =
         |       "-siteroot" +: "../docs"
         |    +: "-project" +: Nil
         |""".stripMargin,
      assertLayout = Some("""def withClasspath = "-siteroot" +: "../docs" +: "-project" +: Nil""")
    )(Defn.Def(
      Nil,
      tname("withClasspath"),
      Nil,
      Nil,
      None,
      Term.ApplyInfix(
        str("-siteroot"),
        tname("+:"),
        Nil,
        List(Term.ApplyInfix(
          str("../docs"),
          tname("+:"),
          Nil,
          List(Term.ApplyInfix(str("-project"), tname("+:"), Nil, List(tname("Nil"))))
        ))
      )
    ))
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
    )(Term.Match(
      tname("a"),
      List(Case(
        Pat.Extract(tname("List"), List(Pat.Bind(Pat.Var(tname("xs")), Pat.SeqWildcard()))),
        None,
        Term.Block(Nil)
      ))
    ))
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
    )(Term.Match(
      tname("a"),
      List(Case(
        Pat.Extract(tname("List"), List(Pat.Bind(Pat.Var(tname("xs")), Pat.SeqWildcard()))),
        None,
        Term.Block(Nil)
      ))
    ))
  }

  test("vararg-wildcard-postfix-star") {
    runTestAssert[Stat]("val lst = List(0, arr*)")(Defn.Val(
      Nil,
      List(Pat.Var(tname("lst"))),
      None,
      Term.Apply(tname("List"), List(int(0), Term.Repeated(tname("arr"))))
    ))
  }

  test("vararg-wildcard-like-postfix-star") {
    val tree = Defn.Val(
      Nil,
      List(Pat.Var(tname("lst"))),
      None,
      Term.Apply(tname("List"), List(lit(0), Term.Select(tname("arr"), tname("*"))))
    )
    runTestAssert[Stat]("val lst = List(0, arr`*`)", "val lst = List(0, arr.*)")(tree)
  }

  test("non-vararg-infix-star") {
    runTestAssert[Stat]("val lst = List(0, a * b)")(Defn.Val(
      Nil,
      List(Pat.Var(tname("lst"))),
      None,
      Term.Apply(
        tname("List"),
        List(int(0), Term.ApplyInfix(tname("a"), tname("*"), Nil, List(tname("b"))))
      )
    ))
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
    )(Term.Match(
      tname("a"),
      List(Case(Pat.Extract(tname("List"), List(Pat.Repeated(tname("xs")))), None, Term.Block(Nil))),
      Nil
    ))
  }

  test("vararg-wildcard-like-postfix-start-pat") {
    val error = """|<input>:1: error: bad simple pattern: use _* to match a sequence
                   |a match {case List(xs`*`) => }
                   |                        ^""".stripMargin
    runTestError[Stat]("a match {case List(xs`*`) => }", error)
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
    )(Defn.Class(List(Mod.Case()), pname("A"), Nil, ctorp(tparam("x", "X")), tplNoBody()))
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
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("base"))),
      None,
      Term
        .ApplyInfix(str(""), tname("++"), Nil, List(Term.Interpolate(tname("s"), List(str("")), Nil)))
    ))
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
    )(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      List(List(tparam("t", Type.Tuple(List(pname("String"), pname("String")))))),
      Some(pname("String")),
      Term.Match(
        tname("t"),
        List(Case(
          Pat.Typed(
            Pat.Tuple(List(Pat.Var(tname("m")), Pat.Wildcard())),
            Type.Tuple(List(pname("String"), pname("String")))
          ),
          None,
          tname("m")
        )),
        Nil
      )
    ))
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
    )(Term.Match(
      tname("s"),
      List(
        Case(
          Pat.Typed(Pat.Extract(tname("re"), List(Pat.Var(tname("v")))), pname("String")),
          None,
          Term.Select(tname("v"), tname("toDouble"))
        ),
        Case(Pat.Var(tname("other")), None, tname("o"))
      ),
      Nil
    ))
  }

  test("multi-pattern") {
    runTestAssert[Stat](
      """|val x * y = v
         |""".stripMargin
    )(Defn.Val(
      Nil,
      List(Pat.ExtractInfix(Pat.Var(tname("x")), tname("*"), List(Pat.Var(tname("y"))))),
      None,
      tname("v")
    ))
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
    )(Term.Match(
      tname("s"),
      List(
        Case(
          Pat.Typed(Pat.Typed(Pat.Var(tname("v")), pname("String")), pname("String")),
          None,
          Term.Select(tname("v"), tname("toDouble"))
        ),
        Case(Pat.Var(tname("other")), None, tname("o"))
      ),
      Nil
    ))
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
    )(Defn.Val(
      Nil,
      List(Pat.Var(tname("f"))),
      Some(Type.Function(
        List(pname("String")),
        Type.Apply(pname("PartialFunction"), List(pname("String"), pname("Int")))
      )),
      Term.Function(
        List(tparam("s")),
        Term
          .PartialFunction(List(Case(str("Hello"), None, int(5)), Case(str("Goodbye"), None, int(0))))
      )
    ))
  }

  test("underscore-placeholder") {
    implicit val dialect: Dialect = dialects.Scala3Future
    runTestAssert[Stat](
      """|object A:
         |  type X = +_ => Int
         |  type Y = -_ => Int
         |  type Z = _ => Int
         |""".stripMargin,
      assertLayout = Some(
        """|object A {
           |  type X = +_ => Int
           |  type Y = -_ => Int
           |  type Z = _ => Int
           |}
           |""".stripMargin
      )
    )(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Type(
          Nil,
          pname("X"),
          Nil,
          Type.Function(List(Type.AnonymousParam(Some(Mod.Covariant()))), pname("Int")),
          noBounds
        ),
        Defn.Type(
          Nil,
          pname("Y"),
          Nil,
          Type.Function(List(Type.AnonymousParam(Some(Mod.Contravariant()))), pname("Int")),
          noBounds
        ),
        Defn.Type(
          Nil,
          pname("Z"),
          Nil,
          Type.Function(List(Type.AnonymousParam(None)), pname("Int")),
          noBounds
        )
      )
    ))
  }

  test("type-param-last") {
    runTestAssert[Stat](
      """|def b2 = "".foo2(using foo)[Any]
         |""".stripMargin,
      assertLayout = Some(
        """|def b2 = "".foo2(using foo)[Any]
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("b2"),
      Nil,
      Nil,
      None,
      Term.ApplyType(
        Term.Apply(
          Term.Select(str(""), tname("foo2")),
          Term.ArgClause(List(tname("foo")), Some(Mod.Using()))
        ),
        List(pname("Any"))
      )
    ))
  }

  test("refinements") {
    runTestAssert[Stat](
      """|val x: (C { type U = T } { type T = String }) # U 
         |""".stripMargin,
      assertLayout = Some(
        """|val x: ((C {
           |  type U = T
           |}) {
           |  type T = String
           |})#U
           |""".stripMargin
      )
    )(Decl.Val(
      Nil,
      List(Pat.Var(tname("x"))),
      Type.Project(
        Type.Refine(
          Some(Type.Refine(
            Some(pname("C")),
            List(Defn.Type(Nil, pname("U"), Nil, pname("T"), Type.Bounds(None, None)))
          )),
          List(Defn.Type(Nil, pname("T"), Nil, pname("String"), Type.Bounds(None, None)))
        ),
        pname("U")
      )
    ))
  }

  test("issue-2506") {
    runTestAssert[Stat](
      """|??? match {
         |  case x2: ([V] => () => Int) => ???
         |}
         |""".stripMargin
    )(Term.Match(
      tname("???"),
      List(Case(
        Pat.Typed(
          Pat.Var(tname("x2")),
          Type.PolyFunction(List(pparam("V")), Type.Function(Nil, pname("Int")))
        ),
        None,
        tname("???")
      )),
      Nil
    ))
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
    )(Source(List(
      Import(
        Importer(
          Term.Select(
            Term.Select(
              Term.Select(Term.Select(tname("_root_com"), tname("olegych")), tname("scastie")),
              tname("api")
            ),
            tname("runtime")
          ),
          List(Importee.Wildcard())
        ) :: Nil
      ),
      Defn.Object(
        Nil,
        tname("Playground"),
        tpl(
          List(Init(pname("ScastieApp"), anon, emptyArgClause)),
          List(Term.Apply(
            Term.Select(Term.Apply(tname("List"), List(int(1), int(2), int(3))), tname("map")),
            Term.Block(
              Term.Function(tparam(List(Mod.Using()), "i", "Int") :: Nil, tname("i")) :: Nil
            ) :: Nil
          ))
        )
      )
    )))
  }

  test("#2727-newline-macro") {

    runTestAssert[Stat](
      """|implicit def generate[T](value: T): Clue[T] =
         |  macro MacroCompatScala2.clueImpl""".stripMargin,
      assertLayout =
        Some("implicit def generate[T](value: T): Clue[T] = macro MacroCompatScala2.clueImpl")
    )(Defn.Macro(
      List(Mod.Implicit()),
      tname("generate"),
      List(pparam("T")),
      List(List(tparam("value", "T"))),
      Some(Type.Apply(pname("Clue"), List(pname("T")))),
      Term.Select(tname("MacroCompatScala2"), tname("clueImpl"))
    ))
  }

  test("kleisli") {
    runTestAssert[Stat]("new (Kleisli[F, Span[F], *] ~> F) {}")(Term.NewAnonymous(tpl(
      Init(
        Type.AnonymousLambda(Type.ApplyInfix(
          Type.Apply(
            pname("Kleisli"),
            List(pname("F"), Type.Apply(pname("Span"), List(pname("F"))), Type.AnonymousParam(None))
          ),
          pname("~>"),
          pname("F")
        )),
        anon,
        emptyArgClause
      ) :: Nil,
      Nil
    )))
  }

  test("class Baz1 @deprecated(implicit c: C)") {
    runTestAssert[Stat](
      "class Baz1 @deprecated(implicit c: C)",
      Some("class Baz1 @deprecated (implicit c: C)")
    )(Defn.Class(
      Nil,
      pname("Baz1"),
      Nil,
      Ctor.Primary(
        List(Mod.Annot(init("deprecated"))),
        anon,
        List(List(tparam(List(Mod.Implicit()), "c", "C")))
      ),
      tplNoBody()
    ))
  }

  test("class Baz1 @deprecated(c: C)") {
    runTestAssert[Stat]("class Baz1 @deprecated(c: C)", Some("class Baz1 @deprecated (c: C)"))(
      Defn.Class(
        Nil,
        pname("Baz1"),
        Nil,
        Ctor.Primary(List(Mod.Annot(init("deprecated"))), anon, List(List(tparam("c", "C")))),
        tplNoBody()
      )
    )
  }

  test("class Baz1 @deprecated(c: C = some)") {
    runTestAssert[Stat](
      "class Baz1 @deprecated(c: C = some)",
      Some("class Baz1 @deprecated (c: C = some)")
    )(Defn.Class(
      Nil,
      pname("Baz1"),
      Nil,
      Ctor.Primary(
        List(Mod.Annot(init("deprecated"))),
        anon,
        List(List(Term.Param(Nil, tname("c"), Some(pname("C")), Some(tname("some")))))
      ),
      tplNoBody()
    ))
  }

  test("class Baz1 @deprecated(foo)(c: C)") {
    runTestAssert[Stat](
      "class Baz1 @deprecated(foo)(c: C)",
      Some("class Baz1 @deprecated(foo) (c: C)")
    )(Defn.Class(
      Nil,
      pname("Baz1"),
      Nil,
      Ctor.Primary(
        List(Mod.Annot(Init(pname("deprecated"), anon, List(List(tname("foo")))))),
        anon,
        List(List(tparam("c", "C")))
      ),
      tplNoBody()
    ))
  }

  test("expr with annotation, then match") {
    val code = """|underlyingStableClassRef(mbr.info.loBound): @unchecked match {
                  |  case ref: TypeRef =>
                  |}""".stripMargin
    val layout = """|(underlyingStableClassRef(mbr.info.loBound): @unchecked) match {
                    |  case ref: TypeRef =>
                    |}""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Term.Match(
      Term.Annotate(
        Term.Apply(
          tname("underlyingStableClassRef"),
          List(Term.Select(Term.Select(tname("mbr"), tname("info")), tname("loBound")))
        ),
        List(Mod.Annot(Init(pname("unchecked"), anon, emptyArgClause)))
      ),
      List(Case(Pat.Typed(Pat.Var(tname("ref")), pname("TypeRef")), None, Term.Block(Nil))),
      Nil
    ))
  }

  test("match on array-of-wildcard") {
    val code = """|obj match { case arr: Array[Array[_]] => }
                  |""".stripMargin
    val layout = """|obj match {
                    |  case arr: Array[Array[_]] =>
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Term.Match(
      tname("obj"),
      Case(
        Pat.Typed(
          Pat.Var(tname("arr")),
          Type
            .Apply(pname("Array"), List(Type.Apply(pname("Array"), List(Type.Wildcard(noBounds)))))
        ),
        None,
        Term.Block(Nil)
      ) :: Nil,
      Nil
    ))
  }

  test("apply with arguments of various complexity") {
    val code = """|sc.submitJob(
                  |  rdd,
                  |  (iter: Iterator[Int]) => iter.toArray,
                  |  partitions.getOrElse(rdd.partitions.indices),
                  |  { case (_, _) => return }: (Int, Array[Int]) => Unit,
                  |  { return }
                  |)""".stripMargin
    val layout =
      """|sc.submitJob(rdd, (iter: Iterator[Int]) => iter.toArray, partitions.getOrElse(rdd.partitions.indices), {
         |  case (_, _) =>
         |    return
         |}: (Int, Array[Int]) => Unit, {
         |  return
         |})
         |""".stripMargin
    val tree = Term.Apply(
      Term.Select(tname("sc"), tname("submitJob")),
      List(
        tname("rdd"),
        Term.Function(
          List(tparam("iter", Type.Apply(pname("Iterator"), List(pname("Int"))))),
          Term.Select(tname("iter"), tname("toArray"))
        ),
        Term.Apply(
          Term.Select(tname("partitions"), tname("getOrElse")),
          List(Term.Select(Term.Select(tname("rdd"), tname("partitions")), tname("indices")))
        ),
        Term.Ascribe(
          Term.PartialFunction(List(
            Case(Pat.Tuple(List(Pat.Wildcard(), Pat.Wildcard())), None, Term.Return(Lit.Unit()))
          )),
          Type.Function(
            List(pname("Int"), Type.Apply(pname("Array"), List(pname("Int")))),
            pname("Unit")
          )
        ),
        Term.Block(List(Term.Return(Lit.Unit())))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

}
