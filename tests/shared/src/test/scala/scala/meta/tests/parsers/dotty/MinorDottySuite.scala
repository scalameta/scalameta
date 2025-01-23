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
      ctorp(tparam(List(Mod.ValParam()), "open", "Int")),
      tpl(
        Defn.Val(Nil, List(patvar("price")), None, tapply(tname("OHLCPrice"), int(1))),
        Defn.Val(Nil, List(patvar("price1")), None, tselect("price", "open")),
        Defn.Val(Nil, List(patvar("price2")), None, int(1))
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
      blk(tname("start"), tapply(tname("open"), tname("p")), tname("end"))
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
      bounds(hi = pinfix("A", "&", pname("B")))
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
      tpl(Defn.Type(List(Mod.Private(anon), Mod.Opaque()), pname("T"), Nil, papply("List", "Int")))
    ))
    runTestAssert[Stat]("object X { opaque private type T = List[Int] }")(Defn.Object(
      Nil,
      tname("X"),
      tpl(Defn.Type(List(Mod.Opaque(), Mod.Private(anon)), pname("T"), Nil, papply("List", "Int")))
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
    runTestAssert[Stat]("trait Foo[T: Eq]")(
      Defn
        .Trait(Nil, pname("Foo"), List(pparam("T", bounds(cb = List(pname("Eq"))))), ctor, tplNoBody())
    )
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
    runTestAssert[Stat]("(new A(), new B())")(Term.Tuple(
      List(Term.New(init("A", Nil)), Term.New(init("B", Nil)))
    ))
  }

  test("new A(using b)(c)(using d, e)") {
    runTestAssert[Stat]("new A(using b)(c)(using d, e)", Some("new A(using b)(c)(using d, e)"))(
      Term.New(init(
        pname("A"),
        Term.ArgClause(List(tname("b")), Some(Mod.Using())),
        Term.ArgClause(List(tname("c")), None),
        Term.ArgClause(List(tname("d"), tname("e")), Some(Mod.Using()))
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
    runTestAssert[Stat]("val stat: Tree[? >: Untyped]")(
      Decl.Val(Nil, List(patvar("stat")), papply("Tree", pwildcard(loBound("Untyped"))))
    )
  }

  test("question-type-like") {
    val treeWithoutBounds = Decl.Val(Nil, List(patvar("stat")), papply("Tree", pname("?")))
    runTestAssert[Stat]("val stat: Tree[`?`]")(treeWithoutBounds)
    val errorWithBounds =
      """|<input>:1: error: `]` expected but `>:` found
         |val stat: Tree[`?` >: Untyped]
         |                   ^""".stripMargin
    runTestError[Stat]("val stat: Tree[`?` >: Untyped]", errorWithBounds)
  }

  test("lazy-val-toplevel") {
    runTestAssert[Source]("lazy val x = 3")(Source(
      List(Defn.Val(List(Mod.Lazy()), List(patvar("x")), None, int(3)))
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
          tapply(tname("println"), str("hello")),
          tname("???"),
          tmatch(tname("???"), Case(int(0), None, int(1)))
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
      "given [_](using Ord[T]): Ord[List[T]] with {}",
      """|<input>:1: error: `identifier` expected but `[` found
         |given [_](using Ord[T]): Ord[List[T]] with {}
         |      ^""".stripMargin
    )
  }

  test("repeated-byname-class-parameter") {
    runTestAssert[Stat]("class Foo(bars: => Int*)", "class Foo(bars: => (Int*))")(Defn.Class(
      Nil,
      pname("Foo"),
      Nil,
      ctorp(tparam("bars", Type.ByName(Type.Repeated(pname("Int"))))),
      tplNoBody()
    ))

    runTestAssert[Stat]("def fx(x: => Int*): Int = 3", "def fx(x: => (Int*)): Int = 3")(Defn.Def(
      Nil,
      tname("fx"),
      Nil,
      List(List(tparam("x", Type.ByName(Type.Repeated(pname("Int")))))),
      Some(pname("Int")),
      int(3)
    ))
  }

  test("repeated-like-class-parameter") {
    val error =
      """|<input>:1: error: `identifier` expected but `)` found
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
      tpl(
        Decl.Val(List(Mod.Protected(Term.This(anon)), Mod.Lazy()), List(patvar("from")), pname("Int"))
      )
    ))
  }

  test("type-wildcard-questionmark") {
    runTestAssert[Stat]("val x: List[?] = List(1)")(
      Defn.Val(Nil, List(patvar("x")), Some(papply("List", pwildcard)), tapply(tname("List"), int(1)))
    )

    runTestAssert[Stat]("def x(a: List[?]): Unit = ()")(Defn.Def(
      Nil,
      tname("x"),
      Nil,
      List(List(tparam(Nil, "a", papply("List", pwildcard)))),
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
      List(patinfix(patvar("a"), "::", tname("Nil"))),
      Some(Type.Annotate(Type.AnonymousName(), List(Mod.Annot(init("unchecked"))))),
      tname("args")
    ))

    runTestAssert[Stat]("val x:  @annotation.switch = 2")(Defn.Val(
      Nil,
      List(patvar("x")),
      Some(Type.Annotate(Type.AnonymousName(), List(Mod.Annot(init(pselect("annotation", "switch")))))),
      int(2)
    ))
  }

  val patternBinding = Term.Match(int(1), List(Case(Pat.Bind(patvar("intValue"), int(1)), None, blk())))

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
    )(Defn.Type(List(Mod.Opaque()), pname("LinearSet"), List(pparam("Elem")), papply("Set", "Elem")))
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
    )(Defn.Val(Nil, List(Pat.Bind(patvar("Private"), patwildcard)), None, tapply(tname("flags"))))
  }

  test("capital-var-pattern-case") {
    runTestAssert[Stat](
      """|flags() match {
         |  case Pattern @ _ =>
         |}
         |""".stripMargin
    )(tmatch(tapply(tname("flags")), Case(Pat.Bind(patvar("Pattern"), patwildcard), None, blk())))
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
          Term
            .Try(tapply(tname("action")), List(Case(patvar("ex"), None, tapply(tname("err")))), None)
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
    )(Defn.Def(Nil, tname("hello"), Nil, Nil, None, blk(Decl.Type(Nil, pname("T"), Nil, noBounds))))
  }

  test("operator-next-line") {
    runTestAssert[Stat](
      """|val all = "-siteroot" +: "../docs"
         |    +: "-project" +:  Nil""".stripMargin,
      assertLayout = Some("""val all = "-siteroot" +: "../docs" +: "-project" +: Nil""")
    )(Defn.Val(
      Nil,
      List(patvar("all")),
      None,
      tinfix(
        str("-siteroot"),
        "+:",
        tinfix(str("../docs"), "+:", tinfix(str("-project"), "+:", tname("Nil")))
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
      tinfix(
        str("-siteroot"),
        "+:",
        tinfix(str("../docs"), "+:", tinfix(str("-project"), "+:", tname("Nil")))
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
    )(tmatch(
      tname("a"),
      Case(Pat.Extract(tname("List"), List(Pat.Bind(patvar("xs"), Pat.SeqWildcard()))), None, blk())
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
    )(tmatch(
      tname("a"),
      Case(Pat.Extract(tname("List"), List(Pat.Bind(patvar("xs"), Pat.SeqWildcard()))), None, blk())
    ))
  }

  test("vararg-wildcard-postfix-star") {
    runTestAssert[Stat]("val lst = List(0, arr*)")(
      Defn
        .Val(Nil, List(patvar("lst")), None, tapply(tname("List"), int(0), Term.Repeated(tname("arr"))))
    )
  }

  test("vararg-wildcard-like-postfix-star") {
    val codeOriginal = "val lst = List(0, arr`*`)"
    val codeObtained = "val lst = List(0, arr `*`)"
    runTestAssert[Stat](codeOriginal, codeObtained)(
      Defn.Val(Nil, List(patvar("lst")), None, tapply("List", lit(0), tpostfix("arr", "*")))
    )
  }

  test("non-vararg-infix-star") {
    runTestAssert[Stat]("val lst = List(0, a * b)")(Defn.Val(
      Nil,
      List(patvar("lst")),
      None,
      tapply(tname("List"), int(0), tinfix(tname("a"), "*", tname("b")))
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
    )(tmatch(tname("a"), Case(Pat.Extract(tname("List"), List(Pat.Repeated(tname("xs")))), None, blk())))
  }

  test("vararg-wildcard-like-postfix-start-pat") {
    val error =
      """|<input>:1: error: bad simple pattern: use _* to match a sequence
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
      List(patvar("base")),
      None,
      tinfix(str(""), "++", Term.Interpolate(tname("s"), List(str("")), Nil))
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
      tmatch(
        tname("t"),
        Case(
          Pat.Typed(
            Pat.Tuple(List(patvar("m"), patwildcard)),
            Type.Tuple(List(pname("String"), pname("String")))
          ),
          None,
          tname("m")
        )
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
    )(tmatch(
      tname("s"),
      Case(
        Pat.Typed(Pat.Extract(tname("re"), List(patvar("v"))), pname("String")),
        None,
        tselect("v", "toDouble")
      ),
      Case(patvar("other"), None, tname("o"))
    ))
  }

  test("multi-pattern") {
    runTestAssert[Stat](
      """|val x * y = v
         |""".stripMargin
    )(Defn.Val(Nil, List(patinfix(patvar("x"), "*", patvar("y"))), None, tname("v")))
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
    )(tmatch(
      tname("s"),
      Case(
        Pat.Typed(Pat.Typed(patvar("v"), pname("String")), pname("String")),
        None,
        tselect("v", "toDouble")
      ),
      Case(patvar("other"), None, tname("o"))
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
      List(patvar("f")),
      Some(pfunc(pname("String"))(papply("PartialFunction", "String", "Int"))),
      tfunc(
        tparam("s")
      )(Term.PartialFunction(List(Case(str("Hello"), None, int(5)), Case(str("Goodbye"), None, int(0)))))
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
          pfunc(Type.AnonymousParam(Some(Mod.Covariant())))(pname("Int")),
          noBounds
        ),
        Defn.Type(
          Nil,
          pname("Y"),
          Nil,
          pfunc(Type.AnonymousParam(Some(Mod.Contravariant())))(pname("Int")),
          noBounds
        ),
        Defn.Type(Nil, pname("Z"), Nil, pfunc(Type.AnonymousParam(None))(pname("Int")), noBounds)
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
      tapplytype(tapplyUsing(tselect(str(""), "foo2"), tname("foo")), pname("Any"))
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
      List(patvar("x")),
      Type.Project(
        Type.Refine(
          Some(
            Type.Refine(Some(pname("C")), List(Defn.Type(Nil, pname("U"), Nil, pname("T"), noBounds)))
          ),
          List(Defn.Type(Nil, pname("T"), Nil, pname("String"), noBounds))
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
    )(tmatch(
      tname("???"),
      Case(Pat.Typed(patvar("x2"), ppolyfunc(pparam("V"))(pfunc()(pname("Int")))), None, tname("???"))
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
          tselect("_root_com", "olegych", "scastie", "api", "runtime"),
          List(Importee.Wildcard())
        ) :: Nil
      ),
      Defn.Object(
        Nil,
        tname("Playground"),
        tpl(
          List(init("ScastieApp")),
          List(tapply(
            tselect(tapply(tname("List"), int(1), int(2), int(3)), "map"),
            blk(tfunc(tparam(List(Mod.Using()), "i", "Int"))(tname("i")))
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
      Some(papply("Clue", "T")),
      tselect("MacroCompatScala2", "clueImpl")
    ))
  }

  test("kleisli") {
    runTestAssert[Stat]("new (Kleisli[F, Span[F], *] ~> F) {}")(Term.NewAnonymous(tpl(
      init(Type.AnonymousLambda(pinfix(
        papply("Kleisli", "F", papply("Span", "F"), Type.AnonymousParam(None)),
        "~>",
        pname("F")
      ))) :: Nil,
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
        List(Mod.Annot(init("deprecated", List(tname("foo"))))),
        anon,
        List(List(tparam("c", "C")))
      ),
      tplNoBody()
    ))
  }

  test("expr with annotation, then match") {
    val code =
      """|underlyingStableClassRef(mbr.info.loBound): @unchecked match {
         |  case ref: TypeRef =>
         |}""".stripMargin
    val layout =
      """|(underlyingStableClassRef(mbr.info.loBound): @unchecked) match {
         |  case ref: TypeRef =>
         |}""".stripMargin
    runTestAssert[Stat](code, Some(layout))(tmatch(
      Term.Annotate(
        tapply(tname("underlyingStableClassRef"), tselect("mbr", "info", "loBound")),
        List(Mod.Annot(init("unchecked")))
      ),
      Case(Pat.Typed(patvar("ref"), pname("TypeRef")), None, blk())
    ))
  }

  test("match on array-of-wildcard") {
    val code =
      """|obj match { case arr: Array[Array[_]] => }
         |""".stripMargin
    val layout =
      """|obj match {
         |  case arr: Array[Array[_]] =>
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(tmatch(
      tname("obj"),
      Case(Pat.Typed(patvar("arr"), papply(pname("Array"), papply("Array", pwildcard))), None, blk())
    ))
  }

  test("apply with arguments of various complexity") {
    val code =
      """|sc.submitJob(
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
    val tree = tapply(
      tselect("sc", "submitJob"),
      tname("rdd"),
      tfunc(tparam("iter", papply("Iterator", "Int")))(tselect("iter", "toArray")),
      tapply(tselect("partitions", "getOrElse"), tselect("rdd", "partitions", "indices")),
      Term.Ascribe(
        Term.PartialFunction(List(
          Case(Pat.Tuple(List(patwildcard, patwildcard)), None, Term.Return(Lit.Unit()))
        )),
        pfunc(pname("Int"), papply("Array", "Int"))(pname("Unit"))
      ),
      blk(Term.Return(Lit.Unit()))
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

}
