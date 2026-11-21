package scala.meta.tests.parsers.dotty

import scala.meta._

class SignificantIndentationSuite extends BaseDottySuite {

  val defx = Decl.Def(Nil, tname("f"), Nil, Nil, pname("Int"))
  val defy = Defn.Def(
    Nil,
    tname("y"),
    Nil,
    Nil,
    Some(pname("String")),
    blk(tapply(tname("fa")), tapply(tname("fb"))),
  )

  test("basic-example") {
    val code =
      """|trait A:
         |  def f: Int
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("trait A { def f: Int }"))(
      Defn.Trait(Nil, pname("A"), Nil, EmptyCtor(), tpl(defx)),
    )
  }

  test("package-comment") {
    val code =
      """|package mysadpackage:
         |  def f: Int
         |package anotherpackage:
         |  def f: Int
         |""".stripMargin
    runTestAssert[Source](
      code,
      assertLayout = Some(
        """|package mysadpackage {
           |  def f: Int
           |}
           |package anotherpackage {
           |  def f: Int
           |}
           |""".stripMargin,
      ),
    )(Source(List(
      Pkg(tname("mysadpackage"), List(Decl.Def(Nil, tname("f"), Nil, Nil, pname("Int")))),
      Pkg(tname("anotherpackage"), List(Decl.Def(Nil, tname("f"), Nil, Nil, pname("Int")))),
    )))
  }

  test("multistat-example") {
    val code =
      """|trait A:
         |  def f: Int
         |  def y: String = { fa(); fb() }""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Trait(Nil, pname("A"), Nil, EmptyCtor(), tpl(defx, defy)),
    )
  }

  test("anonymous-class") {
    val code =
      """|new A:
         |  def f: Int
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("new A { def f: Int }"))(Term.NewAnonymous(
      tpl(List(init("A")), List(Decl.Def(Nil, tname("f"), Nil, Nil, pname("Int")))),
    ))
  }

  // https://github.com/scalameta/scalameta/issues/3093
  test("anonymous-class-in-block") {
    val code =
      """|if(x) {
         |  new A:
         |    def f: Int
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(Term.If(
      tname("x"),
      blk(Term.NewAnonymous(tpl(List(init("A")), List(Decl.Def(Nil, tname("f"), Nil, pname("Int")))))),
      Lit.Unit(),
      Nil,
    ))
  }

  test("empty-anonymous-class") {
    val code =
      """|new:
         |  def f: Int
         |  
         |  def g: Int
         |""".stripMargin
    runTestAssert[Stat](
      code,
      assertLayout = Some(
        """|new {
           |  def f: Int
           |  def g: Int
           |}
           |""".stripMargin,
      ),
    )(Term.NewAnonymous(tpl(
      Decl.Def(Nil, tname("f"), Nil, Nil, pname("Int")),
      Decl.Def(Nil, tname("g"), Nil, Nil, pname("Int")),
    )))
  }

  test("indent-and-back") {
    val code =
      """|object O:
         |  class C:
         |    def f: Int = 3
         |  trait T:
         |    def f: Int = 4
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Class(
          Nil,
          pname("C"),
          Nil,
          ctor,
          tpl(Defn.Def(Nil, tname("f"), Nil, Nil, Some(pname("Int")), int(3))),
        ),
        Defn.Trait(
          Nil,
          pname("T"),
          Nil,
          EmptyCtor(),
          tpl(Defn.Def(Nil, tname("f"), Nil, Nil, Some(pname("Int")), int(4))),
        ),
      ),
    ))
  }

  test("then-no-indent") {
    // this test is related to dotty issue: https://github.com/lampepfl/dotty/issues/9790
    // It should either assert error during parsing: "illegal start of simple expression"
    // Or accept mismatch with parsing rules and parse as 'if (cond) { truep } else {falsep }'
    // Why error is thrown is described in mentioned issue.
    val code =
      """|def fn: Unit =
         |    if cond then
         |  truep
         |    else
         |  falsep
         |    otherStatement()
         |""".stripMargin
    runTestAssert[Stat](
      code,
      assertLayout = Some(
        """|def fn: Unit = {
           |  if (cond) truep else falsep
           |  otherStatement()
           |}
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("fn"),
      Nil,
      Nil,
      Some(pname("Unit")),
      blk(Term.If(tname("cond"), tname("truep"), tname("falsep")), tapply(tname("otherStatement"))),
    ))
  }

  test("then-no-indent-wrong") {
    // this test is related to dotty issue: https://github.com/lampepfl/dotty/issues/9790
    // It should either assert error during parsing: "illegal start of simple expression"
    // Or accept mismatch with parsing rules and parse as 'if (cond) { truep } else {falsep }'
    // Why error is thrown is described in mentioned issue.
    val code =
      """|def fn: Unit =
         |    if cond then
         |  truep1
         |  truep2
         |    else
         |  falsep
         |""".stripMargin
    runTestError[Stat](code, "`;` expected but `else` found")
  }

  test("indent-inside-brace-ok") {
    val code =
      """|class X {
         |  def fx(): Unit =
         |    f1
         |    f2
         |}
         |""".stripMargin
    val output =
      """|class X {
         |  def fx(): Unit = {
         |    f1
         |    f2
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Class(
      Nil,
      pname("X"),
      Nil,
      EmptyCtor(),
      tpl(
        Defn
          .Def(Nil, tname("fx"), Nil, List(List()), Some(pname("Unit")), blk(tname("f1"), tname("f2"))),
      ),
    ))
  }

  test("indent-inside-brace-not") {
    val code =
      """|class X {
         |      def fx(): Unit =
         |    f1
         |    f2
         |}
         |""".stripMargin
    val output =
      """|class X {
         |  def fx(): Unit = f1
         |  f2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Class(
      Nil,
      pname("X"),
      Nil,
      EmptyCtor(),
      tpl(
        Defn.Def(Nil, tname("fx"), Nil, List(List()), Some(pname("Unit")), tname("f1")),
        tname("f2"),
      ),
    ))
  }

  test("should-indent-yet-brace") {
    val code =
      """|class X {
         |  def fx(): Unit =
         |  {
         |
         |  }
         |  private def f2: Int = 1
         |}
         |""".stripMargin
    val output =
      """|class X {
         |  def fx(): Unit = {}
         |  private def f2: Int = 1
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Class(
      Nil,
      pname("X"),
      Nil,
      EmptyCtor(),
      tpl(
        Defn.Def(Nil, tname("fx"), Nil, List(List()), Some(pname("Unit")), blk()),
        Defn.Def(List(Mod.Private(anon)), tname("f2"), Nil, Nil, Some(pname("Int")), int(1)),
      ),
    ))
  }

  test("selftype-class") {
    val code =
      """|class A extends B:
         |  thisPhase =>
         |  expr1
         |  expr2
         |""".stripMargin
    val output =
      """|class A extends B { thisPhase =>
         |  expr1
         |  expr2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(List(init("B")), self("thisPhase"), tname("expr1"), tname("expr2")),
    ))
  }

  test("lambda-method") {
    val code =
      """|object X:
         |  val fn = (pa, pb) =>
         |    def helper = 3
         |    3
         |  end fn
         |""".stripMargin
    val output =
      """|object X {
         |  val fn = (pa, pb) => {
         |    def helper = 3
         |    3
         |  }
         |  end fn
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Object(
      Nil,
      tname("X"),
      tpl(
        Defn.Val(
          Nil,
          List(patvar("fn")),
          None,
          tfunc(tparam("pa"), tparam("pb"))(
            blk(Defn.Def(Nil, tname("helper"), Nil, Nil, None, int(3)), int(3)),
          ),
        ),
        Term.EndMarker(tname("fn")),
      ),
    ))
  }

  test("case-for-in-match") {
    val code =
      """|class A {
         |    def forward: Unit = parents match
         |      case a =>
         |        for case a: TP <- body do
         |          fordo
         |      case b => ok
         |
         |    private def transformAnnot: Tree
         |}
         |""".stripMargin
    val output =
      """|class A {
         |  def forward: Unit = parents match {
         |    case a =>
         |      for ( case a: TP <- body) fordo
         |    case b =>
         |      ok
         |  }
         |  private def transformAnnot: Tree
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      ctor,
      tpl(
        Defn.Def(
          Nil,
          tname("forward"),
          Nil,
          Nil,
          Some(pname("Unit")),
          tmatch(
            tname("parents"),
            Case(
              patvar("a"),
              None,
              Term.For(
                List(Enumerator.CaseGenerator(Pat.Typed(patvar("a"), pname("TP")), tname("body"))),
                tname("fordo"),
              ),
            ),
            Case(patvar("b"), None, tname("ok")),
          ),
        ),
        Decl.Def(List(Mod.Private(anon)), tname("transformAnnot"), Nil, Nil, pname("Tree")),
      ),
    ))
  }

  test("this-constructor") {
    val code =
      """|class A:
         |  def this(msg: String) =
         |    this(message, false)
         |""".stripMargin
    val output = "class A { def this(msg: String) = this(message, false) }"
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(Ctor.Secondary(
        Nil,
        Name.This(),
        List(List(tparam("msg", "String"))),
        init(Type.Singleton(Term.This(anon)), List(tname("message"), bool(false))),
        Nil,
      )),
    ))
  }

  test("this-constructor-indented-block") {
    val code =
      """|class A:
         |  def this(msg: String) =
         |    this(message, false)
         |    otherStat
         |""".stripMargin
    val output =
      """|class A {
         |  def this(msg: String) = {
         |    this(message, false)
         |    otherStat
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(Ctor.Secondary(
        Nil,
        Name.This(),
        List(List(tparam("msg", "String"))),
        init(Type.Singleton(Term.This(anon)), List(tname("message"), bool(false))),
        List(tname("otherStat")),
      )),
    ))
  }

  test("indent-equals") {
    runTestAssert[Stat](
      """|def genApply() = {
         |      app match {
         |        case Apply2() =>
         |          generatedType =
         |            genTypeApply(t)
         |
         |        case Apply() =>
         |      }
         |} 
         |""".stripMargin,
      assertLayout = Some(
        """|def genApply() = {
           |  app match {
           |    case Apply2() =>
           |      generatedType = genTypeApply(t)
           |    case Apply() =>
           |  }
           |}
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("genApply"),
      Nil,
      List(List()),
      None,
      blk(tmatch(
        tname("app"),
        Case(
          Pat.Extract(tname("Apply2"), Nil),
          None,
          Term.Assign(tname("generatedType"), tapply(tname("genTypeApply"), tname("t"))),
        ),
        Case(Pat.Extract(tname("Apply"), Nil), None, blk()),
      )),
    ))
  }

  test("outdent-with-prev-check") {
    runTestAssert[Stat](
      """|def wrapPlaceholders(t: Tree) = try
         |    if (placeholderParams.isEmpty) t
         |    else new WildcardFunction(placeholderParams.reverse, t)
         |  finally placeholderParams = saved
         |""".stripMargin,
      assertLayout =
        Some("def wrapPlaceholders(t: Tree) = try if (placeholderParams.isEmpty) t else new WildcardFunction(placeholderParams.reverse, t) finally placeholderParams = saved"),
    )(Defn.Def(
      Nil,
      tname("wrapPlaceholders"),
      Nil,
      List(List(tparam("t", "Tree"))),
      None,
      Term.Try(
        Term.If(
          tselect("placeholderParams", "isEmpty"),
          tname("t"),
          Term.New(
            init(pname("WildcardFunction"), List(tselect("placeholderParams", "reverse"), tname("t"))),
          ),
        ),
        Nil,
        Some(Term.Assign(tname("placeholderParams"), tname("saved"))),
      ),
    ))
  }

  test("type-in-next-line") {
    runTestAssert[Stat](
      """|abstract class Documentation{
         |
         |  class Graph {
         |    type Node = Int
         |    val a:
         |      Int = 3
         |  }
         |
         |  val refinementTest:
         |    Graph {
         |      def x: Int
         |    }
         |
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|abstract class Documentation {
           |  class Graph {
           |    type Node = Int
           |    val a: Int = 3
           |  }
           |  val refinementTest: Graph {
           |    def x: Int
           |  }
           |}
           |""".stripMargin,
      ),
    )(Defn.Class(
      List(Mod.Abstract()),
      pname("Documentation"),
      Nil,
      EmptyCtor(),
      tpl(
        Defn.Class(
          Nil,
          pname("Graph"),
          Nil,
          EmptyCtor(),
          tpl(
            Defn.Type(Nil, pname("Node"), Nil, pname("Int")),
            Defn.Val(Nil, List(patvar("a")), Some(pname("Int")), int(3)),
          ),
        ),
        Decl.Val(
          Nil,
          List(patvar("refinementTest")),
          Type.Refine(Some(pname("Graph")), List(Decl.Def(Nil, tname("x"), Nil, Nil, pname("Int")))),
        ),
      ),
    ))
  }

  test("type-in-next-line-equals")(
    runTestAssert[Stat](
      """|val refinementTest:
         |      Int = 3
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = 3"),
    )(Defn.Val(Nil, List(patvar("refinementTest")), Some(pname("Int")), int(3))),
  )

  test("type-in-next-line-equals-newline")(
    runTestAssert[Stat](
      """|val refinementTest:
         |      Int = 
         |3
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = 3"),
    )(Defn.Val(Nil, List(patvar("refinementTest")), Some(pname("Int")), int(3))),
  )

  test("type-equals-separate")(
    runTestAssert[Stat](
      """|def refinementTest(a :
         |      Int = 
         |3) = a
         |""".stripMargin,
      assertLayout = Some("def refinementTest(a: Int = 3) = a"),
    )(Defn.Def(
      Nil,
      tname("refinementTest"),
      Nil,
      List(List(Term.Param(Nil, tname("a"), Some(pname("Int")), Some(int(3))))),
      None,
      tname("a"),
    )),
  )

  test("type-multi-seq")(
    runTestAssert[Stat](
      """|val refinementTest:
         |    Int = 
         |  fx
         |  fy
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = {\n  fx\n  fy\n}"),
    )(Defn.Val(Nil, List(patvar("refinementTest")), Some(pname("Int")), blk(tname("fx"), tname("fy")))),
  )

  test("equals-block")(
    runTestAssert[Stat](
      """|val refinementTest:
         |  Int = 
         |    fx
         |    fy
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = {\n  fx\n  fy\n}"),
    )(Defn.Val(Nil, List(patvar("refinementTest")), Some(pname("Int")), blk(tname("fx"), tname("fy")))),
  )

  test("given-block-indent") {
    runTestAssert[Stat](
      """|given intOrd: Ord[Int] with
         |   def fa: Int = 1
         |   def fb: Int = 2
         |""".stripMargin,
      assertLayout = Some(
        """|given intOrd: Ord[Int] with {
           |  def fa: Int = 1
           |  def fb: Int = 2
           |}""".stripMargin,
      ),
    )(Defn.Given(
      Nil,
      tname("intOrd"),
      Nil,
      Nil,
      tpl(
        List(init(papply("Ord", "Int"))),
        List(
          Defn.Def(Nil, tname("fa"), Nil, Nil, Some(pname("Int")), int(1)),
          Defn.Def(Nil, tname("fb"), Nil, Nil, Some(pname("Int")), int(2)),
        ),
      ),
    ))
  }

  test("given-block-indent-edge-cases: `with` and no-indent")(runTestError[Stat](
    """|given intOrd: Ord[Int] with
       |def fa: Int = 1
       |def fb: Int = 2
       |""".stripMargin,
    """|<input>:2: error: `identifier` expected but `def` found
       |def fa: Int = 1
       |^""".stripMargin,
  ))

  test("given-block-indent-edge-cases: coloneol") {
    runTestAssert[Stat](
      """|given intOrd: Ord[Int]:
         |  def fa: Int = 1
         |  def fb: Int = 2
         |""".stripMargin,
      """|given intOrd: Ord[Int] {
         |  def fa: Int = 1
         |  def fb: Int = 2
         |}
         |""".stripMargin,
    )(Defn.Given(
      Nil,
      "intOrd",
      Nil,
      tpl(
        List(init(papply("Ord", "Int"))),
        List(
          Defn.Def(Nil, "fa", Nil, Some("Int"), lit(1)),
          Defn.Def(Nil, "fb", Nil, Some("Int"), lit(2)),
        ),
      ),
    ))
  }

  test("given-block-indent-edge-cases: coloneol [scala35]") {
    implicit val dialect: Dialect = dialects.Scala35
    runTestError[Stat](
      """|given intOrd: Ord[Int]:
         |  def fa: Int = 1
         |  def fb: Int = 2
         |""".stripMargin,
      "`;` expected but `:` found",
    )
  }

  test("given-block-indent-edge-cases: `with` another parent type")(
    runTestAssert[Stat](
      """|class A extends A with 
         |  B
         |""".stripMargin,
      assertLayout = Some("class A extends A with B"),
    )(Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tplNoBody(init("A"), init("B")))),
  )

  test("nested-coloneol") {
    runTestAssert[Stat](
      """|case class Test(
         |  a: A = new A,
         |):
         |  def hello = 1
         |""".stripMargin,
      assertLayout = Some("case class Test(a: A = new A) { def hello = 1 }"),
    )(Defn.Class(
      List(Mod.Case()),
      pname("Test"),
      Nil,
      ctorp(Term.Param(Nil, tname("a"), Some(pname("A")), Some(Term.New(init("A"))))),
      tpl(Defn.Def(Nil, tname("hello"), Nil, Nil, None, int(1))),
    ))
  }

  test("semicolon-closes-indent-region")(
    runTestAssert[Stat](
      """|val z =
         |  val a = 
         |    0;
         |  f(a)
         |""".stripMargin,
      assertLayout = Some(
        """|val z = {
           |  val a = 0
           |  f(a)
           |}""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("z")),
      None,
      blk(Defn.Val(Nil, List(patvar("a")), None, int(0)), tapply(tname("f"), tname("a"))),
    )),
  )

  test("observe-indented-in-braces") {
    val code =
      """|object X:
         |  if (cond)
         |    (f)
         |  foo
         |""".stripMargin
    val output =
      """|object X {
         |  if (cond) f
         |  foo
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Object(
      Nil,
      tname("X"),
      tpl(Term.If(tname("cond"), tname("f"), Lit.Unit(), Nil), tname("foo")),
    ))
  }

  test("match-case-same-line") {
    runTestAssert[Stat](
      """|widen match
         |  case tp @ OrNull(tp1): OrType =>
         |  case tp => tp
         |""".stripMargin,
      assertLayout = Some(
        """|widen match {
           |  case (tp @ OrNull(tp1)): OrType =>
           |  case tp => tp
           |}
           |""".stripMargin,
      ),
    )(tmatch(
      tname("widen"),
      Case(
        Pat.Typed(
          Pat.Bind(patvar("tp"), Pat.Extract(tname("OrNull"), List(patvar("tp1")))),
          pname("OrType"),
        ),
        None,
        blk(),
      ),
      Case(patvar("tp"), None, tname("tp")),
    ))
  }

  test("object-type")(
    runTestAssert[Stat](
      """|object typeAndObjects:
         |  type Ala
         |""".stripMargin,
      assertLayout = Some("object typeAndObjects { type Ala }"),
    )(Defn.Object(Nil, tname("typeAndObjects"), tpl(Decl.Type(Nil, pname("Ala"), Nil, noBounds)))),
  )

  test("old-try-catch-same-indent") {
    val code =
      """|try 
         |  fx
         |  gx 
         |  catch 
         |  case aa =>
         |  case bb =>
         |  finally
         |  cc
         |  dd
         |""".stripMargin
    val output =
      """|try {
         |  fx
         |  gx
         |} catch {
         |  case aa =>
         |  case bb =>
         |} finally {
         |  cc
         |  dd
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.Try(
      blk(tname("fx"), tname("gx")),
      List(Case(patvar("aa"), None, blk()), Case(patvar("bb"), None, blk())),
      Some(blk(tname("cc"), tname("dd"))),
    ))
  }

  test("if-else-same-indent") {
    val code =
      """|if 
         |  cond
         |  cond2
         |  then 
         |  fx
         |  gx
         |  else gx
         |""".stripMargin
    val output =
      """|if ({
         |  cond
         |  cond2
         |}) {
         |  fx
         |  gx
         |} else gx
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(blk(tname("cond"), tname("cond2")), blk(tname("fx"), tname("gx")), tname("gx"), Nil),
    )
  }

  test("new-fordo-same-indent") {
    val code =
      """|for
         |  a <- gen
         |  do fx
         |""".stripMargin
    val output = "for (a <- gen) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(List(Enumerator.Generator(patvar("a"), tname("gen"))), tname("fx")),
    )
  }

  test("new-for-yield-same-indent") {
    val code =
      """|for
         |  a <- x
         |  b <- y
         |  yield fx
         |""".stripMargin
    val output = "for (a <- x; b <- y) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(
        Enumerator.Generator(patvar("a"), tname("x")),
        Enumerator.Generator(patvar("b"), tname("y")),
      ),
      tname("fx"),
    ))
  }

  test("new-for-yield-if-with-indent") {
    val code =
      """|for
         |  a <- x
         |  b <- y
         |  if
         |    a < b
         |  yield fx
         |""".stripMargin
    val output = "for (a <- x; b <- y; if a < b) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(
        Enumerator.Generator(patvar("a"), tname("x")),
        Enumerator.Generator(patvar("b"), tname("y")),
        Enumerator.Guard(tinfix(tname("a"), "<", tname("b"))),
      ),
      tname("fx"),
    ))
  }

  test("old-for-yield-if-with-indent") {
    val code =
      """|for {
         |  a <- x
         |  b <- y
         |  if
         |    a < b
         |} yield fx
         |""".stripMargin
    val output = "for (a <- x; b <- y; if a < b) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(
        Enumerator.Generator(patvar("a"), tname("x")),
        Enumerator.Generator(patvar("b"), tname("y")),
        Enumerator.Guard(tinfix(tname("a"), "<", tname("b"))),
      ),
      tname("fx"),
    ))
  }

  test("for-in-parens") {
    val code =
      """|def foo =
         |  (for a <- List(1)
         |       b <- List(2)
         |   yield a + b).toSet.size
         |""".stripMargin
    val output = "def foo = (for (a <- List(1); b <- List(2)) yield a + b).toSet.size"
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      Nil,
      None,
      tselect(
        tselect(
          Term.ForYield(
            List(
              Enumerator.Generator(patvar("a"), tapply(tname("List"), int(1))),
              Enumerator.Generator(patvar("b"), tapply(tname("List"), int(2))),
            ),
            tinfix(tname("a"), "+", tname("b")),
          ),
          "toSet",
        ),
        "size",
      ),
    ))
  }

  test("match-chained-same-indent") {
    runTestAssert[Stat](
      """|val hello = xs match
         |  case Nil => "empty"
         |  case x :: xs1 => "nonempty"
         |  match
         |  case true => 0
         |  case false => 1
         |
         |""".stripMargin,
      Some(
        """|val hello = xs match {
           |  case Nil =>
           |    "empty"
           |  case x :: xs1 =>
           |    "nonempty" match {
           |      case true => 0
           |      case false => 1
           |    }
           |}
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("hello")),
      None,
      tmatch(
        tname("xs"),
        Case(tname("Nil"), None, str("empty")),
        Case(
          patinfix(patvar("x"), "::", patvar("xs1")),
          None,
          tmatch(str("nonempty"), Case(bool(true), None, int(0)), Case(bool(false), None, int(1))),
        ),
      ),
    ))

  }

  test("match-if-indent") {
    runTestAssert[Stat](
      """|def hackGetmembers = a match
         |   case sym if
         |     cond =>
         |     sym
         |
         |""".stripMargin,
      assertLayout = Some(
        """|def hackGetmembers = a match {
           |  case sym if cond => sym
           |}
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("hackGetmembers"),
      Nil,
      Nil,
      None,
      tmatch(tname("a"), Case(patvar("sym"), Some(tname("cond")), tname("sym"))),
    ))

  }
  test("match-empty") {
    runTestAssert[Stat](
      """|def mapSymbols =
         |  originals.foreach { a =>
         |    copy.denot match
         |      case cd: ClassDenotation =>
         |      case _ =>
         |  }
         |""".stripMargin,
      assertLayout = Some(
        """|def mapSymbols = originals.foreach {
           |  a => copy.denot match {
           |    case cd: ClassDenotation =>
           |    case _ =>
           |  }
           |}
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("mapSymbols"),
      Nil,
      Nil,
      None,
      tapply(
        tselect("originals", "foreach"),
        blk(tfunc(tparam("a"))(tmatch(
          tselect("copy", "denot"),
          Case(Pat.Typed(patvar("cd"), pname("ClassDenotation")), None, blk()),
          Case(patwildcard, None, blk()),
        ))),
      ),
    ))

  }

  test("return-indent") {
    runTestAssert[Stat](
      """|def method =
         |   return
         |     val a = 2 + 3
         |     a
         |
         |""".stripMargin,
      assertLayout = Some(
        """|def method = return {
           |  val a = 2 + 3
           |  a
           |}
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("method"),
      Nil,
      Nil,
      None,
      Term.Return(blk(Defn.Val(Nil, List(patvar("a")), None, tinfix(int(2), "+", int(3))), tname("a"))),
    ))
  }

  test("return-single-indent")(
    runTestAssert[Stat](
      """|def method =
         |   return
         |     2 
         |     + 3
         |""".stripMargin,
      assertLayout = Some("def method = return 2 + 3"),
    )(Defn.Def(Nil, tname("method"), Nil, Nil, None, Term.Return(tinfix(int(2), "+", int(3))))),
  )

  test("empty-return") {
    runTestAssert[Stat](
      """|    def skip = {
         |        token match {
         |          case RBRACE =>
         |            if (true)
         |              return
         |            change(-1)
         |          case LBRACE =>
         |             if (true)
         |               return
         |             change(-1)
         |        }
         |    }
         |""".stripMargin,
      assertLayout = Some(
        """|def skip = {
           |  token match {
           |    case RBRACE =>
           |      if (true) return
           |      change(-1)
           |    case LBRACE =>
           |      if (true) return
           |      change(-1)
           |  }
           |}
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("skip"),
      Nil,
      Nil,
      None,
      blk(tmatch(
        tname("token"),
        Case(
          tname("RBRACE"),
          None,
          blk(
            Term.If(bool(true), Term.Return(Lit.Unit()), Lit.Unit(), Nil),
            tapply(tname("change"), int(-1)),
          ),
        ),
        Case(
          tname("LBRACE"),
          None,
          blk(
            Term.If(bool(true), Term.Return(Lit.Unit()), Lit.Unit(), Nil),
            tapply(tname("change"), int(-1)),
          ),
        ),
      )),
    ))
  }

  test("case-block") {
    runTestAssert[Stat](
      """|val success = suffixes.find { suffix =>
         |  try {
         |    true
         |  } catch {
         |    case e: StorageException =>
         |      false
         |  }
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|val success = suffixes.find {
           |  suffix => try {
           |    true
           |  } catch {
           |    case e: StorageException => false
           |  }
           |}
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("success")),
      None,
      tapply(
        tselect("suffixes", "find"),
        blk(tfunc(tparam("suffix"))(Term.Try(
          blk(bool(true)),
          Case(Pat.Typed(patvar("e"), pname("StorageException")), None, bool(false)) :: Nil,
          None,
        ))),
      ),
    ))
  }

  test("complext-match-else") {
    runTestAssert[Stat](
      """|val calleeType = a match {
         |  case _ =>
         |    if cond then
         |      expr match
         |        case _ =>
         |          f
         |    else NoType
         |  case _ =>
         |     NoType
         |}
         |""".stripMargin,
      assertLayout = Some(
        """|val calleeType = a match {
           |  case _ =>
           |    if (cond) expr match {
           |      case _ => f
           |    } else NoType
           |  case _ =>
           |    NoType
           |}
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("calleeType")),
      None,
      tmatch(
        tname("a"),
        Case(
          patwildcard,
          None,
          Term.If(
            tname("cond"),
            tmatch(tname("expr"), Case(patwildcard, None, tname("f"))),
            tname("NoType"),
            Nil,
          ),
        ),
        Case(patwildcard, None, tname("NoType")),
      ),
    ))
  }

  test("partial-function") {
    runTestAssert[Stat](
      """|val withDefault: Option[Int] => Int =
         |  case Some(x) => x
         |  case None => 0
         |""".stripMargin,
      assertLayout = Some(
        """|val withDefault: Option[Int] => Int = {
           |  case Some(x) => x
           |  case None => 0
           |}
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("withDefault")),
      Some(pfunc(papply("Option", "Int"))("Int")),
      Term.PartialFunction(List(
        Case(Pat.Extract(tname("Some"), List(patvar("x"))), None, tname("x")),
        Case(tname("None"), None, int(0)),
      )),
    ))
  }

  test("for-left-arrow") {
    runTestAssert[Stat](
      """|for
         |  a <-
         |    val b = 123
         |    Some(b)
         |yield
         |  a
         |""".stripMargin,
      assertLayout = Some(
        """|for (a <- {
           |  val b = 123
           |  Some(b)
           |}) yield a
           |""".stripMargin,
      ),
    )(Term.ForYield(
      List(Enumerator.Generator(
        patvar("a"),
        blk(Defn.Val(Nil, List(patvar("b")), None, int(123)), tapply(tname("Some"), tname("b"))),
      )),
      tname("a"),
    ))
  }

  test("context-arrow") {
    runTestAssert[Stat](
      """|val a = (s: Int) ?=> 
         |  val a = 123
         |  s + a
         |
         |""".stripMargin,
      assertLayout = Some(
        """|val a = (s: Int) ?=> {
           |  val a = 123
           |  s + a
           |}
           |""".stripMargin,
      ),
    )(Defn.Val(
      Nil,
      List(patvar("a")),
      None,
      tctxfunc(
        tparam("s", "Int"),
      )(blk(Defn.Val(Nil, List(patvar("a")), None, int(123)), tinfix(tname("s"), "+", tname("a")))),
    ))
  }

  test("indented-apply")(
    runTestAssert[Stat](
      """|def method = 
         |  fun(a,b,c)
         |    (d, e)
         |""".stripMargin,
      assertLayout = Some("def method = fun(a, b, c)(d, e)"),
    )(Defn.Def(
      Nil,
      tname("method"),
      Nil,
      Nil,
      None,
      tapply(tapply(tname("fun"), tname("a"), tname("b"), tname("c")), tname("d"), tname("e")),
    )),
  )

  test("#3531 apply with optional braces and trailing comma") {
    val code =
      """|Request(
         |  value = true match
         |    case true => Some(1)
         |    case _ => None,
         |  b = "xxx"
         |)
         |""".stripMargin
    val layout =
      """|Request(value = true match {
         |  case true =>
         |    Some(1)
         |  case _ =>
         |    None
         |}, b = "xxx")
         |""".stripMargin
    val tree = tapply(
      tname("Request"),
      Term.Assign(
        tname("value"),
        tmatch(
          bool(true),
          Case(bool(true), None, tapply(tname("Some"), int(1))),
          Case(patwildcard, None, tname("None")),
        ),
      ),
      Term.Assign(tname("b"), str("xxx")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3542 apply with optional braces in intermediate arg, with multiple outdents") {
    val code =
      """|A(
         |  foo = x =>
         |    x match
         |      case baz => baz,
         |  bar = bar
         |)
         |""".stripMargin
    val layout =
      """|A(foo = x => x match {
         |  case baz => baz
         |}, bar = bar)
         |""".stripMargin
    val tree = tapply(
      tname("A"),
      Term.Assign(
        tname("foo"),
        tfunc(tparam("x"))(tmatch(tname("x"), Case(patvar("baz"), None, tname("baz")))),
      ),
      Term.Assign(tname("bar"), tname("bar")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented-double-apply")(
    runTestAssert[Stat](
      """|def method = 
         |  fun(a,b,c)
         |    (d, e)
         |    (f, g)
         |""".stripMargin,
      assertLayout = Some("def method = fun(a, b, c)(d, e)(f, g)"),
    )(Defn.Def(
      Nil,
      tname("method"),
      Nil,
      Nil,
      None,
      tapply(
        tapply(tapply(tname("fun"), tname("a"), tname("b"), tname("c")), tname("d"), tname("e")),
        tname("f"),
        tname("g"),
      ),
    )),
  )

  test("indented-for") {
    runTestAssert[Stat](
      """|for { project <- projects
         |      (source, id) <- project.sources.zipWithIndex } yield source 
         |""".stripMargin,
      assertLayout =
        Some("for (project <- projects; (source, id) <- project.sources.zipWithIndex) yield source"),
    )(Term.ForYield(
      List(
        Enumerator.Generator(patvar("project"), tname("projects")),
        Enumerator.Generator(
          Pat.Tuple(List(patvar("source"), patvar("id"))),
          tselect("project", "sources", "zipWithIndex"),
        ),
      ),
      tname("source"),
    ))
  }

  test("non-indented-apply") {
    runTestAssert[Stat](
      """|def method = 
         |  fun(a,b,c)
         |  (d, e)
         |""".stripMargin,
      assertLayout = Some(
        """|def method = {
           |  fun(a, b, c)
           |  (d, e)
           |}
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("method"),
      Nil,
      Nil,
      None,
      blk(
        tapply(tname("fun"), tname("a"), tname("b"), tname("c")),
        Term.Tuple(List(tname("d"), tname("e"))),
      ),
    ))
  }

  test("indented-apply-braces") {
    runTestAssert[Stat](
      """|def method: String = 
         |  fun(1, 2, 3) 
         |    {4} 
         |""".stripMargin,
      assertLayout = Some(
        """|def method: String = fun(1, 2, 3) {
           |  4
           |}
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("method"),
      Nil,
      Nil,
      Some(pname("String")),
      tapply(tapply(tname("fun"), int(1), int(2), int(3)), blk(int(4))),
    ))
  }

  test("non-indented-apply-braces") {
    runTestAssert[Stat](
      """|def method2: String = 
         |  fun(1, 2, 3)
         |  {4}
         |""".stripMargin,
      assertLayout = Some(
        """|def method2: String = {
           |  fun(1, 2, 3)
           |  {
           |    4
           |  }
           |}
           |""".stripMargin,
      ),
    )(Defn.Def(
      Nil,
      tname("method2"),
      Nil,
      Nil,
      Some(pname("String")),
      blk(tapply(tname("fun"), int(1), int(2), int(3)), blk(int(4))),
    ))
  }

  test("indented-contructor-params") {
    runTestAssert[Stat](
      """|object ExampleThing
         |    extends CompositeThing
         |          (
         |                "One",
         |                "Two",
         |                "Three",
         |                "Four")
         |""".stripMargin,
      assertLayout = Some(
        """|object ExampleThing extends CompositeThing("One", "Two", "Three", "Four")
           |""".stripMargin,
      ),
    )(Defn.Object(
      Nil,
      tname("ExampleThing"),
      tplNoBody(init("CompositeThing", List(str("One"), str("Two"), str("Three"), str("Four")))),
    ))
  }

  test("non-indented-contructor-params") {
    runTestAssert[Stat](
      """|object O:
         |  object ExampleThing
         |    extends CompositeThing
         |  (
         |                "One",
         |                "Two",
         |                "Three",
         |                "Four")
         |""".stripMargin,
      assertLayout = Some(
        """|object O {
           |  object ExampleThing extends CompositeThing
           |
           |  ("One", "Two", "Three", "Four")
           |}
           |""".stripMargin,
      ),
    )(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Object(Nil, tname("ExampleThing"), tplNoBody(List(init("CompositeThing")))),
        Term.Tuple(List(str("One"), str("Two"), str("Three"), str("Four"))),
      ),
    ))
  }

  test("indented-enum-contructor-params") {
    runTestAssert[Stat](
      """|enum Namespace(val uri: String | Null):
         |  case xhtml
         |      extends Namespace // Defn.EnumCase ends here
         |        ("http://www.w3.org/1999/xhtml") // str
         |""".stripMargin,
      """|enum Namespace(val uri: String | Null) {
         |  case xhtml extends Namespace // Defn.EnumCase ends here
         |    ("http://www.w3.org/1999/xhtml") // str
         |}
         |""".stripMargin,
    )(Defn.Enum(
      Nil,
      pname("Namespace"),
      Nil,
      ctorp(tparam(List(Mod.ValParam()), "uri", pinfix("String", "|", pname("Null")))),
      tpl(
        Defn.EnumCase.newBuilder(
          Nil,
          tname("xhtml"),
          Nil,
          EmptyCtor(),
          List(init(
            Type.Name.newBuilder("Namespace").endComment(Seq("// Defn.EnumCase ends here")).result(),
            List(str("http://www.w3.org/1999/xhtml")),
          )),
        ).endComment(Seq("// str")).result(),
      ),
    ))
  }

  test("indented-double-new")(
    runTestAssert[Stat](
      """|new fun
         |    (a,b,c)
         |    (d, e)
         |    (f, g)
         |""".stripMargin,
      assertLayout = Some("new fun(a, b, c)(d, e)(f, g)"),
    )(Term.New(init(
      pname("fun"),
      List(tname("a"), tname("b"), tname("c")),
      List(tname("d"), tname("e")),
      List(tname("f"), tname("g")),
    ))),
  )

  test("then-same-line") {
    runTestAssert[Stat](
      """|def f =
         |   if
         |      x.exists
         |         (x => x == 10) then
         |      println("Yes")
         |   else
         |      println("No")
         |""".stripMargin,
      assertLayout =
        Some("def f = if (x.exists(x => x == 10)) println(\"Yes\") else println(\"No\")"),
    )(Defn.Def(
      Nil,
      tname("f"),
      Nil,
      Nil,
      None,
      Term.If(
        tapply(tselect("x", "exists"), tfunc(tparam("x"))(tinfix(tname("x"), "==", int(10)))),
        tapply(tname("println"), str("Yes")),
        tapply(tname("println"), str("No")),
        Nil,
      ),
    ))
  }

  test("then-same-line-nested") {
    runTestAssert[Stat](
      """|def f =
         |   if
         |      if a > 0 then true else false
         |   then
         |      println("Yes")
         |   else
         |      println("No")
         |""".stripMargin,
      assertLayout =
        Some("def f = if (if (a > 0) true else false) println(\"Yes\") else println(\"No\")"),
    )(Defn.Def(
      Nil,
      tname("f"),
      None,
      None,
      Term.If(
        Term.If(tinfix(tname("a"), ">", int(0)), bool(true), bool(false), Nil),
        tapply(tname("println"), str("Yes")),
        tapply(tname("println"), str("No")),
        Nil,
      ),
    ))
  }

  test("package-mixed") {
    runTestAssert[Source](
      """|package tests.site
         |
         |package some.other:
         |  class SomeOtherPackage
         |
         |class BrokenLink
         |""".stripMargin,
      assertLayout = Some(
        """|package tests.site
           |package some.other {
           |  class SomeOtherPackage
           |}
           |class BrokenLink
           |""".stripMargin,
      ),
    )(Source(List(Pkg(
      tselect("tests", "site"),
      List(
        Pkg(
          tselect("some", "other"),
          List(Defn.Class(Nil, pname("SomeOtherPackage"), Nil, ctor, tplNoBody())),
        ),
        Defn.Class(Nil, pname("BrokenLink"), Nil, ctor, tplNoBody()),
      ),
    ))))
  }

  test("empty-case-end-marker")(
    runTestAssert[Source](
      """|
         |  def abc: Unit =
         |    x match
         |      case _ =>
         |  end abc
         |""".stripMargin,
      assertLayout = None,
    )(Source(List(
      Defn.Def(
        Nil,
        tname("abc"),
        Nil,
        Nil,
        Some(pname("Unit")),
        tmatch(tname("x"), Case(patwildcard, None, blk())),
      ),
      Term.EndMarker(tname("abc")),
    ))),
  )

  test("infix-operator-with-alpha") {
    runTestAssert[Stat](
      """|def send() =
         |  c ! "hello"
         |    ! "world"
         |    send_! "!"
         |""".stripMargin,
      assertLayout = Some("""def send() = c ! "hello" ! "world" send_! "!""""),
    )(Defn.Def(
      Nil,
      tname("send"),
      Nil,
      List(List()),
      None,
      tinfix(tinfix(tinfix(tname("c"), "!", str("hello")), "!", str("world")), "send_!", str("!")),
    ))
  }

  test("colon-eol-comment1") {
    runTestAssert[Stat](
      """|object Foo:
         |  /*inline*/ def foo: Int = ???
         |  def bar: Int = ???
         |""".stripMargin,
      assertLayout = None,
    )(Defn.Object(
      Nil,
      tname("Foo"),
      tpl(
        Defn.Def.newBuilder(Nil, tname("foo"), Nil, Some(pname("Int")), tname("???"))
          .begComment(Seq("/*inline*/")).endComment(None).result(),
        Defn.Def(Nil, tname("bar"), Nil, Nil, Some(pname("Int")), tname("???")),
      ),
    ))
  }

  test("colon-eol-comment2")(
    runTestAssert[Stat](
      """|object Foo: /* comment*/
         |  def foo: Int = ???
         |""".stripMargin,
      assertLayout = None,
    )(Defn.Object(
      Nil,
      tname("Foo"),
      tpl(
        Defn.Def.newBuilder(Nil, tname("foo"), Nil, Some(pname("Int")), tname("???"))
          .begComment(Seq("/* comment*/")).endComment(None).result(),
      ),
    )),
  )

  test("colon-eol-multiline-comment") {
    runTestAssert[Stat](
      """|object Foo:/* multi
         |  line
         |   comment */ def foo: Int = ???
         |   def bar: Int = ???
         |""".stripMargin,
      assertLayout = None,
    )(Defn.Object(
      Nil,
      tname("Foo"),
      tpl(
        Defn.Def.newBuilder(Nil, tname("foo"), Nil, Some(pname("Int")), tname("???"))
          .begComment(Seq("/* multi\n  line\n   comment */")).endComment(None).result(),
        Defn.Def(Nil, tname("bar"), Nil, Nil, Some(pname("Int")), tname("???")),
      ),
    ))
  }

  test("def-body-indent-comment") {
    val code =
      """|def foo: Int =
         |  // c
         |  bar
         |""".stripMargin
    val layout =
      """|def foo: Int =
         |  // c
         |  bar
         |""".stripMargin
    runTestAssert[Stat](code, layout)(
      Defn.Def(Nil, tname("foo"), Nil, Nil, Some(pname("Int")), tnameComments("bar")("// c")()),
    )
  }

  test("def-body-indent-comment-two-stats") {
    val code =
      """|def foo: Int =
         |  // c
         |  bar
         |  baz
         |""".stripMargin
    val layout =
      """|def foo: Int = {
         |  // c
         |  bar
         |  baz
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      Nil,
      Some(pname("Int")),
      blk(tnameComments("bar")("// c")(), tname("baz")),
    ))
  }

  test("val-body-indent-same-line-comment") {
    val code =
      """|val foo =
         |  /* c */ bar
         |""".stripMargin
    val layout =
      """|val foo =
         |  /* c */ bar
         |""".stripMargin
    runTestAssert[Stat](code, layout)(
      Defn.Val(Nil, List(patvar("foo")), None, tnameComments("bar")("/* c */")()),
    )
  }

  test("if-then-indent-comment") {
    val code =
      """|if cond then
         |  // c
         |  foo
         |else bar
         |""".stripMargin
    val layout =
      """|if (cond)
         |  // c
         |  foo
         |else bar
         |""".stripMargin
    runTestAssert[Stat](code, layout)(
      Term.If(tname("cond"), tnameComments("foo")("// c")(), tname("bar")),
    )
  }

  test("case-body-indent-comment") {
    val code =
      """|x match
         |  case 1 =>
         |    // c
         |    foo
         |""".stripMargin
    val layout =
      """|x match {
         |  case 1 =>
         |    // c
         |    foo
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(
      Term.Match(tname("x"), List(Case(int(1), None, tnameComments("foo")("// c")()))),
    )
  }

  test("def-body-indent-comment-braces") {
    val code =
      """|def foo: Int =
         |  // c
         |  {
         |    bar
         |  }
         |""".stripMargin
    val layout =
      """|def foo: Int =
         |  // c
         |  {
         |    bar
         |  }
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      Nil,
      Some(pname("Int")),
      Term.Block.newBuilder(List(tname("bar"))).begComment(detachedComments("// c")).result(),
    ))
  }

  test("def-body-indent-braces-trailing-comment") {
    val code =
      """|def foo: Int =
         |  {
         |    bar
         |  }
         |  // c
         |""".stripMargin
    val layout =
      """|def foo: Int = {
         |  {
         |    bar
         |  }
         |  // c
         |}
         |""".stripMargin
    val body = blk(Term.Block.newBuilder(List(tname("bar"))).endComment(Seq("// c")).result())
    val tree = Defn.Def(Nil, tname("foo"), Nil, Nil, Some(pname("Int")), body)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case-body-indent-comment-braces") {
    val code =
      """|x match
         |  case 1 =>
         |    // c
         |    {
         |      foo
         |    }
         |""".stripMargin
    val layout =
      """|x match {
         |  case 1 =>
         |    // c
         |    {
         |      foo
         |    }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(layout))(Term.Match(
      tname("x"),
      List(
        Case(int(1), None, Term.Block.newBuilder(List(tname("foo"))).begComment(Seq("// c")).result()),
      ),
    ))
  }

  test("case-body-braced-match-comment") {
    val code =
      """|x match {
         |  case 1 =>
         |    // c
         |    foo
         |}
         |""".stripMargin
    val layout =
      """|x match {
         |  case 1 =>
         |    // c
         |    foo
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(
      Term.Match(tname("x"), List(Case(int(1), None, tnameComments("foo")("// c")()))),
    )
  }

  test("case-body-braced-comment") {
    val code =
      """|x match
         |  case 1 => {
         |    // c
         |    foo
         |  }
         |""".stripMargin
    val layout =
      """|x match {
         |  case 1 =>
         |    // c
         |    foo
         |}
         |""".stripMargin
    val tree = Term.Match(tname("x"), List(Case(int(1), None, blk(tnameComments("foo")("// c")()))))
    parseAndCheckTree[Stat](code, layout)(tree)
    val reparsed = Term.Match(tname("x"), List(Case(int(1), None, tnameComments("foo")("// c")())))
    runTestAssert[Stat](layout)(reparsed)
  }

  test("given-with-comment") {
    runTestAssert[Stat](
      """|given Foo with
         |   /* comment */  def foo: Int = ???
         |   def bar: Int = ???
         |""".stripMargin,
      assertLayout = None,
    )(Defn.Given(
      Nil,
      anon,
      Nil,
      Nil,
      tpl(
        List(init("Foo")),
        Template.Body(
          None,
          List(
            Defn.Def.newBuilder(Nil, tname("foo"), Nil, Some(pname("Int")), tname("???"))
              .begComment(Seq("/* comment */")).endComment(None).result(),
            Defn.Def(Nil, tname("bar"), Nil, Nil, Some(pname("Int")), tname("???")),
          ),
        ),
      ),
    ))
  }

  test("given-with-miltiline-comment") {
    runTestAssert[Stat](
      """|given Foo with /* multi
         |   line
         |   comment */  def foo: Int = ???
         |   def bar: Int = ???
         |""".stripMargin,
      assertLayout = None,
    )(Defn.Given(
      Nil,
      anon,
      Nil,
      Nil,
      tpl(
        List(init("Foo")),
        Template.Body(
          None,
          List(
            Defn.Def.newBuilder(Nil, tname("foo"), Nil, Some(pname("Int")), tname("???"))
              .begComment(Seq("/* multi\n   line\n   comment */")).endComment(None).result(),
            Defn.Def(Nil, tname("bar"), Nil, Nil, Some(pname("Int")), tname("???")),
          ),
        ),
      ),
    ))
  }

  test("comment-bettween-annotation-set") {
    runTestAssert[Source](
      s"""|
          |class A1 extends scala.annotation.StaticAnnotation
          |class A2 extends scala.annotation.StaticAnnotation
          | @A1
          | /*
          | * hello
          | */
          | @A2
          |class B
          |""".stripMargin,
      assertLayout = None,
    ) {
      Source {
        List(
          Defn.Class(
            Nil,
            pname("A1"),
            Nil,
            EmptyCtor(),
            tplNoBody(
              init(Type.Select(tselect("scala", "annotation"), pname("StaticAnnotation"))) :: Nil,
            ),
          ),
          Defn.Class(
            Nil,
            pname("A2"),
            Nil,
            EmptyCtor(),
            tplNoBody(
              init(Type.Select(tselect("scala", "annotation"), pname("StaticAnnotation"))) :: Nil,
            ),
          ),
          Defn.Class(
            List(
              Mod.Annot(init("A1")),
              Mod.Annot.newBuilder(init("A2")).begComment(detachedComments("/*\n * hello\n */"))
                .result(),
            ),
            pname("B"),
            Nil,
            EmptyCtor(),
            tplNoBody(),
          ),
        )
      }
    }
  }

  test("i2505") {
    runTestAssert[Source](
      """|trait T2 { self: T =>
         |  enum T2Enum:
         |    case EnumCase
         |  
         |  extension (n: Int) def negate: Int = -n
         |}""".stripMargin,
      assertLayout = Some(
        """|trait T2 { self: T =>
           |  enum T2Enum { case EnumCase }
           |  extension (n: Int) def negate: Int = -n
           |}
           |""".stripMargin,
      ),
    )(Source(List(Defn.Trait(
      Nil,
      pname("T2"),
      Nil,
      EmptyCtor(),
      tpl(
        self("self", "T"),
        Defn.Enum(
          Nil,
          pname("T2Enum"),
          Nil,
          EmptyCtor(),
          tpl(Defn.EnumCase(Nil, tname("EnumCase"), Nil, EmptyCtor(), Nil)),
        ),
        Defn.ExtensionGroup(
          Nil,
          List(List(tparam("n", "Int"))),
          Defn.Def(
            Nil,
            tname("negate"),
            Nil,
            Nil,
            Some(pname("Int")),
            Term.ApplyUnary(tname("-"), tname("n")),
          ),
        ),
      ),
    ))))

  }

  test("#3113") {
    runTestAssert[Source](
      """|object Hello {
         |  val fun = () =>
         |    if (true) {
         |      new Object { obj =>
         |        println(toString)
         |      }
         |    }
         |
         |  def main(args: Array[String]): Unit = fun()
         |}""".stripMargin,
      assertLayout = Some(
        """|object Hello {
           |  val fun = () => if (true) {
           |    new Object { obj => println(toString) }
           |  }
           |  def main(args: Array[String]): Unit = fun()
           |}
           |""".stripMargin,
      ),
    )(Source(
      Defn.Object(
        Nil,
        tname("Hello"),
        tpl(
          Defn.Val(
            Nil,
            List(patvar("fun")),
            None,
            tfunc()(Term.If(
              bool(true),
              blk(Term.NewAnonymous(
                tpl(List(init("Object")), self("obj"), tapply(tname("println"), tname("toString"))),
              )),
              Lit.Unit(),
              Nil,
            )),
          ),
          Defn.Def(
            Nil,
            tname("main"),
            Nil,
            List(List(tparam("args", papply("Array", "String")))),
            Some(pname("Unit")),
            tapply(tname("fun")),
          ),
        ),
      ) :: Nil,
    ))
  }

  test("newline within self-type") {
    val layout =
      """|trait T2 {
         |  self { T =>
         |  }
         |  enum T2Enum { case EnumCase }
         |}
         |""".stripMargin
    val tree = Defn.Trait(
      Nil,
      pname("T2"),
      Nil,
      ctor,
      tpl(
        tapply(tname("self"), blk(tfunc(tparam("T"))(blk()))),
        Defn.Enum(
          Nil,
          pname("T2Enum"),
          Nil,
          ctor,
          tpl(Defn.EnumCase(Nil, tname("EnumCase"), Nil, ctor, Nil)),
        ),
      ),
    )
    runTestAssert[Stat](
      """|trait T2 {
         |  self:
         |    T =>
         |  enum T2Enum:
         |    case EnumCase
         |}""".stripMargin,
      assertLayout = Some(layout),
    )(tree)
    runTestAssert[Stat](
      """|trait T2:
         |  self:
         |    T
         |    =>
         |  enum T2Enum:
         |    case EnumCase
         |""".stripMargin,
      assertLayout = Some(layout),
    )(tree)
  }

  test("#3210") {
    val code =
      """|a3 match {
         |  case Some(_) =>
         |    case class A6(a7: A8)
         |
         |    object A9
         |}
         |""".stripMargin
    val layout =
      """|a3 match {
         |  case Some(_) =>
         |    case class A6(a7: A8)
         |    object A9
         |}
         |""".stripMargin
    val tree = tmatch(
      tname("a3"),
      Case(
        Pat.Extract(tname("Some"), List(patwildcard)),
        None,
        blk(
          Defn.Class(List(Mod.Case()), pname("A6"), Nil, ctorp(tparam("a7", "A8")), tplNoBody()),
          Defn.Object(Nil, tname("A9"), tplNoBody()),
        ),
      ),
    )
    runTestAssert[Stat](code, assertLayout = Some(layout))(tree)
  }

  test("def body is partial function") {
    val code =
      """|object a:
         |  def foo: Bar =
         |    case sym
         |        if baz =>
         |  end foo
         |""".stripMargin
    val layout =
      """|object a {
         |  def foo: Bar = {
         |    case sym if baz =>
         |  }
         |  end foo
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("a"),
      tpl(
        Defn.Def(
          Nil,
          tname("foo"),
          Nil,
          Some(pname("Bar")),
          Term.PartialFunction(List(Case(patvar("sym"), Some(tname("baz")), blk()))),
        ),
        Term.EndMarker(tname("foo")),
      ),
    ))
  }

  test("`if` body is a partial function") {
    val code =
      """|  if foo then
         |    case msg1: Int => msg1 + 1
         |  else
         |    case msg2: Int => msg2 + 2
         |""".stripMargin
    val layout =
      """|if (foo) {
         |  case msg1: Int =>
         |    msg1 + 1
         |} else {
         |  case msg2: Int =>
         |    msg2 + 2
         |}
         |""".stripMargin
    val tree = Term.If(
      "foo",
      Term
        .PartialFunction(List(Case(Pat.Typed(patvar("msg1"), "Int"), None, tinfix("msg1", "+", lit(1))))),
      Term
        .PartialFunction(List(Case(Pat.Typed(patvar("msg2"), "Int"), None, tinfix("msg2", "+", lit(2))))),
      Nil,
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("def body is non-partial function") {
    val code =
      """|object a:
         |  def foo: Bar =
         |    case class Baz(baz: Int)
         |    new Baz(0)
         |""".stripMargin
    val layout =
      """|object a {
         |  def foo: Bar = {
         |    case class Baz(baz: Int)
         |    new Baz(0)
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("a"),
      tpl(Defn.Def(
        Nil,
        tname("foo"),
        Nil,
        Some(pname("Bar")),
        blk(
          Defn.Class(List(Mod.Case()), pname("Baz"), Nil, ctorp(tparam("baz", "Int")), tplNoBody()),
          Term.New(init("Baz", List(int(0)))),
        ),
      )),
    ))
  }

  test("#3252 `new` in arg, then comma 1") {
    val code =
      """|object a:
         |  A(
         |    b = new B,
         |    c =
         |      d,
         |    e = f
         |  )
         |""".stripMargin
    val layout = "object a { A(b = new B, c = d, e = f) }"
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("a"),
      tpl(tapply(
        tname("A"),
        Term.Assign(tname("b"), Term.New(init("B"))),
        Term.Assign(tname("c"), tname("d")),
        Term.Assign(tname("e"), tname("f")),
      )),
    ))
  }

  test("#3252 `new` in arg, then comma 2") {
    val code =
      """|object a:
         |  A(
         |    b = new B(0),
         |    c =
         |      d,
         |    e = f
         |  )
         |""".stripMargin
    val layout = "object a { A(b = new B(0), c = d, e = f) }"
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("a"),
      tpl(tapply(
        tname("A"),
        Term.Assign(tname("b"), Term.New(init("B", List(int(0))))),
        Term.Assign(tname("c"), tname("d")),
        Term.Assign(tname("e"), tname("f")),
      )),
    ))
  }

  test("#3252 `new` in arg, then comma 3") {
    val code =
      """|object a:
         |  A(
         |    b = new B(0) { def foo = ??? },
         |    c =
         |      d,
         |    e = f
         |  )
         |""".stripMargin
    val layout = "object a { A(b = new B(0) { def foo = ??? }, c = d, e = f) }"
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("a"),
      tpl(tapply(
        tname("A"),
        Term.Assign(
          tname("b"),
          Term.NewAnonymous(tpl(
            List(init("B", List(int(0)))),
            List(Defn.Def(Nil, tname("foo"), Nil, None, tname("???"))),
          )),
        ),
        Term.Assign(tname("c"), tname("d")),
        Term.Assign(tname("e"), tname("f")),
      )),
    ))
  }

  test("#3257 `new` in arg value after indent, then comma") {
    val code =
      """|object a:
         |  A(
         |    b =
         |      new B,
         |    c = d
         |  )
         |""".stripMargin
    val layout = "object a { A(b = new B, c = d) }"
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("a"),
      tpl(tapply(
        tname("A"),
        Term.Assign(tname("b"), Term.New(init("B"))),
        Term.Assign(tname("c"), tname("d")),
      )),
    ))
  }

  test("#3257 `if-no-else` in arg value after indent, then comma") {
    val code =
      """|object a:
         |  A(
         |    b =
         |      if (a + b) c,
         |    c = d
         |  )
         |""".stripMargin
    val layout = "object a { A(b = if (a + b) c, c = d) }"
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("a"),
      tpl(tapply(
        tname("A"),
        Term.Assign(
          tname("b"),
          Term.If(tinfix(tname("a"), "+", tname("b")), tname("c"), Lit.Unit(), Nil),
        ),
        Term.Assign(tname("c"), tname("d")),
      )),
    ))
  }

  test("#3261 chained `match` with outdent, no block, in assign") {
    val code =
      """|object small:
         |  val value =
         |    Nil match
         |    case Nil => "empty"
         |    case _   => "nonempty"
         |  match
         |    case "empty"    => 0
         |    case "nonempty" => 1
         |""".stripMargin
    val layout =
      """|object small {
         |  val value = (Nil match {
         |    case Nil => "empty"
         |    case _ => "nonempty"
         |  }) match {
         |    case "empty" => 0
         |    case "nonempty" => 1
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("small"),
      tpl(Defn.Val(
        Nil,
        List(patvar("value")),
        None,
        tmatch(
          tmatch(
            tname("Nil"),
            Case(tname("Nil"), None, str("empty")),
            Case(patwildcard, None, str("nonempty")),
          ),
          Case(str("empty"), None, int(0)),
          Case(str("nonempty"), None, int(1)),
        ),
      )),
    ))
  }

  test("#3261 chained `match` with outdent, block, in assign") {
    val code =
      """|object small:
         |  val value =
         |    foo()
         |    Nil match
         |    case Nil => "empty"
         |    case _   => "nonempty"
         |  match
         |    case "empty"    => 0
         |    case "nonempty" => 1
         |""".stripMargin
    val layout =
      """|object small {
         |  val value = {
         |    foo()
         |    Nil match {
         |      case Nil => "empty"
         |      case _ => "nonempty"
         |    }
         |  } match {
         |    case "empty" => 0
         |    case "nonempty" => 1
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("small"),
      tpl(Defn.Val(
        Nil,
        List(patvar("value")),
        None,
        tmatch(
          blk(
            tapply(tname("foo")),
            tmatch(
              tname("Nil"),
              Case(tname("Nil"), None, str("empty")),
              Case(patwildcard, None, str("nonempty")),
            ),
          ),
          Case(str("empty"), None, int(0)),
          Case(str("nonempty"), None, int(1)),
        ),
      )),
    ))
  }

  test("#3261 chained `match` with outdent, no block, in if cond") {
    val code =
      """|object small:
         |  if
         |    Nil match
         |    case Nil => "empty"
         |    case _   => "nonempty"
         |  match
         |    case "empty"    => true
         |    case "nonempty" => false
         |  then
         |    bar
         |""".stripMargin
    val layout =
      """|object small {
         |  if ((Nil match {
         |    case Nil => "empty"
         |    case _ => "nonempty"
         |  }) match {
         |    case "empty" => true
         |    case "nonempty" => false
         |  }) bar
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("small"),
      tpl(Term.If(
        tmatch(
          tmatch(
            tname("Nil"),
            Case(tname("Nil"), None, str("empty")),
            Case(patwildcard, None, str("nonempty")),
          ),
          Case(str("empty"), None, bool(true)),
          Case(str("nonempty"), None, bool(false)),
        ),
        tname("bar"),
        Lit.Unit(),
        Nil,
      )),
    ))
  }

  test("#3261 chained `match` with outdent, block, in if cond") {
    val code =
      """|object small:
         |  if
         |    foo()
         |    Nil match
         |    case Nil => "empty"
         |    case _   => "nonempty"
         |  match
         |    case "empty"    => true
         |    case "nonempty" => false
         |  then
         |    bar
         |""".stripMargin
    val layout =
      """|object small {
         |  if ({
         |    foo()
         |    Nil match {
         |      case Nil => "empty"
         |      case _ => "nonempty"
         |    }
         |  } match {
         |    case "empty" => true
         |    case "nonempty" => false
         |  }) bar
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("small"),
      tpl(Term.If(
        tmatch(
          blk(
            tapply(tname("foo")),
            tmatch(
              tname("Nil"),
              Case(tname("Nil"), None, str("empty")),
              Case(patwildcard, None, str("nonempty")),
            ),
          ),
          Case(str("empty"), None, bool(true)),
          Case(str("nonempty"), None, bool(false)),
        ),
        tname("bar"),
        Lit.Unit(),
        Nil,
      )),
    ))
  }

  test("#3261 chained `match` with outdent, no block, in try") {
    val code =
      """|object small:
         |  try
         |    Nil match
         |    case Nil => "empty"
         |    case _   => "nonempty"
         |  match
         |    case "empty"    => 0
         |    case "nonempty" => 1
         |  catch
         |    case e => e.getMessage()
         |""".stripMargin
    val layout =
      """|object small {
         |  try (Nil match {
         |    case Nil => "empty"
         |    case _ => "nonempty"
         |  }) match {
         |    case "empty" => 0
         |    case "nonempty" => 1
         |  } catch {
         |    case e =>
         |      e.getMessage()
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("small"),
      tpl(Term.Try(
        tmatch(
          tmatch(
            tname("Nil"),
            Case(tname("Nil"), None, str("empty")),
            Case(patwildcard, None, str("nonempty")),
          ),
          Case(str("empty"), None, int(0)),
          Case(str("nonempty"), None, int(1)),
        ),
        Case(patvar("e"), None, tapply(tselect("e", "getMessage"))) :: Nil,
        None,
      )),
    ))
  }

  test("#3261 chained `match` with outdent, block, in try") {
    val code =
      """|object small:
         |  try
         |    foo()
         |    Nil match
         |    case Nil => "empty"
         |    case _   => "nonempty"
         |  match
         |    case "empty"    => 0
         |    case "nonempty" => 1
         |  catch
         |    case e => e.getMessage()
         |""".stripMargin
    val layout =
      """|object small {
         |  try {
         |    foo()
         |    Nil match {
         |      case Nil => "empty"
         |      case _ => "nonempty"
         |    }
         |  } match {
         |    case "empty" => 0
         |    case "nonempty" => 1
         |  } catch {
         |    case e =>
         |      e.getMessage()
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("small"),
      tpl(Term.Try(
        tmatch(
          blk(
            tapply(tname("foo")),
            tmatch(
              tname("Nil"),
              Case(tname("Nil"), None, str("empty")),
              Case(patwildcard, None, str("nonempty")),
            ),
          ),
          Case(str("empty"), None, int(0)),
          Case(str("nonempty"), None, int(1)),
        ),
        Case(patvar("e"), None, tapply(tselect("e", "getMessage"))) :: Nil,
        None,
      )),
    ))
  }

  test("#3261 partial func, in try") {
    val code =
      """|object small:
         |  try
         |    case Nil => "empty"
         |    case _   => "nonempty"
         |  catch
         |    case e => e.getMessage()
         |""".stripMargin
    val layout =
      """|object small {
         |  try {
         |    case Nil => "empty"
         |    case _ => "nonempty"
         |  } catch {
         |    case e =>
         |      e.getMessage()
         |  }
         |}
         |""".stripMargin
    runTestAssert[Stat](code, Some(layout))(Defn.Object(
      Nil,
      tname("small"),
      tpl(Term.Try(
        Term.PartialFunction(
          List(Case(tname("Nil"), None, str("empty")), Case(patwildcard, None, str("nonempty"))),
        ),
        Case(patvar("e"), None, tapply(tselect("e", "getMessage"))) :: Nil,
        None,
      )),
    ))
  }

  test("blank after template 1") {
    val code =
      """|class DerivationSpec {
         |  case class Foo()
         |
         |  {
         |    deriveEncoder[Foo]
         |  }
         |
         |  {
         |    deriveEncoder[Foo]
         |  }
         |}
         |""".stripMargin
    val layout =
      """|class DerivationSpec {
         |  case class Foo()
         |
         |  {
         |    deriveEncoder[Foo]
         |  }
         |  {
         |    deriveEncoder[Foo]
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Class(
      Nil,
      pname("DerivationSpec"),
      Nil,
      ctor,
      tpl(
        Defn.Class(List(Mod.Case()), pname("Foo"), Nil, ctorp(Nil), tplNoBody()),
        blk(tapplytype(tname("deriveEncoder"), pname("Foo"))),
        blk(tapplytype(tname("deriveEncoder"), pname("Foo"))),
      ),
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("blank after template 2") {
    val code =
      """|class DerivationSpec {
         |  case class Foo() extends Bar
         |
         |  {
         |    deriveEncoder[Foo]
         |  }
         |
         |  {
         |    deriveEncoder[Foo]
         |  }
         |}
         |""".stripMargin
    val layout =
      """|class DerivationSpec {
         |  case class Foo() extends Bar
         |
         |  {
         |    deriveEncoder[Foo]
         |  }
         |  {
         |    deriveEncoder[Foo]
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Class(
      Nil,
      pname("DerivationSpec"),
      Nil,
      ctor,
      tpl(
        Defn.Class(List(Mod.Case()), pname("Foo"), Nil, ctorp(), tplNoBody(init("Bar"))),
        blk(tapplytype(tname("deriveEncoder"), pname("Foo"))),
        blk(tapplytype(tname("deriveEncoder"), pname("Foo"))),
      ),
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def name after newline") {
    val code =
      """|def
         |  foo: Bar =
         |    baz
         |""".stripMargin
    val layout = "def foo: Bar = baz"
    val tree = Defn.Def(Nil, tname("foo"), Nil, Some(pname("Bar")), tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def colon after newline") {
    val code =
      """|def foo
         |  : Bar =
         |    baz
         |""".stripMargin
    val layout = "def foo: Bar = baz"
    val tree = Defn.Def(Nil, tname("foo"), Nil, Some(pname("Bar")), tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def newline after colon") {
    val code =
      """|def foo
         |  :
         |  Bar =
         |    baz
         |""".stripMargin
    val layout = "def foo: Bar = baz"
    val tree = Defn.Def(Nil, tname("foo"), Nil, Some(pname("Bar")), tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def newline around hash") {
    val code =
      """|def foo: Bar
         |  #
         |  SubBar =
         |    baz
         |""".stripMargin
    val layout = "def foo: Bar#SubBar = baz"
    val tree = Defn
      .Def(Nil, tname("foo"), Nil, Some(Type.Project(pname("Bar"), pname("SubBar"))), tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def equals after newline, without type") {
    val code =
      """|def foo
         |  =
         |    baz
         |""".stripMargin
    val layout = "def foo = baz"
    val tree = Defn.Def(Nil, tname("foo"), Nil, None, tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def equals after newline, with type") {
    val code =
      """|def foo: Bar
         |  =
         |    baz
         |""".stripMargin
    val layout = "def foo: Bar = baz"
    val tree = Defn.Def(Nil, tname("foo"), Nil, Some(pname("Bar")), tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3571 scala3") {
    val code =
      """|new A {
         |  def b: C =
         |    ???
         |}
         |""".stripMargin
    val layout =
      """|new A { def b: C = ??? }
         |""".stripMargin
    val tree = Term.NewAnonymous(
      tpl(List(init("A")), List(Defn.Def(Nil, tname("b"), Nil, Some(pname("C")), tname("???")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3605 scala3") {
    val code =
      """|new A {
         |  def b: C =
         |    ???
         |
         |}
         |""".stripMargin
    val layout =
      """|new A { def b: C = ??? }
         |""".stripMargin
    val tree = Term.NewAnonymous(
      tpl(List(init("A")), List(Defn.Def(Nil, tname("b"), Nil, Some(pname("C")), tname("???")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3617 scala3") {
    val codeWithoutBlank =
      """|object Test {
         |  def bar =
         |    `f-oo`
         |}
         |""".stripMargin
    val codeWithBlank =
      """|object Test {
         |  def bar =
         |    `f-oo`
         |
         |}
         |""".stripMargin
    val layout = "object Test { def bar = `f-oo` }"
    val tree = Defn
      .Object(Nil, tname("Test"), tpl(Defn.Def(Nil, tname("bar"), Nil, None, tname("f-oo"))))
    runTestAssert[Stat](codeWithoutBlank, layout)(tree)
    runTestAssert[Stat](codeWithBlank, layout)(tree)
  }

  test("scala3 code using CRLF 1: fewer braces and leading infix") {
    val code =
      """|object A:
         |  foo.map:
         |    bar
         |    + baz
         |""".stripMargin
    val layout =
      """|object A {
         |  foo.map {
         |    bar + baz
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(tapply(tselect("foo", "map"), blk(tinfix(tname("bar"), "+", tname("baz"))))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("fewer braces and leading infix 2") {
    val code =
      """|object A:
         |  foo.map:
         |    bar
         |  + baz
         |""".stripMargin
    val layout =
      """|object A {
         |  foo.map {
         |    bar
         |  } + baz
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(tinfix(tapply(tselect("foo", "map"), blk(tname("bar"))), "+", tname("baz"))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("optional braces and leading infix 3") {
    val code =
      """|object A:
         |  foo match
         |    case bar => bar * bar
         |  + baz
         |""".stripMargin
    val layout =
      """|object A {
         |  (foo match {
         |    case bar =>
         |      bar * bar
         |  }) + baz
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(tinfix(
        tmatch(tname("foo"), Case(patvar("bar"), None, tinfix(tname("bar"), "*", tname("bar")))),
        "+",
        tname("baz"),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scala3 code using CRLF 2: for-yield without braces") {
    val code =
      """|object A:
         |  for
         |    a <- foo
         |    b <- bar
         |  yield
         |    a + b
         |""".stripMargin
    val layout =
      """|object A { for (a <- foo; b <- bar) yield a + b }
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Term.ForYield(
        List(
          Enumerator.Generator(patvar("a"), tname("foo")),
          Enumerator.Generator(patvar("b"), tname("bar")),
        ),
        tinfix(tname("a"), "+", tname("b")),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scala3 code using CRLF 3: for-yield with braces") {
    val code =
      """|object A:
         |  for {
         |    a <- foo
         |    b <- bar
         |  }
         |  yield
         |    a + b
         |""".stripMargin
    val layout =
      """|object A { for (a <- foo; b <- bar) yield a + b }
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Term.ForYield(
        List(
          Enumerator.Generator(patvar("a"), tname("foo")),
          Enumerator.Generator(patvar("b"), tname("bar")),
        ),
        tinfix(tname("a"), "+", tname("b")),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scala3 code using CRLF 4: for-yield with parens") {
    val code =
      """|object A:
         |  for (
         |    a <- foo;
         |    b <- bar
         |  )
         |  yield
         |    a + b
         |""".stripMargin
    val layout =
      """|object A { for (a <- foo; b <- bar) yield a + b }
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Term.ForYield(
        List(
          Enumerator.Generator(patvar("a"), tname("foo")),
          Enumerator.Generator(patvar("b"), tname("bar")),
        ),
        tinfix(tname("a"), "+", tname("b")),
      )),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scala3 code using CRLF 5: match operator with select") {
    val code =
      """|object A:
         |  foo.match
         |    case bar => baz
         |  .qux
         |""".stripMargin
    val layout =
      """|object A {
         |  foo.match {
         |    case bar => baz
         |  }.qux
         |}
         |""".stripMargin
    val tree = Defn
      .Object(Nil, "A", tpl(tselect(tselectmatch("foo", Case(patvar("bar"), None, "baz")), "qux")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scala3 code using CRLF 6: while-do") {
    val code =
      """|object A:
         |  while
         |    a < b
         |  do
         |    a + b
         |""".stripMargin
    val layout =
      """|object A { while (a < b) a + b }
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Term.While(tinfix(tname("a"), "<", tname("b")), tinfix(tname("a"), "+", tname("b")))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("args-like tuple after optional braces") {
    val code =
      """|def foo =
         |  val baz =
         |    if (qux) quux
         |    else fred // not very, but a somewhat long comment
         |  (baz, null)
         |""".stripMargin
    val layout =
      """|def foo = {
         |  val baz = if (qux) quux else fred // not very, but a somewhat long comment
         |  (baz, null)
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      None,
      blk(
        Defn.Val.newBuilder(
          Nil,
          List(patvar("baz")),
          None,
          Term.If(tname("qux"), tname("quux"), tname("fred"), Nil),
        ).endComment(Seq("// not very, but a somewhat long comment")).result(),
        Term.Tuple(List(tname("baz"), Lit.Null())),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case at same indent as line containing match, then template") {
    val code =
      """|object Definitions:
         |  def foo = bar
         |    .baz match
         |    case none => false
         |
         |  class Qux:
         |    self =>
         |  end Qux
         |""".stripMargin
    val layout =
      """|object Definitions {
         |  def foo = bar.baz match {
         |    case none => false
         |  }
         |  class Qux { self => }
         |  end Qux
         |}
         |""".stripMargin
    val tree = Defn.Object(
      Nil,
      "Definitions",
      tpl(
        Defn.Def(
          Nil,
          "foo",
          Nil,
          Nil,
          None,
          tmatch(tselect("bar", "baz"), Case(patvar("none"), None, lit(false))),
        ),
        Defn.Class(Nil, pname("Qux"), Nil, ctor, tpl(Nil, tplBody("self"))),
        Term.EndMarker("Qux"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4147 func") {
    implicit val dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def a: Int = c { D => e =>
         |  def f = e
         |
         |  f
         |}
         |""".stripMargin
    val layout =
      """|def a: Int = c {
         |  D => e => {
         |    def f = e
         |    f
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "a",
      Nil,
      Some("Int"),
      tapply(
        "c",
        blk(tfunc(tparam("D"))(tfunc(tparam("e"))(blk(Defn.Def(Nil, "f", Nil, None, "e"), "f")))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4147 poly func") {
    implicit val dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code =
      """|def a: Int = c { [D] => e =>
         |  def f = e
         |
         |  f
         |}
         |""".stripMargin
    val layout =
      """|def a: Int = c {
         |  [D] => e => {
         |    def f = e
         |    f
         |  }
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      "a",
      Nil,
      Some("Int"),
      tapply(
        "c",
        blk(tpolyfunc(pparam("D"))(tfunc(tparam("e"))(blk(Defn.Def(Nil, "f", Nil, None, "e"), "f")))),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4252") {
    val code =
      """|foo:
         |  case (x, y) =>
         |    case "abc" => ()
         |""".stripMargin
    val layout =
      """|foo {
         |  case (x, y) =>
         |    {
         |      case "abc" => ()
         |    }
         |}
         |""".stripMargin
    val tree = tapply(
      "foo",
      Term.PartialFunction(List(Case(
        Pat.Tuple(List(patvar("x"), patvar("y"))),
        None,
        Term.PartialFunction(List(Case(lit("abc"), None, lit()))),
      ))),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("break+indent in then-body before else") {
    val code =
      """|  if (foo)
         |    if (bar) baz &&
         |      qux
         |    else quux
         |  else
         |    xyzzy
         |""".stripMargin
    val layout = "if (foo) if (bar) baz && qux else quux else xyzzy"
    val tree = Term.If("foo", Term.If("bar", tinfix("baz", "&&", "qux"), "quux", Nil), "xyzzy", Nil)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("break+noindent in then-body before else") {
    val code =
      """|  if (foo)
         |    if (bar) baz &&
         |    qux
         |    else quux
         |  else
         |    xyzzy
         |""".stripMargin
    val layout = "if (foo) if (bar) baz && qux else quux else xyzzy"
    val tree = Term.If("foo", Term.If("bar", tinfix("baz", "&&", "qux"), "quux", Nil), "xyzzy", Nil)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("semicolon within lambda as arg in parens") {
    val code =
      """|transformAfter(
         |  phase,
         |  sd =>
         |    sd.setFlag(flags);
         |    sd
         |)
         |""".stripMargin
    val layout =
      """|transformAfter(phase, sd => {
         |  sd.setFlag(flags)
         |  sd
         |})
         |""".stripMargin
    val tree = tapply(
      "transformAfter",
      "phase",
      tfunc(tparam("sd"))(blk(tapply(tselect("sd", "setFlag"), "flags"), "sd")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  // the trees inside the parsed one that end at EOF
  private def endAtEof(tree: Tree): List[String] = tree.collect {
    case t if (t ne tree) && t.tokens.lastOption.exists(_.is[Token.EOF]) => t.productPrefix
  }

  test("region closed at EOF: package, enum") {
    val code =
      """|package a
         |
         |enum E:
         |  case A""".stripMargin
    assertEquals(endAtEof(source(code)), Nil)
  }

  test("region closed at EOF: package, enum, newline") {
    val code =
      """|package a
         |
         |enum E:
         |  case A
         |""".stripMargin
    assertEquals(endAtEof(source(code)), Nil)
  }

  test("region closed at EOF: package, extension, comment") {
    val code =
      """|package a
         |
         |extension (s: String)
         |
         |  /** c */
         |  def f: Unit =
         |    ???""".stripMargin
    assertEquals(endAtEof(source(code)), Nil)
  }

  test("region closed at EOF: package with colon, object") {
    val code =
      """|package a:
         |  object A:
         |    def f = 1""".stripMargin
    assertEquals(endAtEof(source(code)), Nil)
  }

  test("region closed at EOF: object, def") {
    val code =
      """|object A:
         |  def f =
         |    1""".stripMargin
    assertEquals(endAtEof(source(code)), Nil)
  }

  test("region closed at EOF: object, def, as stat") {
    val code =
      """|object A:
         |  def f =
         |    1""".stripMargin
    assertEquals(endAtEof(stat(code)), Nil)
  }

  test("region closed at EOF: object in braces") {
    val code =
      """|object A {
         |  def f = 1
         |}""".stripMargin
    assertEquals(endAtEof(source(code)), Nil)
  }

  test("anonymous class: a continued `val`, a colon body, `+` at the column of the continuation") {
    val code =
      """|object A:
         |  val e = 1
         |    + q
         |  val v = new B:
         |      def x = 1
         |    + 2
         |  def h = 1
         |""".stripMargin
    val layout =
      """|object A {
         |  val e = 1 + q
         |  val v = new B { def x = 1 } + 2
         |  def h = 1
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(
        Defn.Val(Nil, List(patvar("e")), None, tinfix(int(1), "+", tname("q"))),
        Defn.Val(
          Nil,
          List(patvar("v")),
          None,
          tinfix(
            Term.NewAnonymous(tpl(List(init("B")), List(Defn.Def(Nil, tname("x"), Nil, None, int(1))))),
            "+",
            int(2),
          ),
        ),
        Defn.Def(Nil, tname("h"), Nil, None, int(1)),
      ),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("anonymous class: a continued statement in a `case` body in braces, a colon body, `+` at the column of the continuation") {
    val code =
      """|object A:
         |  def f = xs.map {
         |    case 1 =>
         |      val v = a
         |        + b
         |      new B:
         |          def x = 1
         |        + 2
         |  }
         |""".stripMargin
    val layout =
      """|object A {
         |  def f = xs.map {
         |    case 1 =>
         |      val v = a + b
         |      new B { def x = 1 } + 2
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        tapply(
          tselect("xs", "map"),
          Term.PartialFunction(List(Case(
            int(1),
            None,
            blk(
              Defn.Val(Nil, List(patvar("v")), None, tinfix(tname("a"), "+", tname("b"))),
              tinfix(
                Term
                  .NewAnonymous(tpl(List(init("B")), List(Defn.Def(Nil, tname("x"), Nil, None, int(1))))),
                "+",
                int(2),
              ),
            ),
          ))),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("match: a `case` body on the next line, leading infix at the column of the `case`") {
    val code =
      """|object A:
         |  def f =
         |    y match
         |      case 1 =>
         |        a
         |      + b
         |""".stripMargin
    val error =
      """|<input>:6: error: `outdent` expected but `identifier` found
         |      + b
         |      ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("lambda in braces with a parameter on the line after the brace and a deeper next line, a shallower next statement") {
    val code =
      """|object O:
         |  def h = xs.foreach {
         |    x =>
         |      g(x)
         |    k(x)
         |  }
         |""".stripMargin
    val layout =
      """|object O {
         |  def h = xs.foreach {
         |    x => g(x)
         |    k(x)
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("O"),
      tpl(Defn.Def(
        Nil,
        tname("h"),
        Nil,
        None,
        tapply(
          tselect("xs", "foreach"),
          blk(tfunc(tparam("x"))(tapply(tname("g"), tname("x"))), tapply(tname("k"), tname("x"))),
        ),
      )),
    )))
    parseAndCheckTree[Source](code, layout)(tree)
  }

  test("a carriage return in the indent of the second definition of an indented object body") {
    val code =
      s"""|object O:
          |  def f = 1
          |  \rdef g = 2
          |""".stripMargin
    val layout =
      """|object O { def f = 1 }
         |def g = 2
         |""".stripMargin
    val tree = Source(List(
      Defn.Object(Nil, tname("O"), tpl(Defn.Def(Nil, tname("f"), Nil, None, int(1)))),
      Defn.Def(Nil, tname("g"), Nil, None, int(2)),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`throw` at the end of a line within parentheses, a colon argument on the next line, the argument on the line after, an expression at the column of the colon argument") {
    val code =
      """|object A:
         |  def f =
         |    g(throw
         |        foo:
         |          z
         |        b)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwThrow [26..31)
         |Ident(foo) [40..43)
         |Colon [43..44)
         |Indentation.Indent [44..44)
         |Ident(z) [55..56)
         |Indentation.Outdent [56..56)
         |Ident(b) [65..66)
         |RightParen [66..67)
         |LF [67..68)
         |Ident(h) [72..73)
         |Indentation.Outdent [73..73)
         |Indentation.Outdent [73..73)
         |EOF [74..74)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(throw foo {
         |      z
         |    } b)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(tname("g"), Term.Throw(tpostfix(tapply(tname("foo"), blk(tname("z"))), "b"))),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`throw` at the end of a line within parentheses, a colon argument on the next line, the argument on the line after, a blank line, an expression at the column of the colon argument") {
    val code =
      """|object A:
         |  def f =
         |    g(throw
         |        foo:
         |          z
         |
         |        b)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwThrow [26..31)
         |Ident(foo) [40..43)
         |Colon [43..44)
         |Indentation.Indent [44..44)
         |Ident(z) [55..56)
         |Indentation.Outdent [57..57)
         |Ident(b) [66..67)
         |RightParen [67..68)
         |LF [68..69)
         |Ident(h) [73..74)
         |Indentation.Outdent [74..74)
         |Indentation.Outdent [74..74)
         |EOF [75..75)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    g(throw foo {
         |      z
         |    } b)
         |    h
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tapply(tname("g"), Term.Throw(tpostfix(tapply(tname("foo"), blk(tname("z"))), "b"))),
          tname("h"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`throw` at the end of a line within parentheses, a colon argument on the next line, the argument on the line after, a blank line, a definition and an expression at the column of the colon argument") {
    val code =
      """|object A:
         |  def f =
         |    g(throw
         |        foo:
         |          z
         |
         |        val c = 1
         |        c)
         |    h
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(g) [24..25)
         |LeftParen [25..26)
         |KwThrow [26..31)
         |Ident(foo) [40..43)
         |Colon [43..44)
         |Indentation.Indent [44..44)
         |Ident(z) [55..56)
         |Indentation.Outdent [57..57)
         |KwVal [66..69)
         |Ident(c) [70..71)
         |Equals [72..73)
         |Constant.Int(1) [74..75)
         |Ident(c) [84..85)
         |RightParen [85..86)
         |LF [86..87)
         |Ident(h) [91..92)
         |Indentation.Outdent [92..92)
         |Indentation.Outdent [92..92)
         |EOF [93..93)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val error =
      """|<input>:7: error: `)` expected but `val` found
         |        val c = 1
         |        ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("`throw` at the end of a line within a condition, a colon argument on the next line, the argument on the line after, an expression at the column of the colon argument") {
    val code =
      """|object A:
         |  def f =
         |    if throw
         |        foo:
         |          z
         |        b
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwThrow [27..32)
         |Ident(foo) [41..44)
         |Colon [44..45)
         |Indentation.Indent [45..45)
         |Ident(z) [56..57)
         |Indentation.Outdent [57..57)
         |Ident(b) [66..67)
         |KwThen [72..76)
         |Ident(c) [77..78)
         |KwElse [79..83)
         |Ident(d) [84..85)
         |LF [85..86)
         |Ident(g) [90..91)
         |Indentation.Outdent [91..91)
         |Indentation.Outdent [91..91)
         |EOF [92..92)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (throw foo {
         |      z
         |    } b) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Throw(tpostfix(tapply(tname("foo"), blk(tname("z"))), "b")),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`throw` at the end of a line within a condition, a colon argument on the next line, the argument on the line after, a blank line, an expression at the column of the colon argument") {
    val code =
      """|object A:
         |  def f =
         |    if throw
         |         foo:
         |           z
         |
         |         b
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwThrow [27..32)
         |Ident(foo) [42..45)
         |Colon [45..46)
         |Indentation.Indent [46..46)
         |Ident(z) [58..59)
         |Indentation.Outdent [60..60)
         |Ident(b) [70..71)
         |KwThen [76..80)
         |Ident(c) [81..82)
         |KwElse [83..87)
         |Ident(d) [88..89)
         |LF [89..90)
         |Ident(g) [94..95)
         |Indentation.Outdent [95..95)
         |Indentation.Outdent [95..95)
         |EOF [96..96)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (throw foo {
         |      z
         |    } b) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Throw(tpostfix(tapply(tname("foo"), blk(tname("z"))), "b")),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`throw` at the end of a line within a condition, a colon argument on the next line, the argument on the line after, a comment line, an expression at the column of the colon argument") {
    val code =
      """|object A:
         |  def f =
         |    if throw
         |        foo:
         |          z
         |        // c
         |        b
         |    then c else d
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwThrow [27..32)
         |Ident(foo) [41..44)
         |Colon [44..45)
         |Indentation.Indent [45..45)
         |Ident(z) [56..57)
         |Indentation.Outdent [57..57)
         |Ident(b) [79..80)
         |KwThen [85..89)
         |Ident(c) [90..91)
         |KwElse [92..96)
         |Ident(d) [97..98)
         |LF [98..99)
         |Ident(g) [103..104)
         |Indentation.Outdent [104..104)
         |Indentation.Outdent [104..104)
         |EOF [105..105)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (throw foo {
         |      z
         |    }
         |      // c
         |      b) c else d
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            Term.Throw(Term.SelectPostfix(
              tapply(tname("foo"), blk(tname("z"))),
              Term.Name.newBuilder("b").begComment(detachedComments("// c")).result(),
            )),
            tname("c"),
            tname("d"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("`throw` at the end of a line within an inline `then` body nested in a condition, a colon argument on the next line, the argument on the line after, an expression at the column of the colon argument") {
    val code =
      """|object A:
         |  def f =
         |    if if a then throw
         |          foo:
         |            z
         |          b
         |       else d
         |    then c else e
         |    g
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(A) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |KwIf [24..26)
         |KwIf [27..29)
         |Ident(a) [30..31)
         |KwThen [32..36)
         |KwThrow [37..42)
         |Ident(foo) [53..56)
         |Colon [56..57)
         |Indentation.Indent [57..57)
         |Ident(z) [70..71)
         |Indentation.Outdent [71..71)
         |Ident(b) [82..83)
         |KwElse [91..95)
         |Ident(d) [96..97)
         |KwThen [102..106)
         |Ident(c) [107..108)
         |KwElse [109..113)
         |Ident(e) [114..115)
         |LF [115..116)
         |Ident(g) [120..121)
         |Indentation.Outdent [121..121)
         |Indentation.Outdent [121..121)
         |EOF [122..122)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
    val layout =
      """|object A {
         |  def f = {
         |    if (if (a) throw foo {
         |      z
         |    } b else d) c else e
         |    g
         |  }
         |}
         |""".stripMargin
    val tree = Source(List(Defn.Object(
      Nil,
      tname("A"),
      tpl(Defn.Def(
        Nil,
        tname("f"),
        Nil,
        None,
        blk(
          tif(
            tif(
              tname("a"),
              Term.Throw(tpostfix(tapply(tname("foo"), blk(tname("z"))), "b")),
              tname("d"),
            ),
            tname("c"),
            tname("e"),
          ),
          tname("g"),
        ),
      )),
    )))
    runTestAssert[Source](code, layout)(tree)
  }

  test("a carriage return in the indent of a `def` body on the next line") {
    val code =
      s"""|object O:
          |  def f =
          |    \r1
          |  def g = 2
          |""".stripMargin
    val error =
      """|<input>:3: error: illegal start of simple expression
         |    
         |    ^""".stripMargin
    runTestError[Source](code, error)
  }

  test(
    "block comment with a line break right after its opener, a statement after it on its last line",
  ) {
    val code =
      """|def f =
         |  a
         |  /*
         |  */ b
         |""".stripMargin
    val b = tname("b").toBuilder.begComment(Seq("/*\n  */")).result()
    assertTree(parseStat(code))(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))
  }

  test("block comment deeper than its last line, a line break right after its opener, a statement after it") {
    val code =
      """|def f =
         |  a
         |    /*
         |  x */ b
         |""".stripMargin
    val b = tname("b").toBuilder.begComment(Seq("/*\n  x */")).result()
    assertTree(parseStat(code))(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))
  }

  test("block comment with text after its opener, a statement after it on its last line") {
    val code =
      """|def f =
         |  a
         |    /* y
         |  x */ b
         |""".stripMargin
    val layout =
      """|def f = {
         |  a
         |  /* y
         |  x */ b
         |}
         |""".stripMargin
    val b = tname("b").toBuilder.begComment(Seq("/* y\n  x */")).result()
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))
  }

  test("block comment with a line break right after its opener, a statement after it on its last line, at the indent of the enclosing body") {
    val code =
      """|object O:
         |  def f =
         |    a
         |    /*
         |  */ b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/*\n  */")).result()
    val b = tname("b").toBuilder.begComment(Seq("/*\n  */")).result()
    assertTree(source(code))(Source(
      List(Defn.Object(Nil, tname("O"), tpl(Defn.Def(Nil, tname("f"), Nil, None, blk(a)), b))),
    ))
  }

  test("block comment with a line break right after its opener, a statement right after it on its last line, at the indent of the enclosing body") {
    val code =
      """|object O:
         |  def f =
         |    a
         |    /*
         |  */b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/*\n  */")).result()
    val b = tname("b").toBuilder.begComment(Seq("/*\n  */")).result()
    assertTree(source(code))(Source(
      List(Defn.Object(Nil, tname("O"), tpl(Defn.Def(Nil, tname("f"), Nil, None, blk(a)), b))),
    ))
  }

  test("block comment with a carriage return right after its opener, a statement after it on its last line") {
    val code = "def f =\n  a\n  /*\r  */ b\n"
    val b = tname("b").toBuilder.begComment(Seq("/*\r  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))))
  }

  test("block comment with a CR LF right after its opener, a statement after it on its last line") {
    val code = "def f =\r\n  a\r\n  /*\r\n  */ b\r\n"
    val b = tname("b").toBuilder.begComment(Seq("/*\r\n  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))))
  }

  test("block comment with a line break right after its opener, a leading infix operator after it on its last line") {
    val code =
      """|def f =
         |  a
         |  /*
         |  */ + b
         |""".stripMargin
    val plus = tname("+").toBuilder.begComment(Seq("/*\n  */")).result()
    assertTree(source(code))(Source(
      List(Defn.Def(Nil, tname("f"), Nil, None, tinfix(tname("a"), plus, tname("b")))),
    ))
  }

  test("block comment opened after a statement, a line break right after its opener, a statement after it on its last line") {
    val code =
      """|def f =
         |  a /*
         |  */ b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/*\n  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(a, tname("b"))))))
  }

  test("block comment with a line break right after its opener, a `then` body after it on its last line") {
    val code =
      """|def f =
         |  if c then
         |    /*
         |    */ b
         |  else d
         |""".stripMargin
    val b = tname("b").toBuilder.begComment(Seq("/*\n    */")).result()
    assertTree(source(code))(Source(
      List(Defn.Def(Nil, tname("f"), Nil, None, tif(tname("c"), b, tname("d")))),
    ))
  }

  test("block comment of several lines, a statement after it on its last line") {
    val code =
      """|def f =
         |  a
         |  /*
         | x
         |  */ b
         |""".stripMargin
    val b = tname("b").toBuilder.begComment(Seq("/*\n x\n  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))))
  }

  test("block comment with a line break right after its opener at the top level, a definition after it on its last line") {
    val code =
      """|object A:
         |  def f = 1
         |/*
         |*/ val b = 2
         |""".stripMargin
    val layout =
      """|object A { def f = 1 }
         |/*
         |*/ val b = 2
         |""".stripMargin
    assertNoDiff(source(code).reprint, layout)
  }

  test("block comment opened after a statement, a line break right after its opener, a statement after it on a deeper last line") {
    val code =
      """|def f =
         |  a /*
         |    */ b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/*\n    */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(a, tname("b"))))))
  }

  test("block comment with ragged lines, its last line at the indent of the statement before it, a statement after it") {
    val code =
      """|def f =
         |  a
         |  /* x
         |y
         |      z
         |  */ b
         |""".stripMargin
    val b = tname("b").toBuilder.begComment(Seq("/* x\ny\n      z\n  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))))
  }

  test("block comment with ragged lines, its last line deeper than the statement before it, a statement after it") {
    val code =
      """|def f =
         |  a
         |  /*
         |x
         |    */ b
         |""".stripMargin
    val b = tname("b").toBuilder.begComment(Seq("/*\nx\n    */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))))
  }

  test("block comment with ragged lines, a leading infix operator after it on its last line") {
    val code =
      """|def f =
         |  a
         |  /* x
         | y
         |        z
         |  */ + b
         |""".stripMargin
    val plus = tname("+").toBuilder.begComment(Seq("/* x\n y\n        z\n  */")).result()
    assertTree(source(code))(Source(
      List(Defn.Def(Nil, tname("f"), Nil, None, tinfix(tname("a"), plus, tname("b")))),
    ))
  }

  test("block comment with ragged lines and a blank line, a statement after it on its last line") {
    val code =
      """|def f =
         |  a
         |  /* x
         |
         |      z
         |  */ b
         |""".stripMargin
    val b = tname("b").toBuilder.begComment(Seq("/* x\n\n      z\n  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("a"), b)))))
  }

  test("block comment opened after a statement, with ragged lines, a statement after it on its last line") {
    val code =
      """|def f =
         |  a /* x
         |y
         |      z
         |  */ b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\ny\n      z\n  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(a, tname("b"))))))
  }

  test("block comment opened after a statement, a carriage return right after its opener, a statement after it on its last line") {
    val code = "def f =\n  a /*\r  */ b\n"
    val a = tname("a").toBuilder.endComment(Seq("/*\r  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(a, tname("b"))))))
  }

  test("block comment opened after a statement, a CR LF right after its opener, a statement after it on its last line") {
    val code = "def f =\r\n  a /*\r\n  */ b\r\n"
    val a = tname("a").toBuilder.endComment(Seq("/*\r\n  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(a, tname("b"))))))
  }

  test("block comment opened after a statement, a one-line block comment and a statement after it on its last line") {
    val code =
      """|def f =
         |  a /* x
         |  */ /* y */ b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\n  */", "/* y */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(a, tname("b"))))))
  }

  test("block comment opened after a statement, a second one opened on its last line, a statement after the second") {
    val code =
      """|def f =
         |  a /* x
         |  */ /* y
         |  */ b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\n  */", "/* y\n  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(a, tname("b"))))))
  }

  test("block comment opened after a statement, a statement on the line after its last line") {
    val code =
      """|def f =
         |  a /* x
         |  */
         |  b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\n  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, blk(a, tname("b"))))))
  }

  test("block comment opened after a statement, a leading infix operator after it on its last line") {
    val code =
      """|def f =
         |  a /* x
         |  */ + b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\n  */")).result()
    assertTree(source(code))(Source(
      List(Defn.Def(Nil, tname("f"), Nil, None, tinfix(a, tname("+"), tname("b")))),
    ))
  }

  test("block comment opened after a statement, a leading infix operator after it on a last line left of the statement") {
    val code =
      """|object O:
         |  def f =
         |    a /* x
         |  */ + b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\n  */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, tinfix(a, tname("+"), tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after an infix operator, its operand after it on its last line") {
    val code =
      """|def f =
         |  a + /* x
         |  */ b
         |""".stripMargin
    val plus = tname("+").toBuilder.endComment(Seq("/* x\n  */")).result()
    assertTree(source(code))(Source(
      List(Defn.Def(Nil, tname("f"), Nil, None, tinfix(tname("a"), plus, tname("b")))),
    ))
  }

  test("block comment opened after `=`, the body after it on a deeper last line") {
    val code =
      """|def f = /* x
         |  */ b
         |""".stripMargin
    val b = tname("b").toBuilder.begComment(Seq("/* x\n  */")).result()
    assertTree(source(code))(Source(List(Defn.Def(Nil, tname("f"), Nil, None, b))))
  }

  test("block comment opened after a body, a definition after it on a last line left of the body") {
    val code =
      """|object O:
         |  def f =
         |    a /* x
         |  */ def g = 1
         |""".stripMargin
    val f = Defn.Def(Nil, tname("f"), Nil, None, tname("a")).toBuilder.endComment(Seq("/* x\n  */"))
      .result()
    assertTree(source(code))(Source(
      List(Defn.Object(Nil, tname("O"), tpl(f, Defn.Def(Nil, tname("g"), Nil, None, int(1))))),
    ))
  }

  test("block comment opened after a body, text before its closer, a statement after it on a last line left of the body") {
    val code =
      """|object O:
         |  def f =
         |    a /* x
         |  yyyy */ b
         |""".stripMargin
    // dotty (a41): `def f = a` and `b`, with the indent of the text before `*/`
    val f = Defn.Def(Nil, tname("f"), Nil, None, tname("a")).toBuilder
      .endComment(Seq("/* x\n  yyyy */")).result()
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f, tname("b"))))))
  }

  test("block comment opened after a statement, text before its closer, a statement after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    a /* x
         |    yyyy */ b
         |  def g = 1
         |""".stripMargin
    // dotty (a43): `def f = { a; b }`
    val a = tname("a").toBuilder.endComment(Seq("/* x\n    yyyy */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(a, tname("b")))
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f, g)))))
  }

  test("block comment opened after a definition, a definition after it on its last line") {
    val code =
      """|object O:
         |  val a = 1 /* x
         |  */ val b = 2
         |  def g = 3
         |""".stripMargin
    // dotty (a42): `val a = 1`, `val b = 2` and `def g = 3`
    val a = Defn.Val(Nil, List(Pat.Var(tname("a"))), None, int(1)).toBuilder
      .endComment(Seq("/* x\n  */")).result()
    val b = Defn.Val(Nil, List(Pat.Var(tname("b"))), None, int(2))
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(3))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(a, b, g)))))
  }

  test("block comment opened after `then`, the body after it on a deeper last line") {
    val code =
      """|def f =
         |  if c then /* x
         |    */ b
         |  else d
         |""".stripMargin
    val b = tname("b").toBuilder.begComment(Seq("/* x\n    */")).result()
    assertTree(source(code))(Source(
      List(Defn.Def(Nil, tname("f"), Nil, None, tif(tname("c"), b, tname("d")))),
    ))
  }

  test("block comment opened after the colon of a colon argument, the argument after it on a deeper last line") {
    val code =
      """|def f =
         |  xs.foo: /* x
         |    */ bar
         |""".stripMargin
    val bar = tname("bar").toBuilder.begComment(Seq("/* x\n    */")).result()
    assertTree(source(code))(Source(
      List(Defn.Def(Nil, tname("f"), Nil, None, tapply(tselect(tname("xs"), tname("foo")), blk(bar)))),
    ))
  }

  test(
    "block comment opened after the colon of an object, a definition after it on its last line",
  ) {
    val code =
      """|object A: /* x
         |*/ val b = 2
         |""".stripMargin
    val error =
      """|<input>:1: error: template body: `indent` expected but `\n` found
         |object A: /* x
         |              ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("block comment opened after the colon of an object, a second one opened on its last line, a definition after the second") {
    val code =
      """|object A: /* x
         |*/ /* y
         |*/ val b = 2
         |""".stripMargin
    val error =
      """|<input>:1: error: template body: `indent` expected but `\n` found
         |object A: /* x
         |              ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("block comment opened after a lambda arrow, the body after it on a deeper last line") {
    val code =
      """|object O:
         |  def f =
         |    xs.map: x => /* c
         |      */ g(x)
         |    h
         |""".stripMargin
    val gx = tapply(tname("g"), tname("x")).toBuilder.begComment(Seq("/* c\n      */")).result()
    val map = tapply(tselect(tname("xs"), tname("map")), blk(tfunc(tparam("x"))(gx)))
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(map, tname("h")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after a lambda arrow, a body of two lines starting on its deeper last line") {
    val code =
      """|object O:
         |  def f =
         |    xs.map: x => /* c
         |      */ g(x)
         |      k(x)
         |    h
         |""".stripMargin
    val gx = tapply(tname("g"), tname("x")).toBuilder.begComment(Seq("/* c\n      */")).result()
    val body = blk(gx, tapply(tname("k"), tname("x")))
    val map = tapply(tselect(tname("xs"), tname("map")), blk(tfunc(tparam("x"))(body)))
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(map, tname("h")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after an argument, a comma after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    g(a /* x
         |    */ , b)
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\n    */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, tapply(tname("g"), a, tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after the colon of a colon argument, the argument after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    xs.foo: /* x
         |    */ bar
         |""".stripMargin
    val error =
      """|<input>:3: error: expected fewer-braces method body
         |    xs.foo: /* x
         |          ^""".stripMargin
    runTestError[Source](code, error)
  }

  test("block comment opened after the colon of `new`, a definition after it on a deeper last line") {
    val code =
      """|object O:
         |  val v = new A: /* c
         |    */ def f = 1
         |  val w = 2
         |""".stripMargin
    val f = Defn.Def(Nil, tname("f"), Nil, None, int(1)).toBuilder.begComment(Seq("/* c\n    */"))
      .result()
    val v = Defn.Val(Nil, List(patvar("v")), None, Term.NewAnonymous(tpl(List(init("A")), List(f))))
    val w = Defn.Val(Nil, List(patvar("w")), None, int(2))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(v, w)))))
  }

  test("block comment opened after a `then` body, a definition after it on a last line two regions left") {
    val code =
      """|object O:
         |  def f =
         |    if c then
         |      a /* x
         |  */ def g = 1
         |""".stripMargin
    val f = Defn.Def(Nil, tname("f"), Nil, None, tif(tname("c"), tname("a"))).toBuilder
      .endComment(Seq("/* x\n  */")).result()
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f, g)))))
  }

  test("block comment opened after a case body, the next case after it on its last line") {
    val code =
      """|object O:
         |  def f = x match
         |    case 1 => a /* x
         |    */ case 2 => b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\n    */")).result()
    val m = tmatch(tname("x"), Case(int(1), None, a), Case(int(2), None, tname("b")))
    val f = Defn.Def(Nil, tname("f"), Nil, None, m)
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after a statement, the end of input after it on its last line") {
    val code = "object O:\n  def f =\n    a /* x\n  */"
    val f = Defn.Def(Nil, tname("f"), Nil, None, tname("a")).toBuilder.endComment(Seq("/* x\n  */"))
      .result()
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment with ragged lines, its last line left of the body before it, a definition after it") {
    val code =
      """|object O:
         |  def f =
         |    a
         |    /* x
         |        y
         |z
         |  */ def g = 1
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\n        y\nz\n  */")).result()
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1)).toBuilder
      .begComment(Seq("/* x\n        y\nz\n  */")).result()
    assertTree(source(code))(Source(
      List(Defn.Object(Nil, tname("O"), tpl(Defn.Def(Nil, tname("f"), Nil, None, blk(a)), g))),
    ))
  }

  test(
    "block comment opened after the colon of an object, a definition after it on a deeper last line",
  ) {
    val code =
      """|object O:
         |  object A: /* x
         |    */ val b = 2
         |  val c = 3
         |""".stripMargin
    val b = Defn.Val(Nil, List(Pat.Var(tname("b"))), None, int(2)).toBuilder
      .begComment(Seq("/* x\n    */")).result()
    val c = Defn.Val(Nil, List(Pat.Var(tname("c"))), None, int(3))
    assertTree(source(code))(Source(
      List(Defn.Object(Nil, tname("O"), tpl(Defn.Object(Nil, tname("A"), tpl(b)), c))),
    ))
  }

  test("block comment opened after a `then` body, `else` after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    if c then a /* x
         |    */ else b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\n    */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, Term.If(tname("c"), a, tname("b"), Nil))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after an enumerator, an enumerator after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    for
         |      x <- xs /* x
         |      */ y <- ys
         |    yield x
         |""".stripMargin
    val x = Enumerator.Generator(Pat.Var(tname("x")), tname("xs")).toBuilder
      .endComment(Seq("/* x\n      */")).result()
    val y = Enumerator.Generator(Pat.Var(tname("y")), tname("ys"))
    val f = Defn
      .Def(Nil, tname("f"), Nil, None, Term.ForYield(Term.EnumeratorsBlock(List(x, y)), tname("x")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after the condition of an `if` in parentheses, the body after it on a deeper last line") {
    val code =
      """|object O:
         |  def f =
         |    if (c) /* x
         |      */ a
         |    else b
         |""".stripMargin
    val a = tname("a").toBuilder.begComment(Seq("/* x\n      */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, Term.If(tname("c"), a, tname("b"), Nil))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after the condition of a `while` in parentheses, the body after it on a deeper last line") {
    val code =
      """|object O:
         |  def f =
         |    while (c) /* x
         |      */ a
         |    b
         |""".stripMargin
    val a = tname("a").toBuilder.begComment(Seq("/* x\n      */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(Term.While(tname("c"), a), tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after the enumerators of a `for` in parentheses, the body after it on a deeper last line") {
    val code =
      """|object O:
         |  def f =
         |    for (x <- xs) /* x
         |      */ a
         |    b
         |""".stripMargin
    val enums = Term.EnumeratorsBlock(List(Enumerator.Generator(Pat.Var(tname("x")), tname("xs"))))
      .toBuilder.endComment(Seq("/* x\n      */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(Term.For(enums, tname("a")), tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment on a line of its own at the indent of the statement before it, a leading infix operator after it on a last line left of the statement") {
    val code =
      """|object O:
         |  def f =
         |    a
         |    /* x
         |  */ + b
         |""".stripMargin
    // dotty, and the same input with a line break in place of the comment: `def f = a + b`
    val a = tname("a").toBuilder.endComment(Seq("/* x\n  */")).result()
    val op = tname("+").toBuilder.begComment(Seq("/* x\n  */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, tinfix(blk(a), op, tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after the colon of a colon argument, a lambda after it on a deeper last line") {
    val code =
      """|object O:
         |  def f =
         |    xs.map: /* c
         |      */ x => g(x)
         |    h
         |""".stripMargin
    // dotty (a45): `xs.map(x => g(x))` and `h`, as without the comment (a46)
    val fn = tfunc(tparam("x"))(tapply(tname("g"), tname("x"))).toBuilder
      .begComment(Seq("/* c\n      */")).result()
    val map = tapply(tselect(tname("xs"), tname("map")), blk(fn))
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(map, tname("h")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment on a line of its own after a blank line, a leading infix operator after it on a last line left of the statement") {
    val code =
      """|object O:
         |  def f =
         |    a
         |
         |    /* x
         |  */ + b
         |""".stripMargin
    // dotty (a44): `def f = a` and `+b`
    val a = tname("a").toBuilder.endComment(Seq("/* x\n  */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(a))
    val plusB = Term.ApplyUnary(tname("+"), tname("b")).toBuilder.begComment(Seq("/* x\n  */"))
      .result()
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f, plusB)))))
  }

  test("block comment on a line of its own after a statement with a trailing comment, a leading infix operator after it on a last line left of the statement") {
    val code =
      """|object O:
         |  def f =
         |    a // y
         |    /* x
         |  */ + b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("// y", "/* x\n  */")).result()
    val op = tname("+").toBuilder.begComment(Seq("/* x\n  */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, tinfix(blk(a), op, tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("tokens: block comment opened after a statement, a statement after it on its last line") {
    val code =
      """|def f =
         |  a /* x
         |  */ b
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(a) [10..11)
         |LF [16..17)
         |Ident(b) [22..23)
         |Indentation.Outdent [23..23)
         |EOF [24..24)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens: block comment opened after a statement, a second one opened on its last line, a statement after the second") {
    val code =
      """|def f =
         |  a /* x
         |  */ /* y
         |  */ b
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(a) [10..11)
         |LF [16..17)
         |Ident(b) [32..33)
         |Indentation.Outdent [33..33)
         |EOF [34..34)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens: block comment opened after a statement, a statement on the line after its last line") {
    val code =
      """|def f =
         |  a /* x
         |  */
         |  b
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(a) [10..11)
         |LF [21..22)
         |Ident(b) [24..25)
         |Indentation.Outdent [25..25)
         |EOF [26..26)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens: block comment on a line of its own, a statement after it on its last line") {
    val code =
      """|def f =
         |  a
         |  /* x
         |  */ b
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(a) [10..11)
         |LF [11..12)
         |Ident(b) [24..25)
         |Indentation.Outdent [25..25)
         |EOF [26..26)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens: block comment opened after a statement, a leading infix operator after it on its last line") {
    val code =
      """|def f =
         |  a /* x
         |  */ + b
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(a) [10..11)
         |InfixLF [12..21)
         |Ident(+) [22..23)
         |Ident(b) [24..25)
         |Indentation.Outdent [25..25)
         |EOF [26..26)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens: block comment opened after a statement, a second one opened on its last line, a leading infix operator after the second") {
    val code =
      """|def f =
         |  a /* x
         |  */ /* y
         |  */ + b
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwDef [0..3)
         |Ident(f) [4..5)
         |Equals [6..7)
         |Indentation.Indent [7..7)
         |Ident(a) [10..11)
         |InfixLF [22..31)
         |Ident(+) [32..33)
         |Ident(b) [34..35)
         |Indentation.Outdent [35..35)
         |EOF [36..36)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens: block comment opened after a body, a definition after it on a last line left of the body") {
    val code =
      """|object O:
         |  def f =
         |    a /* x
         |  */ def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(O) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(a) [24..25)
         |Indentation.Outdent [35..35)
         |KwDef [36..39)
         |Ident(g) [40..41)
         |Equals [42..43)
         |Constant.Int(1) [44..45)
         |Indentation.Outdent [45..45)
         |EOF [46..46)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens: block comment on a line of its own at the indent of the body, a definition after it on a last line left of the body") {
    val code =
      """|object O:
         |  def f =
         |    a
         |    /* x
         |  */ def g = 1
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(O) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(a) [24..25)
         |Indentation.Outdent [39..39)
         |KwDef [40..43)
         |Ident(g) [44..45)
         |Equals [46..47)
         |Constant.Int(1) [48..49)
         |Indentation.Outdent [49..49)
         |EOF [50..50)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("tokens: block comment on a line of its own at the indent of the statement before it, a leading infix operator after it on a last line left of the statement") {
    val code =
      """|object O:
         |  def f =
         |    a
         |    /* x
         |  */ + b
         |""".stripMargin
    val struct =
      """|BOF [0..0)
         |KwObject [0..6)
         |Ident(O) [7..8)
         |Colon [8..9)
         |Indentation.Indent [9..9)
         |KwDef [12..15)
         |Ident(f) [16..17)
         |Equals [18..19)
         |Indentation.Indent [19..19)
         |Ident(a) [24..25)
         |Indentation.Outdent [39..39)
         |InfixLF [30..39)
         |Ident(+) [40..41)
         |Ident(b) [42..43)
         |Indentation.Outdent [43..43)
         |EOF [44..44)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Source])
  }

  test("block comment opened after an end marker, a definition after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    a
         |  end f /* x
         |  */ def g = 1
         |""".stripMargin
    val end = Term.EndMarker(tname("f")).toBuilder.endComment(Seq("/* x\n  */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, tname("a"))
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f, end, g)))))
  }

  test("block comment opened after an end marker of `if`, a statement after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    if c then
         |      a
         |    end if /* x
         |    */ b
         |""".stripMargin
    val end = Term.EndMarker(tname("if")).toBuilder.endComment(Seq("/* x\n    */")).result()
    val stat = Term.If(tname("c"), tname("a"), Lit.Unit(), Nil)
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(stat, end, tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after a statement, an end marker of a definition after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    a /* x
         |  */ end f
         |""".stripMargin
    val f = Defn.Def(Nil, tname("f"), Nil, None, tname("a")).toBuilder.endComment(Seq("/* x\n  */"))
      .result()
    assertTree(source(code))(Source(
      List(Defn.Object(Nil, tname("O"), tpl(f, Term.EndMarker(tname("f"))))),
    ))
  }

  test("block comment opened after a statement, an end marker of `if` after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    if c then
         |      a /* x
         |    */ end if
         |    b
         |""".stripMargin
    val stat = Term.If(tname("c"), tname("a"), Lit.Unit(), Nil).toBuilder
      .endComment(Seq("/* x\n    */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(stat, Term.EndMarker(tname("if")), tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after a statement, an end marker of `match` after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    x match
         |      case 1 => a /* x
         |    */ end match
         |    b
         |""".stripMargin
    val a = tname("a").toBuilder.endComment(Seq("/* x\n    */")).result()
    val m = Term.Match(tname("x"), Term.CasesBlock(List(Case(int(1), None, a))), Nil)
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(m, Term.EndMarker(tname("match")), tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after a statement, an end marker of `while` after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    while c do
         |      a /* x
         |    */ end while
         |    b
         |""".stripMargin
    val stat = Term.While(tname("c"), tname("a")).toBuilder.endComment(Seq("/* x\n    */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(stat, Term.EndMarker(tname("while")), tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after a statement, an end marker of `for` after it on its last line") {
    val code =
      """|object O:
         |  def f =
         |    for x <- xs do
         |      a /* x
         |    */ end for
         |    b
         |""".stripMargin
    val stat = Term.For(
      Term.EnumeratorsBlock(List(Enumerator.Generator(Pat.Var(tname("x")), tname("xs")))),
      tname("a"),
    ).toBuilder.endComment(Seq("/* x\n    */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(stat, Term.EndMarker(tname("for")), tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test(
    "block comment opened after a `finally` body, an end marker of `try` after it on its last line",
  ) {
    val code =
      """|object O:
         |  def f =
         |    try a
         |    finally
         |      c /* x
         |    */ end try
         |    b
         |""".stripMargin
    val stat = Term.Try(tname("a"), None, Some(tname("c"))).toBuilder.endComment(Seq("/* x\n    */"))
      .result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(stat, Term.EndMarker(tname("try")), tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after a statement, a second one opened on its last line, an end marker of `if` after the second") {
    val code =
      """|object O:
         |  def f =
         |    if c then
         |      a /* x
         |    */ /* y
         |    */ end if
         |    b
         |""".stripMargin
    val stat = Term.If(tname("c"), tname("a"), Lit.Unit(), Nil).toBuilder
      .endComment(Seq("/* x\n    */", "/* y\n    */")).result()
    val f = Defn.Def(Nil, tname("f"), Nil, None, blk(stat, Term.EndMarker(tname("if")), tname("b")))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f)))))
  }

  test("block comment opened after a statement, an end marker of `val` after it on its last line") {
    val code =
      """|object O:
         |  val (x, y) =
         |    (1, 2) /* c
         |  */ end val
         |  def g = 1
         |""".stripMargin
    val pat = Pat.Tuple(List(Pat.Var(tname("x")), Pat.Var(tname("y"))))
    val v = Defn.Val(Nil, List(pat), None, Term.Tuple(List(int(1), int(2)))).toBuilder
      .endComment(Seq("/* c\n  */")).result()
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(source(code))(Source(
      List(Defn.Object(Nil, tname("O"), tpl(v, Term.EndMarker(tname("val")), g))),
    ))
  }

  test("block comment opened after an extension body, an end marker of `extension` after it on its last line") {
    val code =
      """|object O:
         |  extension (x: Int)
         |    def f = 1 /* x
         |  */ end extension
         |  val w = 2
         |""".stripMargin
    val f = Defn.Def(Nil, tname("f"), Nil, None, int(1))
    val ext = Defn.ExtensionGroup(Nil, List(List(tparam("x", "Int"))), f).toBuilder
      .endComment(Seq("/* x\n  */")).result()
    val w = Defn.Val(Nil, List(Pat.Var(tname("w"))), None, int(2))
    assertTree(source(code))(Source(
      List(Defn.Object(Nil, tname("O"), tpl(ext, Term.EndMarker(tname("extension")), w))),
    ))
  }

  test(
    "block comment opened after a `given` body, an end marker of `given` after it on its last line",
  ) {
    val code =
      """|object O:
         |  given A with
         |    def f = 1 /* x
         |  */ end given
         |  val w = 2
         |""".stripMargin
    val f = Defn.Def(Nil, tname("f"), Nil, None, int(1)).toBuilder.endComment(Seq("/* x\n  */"))
      .result()
    val g = Defn.Given(Nil, anon, Nil, Nil, tpl(List(init(pname("A"))), List(f)))
    val w = Defn.Val(Nil, List(Pat.Var(tname("w"))), None, int(2))
    assertTree(source(code))(Source(
      List(Defn.Object(Nil, tname("O"), tpl(g, Term.EndMarker(tname("given")), w))),
    ))
  }

  test("block comment opened after an end marker, a second one opened on its last line, a definition after the second") {
    val code =
      """|object O:
         |  def f =
         |    1
         |  end f /* x
         |  */ /* y
         |  */ def g = 1
         |""".stripMargin
    val f = Defn.Def(Nil, tname("f"), Nil, None, int(1))
    val end = Term.EndMarker(tname("f")).toBuilder.endComment(Seq("/* x\n  */", "/* y\n  */"))
      .result()
    val g = Defn.Def(Nil, tname("g"), Nil, None, int(1))
    assertTree(source(code))(Source(List(Defn.Object(Nil, tname("O"), tpl(f, end, g)))))
  }

}
