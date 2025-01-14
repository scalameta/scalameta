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
    blk(tapply(tname("fa")), tapply(tname("fb")))
  )

  test("basic-example") {
    val code = """|trait A:
                  |  def f: Int
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("trait A { def f: Int }"))(
      Defn.Trait(Nil, pname("A"), Nil, EmptyCtor(), tpl(defx))
    )
  }

  test("package-comment") {
    val code = """|package mysadpackage:
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
           |""".stripMargin
      )
    )(Source(List(
      Pkg(tname("mysadpackage"), List(Decl.Def(Nil, tname("f"), Nil, Nil, pname("Int")))),
      Pkg(tname("anotherpackage"), List(Decl.Def(Nil, tname("f"), Nil, Nil, pname("Int"))))
    )))
  }

  test("multistat-example") {
    val code = """|trait A:
                  |  def f: Int
                  |  def y: String = { fa(); fb() }""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Trait(Nil, pname("A"), Nil, EmptyCtor(), tpl(defx, defy))
    )
  }

  test("anonymous-class") {
    val code = """|new A:
                  |  def f: Int
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("new A { def f: Int }"))(Term.NewAnonymous(
      tpl(List(init("A")), List(Decl.Def(Nil, tname("f"), Nil, Nil, pname("Int"))))
    ))
  }

  // https://github.com/scalameta/scalameta/issues/3093
  test("anonymous-class-in-block") {
    val code = """|if(x) {
                  |  new A:
                  |    def f: Int
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(Term.If(
      tname("x"),
      blk(Term.NewAnonymous(tpl(List(init("A")), List(Decl.Def(Nil, tname("f"), Nil, pname("Int")))))),
      Lit.Unit(),
      Nil
    ))
  }

  test("empty-anonymous-class") {
    val code = """|new:
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
           |""".stripMargin
      )
    )(Term.NewAnonymous(tpl(
      Decl.Def(Nil, tname("f"), Nil, Nil, pname("Int")),
      Decl.Def(Nil, tname("g"), Nil, Nil, pname("Int"))
    )))
  }

  test("indent-and-back") {
    val code = """|object O:
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
          tpl(Defn.Def(Nil, tname("f"), Nil, Nil, Some(pname("Int")), int(3)))
        ),
        Defn.Trait(
          Nil,
          pname("T"),
          Nil,
          EmptyCtor(),
          tpl(Defn.Def(Nil, tname("f"), Nil, Nil, Some(pname("Int")), int(4)))
        )
      )
    ))
  }

  test("then-no-indent") {
    // this test is related to dotty issue: https://github.com/lampepfl/dotty/issues/9790
    // It should either assert error during parsing: "illegal start of simple expression"
    // Or accept mismatch with parsing rules and parse as 'if (cond) { truep } else {falsep }'
    // Why error is thrown is described in mentioned issue.
    val code = """|def fn: Unit =
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
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("fn"),
      Nil,
      Nil,
      Some(pname("Unit")),
      blk(Term.If(tname("cond"), tname("truep"), tname("falsep")), tapply(tname("otherStatement")))
    ))
  }

  test("then-no-indent-wrong") {
    // this test is related to dotty issue: https://github.com/lampepfl/dotty/issues/9790
    // It should either assert error during parsing: "illegal start of simple expression"
    // Or accept mismatch with parsing rules and parse as 'if (cond) { truep } else {falsep }'
    // Why error is thrown is described in mentioned issue.
    val code = """|def fn: Unit =
                  |    if cond then
                  |  truep1
                  |  truep2
                  |    else
                  |  falsep
                  |""".stripMargin
    runTestError[Stat](code, "`;` expected but `else` found")
  }

  test("indent-inside-brace-ok") {
    val code = """|class X {
                  |  def fx(): Unit =
                  |    f1
                  |    f2
                  |}
                  |""".stripMargin
    val output = """|class X {
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
          .Def(Nil, tname("fx"), Nil, List(List()), Some(pname("Unit")), blk(tname("f1"), tname("f2")))
      )
    ))
  }

  test("indent-inside-brace-not") {
    val code = """|class X {
                  |      def fx(): Unit =
                  |    f1
                  |    f2
                  |}
                  |""".stripMargin
    val output = """|class X {
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
        tname("f2")
      )
    ))
  }

  test("should-indent-yet-brace") {
    val code = """|class X {
                  |  def fx(): Unit =
                  |  {
                  |
                  |  }
                  |  private def f2: Int = 1
                  |}
                  |""".stripMargin
    val output = """|class X {
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
        Defn.Def(List(Mod.Private(anon)), tname("f2"), Nil, Nil, Some(pname("Int")), int(1))
      )
    ))
  }

  test("selftype-class") {
    val code = """|class A extends B:
                  |  thisPhase =>
                  |  expr1
                  |  expr2
                  |""".stripMargin
    val output = """|class A extends B { thisPhase =>
                    |  expr1
                    |  expr2
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(List(init("B")), self("thisPhase"), tname("expr1"), tname("expr2"))
    ))
  }

  test("lambda-method") {
    val code = """|object X:
                  |  val fn = (pa, pb) =>
                  |    def helper = 3
                  |    3
                  |  end fn
                  |""".stripMargin
    val output = """|object X {
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
            blk(Defn.Def(Nil, tname("helper"), Nil, Nil, None, int(3)), int(3))
          )
        ),
        Term.EndMarker(tname("fn"))
      )
    ))
  }

  test("case-for-in-match") {
    val code = """|class A {
                  |    def forward: Unit = parents match
                  |      case a =>
                  |        for case a: TP <- body do
                  |          fordo
                  |      case b => ok
                  |
                  |    private def transformAnnot: Tree
                  |}
                  |""".stripMargin
    val output = """|class A {
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
                tname("fordo")
              )
            ),
            Case(patvar("b"), None, tname("ok"))
          )
        ),
        Decl.Def(List(Mod.Private(anon)), tname("transformAnnot"), Nil, Nil, pname("Tree"))
      )
    ))
  }

  test("this-constructor") {
    val code = """|class A:
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
        Nil
      ))
    ))
  }

  test("this-constructor-indented-block") {
    val code = """|class A:
                  |  def this(msg: String) =
                  |    this(message, false)
                  |    otherStat
                  |""".stripMargin
    val output = """|class A {
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
        List(tname("otherStat"))
      ))
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
           |""".stripMargin
      )
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
          Term.Assign(tname("generatedType"), tapply(tname("genTypeApply"), tname("t")))
        ),
        Case(Pat.Extract(tname("Apply"), Nil), None, blk())
      ))
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
        Some("def wrapPlaceholders(t: Tree) = try if (placeholderParams.isEmpty) t else new WildcardFunction(placeholderParams.reverse, t) finally placeholderParams = saved")
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
            init(pname("WildcardFunction"), List(tselect("placeholderParams", "reverse"), tname("t")))
          )
        ),
        Nil,
        Some(Term.Assign(tname("placeholderParams"), tname("saved")))
      )
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
           |""".stripMargin
      )
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
            Defn.Val(Nil, List(patvar("a")), Some(pname("Int")), int(3))
          )
        ),
        Decl.Val(
          Nil,
          List(patvar("refinementTest")),
          Type.Refine(Some(pname("Graph")), List(Decl.Def(Nil, tname("x"), Nil, Nil, pname("Int"))))
        )
      )
    ))
  }

  test("type-in-next-line-equals") {
    runTestAssert[Stat](
      """|val refinementTest:
         |      Int = 3
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = 3")
    )(Defn.Val(Nil, List(patvar("refinementTest")), Some(pname("Int")), int(3)))
  }

  test("type-in-next-line-equals-newline") {
    runTestAssert[Stat](
      """|val refinementTest:
         |      Int = 
         |3
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = 3")
    )(Defn.Val(Nil, List(patvar("refinementTest")), Some(pname("Int")), int(3)))
  }

  test("type-equals-separate") {
    runTestAssert[Stat](
      """|def refinementTest(a :
         |      Int = 
         |3) = a
         |""".stripMargin,
      assertLayout = Some("def refinementTest(a: Int = 3) = a")
    )(Defn.Def(
      Nil,
      tname("refinementTest"),
      Nil,
      List(List(Term.Param(Nil, tname("a"), Some(pname("Int")), Some(int(3))))),
      None,
      tname("a")
    ))
  }

  test("type-multi-seq") {
    runTestAssert[Stat](
      """|val refinementTest:
         |    Int = 
         |  fx
         |  fy
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = {\n  fx\n  fy\n}")
    )(Defn.Val(Nil, List(patvar("refinementTest")), Some(pname("Int")), blk(tname("fx"), tname("fy"))))
  }

  test("equals-block") {
    runTestAssert[Stat](
      """|val refinementTest:
         |  Int = 
         |    fx
         |    fy
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = {\n  fx\n  fy\n}")
    )(Defn.Val(Nil, List(patvar("refinementTest")), Some(pname("Int")), blk(tname("fx"), tname("fy"))))
  }

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
           |}""".stripMargin
      )
    )(Defn.Given(
      Nil,
      tname("intOrd"),
      Nil,
      Nil,
      tpl(
        List(init(papply("Ord", "Int"))),
        List(
          Defn.Def(Nil, tname("fa"), Nil, Nil, Some(pname("Int")), int(1)),
          Defn.Def(Nil, tname("fb"), Nil, Nil, Some(pname("Int")), int(2))
        )
      )
    ))
  }

  test("given-block-indent-edge-cases: `with` and no-indent") {
    runTestError[Stat](
      """|given intOrd: Ord[Int] with
         |def fa: Int = 1
         |def fb: Int = 2
         |""".stripMargin,
      """|<input>:2: error: `identifier` expected but `def` found
         |def fa: Int = 1
         |^""".stripMargin
    )
  }

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
         |""".stripMargin
    )(Defn.Given(
      Nil,
      "intOrd",
      Nil,
      tpl(
        List(init(papply("Ord", "Int"))),
        List(
          Defn.Def(Nil, "fa", Nil, Some("Int"), lit(1)),
          Defn.Def(Nil, "fb", Nil, Some("Int"), lit(2))
        )
      )
    ))
  }

  test("given-block-indent-edge-cases: coloneol [scala35]") {
    implicit val dialect: Dialect = dialects.Scala35
    runTestError[Stat](
      """|given intOrd: Ord[Int]:
         |  def fa: Int = 1
         |  def fb: Int = 2
         |""".stripMargin,
      "`;` expected but `:` found"
    )
  }

  test("given-block-indent-edge-cases: `with` another parent type") {
    runTestAssert[Stat](
      """|class A extends A with 
         |  B
         |""".stripMargin,
      assertLayout = Some("class A extends A with B")
    )(Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tplNoBody(init("A"), init("B"))))
  }

  test("nested-coloneol") {
    runTestAssert[Stat](
      """|case class Test(
         |  a: A = new A,
         |):
         |  def hello = 1
         |""".stripMargin,
      assertLayout = Some("case class Test(a: A = new A) { def hello = 1 }")
    )(Defn.Class(
      List(Mod.Case()),
      pname("Test"),
      Nil,
      ctorp(Term.Param(Nil, tname("a"), Some(pname("A")), Some(Term.New(init("A"))))),
      tpl(Defn.Def(Nil, tname("hello"), Nil, Nil, None, int(1)))
    ))
  }

  test("semicolon-closes-indent-region") {
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
           |}""".stripMargin
      )
    )(Defn.Val(
      Nil,
      List(patvar("z")),
      None,
      blk(Defn.Val(Nil, List(patvar("a")), None, int(0)), tapply(tname("f"), tname("a")))
    ))
  }

  test("observe-indented-in-braces") {
    val code = """|object X:
                  |  if (cond)
                  |    (f)
                  |  foo
                  |""".stripMargin
    val output = """|object X {
                    |  if (cond) f
                    |  foo
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(Defn.Object(
      Nil,
      tname("X"),
      tpl(Term.If(tname("cond"), tname("f"), Lit.Unit(), Nil), tname("foo"))
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
           |""".stripMargin
      )
    )(tmatch(
      tname("widen"),
      Case(
        Pat.Typed(
          Pat.Bind(patvar("tp"), Pat.Extract(tname("OrNull"), List(patvar("tp1")))),
          pname("OrType")
        ),
        None,
        blk()
      ),
      Case(patvar("tp"), None, tname("tp"))
    ))
  }

  test("object-type") {
    runTestAssert[Stat](
      """|object typeAndObjects:
         |  type Ala
         |""".stripMargin,
      assertLayout = Some("object typeAndObjects { type Ala }")
    )(Defn.Object(Nil, tname("typeAndObjects"), tpl(Decl.Type(Nil, pname("Ala"), Nil, noBounds))))
  }

  test("old-try-catch-same-indent") {
    val code = """|try 
                  |  fx
                  |  gx 
                  |  catch 
                  |  case aa =>
                  |  case bb =>
                  |  finally
                  |  cc
                  |  dd
                  |""".stripMargin
    val output = """|try {
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
      Some(blk(tname("cc"), tname("dd")))
    ))
  }

  test("if-else-same-indent") {
    val code = """|if 
                  |  cond
                  |  cond2
                  |  then 
                  |  fx
                  |  gx
                  |  else gx
                  |""".stripMargin
    val output = """|if ({
                    |  cond
                    |  cond2
                    |}) {
                    |  fx
                    |  gx
                    |} else gx
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.If(blk(tname("cond"), tname("cond2")), blk(tname("fx"), tname("gx")), tname("gx"), Nil)
    )
  }

  test("new-fordo-same-indent") {
    val code = """|for
                  |  a <- gen
                  |  do fx
                  |""".stripMargin
    val output = "for (a <- gen) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(List(Enumerator.Generator(patvar("a"), tname("gen"))), tname("fx"))
    )
  }

  test("new-for-yield-same-indent") {
    val code = """|for
                  |  a <- x
                  |  b <- y
                  |  yield fx
                  |""".stripMargin
    val output = "for (a <- x; b <- y) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(Term.ForYield(
      List(
        Enumerator.Generator(patvar("a"), tname("x")),
        Enumerator.Generator(patvar("b"), tname("y"))
      ),
      tname("fx")
    ))
  }

  test("new-for-yield-if-with-indent") {
    val code = """|for
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
        Enumerator.Guard(tinfix(tname("a"), "<", tname("b")))
      ),
      tname("fx")
    ))
  }

  test("old-for-yield-if-with-indent") {
    val code = """|for {
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
        Enumerator.Guard(tinfix(tname("a"), "<", tname("b")))
      ),
      tname("fx")
    ))
  }

  test("for-in-parens") {
    val code = """|def foo =
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
              Enumerator.Generator(patvar("b"), tapply(tname("List"), int(2)))
            ),
            tinfix(tname("a"), "+", tname("b"))
          ),
          "toSet"
        ),
        "size"
      )
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
        """|val hello = (xs match {
           |  case Nil => "empty"
           |  case x :: xs1 => "nonempty"
           |}) match {
           |  case true => 0
           |  case false => 1
           |}
           |""".stripMargin
      )
    )(Defn.Val(
      Nil,
      List(patvar("hello")),
      None,
      tmatch(
        tmatch(
          tname("xs"),
          Case(tname("Nil"), None, str("empty")),
          Case(patinfix(patvar("x"), "::", patvar("xs1")), None, str("nonempty"))
        ),
        Case(bool(true), None, int(0)),
        Case(bool(false), None, int(1))
      )
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
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("hackGetmembers"),
      Nil,
      Nil,
      None,
      tmatch(tname("a"), Case(patvar("sym"), Some(tname("cond")), tname("sym")))
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
           |""".stripMargin
      )
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
          Case(patwildcard, None, blk())
        )))
      )
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
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("method"),
      Nil,
      Nil,
      None,
      Term
        .Return(blk(Defn.Val(Nil, List(patvar("a")), None, tinfix(int(2), "+", int(3))), tname("a")))
    ))
  }

  test("return-single-indent") {
    runTestAssert[Stat](
      """|def method =
         |   return
         |     2 
         |     + 3
         |""".stripMargin,
      assertLayout = Some("def method = return 2 + 3")
    )(Defn.Def(Nil, tname("method"), Nil, Nil, None, Term.Return(tinfix(int(2), "+", int(3)))))
  }

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
           |""".stripMargin
      )
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
            tapply(tname("change"), int(-1))
          )
        ),
        Case(
          tname("LBRACE"),
          None,
          blk(
            Term.If(bool(true), Term.Return(Lit.Unit()), Lit.Unit(), Nil),
            tapply(tname("change"), int(-1))
          )
        )
      ))
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
           |""".stripMargin
      )
    )(Defn.Val(
      Nil,
      List(patvar("success")),
      None,
      tapply(
        tselect("suffixes", "find"),
        blk(tfunc(tparam("suffix"))(Term.Try(
          blk(bool(true)),
          Case(Pat.Typed(patvar("e"), pname("StorageException")), None, bool(false)) :: Nil,
          None
        )))
      )
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
           |""".stripMargin
      )
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
            Nil
          )
        ),
        Case(patwildcard, None, tname("NoType"))
      )
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
           |""".stripMargin
      )
    )(Defn.Val(
      Nil,
      List(patvar("withDefault")),
      Some(pfunc(papply("Option", "Int"))("Int")),
      Term.PartialFunction(List(
        Case(Pat.Extract(tname("Some"), List(patvar("x"))), None, tname("x")),
        Case(tname("None"), None, int(0))
      ))
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
           |""".stripMargin
      )
    )(Term.ForYield(
      List(Enumerator.Generator(
        patvar("a"),
        blk(Defn.Val(Nil, List(patvar("b")), None, int(123)), tapply(tname("Some"), tname("b")))
      )),
      tname("a")
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
           |""".stripMargin
      )
    )(Defn.Val(
      Nil,
      List(patvar("a")),
      None,
      tctxfunc(tparam("s", "Int"))(
        blk(Defn.Val(Nil, List(patvar("a")), None, int(123)), tinfix(tname("s"), "+", tname("a")))
      )
    ))
  }

  test("indented-apply") {
    runTestAssert[Stat](
      """|def method = 
         |  fun(a,b,c)
         |    (d, e)
         |""".stripMargin,
      assertLayout = Some("def method = fun(a, b, c)(d, e)")
    )(Defn.Def(
      Nil,
      tname("method"),
      Nil,
      Nil,
      None,
      tapply(tapply(tname("fun"), tname("a"), tname("b"), tname("c")), tname("d"), tname("e"))
    ))
  }

  test("#3531 apply with optional braces and trailing comma") {
    val code = """|Request(
                  |  value = true match
                  |    case true => Some(1)
                  |    case _ => None,
                  |  b = "xxx"
                  |)
                  |""".stripMargin
    val layout = """|Request(value = true match {
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
          Case(patwildcard, None, tname("None"))
        )
      ),
      Term.Assign(tname("b"), str("xxx"))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3542 apply with optional braces in intermediate arg, with multiple outdents") {
    val code = """|A(
                  |  foo = x =>
                  |    x match
                  |      case baz => baz,
                  |  bar = bar
                  |)
                  |""".stripMargin
    val layout = """|A(foo = x => x match {
                    |  case baz => baz
                    |}, bar = bar)
                    |""".stripMargin
    val tree = tapply(
      tname("A"),
      Term.Assign(
        tname("foo"),
        tfunc(tparam("x"))(tmatch(tname("x"), Case(patvar("baz"), None, tname("baz"))))
      ),
      Term.Assign(tname("bar"), tname("bar"))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("indented-double-apply") {
    runTestAssert[Stat](
      """|def method = 
         |  fun(a,b,c)
         |    (d, e)
         |    (f, g)
         |""".stripMargin,
      assertLayout = Some("def method = fun(a, b, c)(d, e)(f, g)")
    )(Defn.Def(
      Nil,
      tname("method"),
      Nil,
      Nil,
      None,
      tapply(
        tapply(tapply(tname("fun"), tname("a"), tname("b"), tname("c")), tname("d"), tname("e")),
        tname("f"),
        tname("g")
      )
    ))
  }

  test("indented-for") {
    runTestAssert[Stat](
      """|for { project <- projects
         |      (source, id) <- project.sources.zipWithIndex } yield source 
         |""".stripMargin,
      assertLayout =
        Some("for (project <- projects; (source, id) <- project.sources.zipWithIndex) yield source")
    )(Term.ForYield(
      List(
        Enumerator.Generator(patvar("project"), tname("projects")),
        Enumerator.Generator(
          Pat.Tuple(List(patvar("source"), patvar("id"))),
          tselect("project", "sources", "zipWithIndex")
        )
      ),
      tname("source")
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
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("method"),
      Nil,
      Nil,
      None,
      blk(
        tapply(tname("fun"), tname("a"), tname("b"), tname("c")),
        Term.Tuple(List(tname("d"), tname("e")))
      )
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
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("method"),
      Nil,
      Nil,
      Some(pname("String")),
      tapply(tapply(tname("fun"), int(1), int(2), int(3)), blk(int(4)))
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
           |""".stripMargin
      )
    )(Defn.Def(
      Nil,
      tname("method2"),
      Nil,
      Nil,
      Some(pname("String")),
      blk(tapply(tname("fun"), int(1), int(2), int(3)), blk(int(4)))
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
           |""".stripMargin
      )
    )(Defn.Object(
      Nil,
      tname("ExampleThing"),
      tplNoBody(init("CompositeThing", List(str("One"), str("Two"), str("Three"), str("Four"))))
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
           |  ("One", "Two", "Three", "Four")
           |}
           |""".stripMargin
      )
    )(Defn.Object(
      Nil,
      tname("O"),
      tpl(
        Defn.Object(Nil, tname("ExampleThing"), tplNoBody(List(init("CompositeThing")))),
        Term.Tuple(List(str("One"), str("Two"), str("Three"), str("Four")))
      )
    ))
  }

  test("indented-enum-contructor-params") {
    runTestAssert[Stat](
      """|enum Namespace(val uri: String | Null):
         |  case xhtml
         |      extends Namespace // Defn.EnumCase ends here
         |        ("http://www.w3.org/1999/xhtml") // str
         |""".stripMargin,
      assertLayout = Some(
        """|enum Namespace(val uri: String | Null) { case xhtml extends Namespace("http://www.w3.org/1999/xhtml") }
           |""".stripMargin
      )
    )(Defn.Enum(
      Nil,
      pname("Namespace"),
      Nil,
      ctorp(tparam(List(Mod.ValParam()), "uri", pinfix("String", "|", pname("Null")))),
      tpl(Defn.EnumCase(
        Nil,
        tname("xhtml"),
        Nil,
        EmptyCtor(),
        List(init("Namespace", List(str("http://www.w3.org/1999/xhtml"))))
      ))
    ))
  }

  test("indented-double-new") {
    runTestAssert[Stat](
      """|new fun
         |    (a,b,c)
         |    (d, e)
         |    (f, g)
         |""".stripMargin,
      assertLayout = Some("new fun(a, b, c)(d, e)(f, g)")
    )(Term.New(init(
      pname("fun"),
      List(tname("a"), tname("b"), tname("c")),
      List(tname("d"), tname("e")),
      List(tname("f"), tname("g"))
    )))
  }

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
        Some("def f = if (x.exists(x => x == 10)) println(\"Yes\") else println(\"No\")")
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
        Nil
      )
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
        Some("def f = if (if (a > 0) true else false) println(\"Yes\") else println(\"No\")")
    )(Defn.Def(
      Nil,
      tname("f"),
      None,
      None,
      Term.If(
        Term.If(tinfix(tname("a"), ">", int(0)), bool(true), bool(false), Nil),
        tapply(tname("println"), str("Yes")),
        tapply(tname("println"), str("No")),
        Nil
      )
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
           |""".stripMargin
      )
    )(Source(List(Pkg(
      tselect("tests", "site"),
      List(
        Pkg(
          tselect("some", "other"),
          List(Defn.Class(Nil, pname("SomeOtherPackage"), Nil, ctor, tplNoBody()))
        ),
        Defn.Class(Nil, pname("BrokenLink"), Nil, ctor, tplNoBody())
      )
    ))))
  }

  test("empty-case-end-marker") {
    runTestAssert[Source](
      """|
         |  def abc: Unit =
         |    x match
         |      case _ =>
         |  end abc
         |""".stripMargin,
      assertLayout = None
    )(Source(List(
      Defn.Def(
        Nil,
        tname("abc"),
        Nil,
        Nil,
        Some(pname("Unit")),
        tmatch(tname("x"), Case(patwildcard, None, blk()))
      ),
      Term.EndMarker(tname("abc"))
    )))
  }

  test("infix-operator-with-alpha") {
    runTestAssert[Stat](
      """|def send() =
         |  c ! "hello"
         |    ! "world"
         |    send_! "!"
         |""".stripMargin,
      assertLayout = Some("""def send() = c ! "hello" ! "world" send_! "!"""")
    )(Defn.Def(
      Nil,
      tname("send"),
      Nil,
      List(List()),
      None,
      tinfix(tinfix(tinfix(tname("c"), "!", str("hello")), "!", str("world")), "send_!", str("!"))
    ))
  }

  test("colon-eol-comment1") {
    runTestAssert[Stat](
      """|object Foo:
         |  /*inline*/ def foo: Int = ???
         |  def bar: Int = ???
         |""".stripMargin,
      assertLayout = None
    )(Defn.Object(
      Nil,
      tname("Foo"),
      tpl(
        Defn.Def(Nil, tname("foo"), Nil, Nil, Some(pname("Int")), tname("???")),
        Defn.Def(Nil, tname("bar"), Nil, Nil, Some(pname("Int")), tname("???"))
      )
    ))
  }

  test("colon-eol-comment2") {
    runTestAssert[Stat](
      """|object Foo: /* comment*/
         |  def foo: Int = ???
         |""".stripMargin,
      assertLayout = None
    )(Defn.Object(
      Nil,
      tname("Foo"),
      tpl(Defn.Def(Nil, tname("foo"), Nil, Nil, Some(pname("Int")), tname("???")))
    ))
  }

  test("colon-eol-multiline-comment") {
    runTestAssert[Stat](
      """|object Foo:/* multi
         |  line
         |   comment */ def foo: Int = ???
         |   def bar: Int = ???
         |""".stripMargin,
      assertLayout = None
    )(Defn.Object(
      Nil,
      tname("Foo"),
      tpl(
        Defn.Def(Nil, tname("foo"), Nil, Nil, Some(pname("Int")), tname("???")),
        Defn.Def(Nil, tname("bar"), Nil, Nil, Some(pname("Int")), tname("???"))
      )
    ))
  }

  test("given-with-comment") {
    runTestAssert[Stat](
      """|given Foo with
         |   /* comment */  def foo: Int = ???
         |   def bar: Int = ???
         |""".stripMargin,
      assertLayout = None
    )(Defn.Given(
      Nil,
      anon,
      Nil,
      Nil,
      tpl(
        List(init("Foo")),
        List(
          Defn.Def(Nil, tname("foo"), Nil, Nil, Some(pname("Int")), tname("???")),
          Defn.Def(Nil, tname("bar"), Nil, Nil, Some(pname("Int")), tname("???"))
        )
      )
    ))
  }

  test("given-with-miltiline-comment") {
    runTestAssert[Stat](
      """|given Foo with /* multi
         |   line
         |   comment */  def foo: Int = ???
         |   def bar: Int = ???
         |""".stripMargin,
      assertLayout = None
    )(Defn.Given(
      Nil,
      anon,
      Nil,
      Nil,
      tpl(
        List(init("Foo")),
        List(
          Defn.Def(Nil, tname("foo"), Nil, Nil, Some(pname("Int")), tname("???")),
          Defn.Def(Nil, tname("bar"), Nil, Nil, Some(pname("Int")), tname("???"))
        )
      )
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
      assertLayout = None
    )(Source(List(
      Defn.Class(
        Nil,
        pname("A1"),
        Nil,
        EmptyCtor(),
        tplNoBody(
          init(Type.Select(tselect("scala", "annotation"), pname("StaticAnnotation"))) :: Nil
        )
      ),
      Defn.Class(
        Nil,
        pname("A2"),
        Nil,
        EmptyCtor(),
        tplNoBody(
          init(Type.Select(tselect("scala", "annotation"), pname("StaticAnnotation"))) :: Nil
        )
      ),
      Defn.Class(
        List(Mod.Annot(init("A1")), Mod.Annot(init("A2"))),
        pname("B"),
        Nil,
        EmptyCtor(),
        tplNoBody()
      )
    )))
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
           |""".stripMargin
      )
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
          tpl(Defn.EnumCase(Nil, tname("EnumCase"), Nil, EmptyCtor(), Nil))
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
            Term.ApplyUnary(tname("-"), tname("n"))
          )
        )
      )
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
           |""".stripMargin
      )
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
                tpl(List(init("Object")), self("obj"), tapply(tname("println"), tname("toString")))
              )),
              Lit.Unit(),
              Nil
            ))
          ),
          Defn.Def(
            Nil,
            tname("main"),
            Nil,
            List(List(tparam("args", papply("Array", "String")))),
            Some(pname("Unit")),
            tapply(tname("fun"))
          )
        )
      ) :: Nil
    ))
  }

  test("newline within self-type") {
    val layout = """|trait T2 {
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
          tpl(Defn.EnumCase(Nil, tname("EnumCase"), Nil, ctor, Nil))
        )
      )
    )
    runTestAssert[Stat](
      """|trait T2 {
         |  self:
         |    T =>
         |  enum T2Enum:
         |    case EnumCase
         |}""".stripMargin,
      assertLayout = Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|trait T2:
         |  self:
         |    T
         |    =>
         |  enum T2Enum:
         |    case EnumCase
         |""".stripMargin,
      assertLayout = Some(layout)
    )(tree)
  }

  test("#3210") {
    val code = """|a3 match {
                  |  case Some(_) =>
                  |    case class A6(a7: A8)
                  |
                  |    object A9
                  |}
                  |""".stripMargin
    val layout = """|a3 match {
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
          Defn.Object(Nil, tname("A9"), tplNoBody())
        )
      )
    )
    runTestAssert[Stat](code, assertLayout = Some(layout))(tree)
  }

  test("def body is partial function") {
    val code = """|object a:
                  |  def foo: Bar =
                  |    case sym
                  |        if baz =>
                  |  end foo
                  |""".stripMargin
    val layout = """|object a {
                    |  def foo: Bar = {
                    |    case sym if baz =>
                    |  }
                    |  end foo
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("a"),
        tpl(
          Defn.Def(
            Nil,
            tname("foo"),
            Nil,
            Some(pname("Bar")),
            Term.PartialFunction(List(Case(patvar("sym"), Some(tname("baz")), blk())))
          ),
          Term.EndMarker(tname("foo"))
        )
      )
    }
  }

  test("def body is non-partial function") {
    val code = """|object a:
                  |  def foo: Bar =
                  |    case class Baz(baz: Int)
                  |    new Baz(0)
                  |""".stripMargin
    val layout = """|object a {
                    |  def foo: Bar = {
                    |    case class Baz(baz: Int)
                    |    new Baz(0)
                    |  }
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("a"),
        tpl(Defn.Def(
          Nil,
          tname("foo"),
          Nil,
          Some(pname("Bar")),
          blk(
            Defn
              .Class(List(Mod.Case()), pname("Baz"), Nil, ctorp(tparam("baz", "Int")), tplNoBody()),
            Term.New(init("Baz", List(int(0))))
          )
        ))
      )
    }
  }

  test("#3252 `new` in arg, then comma 1") {
    val code = """|object a:
                  |  A(
                  |    b = new B,
                  |    c =
                  |      d,
                  |    e = f
                  |  )
                  |""".stripMargin
    val layout = "object a { A(b = new B, c = d, e = f) }"
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("a"),
        tpl(tapply(
          tname("A"),
          Term.Assign(tname("b"), Term.New(init("B"))),
          Term.Assign(tname("c"), tname("d")),
          Term.Assign(tname("e"), tname("f"))
        ))
      )
    }
  }

  test("#3252 `new` in arg, then comma 2") {
    val code = """|object a:
                  |  A(
                  |    b = new B(0),
                  |    c =
                  |      d,
                  |    e = f
                  |  )
                  |""".stripMargin
    val layout = "object a { A(b = new B(0), c = d, e = f) }"
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("a"),
        tpl(tapply(
          tname("A"),
          Term.Assign(tname("b"), Term.New(init("B", List(int(0))))),
          Term.Assign(tname("c"), tname("d")),
          Term.Assign(tname("e"), tname("f"))
        ))
      )
    }
  }

  test("#3252 `new` in arg, then comma 3") {
    val code = """|object a:
                  |  A(
                  |    b = new B(0) { def foo = ??? },
                  |    c =
                  |      d,
                  |    e = f
                  |  )
                  |""".stripMargin
    val layout = "object a { A(b = new B(0) { def foo = ??? }, c = d, e = f) }"
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("a"),
        tpl(tapply(
          tname("A"),
          Term.Assign(
            tname("b"),
            Term.NewAnonymous(tpl(
              List(init("B", List(int(0)))),
              List(Defn.Def(Nil, tname("foo"), Nil, None, tname("???")))
            ))
          ),
          Term.Assign(tname("c"), tname("d")),
          Term.Assign(tname("e"), tname("f"))
        ))
      )
    }
  }

  test("#3257 `new` in arg value after indent, then comma") {
    val code = """|object a:
                  |  A(
                  |    b =
                  |      new B,
                  |    c = d
                  |  )
                  |""".stripMargin
    val layout = "object a { A(b = new B, c = d) }"
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("a"),
        tpl(tapply(
          tname("A"),
          Term.Assign(tname("b"), Term.New(init("B"))),
          Term.Assign(tname("c"), tname("d"))
        ))
      )
    }
  }

  test("#3257 `if-no-else` in arg value after indent, then comma") {
    val code = """|object a:
                  |  A(
                  |    b =
                  |      if (a + b) c,
                  |    c = d
                  |  )
                  |""".stripMargin
    val layout = "object a { A(b = if (a + b) c, c = d) }"
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("a"),
        tpl(tapply(
          tname("A"),
          Term.Assign(
            tname("b"),
            Term.If(tinfix(tname("a"), "+", tname("b")), tname("c"), Lit.Unit(), Nil)
          ),
          Term.Assign(tname("c"), tname("d"))
        ))
      )
    }
  }

  test("#3261 chained `match` with outdent, no block, in assign") {
    val code = """|object small:
                  |  val value =
                  |    Nil match
                  |    case Nil => "empty"
                  |    case _   => "nonempty"
                  |  match
                  |    case "empty"    => 0
                  |    case "nonempty" => 1
                  |""".stripMargin
    val layout = """|object small {
                    |  val value = (Nil match {
                    |    case Nil => "empty"
                    |    case _ => "nonempty"
                    |  }) match {
                    |    case "empty" => 0
                    |    case "nonempty" => 1
                    |  }
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
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
              Case(patwildcard, None, str("nonempty"))
            ),
            Case(str("empty"), None, int(0)),
            Case(str("nonempty"), None, int(1))
          )
        ))
      )
    }
  }

  test("#3261 chained `match` with outdent, block, in assign") {
    val code = """|object small:
                  |  val value =
                  |    foo()
                  |    Nil match
                  |    case Nil => "empty"
                  |    case _   => "nonempty"
                  |  match
                  |    case "empty"    => 0
                  |    case "nonempty" => 1
                  |""".stripMargin
    val layout = """|object small {
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
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
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
                Case(patwildcard, None, str("nonempty"))
              )
            ),
            Case(str("empty"), None, int(0)),
            Case(str("nonempty"), None, int(1))
          )
        ))
      )
    }
  }

  test("#3261 chained `match` with outdent, no block, in if cond") {
    val code = """|object small:
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
    val layout = """|object small {
                    |  if ((Nil match {
                    |    case Nil => "empty"
                    |    case _ => "nonempty"
                    |  }) match {
                    |    case "empty" => true
                    |    case "nonempty" => false
                    |  }) bar
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("small"),
        tpl(Term.If(
          tmatch(
            tmatch(
              tname("Nil"),
              Case(tname("Nil"), None, str("empty")),
              Case(patwildcard, None, str("nonempty"))
            ),
            Case(str("empty"), None, bool(true)),
            Case(str("nonempty"), None, bool(false))
          ),
          tname("bar"),
          Lit.Unit(),
          Nil
        ))
      )
    }
  }

  test("#3261 chained `match` with outdent, block, in if cond") {
    val code = """|object small:
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
    val layout = """|object small {
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
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("small"),
        tpl(Term.If(
          tmatch(
            blk(
              tapply(tname("foo")),
              tmatch(
                tname("Nil"),
                Case(tname("Nil"), None, str("empty")),
                Case(patwildcard, None, str("nonempty"))
              )
            ),
            Case(str("empty"), None, bool(true)),
            Case(str("nonempty"), None, bool(false))
          ),
          tname("bar"),
          Lit.Unit(),
          Nil
        ))
      )
    }
  }

  test("#3261 chained `match` with outdent, no block, in try") {
    val code = """|object small:
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
    val layout = """|object small {
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
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("small"),
        tpl(Term.Try(
          tmatch(
            tmatch(
              tname("Nil"),
              Case(tname("Nil"), None, str("empty")),
              Case(patwildcard, None, str("nonempty"))
            ),
            Case(str("empty"), None, int(0)),
            Case(str("nonempty"), None, int(1))
          ),
          Case(patvar("e"), None, tapply(tselect("e", "getMessage"))) :: Nil,
          None
        ))
      )
    }
  }

  test("#3261 chained `match` with outdent, block, in try") {
    val code = """|object small:
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
    val layout = """|object small {
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
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("small"),
        tpl(Term.Try(
          tmatch(
            blk(
              tapply(tname("foo")),
              tmatch(
                tname("Nil"),
                Case(tname("Nil"), None, str("empty")),
                Case(patwildcard, None, str("nonempty"))
              )
            ),
            Case(str("empty"), None, int(0)),
            Case(str("nonempty"), None, int(1))
          ),
          Case(patvar("e"), None, tapply(tselect("e", "getMessage"))) :: Nil,
          None
        ))
      )
    }
  }

  test("#3261 partial func, in try") {
    val code = """|object small:
                  |  try
                  |    case Nil => "empty"
                  |    case _   => "nonempty"
                  |  catch
                  |    case e => e.getMessage()
                  |""".stripMargin
    val layout = """|object small {
                    |  try {
                    |    case Nil => "empty"
                    |    case _ => "nonempty"
                    |  } catch {
                    |    case e =>
                    |      e.getMessage()
                    |  }
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, Some(layout)) {
      Defn.Object(
        Nil,
        tname("small"),
        tpl(Term.Try(
          Term.PartialFunction(
            List(Case(tname("Nil"), None, str("empty")), Case(patwildcard, None, str("nonempty")))
          ),
          Case(patvar("e"), None, tapply(tselect("e", "getMessage"))) :: Nil,
          None
        ))
      )
    }
  }

  test("blank after template 1") {
    val code = """|class DerivationSpec {
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
    val layout = """|class DerivationSpec {
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
        blk(tapplytype(tname("deriveEncoder"), pname("Foo")))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("blank after template 2") {
    val code = """|class DerivationSpec {
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
    val layout = """|class DerivationSpec {
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
        blk(tapplytype(tname("deriveEncoder"), pname("Foo")))
      )
    )
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def name after newline") {
    val code = """|def
                  |  foo: Bar =
                  |    baz
                  |""".stripMargin
    val layout = "def foo: Bar = baz"
    val tree = Defn.Def(Nil, tname("foo"), Nil, Some(pname("Bar")), tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def colon after newline") {
    val code = """|def foo
                  |  : Bar =
                  |    baz
                  |""".stripMargin
    val layout = "def foo: Bar = baz"
    val tree = Defn.Def(Nil, tname("foo"), Nil, Some(pname("Bar")), tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def newline after colon") {
    val code = """|def foo
                  |  :
                  |  Bar =
                  |    baz
                  |""".stripMargin
    val layout = "def foo: Bar = baz"
    val tree = Defn.Def(Nil, tname("foo"), Nil, Some(pname("Bar")), tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def newline around hash") {
    val code = """|def foo: Bar
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
    val code = """|def foo
                  |  =
                  |    baz
                  |""".stripMargin
    val layout = "def foo = baz"
    val tree = Defn.Def(Nil, tname("foo"), Nil, None, tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("def equals after newline, with type") {
    val code = """|def foo: Bar
                  |  =
                  |    baz
                  |""".stripMargin
    val layout = "def foo: Bar = baz"
    val tree = Defn.Def(Nil, tname("foo"), Nil, Some(pname("Bar")), tname("baz"))
    runTestAssert[Stat](code, Some(layout))(tree)
  }

  test("#3571 scala3") {
    val code = """|new A {
                  |  def b: C =
                  |    ???
                  |}
                  |""".stripMargin
    val layout = """|new A { def b: C = ??? }
                    |""".stripMargin
    val tree = Term.NewAnonymous(
      tpl(List(init("A")), List(Defn.Def(Nil, tname("b"), Nil, Some(pname("C")), tname("???"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3605 scala3") {
    val code = """|new A {
                  |  def b: C =
                  |    ???
                  |
                  |}
                  |""".stripMargin
    val layout = """|new A { def b: C = ??? }
                    |""".stripMargin
    val tree = Term.NewAnonymous(
      tpl(List(init("A")), List(Defn.Def(Nil, tname("b"), Nil, Some(pname("C")), tname("???"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3617 scala3") {
    val codeWithoutBlank = """|object Test {
                              |  def bar =
                              |    `f-oo`
                              |}
                              |""".stripMargin
    val codeWithBlank = """|object Test {
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
    val code = """|object A:
                  |  foo.map:
                  |    bar
                  |    + baz
                  |""".stripMargin
    val layout = """|object A {
                    |  foo.map {
                    |    bar + baz
                    |  }
                    |}
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(tapply(tselect("foo", "map"), blk(tinfix(tname("bar"), "+", tname("baz")))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("fewer braces and leading infix 2") {
    val code = """|object A:
                  |  foo.map:
                  |    bar
                  |  + baz
                  |""".stripMargin
    val layout = """|object A {
                    |  foo.map {
                    |    bar
                    |  } + baz
                    |}
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(tinfix(tapply(tselect("foo", "map"), blk(tname("bar"))), "+", tname("baz")))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("optional braces and leading infix 3") {
    val code = """|object A:
                  |  foo match
                  |    case bar => bar * bar
                  |  + baz
                  |""".stripMargin
    val layout = """|object A {
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
        tname("baz")
      ))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scala3 code using CRLF 2: for-yield without braces") {
    val code = """|object A:
                  |  for
                  |    a <- foo
                  |    b <- bar
                  |  yield
                  |    a + b
                  |""".stripMargin
    val layout = """|object A { for (a <- foo; b <- bar) yield a + b }
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Term.ForYield(
        List(
          Enumerator.Generator(patvar("a"), tname("foo")),
          Enumerator.Generator(patvar("b"), tname("bar"))
        ),
        tinfix(tname("a"), "+", tname("b"))
      ))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scala3 code using CRLF 3: for-yield with braces") {
    val code = """|object A:
                  |  for {
                  |    a <- foo
                  |    b <- bar
                  |  }
                  |  yield
                  |    a + b
                  |""".stripMargin
    val layout = """|object A { for (a <- foo; b <- bar) yield a + b }
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Term.ForYield(
        List(
          Enumerator.Generator(patvar("a"), tname("foo")),
          Enumerator.Generator(patvar("b"), tname("bar"))
        ),
        tinfix(tname("a"), "+", tname("b"))
      ))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scala3 code using CRLF 4: for-yield with parens") {
    val code = """|object A:
                  |  for (
                  |    a <- foo;
                  |    b <- bar
                  |  )
                  |  yield
                  |    a + b
                  |""".stripMargin
    val layout = """|object A { for (a <- foo; b <- bar) yield a + b }
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Term.ForYield(
        List(
          Enumerator.Generator(patvar("a"), tname("foo")),
          Enumerator.Generator(patvar("b"), tname("bar"))
        ),
        tinfix(tname("a"), "+", tname("b"))
      ))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("scala3 code using CRLF 5: match operator with select") {
    val code = """|object A:
                  |  foo.match
                  |    case bar => baz
                  |  .qux
                  |""".stripMargin
    val layout = """|object A {
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
    val code = """|object A:
                  |  while
                  |    a < b
                  |  do
                  |    a + b
                  |""".stripMargin
    val layout = """|object A { while (a < b) a + b }
                    |""".stripMargin
    val tree = Defn.Object(
      Nil,
      tname("A"),
      tpl(Term.While(tinfix(tname("a"), "<", tname("b")), tinfix(tname("a"), "+", tname("b"))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("args-like tuple after optional braces") {
    val code = """|def foo =
                  |  val baz =
                  |    if (qux) quux
                  |    else fred // not very, but a somewhat long comment
                  |  (baz, null)
                  |""".stripMargin
    val layout = """|def foo = {
                    |  val baz = if (qux) quux else fred
                    |  (baz, null)
                    |}
                    |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("foo"),
      Nil,
      None,
      blk(
        Defn.Val(
          Nil,
          List(patvar("baz")),
          None,
          Term.If(tname("qux"), tname("quux"), tname("fred"), Nil)
        ),
        Term.Tuple(List(tname("baz"), Lit.Null()))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case at same indent as line containing match, then template") {
    val code = """|object Definitions:
                  |  def foo = bar
                  |    .baz match
                  |    case none => false
                  |
                  |  class Qux:
                  |    self =>
                  |  end Qux
                  |""".stripMargin
    val layout = """|object Definitions {
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
          tmatch(tselect("bar", "baz"), Case(patvar("none"), None, lit(false)))
        ),
        Defn.Class(Nil, pname("Qux"), Nil, ctor, tpl(Nil, tplBody("self"))),
        Term.EndMarker("Qux")
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4147 func") {
    implicit val dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code = """|def a: Int = c { D => e =>
                  |  def f = e
                  |
                  |  f
                  |}
                  |""".stripMargin
    val layout = """|def a: Int = c {
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
        blk(tfunc(tparam("D"))(tfunc(tparam("e"))(blk(Defn.Def(Nil, "f", Nil, None, "e"), "f"))))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#4147 poly func") {
    implicit val dialect = dialects.Scala3.withAllowSignificantIndentation(false)
    val code = """|def a: Int = c { [D] => e =>
                  |  def f = e
                  |
                  |  f
                  |}
                  |""".stripMargin
    val layout = """|def a: Int = c {
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
        blk(tpolyfunc(pparam("D"))(tfunc(tparam("e"))(blk(Defn.Def(Nil, "f", Nil, None, "e"), "f"))))
      )
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
