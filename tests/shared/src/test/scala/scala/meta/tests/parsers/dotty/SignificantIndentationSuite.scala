package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._
import scala.meta.Term.Block

class SignificantIndentationSuite extends BaseDottySuite {

  val defx = Decl.Def(Nil, Term.Name("f"), Nil, Nil, Type.Name("Int"))
  val defy = Defn.Def(
    Nil,
    Term.Name("y"),
    Nil,
    Nil,
    Some(Type.Name("String")),
    Term.Block(List(Term.Apply(Term.Name("fa"), Nil), Term.Apply(Term.Name("fb"), Nil)))
  )

  test("basic-example") {
    val code = """|trait A:
                  |  def f: Int
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("trait A { def f: Int }"))(
      Defn.Trait(
        Nil,
        Type.Name("A"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(Nil, Nil, Self(Name(""), None), List(defx))
      )
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
    )(
      Source(
        List(
          Pkg(
            Term.Name("mysadpackage"),
            List(Decl.Def(Nil, Term.Name("f"), Nil, Nil, Type.Name("Int")))
          ),
          Pkg(
            Term.Name("anotherpackage"),
            List(Decl.Def(Nil, Term.Name("f"), Nil, Nil, Type.Name("Int")))
          )
        )
      )
    )
  }

  test("multistat-example") {
    val code = """|trait A:
                  |  def f: Int
                  |  def y: String = { fa(); fb() }""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Trait(
        Nil,
        Type.Name("A"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            defx,
            defy
          )
        )
      )
    )
  }

  test("anonymous-class") {
    val code = """|new A:
                  |  def f: Int
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("new A { def f: Int }"))(
      Term.NewAnonymous(
        Template(
          Nil,
          List(Init(Type.Name("A"), Name(""), Nil)),
          Self(Name(""), None),
          List(Decl.Def(Nil, Term.Name("f"), Nil, Nil, Type.Name("Int"))),
          Nil
        )
      )
    )
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
    )(
      Term.NewAnonymous(
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Decl.Def(Nil, Term.Name("f"), Nil, Nil, Type.Name("Int")),
            Decl.Def(Nil, Term.Name("g"), Nil, Nil, Type.Name("Int"))
          ),
          Nil
        )
      )
    )
  }

  test("indent-and-back") {
    val code = """|object O:
                  |  class C:
                  |    def f: Int = 3
                  |  trait T:
                  |    def f: Int = 4
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Object(
        Nil,
        Term.Name("O"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Class(
              Nil,
              Type.Name("C"),
              Nil,
              ctor,
              Template(
                Nil,
                Nil,
                Self(Name(""), None),
                List(Defn.Def(Nil, Term.Name("f"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(3)))
              )
            ),
            Defn.Trait(
              Nil,
              Type.Name("T"),
              Nil,
              Ctor.Primary(Nil, Name(""), Nil),
              Template(
                Nil,
                Nil,
                Self(Name(""), None),
                List(Defn.Def(Nil, Term.Name("f"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(4)))
              )
            )
          )
        )
      )
    )
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
    )(
      Defn.Def(
        Nil,
        Term.Name("fn"),
        Nil,
        Nil,
        Some(Type.Name("Unit")),
        Term.Block(
          List(
            Term.If(Term.Name("cond"), Term.Name("truep"), Term.Name("falsep")),
            Term.Apply(Term.Name("otherStatement"), Nil)
          )
        )
      )
    )
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
    runTestError[Stat](code, "expected but else found")
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
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Class(
        Nil,
        Type.Name("X"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Def(
              Nil,
              Term.Name("fx"),
              Nil,
              List(List()),
              Some(Type.Name("Unit")),
              Term.Block(List(Term.Name("f1"), Term.Name("f2")))
            )
          )
        )
      )
    )
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
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Class(
        Nil,
        Type.Name("X"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Def(
              Nil,
              Term.Name("fx"),
              Nil,
              List(List()),
              Some(Type.Name("Unit")),
              Term.Name("f1")
            ),
            Term.Name("f2")
          )
        )
      )
    )
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
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Class(
        Nil,
        Type.Name("X"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Def(
              Nil,
              Term.Name("fx"),
              Nil,
              List(List()),
              Some(Type.Name("Unit")),
              Term.Block(Nil)
            ),
            Defn.Def(
              List(Mod.Private(Name(""))),
              Term.Name("f2"),
              Nil,
              Nil,
              Some(Type.Name("Int")),
              Lit.Int(1)
            )
          )
        )
      )
    )
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
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Class(
        Nil,
        Type.Name("A"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          List(Init(Type.Name("B"), Name(""), Nil)),
          Self(Term.Name("thisPhase"), None),
          List(Term.Name("expr1"), Term.Name("expr2"))
        )
      )
    )
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
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Val(
              Nil,
              List(Pat.Var(Term.Name("fn"))),
              None,
              Term.Function(
                List(
                  Term.Param(Nil, Term.Name("pa"), None, None),
                  Term.Param(Nil, Term.Name("pb"), None, None)
                ),
                Term.Block(
                  List(Defn.Def(Nil, Term.Name("helper"), Nil, Nil, None, Lit.Int(3)), Lit.Int(3))
                )
              )
            ),
            Term.EndMarker(Term.Name("fn"))
          )
        )
      )
    )
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
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Class(
        Nil,
        Type.Name("A"),
        Nil,
        ctor,
        tpl(
          List(
            Defn.Def(
              Nil,
              Term.Name("forward"),
              Nil,
              Nil,
              Some(Type.Name("Unit")),
              Term.Match(
                Term.Name("parents"),
                List(
                  Case(
                    Pat.Var(Term.Name("a")),
                    None,
                    Term.For(
                      List(
                        Enumerator.CaseGenerator(
                          Pat.Typed(Pat.Var(Term.Name("a")), Type.Name("TP")),
                          Term.Name("body")
                        )
                      ),
                      Term.Name("fordo")
                    )
                  ),
                  Case(Pat.Var(Term.Name("b")), None, Term.Name("ok"))
                )
              )
            ),
            Decl.Def(
              List(Mod.Private(Name(""))),
              Term.Name("transformAnnot"),
              Nil,
              Nil,
              Type.Name("Tree")
            )
          )
        )
      )
    )
  }

  test("this-constructor") {
    val code = """|class A:
                  |  def this(msg: String) =
                  |    this(message, false)
                  |""".stripMargin
    val output = "class A { def this(msg: String) = this(message, false) }"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Class(
        Nil,
        Type.Name("A"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Ctor.Secondary(
              Nil,
              Name(""),
              List(List(Term.Param(Nil, Term.Name("msg"), Some(Type.Name("String")), None))),
              Init(
                Type.Singleton(Term.This(Name(""))),
                Name(""),
                List(List(Term.Name("message"), Lit.Boolean(false)))
              ),
              Nil
            )
          ),
          Nil
        )
      )
    )
  }

  test("this-constructor-indented-block") {
    val code = """|class A:
                  |  def this(msg: String) =
                  |    this(message, false)
                  |    otherStat
                  |""".stripMargin
    val output = """|class A {
                    |  def this(msg: String) {
                    |    this(message, false)
                    |    otherStat
                    |  }
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Class(
        Nil,
        Type.Name("A"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Ctor.Secondary(
              Nil,
              Name(""),
              List(List(Term.Param(Nil, Term.Name("msg"), Some(Type.Name("String")), None))),
              Init(
                Type.Singleton(Term.This(Name(""))),
                Name(""),
                List(List(Term.Name("message"), Lit.Boolean(false)))
              ),
              List(Term.Name("otherStat"))
            )
          ),
          Nil
        )
      )
    )
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
    )(
      Defn.Def(
        Nil,
        Term.Name("genApply"),
        Nil,
        List(List()),
        None,
        Term.Block(
          List(
            Term.Match(
              Term.Name("app"),
              List(
                Case(
                  Pat.Extract(Term.Name("Apply2"), Nil),
                  None,
                  Term.Assign(
                    Term.Name("generatedType"),
                    Term.Apply(Term.Name("genTypeApply"), List(Term.Name("t")))
                  )
                ),
                Case(Pat.Extract(Term.Name("Apply"), Nil), None, Term.Block(Nil))
              ),
              Nil
            )
          )
        )
      )
    )
  }

  test("outdent-with-prev-check") {
    runTestAssert[Stat](
      """|def wrapPlaceholders(t: Tree) = try
         |    if (placeholderParams.isEmpty) t
         |    else new WildcardFunction(placeholderParams.reverse, t)
         |  finally placeholderParams = saved
         |""".stripMargin,
      assertLayout = Some(
        "def wrapPlaceholders(t: Tree) = try if (placeholderParams.isEmpty) t else new WildcardFunction(placeholderParams.reverse, t) finally placeholderParams = saved"
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("wrapPlaceholders"),
        Nil,
        List(List(Term.Param(Nil, Term.Name("t"), Some(Type.Name("Tree")), None))),
        None,
        Term.Try(
          Term.If(
            Term.Select(Term.Name("placeholderParams"), Term.Name("isEmpty")),
            Term.Name("t"),
            Term.New(
              Init(
                Type.Name("WildcardFunction"),
                Name(""),
                List(
                  List(
                    Term.Select(Term.Name("placeholderParams"), Term.Name("reverse")),
                    Term.Name("t")
                  )
                )
              )
            )
          ),
          Nil,
          Some(Term.Assign(Term.Name("placeholderParams"), Term.Name("saved")))
        )
      )
    )
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
           |  val refinementTest: Graph { def x: Int }
           |}
           |""".stripMargin
      )
    )(
      Defn.Class(
        List(Mod.Abstract()),
        Type.Name("Documentation"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Class(
              Nil,
              Type.Name("Graph"),
              Nil,
              Ctor.Primary(Nil, Name(""), Nil),
              Template(
                Nil,
                Nil,
                Self(Name(""), None),
                List(
                  Defn.Type(Nil, Type.Name("Node"), Nil, Type.Name("Int")),
                  Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), Some(Type.Name("Int")), Lit.Int(3))
                )
              )
            ),
            Decl.Val(
              Nil,
              List(Pat.Var(Term.Name("refinementTest"))),
              Type.Refine(
                Some(Type.Name("Graph")),
                List(Decl.Def(Nil, Term.Name("x"), Nil, Nil, Type.Name("Int")))
              )
            )
          )
        )
      )
    )
  }

  test("type-in-next-line-equals") {
    runTestAssert[Stat](
      """|val refinementTest:
         |      Int = 3
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = 3")
    )(
      Defn.Val(Nil, List(Pat.Var(Term.Name("refinementTest"))), Some(Type.Name("Int")), Lit.Int(3))
    )
  }

  test("type-in-next-line-equals-newline") {
    runTestAssert[Stat](
      """|val refinementTest:
         |      Int = 
         |3
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = 3")
    )(
      Defn.Val(Nil, List(Pat.Var(Term.Name("refinementTest"))), Some(Type.Name("Int")), Lit.Int(3))
    )
  }

  test("type-equals-separate") {
    runTestAssert[Stat](
      """|def refinementTest(a :
         |      Int = 
         |3) = a
         |""".stripMargin,
      assertLayout = Some("def refinementTest(a: Int = 3) = a")
    )(
      Defn.Def(
        Nil,
        Term.Name("refinementTest"),
        Nil,
        List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), Some(Lit.Int(3))))),
        None,
        Term.Name("a")
      )
    )
  }

  test("type-multi-seq") {
    runTestAssert[Stat](
      """|val refinementTest:
         |    Int = 
         |  fx
         |  fy
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = {\n  fx\n  fy\n}")
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("refinementTest"))),
        Some(Type.Name("Int")),
        Term.Block(List(Term.Name("fx"), Term.Name("fy")))
      )
    )
  }

  test("equals-block") {
    runTestAssert[Stat](
      """|val refinementTest:
         |  Int = 
         |    fx
         |    fy
         |""".stripMargin,
      assertLayout = Some("val refinementTest: Int = {\n  fx\n  fy\n}")
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("refinementTest"))),
        Some(Type.Name("Int")),
        Term.Block(List(Term.Name("fx"), Term.Name("fy")))
      )
    )
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
    )(
      Defn.Given(
        Nil,
        Term.Name("intOrd"),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))), Name(""), Nil)),
          Self(Name(""), None),
          List(
            Defn.Def(Nil, Term.Name("fa"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(1)),
            Defn.Def(Nil, Term.Name("fb"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(2))
          ),
          Nil
        )
      )
    )
  }

  test("given-block-indent-edge-cases") {
    runTestError[Stat](
      """|given intOrd: Ord[Int] with
         |def fa: Int = 1
         |def fb: Int = 2
         |""".stripMargin,
      "expected '{' or indentation"
    )

    runTestError[Stat](
      """|given intOrd: Ord[Int]:
         |  def fa: Int = 1
         |  def fb: Int = 2
         |""".stripMargin,
      "; expected but : found"
    )

    runTestAssert[Stat](
      """|class A extends A with 
         |  B
         |""".stripMargin,
      assertLayout = Some("class A extends A with B")
    )(
      Defn.Class(
        Nil,
        Type.Name("A"),
        Nil,
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          List(Init(Type.Name("A"), Name(""), Nil), Init(Type.Name("B"), Name(""), Nil)),
          Self(Name(""), None),
          Nil,
          Nil
        )
      )
    )
  }

  test("nested-coloneol") {
    runTestAssert[Stat](
      """|case class Test(
         |  a: A = new A,
         |):
         |  def hello = 1
         |""".stripMargin,
      assertLayout = Some("case class Test(a: A = new A) { def hello = 1 }")
    )(
      Defn.Class(
        List(Mod.Case()),
        Type.Name("Test"),
        Nil,
        Ctor.Primary(
          Nil,
          Name(""),
          List(
            List(
              Term.Param(
                Nil,
                Term.Name("a"),
                Some(Type.Name("A")),
                Some(Term.New(Init(Type.Name("A"), Name(""), Nil)))
              )
            )
          )
        ),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Defn.Def(Nil, Term.Name("hello"), Nil, Nil, None, Lit.Int(1)))
        )
      )
    )
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
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("z"))),
        None,
        Term.Block(
          List(
            Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Lit.Int(0)),
            Term.Apply(Term.Name("f"), List(Term.Name("a")))
          )
        )
      )
    )
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
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Defn.Object(
        Nil,
        Term.Name("X"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Term.If(Term.Name("cond"), Term.Name("f"), Lit.Unit(), Nil),
            Term.Name("foo")
          ),
          Nil
        )
      )
    )
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
    )(
      Term.Match(
        Term.Name("widen"),
        List(
          Case(
            Pat.Typed(
              Pat.Bind(
                Pat.Var(Term.Name("tp")),
                Pat.Extract(Term.Name("OrNull"), List(Pat.Var(Term.Name("tp1"))))
              ),
              Type.Name("OrType")
            ),
            None,
            Term.Block(Nil)
          ),
          Case(Pat.Var(Term.Name("tp")), None, Term.Name("tp"))
        ),
        Nil
      )
    )
  }

  test("object-type") {
    runTestAssert[Stat](
      """|object typeAndObjects:
         |  type Ala
         |""".stripMargin,
      assertLayout = Some(
        "object typeAndObjects { type Ala }"
      )
    )(
      Defn.Object(
        Nil,
        Term.Name("typeAndObjects"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Decl.Type(Nil, Type.Name("Ala"), Nil, Type.Bounds(None, None))),
          Nil
        )
      )
    )
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
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Try(
        Term.Block(List(Term.Name("fx"), Term.Name("gx"))),
        List(
          Case(Pat.Var(Term.Name("aa")), None, Term.Block(Nil)),
          Case(Pat.Var(Term.Name("bb")), None, Term.Block(Nil))
        ),
        Some(Term.Block(List(Term.Name("cc"), Term.Name("dd"))))
      )
    )
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
      Term.If(
        Term.Block(List(Term.Name("cond"), Term.Name("cond2"))),
        Term.Block(List(Term.Name("fx"), Term.Name("gx"))),
        Term.Name("gx"),
        Nil
      )
    )
  }

  test("new-fordo-same-indent") {
    val code = """|for
                  |  a <- gen
                  |  do fx
                  |""".stripMargin
    val output = "for (a <- gen) fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.For(
        List(Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("gen"))),
        Term.Name("fx")
      )
    )
  }

  test("new-for-yield-same-indent") {
    val code = """|for
                  |  a <- x
                  |  b <- y
                  |  yield fx
                  |""".stripMargin
    val output = "for (a <- x; b <- y) yield fx"
    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(Term.Name("a")), Term.Name("x")),
          Enumerator.Generator(Pat.Var(Term.Name("b")), Term.Name("y"))
        ),
        Term.Name("fx")
      )
    )
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
      assertLayout = None
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("hello"))),
        None,
        Term.Match(
          Term.Match(
            Term.Name("xs"),
            List(
              Case(Term.Name("Nil"), None, Lit.String("empty")),
              Case(
                Pat.ExtractInfix(
                  Pat.Var(Term.Name("x")),
                  Term.Name("::"),
                  List(Pat.Var(Term.Name("xs1")))
                ),
                None,
                Lit.String("nonempty")
              )
            ),
            Nil
          ),
          List(
            Case(Lit.Boolean(true), None, Lit.Int(0)),
            Case(Lit.Boolean(false), None, Lit.Int(1))
          ),
          Nil
        )
      )
    )

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
    )(
      Defn.Def(
        Nil,
        Term.Name("hackGetmembers"),
        Nil,
        Nil,
        None,
        Term.Match(
          Term.Name("a"),
          List(Case(Pat.Var(Term.Name("sym")), Some(Term.Name("cond")), Term.Name("sym"))),
          Nil
        )
      )
    )

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
    )(
      Defn.Def(
        Nil,
        Term.Name("mapSymbols"),
        Nil,
        Nil,
        None,
        Term.Apply(
          Term.Select(Term.Name("originals"), Term.Name("foreach")),
          List(
            Term.Block(
              List(
                Term.Function(
                  List(Term.Param(Nil, Term.Name("a"), None, None)),
                  Term.Match(
                    Term.Select(Term.Name("copy"), Term.Name("denot")),
                    List(
                      Case(
                        Pat.Typed(Pat.Var(Term.Name("cd")), Type.Name("ClassDenotation")),
                        None,
                        Term.Block(Nil)
                      ),
                      Case(Pat.Wildcard(), None, Term.Block(Nil))
                    ),
                    Nil
                  )
                )
              )
            )
          )
        )
      )
    )

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
    )(
      Defn.Def(
        Nil,
        Term.Name("method"),
        Nil,
        Nil,
        None,
        Term.Return(
          Term.Block(
            List(
              Defn.Val(
                Nil,
                List(Pat.Var(Term.Name("a"))),
                None,
                Term.ApplyInfix(Lit.Int(2), Term.Name("+"), Nil, List(Lit.Int(3)))
              ),
              Term.Name("a")
            )
          )
        )
      )
    )
  }

  test("return-single-indent") {
    runTestAssert[Stat](
      """|def method =
         |   return
         |     2 
         |     + 3
         |""".stripMargin,
      assertLayout = Some(
        "def method = return 2 + 3"
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("method"),
        Nil,
        Nil,
        None,
        Term.Return(
          Term.ApplyInfix(Lit.Int(2), Term.Name("+"), Nil, List(Lit.Int(3)))
        )
      )
    )
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
    )(
      Defn.Def(
        Nil,
        Term.Name("skip"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Term.Match(
              Term.Name("token"),
              List(
                Case(
                  Term.Name("RBRACE"),
                  None,
                  Term.Block(
                    List(
                      Term.If(Lit.Boolean(true), Term.Return(Lit.Unit()), Lit.Unit(), Nil),
                      Term.Apply(Term.Name("change"), List(Lit.Int(-1)))
                    )
                  )
                ),
                Case(
                  Term.Name("LBRACE"),
                  None,
                  Term.Block(
                    List(
                      Term.If(Lit.Boolean(true), Term.Return(Lit.Unit()), Lit.Unit(), Nil),
                      Term.Apply(Term.Name("change"), List(Lit.Int(-1)))
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
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("success"))),
        None,
        Term.Apply(
          Term.Select(Term.Name("suffixes"), Term.Name("find")),
          List(
            Term.Block(
              List(
                Term.Function(
                  List(Term.Param(Nil, Term.Name("suffix"), None, None)),
                  Term.Try(
                    Term.Block(List(Lit.Boolean(true))),
                    List(
                      Case(
                        Pat.Typed(Pat.Var(Term.Name("e")), Type.Name("StorageException")),
                        None,
                        Lit.Boolean(false)
                      )
                    ),
                    None
                  )
                )
              )
            )
          )
        )
      )
    )
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
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("calleeType"))),
        None,
        Term.Match(
          Term.Name("a"),
          List(
            Case(
              Pat.Wildcard(),
              None,
              Term.If(
                Term.Name("cond"),
                Term
                  .Match(Term.Name("expr"), List(Case(Pat.Wildcard(), None, Term.Name("f"))), Nil),
                Term.Name("NoType"),
                Nil
              )
            ),
            Case(Pat.Wildcard(), None, Term.Name("NoType"))
          ),
          Nil
        )
      )
    )
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
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("withDefault"))),
        Some(
          Type.Function(
            List(Type.Apply(Type.Name("Option"), List(Type.Name("Int")))),
            Type.Name("Int")
          )
        ),
        Term.PartialFunction(
          List(
            Case(
              Pat.Extract(Term.Name("Some"), List(Pat.Var(Term.Name("x")))),
              None,
              Term.Name("x")
            ),
            Case(Term.Name("None"), None, Lit.Int(0))
          )
        )
      )
    )
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
    )(
      Term.ForYield(
        List(
          Enumerator.Generator(
            Pat.Var(Term.Name("a")),
            Term.Block(
              List(
                Defn.Val(Nil, List(Pat.Var(Term.Name("b"))), None, Lit.Int(123)),
                Term.Apply(Term.Name("Some"), List(Term.Name("b")))
              )
            )
          )
        ),
        Term.Name("a")
      )
    )
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
    )(
      Defn.Val(
        Nil,
        List(Pat.Var(Term.Name("a"))),
        None,
        Term.ContextFunction(
          List(Term.Param(Nil, Term.Name("s"), Some(Type.Name("Int")), None)),
          Term.Block(
            List(
              Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Lit.Int(123)),
              Term.ApplyInfix(Term.Name("s"), Term.Name("+"), Nil, List(Term.Name("a")))
            )
          )
        )
      )
    )
  }

  test("indented-apply") {
    runTestAssert[Stat](
      """|def method = 
         |  fun(a,b,c)
         |    (d, e)
         |""".stripMargin,
      assertLayout = Some(
        "def method = fun(a, b, c)(d, e)"
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("method"),
        Nil,
        Nil,
        None,
        Term.Apply(
          Term.Apply(Term.Name("fun"), List(Term.Name("a"), Term.Name("b"), Term.Name("c"))),
          List(Term.Name("d"), Term.Name("e"))
        )
      )
    )
  }

  test("indented-double-apply") {
    runTestAssert[Stat](
      """|def method = 
         |  fun(a,b,c)
         |    (d, e)
         |    (f, g)
         |""".stripMargin,
      assertLayout = Some(
        "def method = fun(a, b, c)(d, e)(f, g)"
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("method"),
        Nil,
        Nil,
        None,
        Term.Apply(
          Term.Apply(
            Term.Apply(Term.Name("fun"), List(Term.Name("a"), Term.Name("b"), Term.Name("c"))),
            List(Term.Name("d"), Term.Name("e"))
          ),
          List(Term.Name("f"), Term.Name("g"))
        )
      )
    )
  }

  test("indented-for") {
    runTestAssert[Stat](
      """|for { project <- projects
         |      (source, id) <- project.sources.zipWithIndex } yield source 
         |""".stripMargin,
      assertLayout = Some(
        "for (project <- projects; (source, id) <- project.sources.zipWithIndex) yield source"
      )
    )(
      Term.ForYield(
        List(
          Enumerator.Generator(Pat.Var(Term.Name("project")), Term.Name("projects")),
          Enumerator.Generator(
            Pat.Tuple(List(Pat.Var(Term.Name("source")), Pat.Var(Term.Name("id")))),
            Term.Select(
              Term.Select(Term.Name("project"), Term.Name("sources")),
              Term.Name("zipWithIndex")
            )
          )
        ),
        Term.Name("source")
      )
    )
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
    )(
      Defn.Def(
        Nil,
        Term.Name("method"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Term.Apply(Term.Name("fun"), List(Term.Name("a"), Term.Name("b"), Term.Name("c"))),
            Term.Tuple(List(Term.Name("d"), Term.Name("e")))
          )
        )
      )
    )
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
    )(
      Defn.Def(
        Nil,
        Term.Name("method"),
        Nil,
        Nil,
        Some(Type.Name("String")),
        Term.Apply(
          Term.Apply(Term.Name("fun"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3))),
          List(Term.Block(List(Lit.Int(4))))
        )
      )
    )
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
    )(
      Defn.Def(
        Nil,
        Term.Name("method2"),
        Nil,
        Nil,
        Some(Type.Name("String")),
        Term.Block(
          List(
            Term.Apply(Term.Name("fun"), List(Lit.Int(1), Lit.Int(2), Lit.Int(3))),
            Term.Block(List(Lit.Int(4)))
          )
        )
      )
    )
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
    )(
      Defn.Object(
        Nil,
        Term.Name("ExampleThing"),
        Template(
          Nil,
          List(
            Init(
              Type.Name("CompositeThing"),
              Name(""),
              List(
                List(Lit.String("One"), Lit.String("Two"), Lit.String("Three"), Lit.String("Four"))
              )
            )
          ),
          Self(Name(""), None),
          Nil,
          Nil
        )
      )
    )
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
    )(
      Defn.Object(
        Nil,
        Term.Name("O"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Object(
              Nil,
              Term.Name("ExampleThing"),
              Template(
                Nil,
                List(Init(Type.Name("CompositeThing"), Name(""), Nil)),
                Self(Name(""), None),
                Nil,
                Nil
              )
            ),
            Term.Tuple(
              List(Lit.String("One"), Lit.String("Two"), Lit.String("Three"), Lit.String("Four"))
            )
          ),
          Nil
        )
      )
    )
  }

  test("indented-enum-contructor-params") {
    runTestAssert[Stat](
      """|enum Namespace(val uri: String | Null):
         |  case xhtml
         |      extends Namespace // Defn.EnumCase ends here
         |        ("http://www.w3.org/1999/xhtml") // Lit.String
         |""".stripMargin,
      assertLayout = Some(
        """|enum Namespace(val uri: String | Null) { case xhtml extends Namespace("http://www.w3.org/1999/xhtml") }
           |""".stripMargin
      )
    )(
      Defn.Enum(
        Nil,
        Type.Name("Namespace"),
        Nil,
        Ctor.Primary(
          Nil,
          Name(""),
          List(
            List(
              Term.Param(
                List(Mod.ValParam()),
                Term.Name("uri"),
                Some(Type.Or(Type.Name("String"), Type.Name("Null"))),
                None
              )
            )
          )
        ),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.EnumCase(
              Nil,
              Term.Name("xhtml"),
              Nil,
              Ctor.Primary(Nil, Name(""), Nil),
              List(
                Init(
                  Type.Name("Namespace"),
                  Name(""),
                  List(List(Lit.String("http://www.w3.org/1999/xhtml")))
                )
              )
            )
          ),
          Nil
        )
      )
    )
  }

  test("indented-double-new") {
    runTestAssert[Stat](
      """|new fun
         |    (a,b,c)
         |    (d, e)
         |    (f, g)
         |""".stripMargin,
      assertLayout = Some(
        "new fun(a, b, c)(d, e)(f, g)"
      )
    )(
      Term.New(
        Init(
          Type.Name("fun"),
          Name(""),
          List(
            List(Term.Name("a"), Term.Name("b"), Term.Name("c")),
            List(Term.Name("d"), Term.Name("e")),
            List(Term.Name("f"), Term.Name("g"))
          )
        )
      )
    )
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
      assertLayout = Some(
        "def f = if (x.exists(x => x == 10)) println(\"Yes\") else println(\"No\")"
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("f"),
        Nil,
        Nil,
        None,
        Term.If(
          Term.Apply(
            Term.Select(Term.Name("x"), Term.Name("exists")),
            List(
              Term.Function(
                List(Term.Param(Nil, Term.Name("x"), None, None)),
                Term.ApplyInfix(Term.Name("x"), Term.Name("=="), Nil, List(Lit.Int(10)))
              )
            )
          ),
          Term.Apply(Term.Name("println"), List(Lit.String("Yes"))),
          Term.Apply(Term.Name("println"), List(Lit.String("No"))),
          Nil
        )
      )
    )
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
    )(
      Source(
        List(
          Pkg(
            Term.Select(Term.Name("tests"), Term.Name("site")),
            List(
              Pkg(
                Term.Select(Term.Name("some"), Term.Name("other")),
                List(
                  Defn.Class(
                    Nil,
                    Type.Name("SomeOtherPackage"),
                    Nil,
                    Ctor.Primary(Nil, Name(""), Nil),
                    Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
                  )
                )
              ),
              Defn.Class(
                Nil,
                Type.Name("BrokenLink"),
                Nil,
                Ctor.Primary(Nil, Name(""), Nil),
                Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
              )
            )
          )
        )
      )
    )
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
    )(
      Source(
        List(
          Defn.Def(
            Nil,
            Term.Name("abc"),
            Nil,
            Nil,
            Some(Type.Name("Unit")),
            Term.Match(Term.Name("x"), List(Case(Pat.Wildcard(), None, Term.Block(Nil))), Nil)
          ),
          Term.EndMarker(Term.Name("abc"))
        )
      )
    )
  }

  test("infix-operator-with-alpha") {
    runTestAssert[Stat](
      """|def send() =
         |  c ! "hello"
         |    ! "world"
         |    send_! "!"
         |""".stripMargin,
      assertLayout = Some("""def send() = c ! "hello" ! "world" send_! "!"""")
    )(
      Defn.Def(
        Nil,
        Term.Name("send"),
        Nil,
        List(List()),
        None,
        Term.ApplyInfix(
          Term.ApplyInfix(
            Term.ApplyInfix(Term.Name("c"), Term.Name("!"), Nil, List(Lit.String("hello"))),
            Term.Name("!"),
            Nil,
            List(Lit.String("world"))
          ),
          Term.Name("send_!"),
          Nil,
          List(Lit.String("!"))
        )
      )
    )
  }

  test("colon-eol-comment1") {
    runTestAssert[Stat](
      """|object Foo:
         |  /*inline*/ def foo: Int = ???
         |  def bar: Int = ???
         |""".stripMargin,
      assertLayout = None
    )(
      Defn.Object(
        Nil,
        Term.Name("Foo"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Def(Nil, Term.Name("foo"), Nil, Nil, Some(Type.Name("Int")), Term.Name("???")),
            Defn.Def(Nil, Term.Name("bar"), Nil, Nil, Some(Type.Name("Int")), Term.Name("???"))
          ),
          Nil
        )
      )
    )
  }

  test("colon-eol-comment2") {
    runTestAssert[Stat](
      """|object Foo: /* comment*/
         |  def foo: Int = ???
         |""".stripMargin,
      assertLayout = None
    )(
      Defn.Object(
        Nil,
        Term.Name("Foo"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(Defn.Def(Nil, Term.Name("foo"), Nil, Nil, Some(Type.Name("Int")), Term.Name("???"))),
          Nil
        )
      )
    )
  }

  test("colon-eol-multiline-comment") {
    runTestAssert[Stat](
      """|object Foo:/* multi
         |  line
         |   comment */ def foo: Int = ???
         |   def bar: Int = ???
         |""".stripMargin,
      assertLayout = None
    )(
      Defn.Object(
        Nil,
        Term.Name("Foo"),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.Def(Nil, Term.Name("foo"), Nil, Nil, Some(Type.Name("Int")), Term.Name("???")),
            Defn.Def(Nil, Term.Name("bar"), Nil, Nil, Some(Type.Name("Int")), Term.Name("???"))
          ),
          Nil
        )
      )
    )
  }

  test("given-with-comment") {
    runTestAssert[Stat](
      """|given Foo with
         |   /* comment */  def foo: Int = ???
         |   def bar: Int = ???
         |""".stripMargin,
      assertLayout = None
    )(
      Defn.Given(
        Nil,
        Name(""),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Name("Foo"), Name(""), Nil)),
          Self(Name(""), None),
          List(
            Defn.Def(Nil, Term.Name("foo"), Nil, Nil, Some(Type.Name("Int")), Term.Name("???")),
            Defn.Def(Nil, Term.Name("bar"), Nil, Nil, Some(Type.Name("Int")), Term.Name("???"))
          ),
          Nil
        )
      )
    )
  }

  test("given-with-miltiline-comment") {
    runTestAssert[Stat](
      """|given Foo with /* multi
         |   line
         |   comment */  def foo: Int = ???
         |   def bar: Int = ???
         |""".stripMargin,
      assertLayout = None
    )(
      Defn.Given(
        Nil,
        Name(""),
        Nil,
        Nil,
        Template(
          Nil,
          List(Init(Type.Name("Foo"), Name(""), Nil)),
          Self(Name(""), None),
          List(
            Defn.Def(Nil, Term.Name("foo"), Nil, Nil, Some(Type.Name("Int")), Term.Name("???")),
            Defn.Def(Nil, Term.Name("bar"), Nil, Nil, Some(Type.Name("Int")), Term.Name("???"))
          ),
          Nil
        )
      )
    )
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
    )(
      Source(
        List(
          Defn.Class(
            Nil,
            Type.Name("A1"),
            Nil,
            Ctor.Primary(Nil, Name(""), Nil),
            Template(
              Nil,
              List(
                Init(
                  Type.Select(
                    Term.Select(Term.Name("scala"), Term.Name("annotation")),
                    Type.Name("StaticAnnotation")
                  ),
                  Name(""),
                  Nil
                )
              ),
              Self(Name(""), None),
              Nil,
              Nil
            )
          ),
          Defn.Class(
            Nil,
            Type.Name("A2"),
            Nil,
            Ctor.Primary(Nil, Name(""), Nil),
            Template(
              Nil,
              List(
                Init(
                  Type.Select(
                    Term.Select(Term.Name("scala"), Term.Name("annotation")),
                    Type.Name("StaticAnnotation")
                  ),
                  Name(""),
                  Nil
                )
              ),
              Self(Name(""), None),
              Nil,
              Nil
            )
          ),
          Defn.Class(
            List(
              Mod.Annot(Init(Type.Name("A1"), Name(""), Nil)),
              Mod.Annot(Init(Type.Name("A2"), Name(""), Nil))
            ),
            Type.Name("B"),
            Nil,
            Ctor.Primary(Nil, Name(""), Nil),
            Template(Nil, Nil, Self(Name(""), None), Nil, Nil)
          )
        )
      )
    )
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
    )(
      Source(
        List(
          Defn.Trait(
            Nil,
            Type.Name("T2"),
            Nil,
            Ctor.Primary(Nil, Name(""), Nil),
            Template(
              Nil,
              Nil,
              Self(Term.Name("self"), Some(Type.Name("T"))),
              List(
                Defn.Enum(
                  Nil,
                  Type.Name("T2Enum"),
                  Nil,
                  Ctor.Primary(Nil, Name(""), Nil),
                  Template(
                    Nil,
                    Nil,
                    Self(Name(""), None),
                    List(
                      Defn.EnumCase(
                        Nil,
                        Term.Name("EnumCase"),
                        Nil,
                        Ctor.Primary(Nil, Name(""), Nil),
                        Nil
                      )
                    ),
                    Nil
                  )
                ),
                Defn.ExtensionGroup(
                  Nil,
                  List(List(Term.Param(Nil, Term.Name("n"), Some(Type.Name("Int")), None))),
                  Defn.Def(
                    Nil,
                    Term.Name("negate"),
                    Nil,
                    Nil,
                    Some(Type.Name("Int")),
                    Term.ApplyUnary(Term.Name("-"), Term.Name("n"))
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
