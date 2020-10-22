package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class SignificantIndentationSuite extends BaseDottySuite {
  implicit val parseStat: String => Stat = code => templStat(code)(dialects.Dotty)
  implicit val parseSource: String => Source = code => source(code)(dialects.Dotty)

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

  test("multistat-example") {
    val code = """|trait A:
                  |  def f: Int
                  |  def y: String = { fa(); fb() }
                  |""".stripMargin
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

  test("indent-below-not-okay".ignore) {
    // this test is related to dotty issue: https://github.com/lampepfl/dotty/issues/9790
    // It should either assert error during parsing: "illegal start of simple expression"
    // Or accept mismatch with parsing rules and parse as 'if (cond) { truep } else {falsep }'
    // Why error is thrown is described in mentioned issue.
    val code = """|def fn: Unit =
                  |    if cond then
                  |  truep
                  |    else
                  |  falsep
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
                    |      for ( case a: TP <- body) {
                    |        fordo
                    |      }
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
                      Term.Block(List(Term.Name("fordo")))
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
    val code = """|def this(msg: String) =
                  |  this(message, false)
                  |""".stripMargin
    val output = "def this(msg: String) = this(message, false)"
    runTestAssert[Stat](code, assertLayout = Some(output))(
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
    )
  }

  test("this-constructor-indented-block") {
    val code = """|def this(msg: String) =
                  |  this(message, false)
                  |  otherStat
                  |""".stripMargin
    val output = """|def this(msg: String) {
                    |  this(message, false)
                    |  otherStat
                    |}
                    |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some(output))(
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
              )
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
        """|def wrapPlaceholders(t: Tree) = try {
           |  if (placeholderParams.isEmpty) t else new WildcardFunction(placeholderParams.reverse, t)
           |} finally placeholderParams = saved
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("wrapPlaceholders"),
        Nil,
        List(List(Term.Param(Nil, Term.Name("t"), Some(Type.Name("Tree")), None))),
        None,
        Term.Try(
          Term.Block(
            List(
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
      """|def f =
         |  val refinementTest:
         |    Graph {
         |      def x: Int
         |    }
         |  val b = 1 
         |
         |""".stripMargin,
      assertLayout = Some(
        """|def f = {
           |  val refinementTest: Graph { def x: Int }
           |  val b = 1
           |}
           |""".stripMargin
      )
    )(
      Defn.Def(
        Nil,
        Term.Name("f"),
        Nil,
        Nil,
        None,
        Term.Block(
          List(
            Decl.Val(
              Nil,
              List(Pat.Var(Term.Name("refinementTest"))),
              Type.Refine(
                Some(Type.Name("Graph")),
                List(Decl.Def(Nil, Term.Name("x"), Nil, Nil, Type.Name("Int")))
              )
            ),
            Defn.Val(Nil, List(Pat.Var(Term.Name("b"))), None, Lit.Int(1))
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
      Defn.Val(Nil, List(Pat.Var(Term.Name("refinementTest"))), Some(Type.Name("Int")), Term.Block(List(Term.Name("fx"), Term.Name("fy"))))
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
      Defn.Val(Nil, List(Pat.Var(Term.Name("refinementTest"))), Some(Type.Name("Int")), Term.Block(List(Term.Name("fx"), Term.Name("fy"))))
    )
  }
}
