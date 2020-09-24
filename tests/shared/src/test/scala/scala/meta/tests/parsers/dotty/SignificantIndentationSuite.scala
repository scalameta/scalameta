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

  test("indent-below-not-okay") {
    val code = """|def fn: Unit =
                  |    if cond then
                  |  truep
                  |    else
                  |  falsep
                  |""".stripMargin
    runTestError[Stat](code, "error: illegal start of simple expression")
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
    runTestAssert[Stat](code, assertLayout = None)(
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
    runTestAssert[Stat](code, assertLayout = None)(
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
}
