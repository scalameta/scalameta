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

  test("indent-match-two") {
    val code = """|
                  |x match
                  |  case 1 => "OK"
                  |  case 2 => "ERROR"
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Match(
        Term.Name("x"),
        List(Case(Lit.Int(1), None, Lit.String("OK")), Case(Lit.Int(2), None, Lit.String("ERROR")))
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
    runTestError[Stat](code, "error:")
  }

  test("indent-case-empty") {
    val code = """|x match {
                  |  case 1 =>
                  |  case 2 =>
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Match(
        Term.Name("x"),
        List(Case(Lit.Int(1), None, Term.Block(Nil)), Case(Lit.Int(2), None, Term.Block(Nil)))
      )
    )
  }

  test("match-case-one-align".ignore) {
    val code = """|cond match {
                  |  case a =>
                  |  fa
                  |  case b =>
                  |  fb
                  |}
                  |""".stripMargin
    val output = """|cond match {
                    |  case a => fa
                    |  case b => fb
                    |}
                    |""".stripMargin

    runTestAssert[Stat](code, assertLayout = Some(output))(
      Term.Match(Term.Name("cond"), List(Case(Pat.Var(Term.Name("a")), None, Term.Name("fa")), Case(Pat.Var(Term.Name("b")), None, Term.Name("fb"))))
    )
  }

  test("should-indent-yet-brace".ignore) {
    val code = """|class X {
                  |  def fx(): Unit =
                  |  {
                  |
                  |  }
                  |  private def f2: Int = 1
                  |}
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Match(
        Term.Name("x"),
        List(Case(Lit.Int(1), None, Term.Block(Nil)), Case(Lit.Int(2), None, Term.Block(Nil)))
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
      Defn.Class(Nil, Type.Name("A"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, List(Init(Type.Name("B"), Name(""), Nil)), Self(Term.Name("thisPhase"), None), List(Term.Name("expr1"), Term.Name("expr2"))))
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
