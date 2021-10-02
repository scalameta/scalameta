package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class MacroSuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)

  /**
   * All examples based on dotty documentation:
   * https://dotty.epfl.ch/docs/reference/metaprogramming/toc.html
   * https://dotty.epfl.ch/docs/reference/metaprogramming/macros.html
   * https://dotty.epfl.ch/docs/reference/metaprogramming/tasty-reflect.html
   *
   * - '{ ... } OR 'ident - QuotedMacroExpr
   * - '[ ... ] - QuotedMacroType
   * - ${ ... } OR $ident -  SplicedMacroExpr
   */
  test("parse-single-quote-character") {
    runTestAssert[Stat]("val a = 'c'")(
      Defn.Val(Nil, List(Pat.Var(tname("a"))), None, Lit.Char('c'))
    )
  }

  test("parse-single-quote-space") {
    runTestAssert[Stat]("' { 'ax }", assertLayout = Some("'{ 'ax }"))(
      Term.QuotedMacroExpr(Term.Block(List(Term.QuotedMacroExpr(tname("ax")))))
    )
  }

  test("macro-quote-expr") {
    runTestAssert[Stat]("'{ 'ax }")(
      Term.QuotedMacroExpr(Term.Block(List(Term.QuotedMacroExpr(tname("ax")))))
    )
    runTestAssert[Stat]("'{ 'a + 'b }")(
      Term.QuotedMacroExpr(
        Term.Block(
          List(
            Term.ApplyInfix(
              Term.QuotedMacroExpr(tname("a")),
              tname("+"),
              Nil,
              List(Term.QuotedMacroExpr(tname("b")))
            )
          )
        )
      )
    )
    runTestAssert[Stat]("'{ 'a + 'b + 'c }")(
      Term.QuotedMacroExpr(
        Term.Block(
          List(
            Term.ApplyInfix(
              Term.ApplyInfix(
                Term.QuotedMacroExpr(tname("a")),
                tname("+"),
                Nil,
                List(Term.QuotedMacroExpr(tname("b")))
              ),
              tname("+"),
              Nil,
              List(Term.QuotedMacroExpr(tname("c")))
            )
          )
        )
      )
    )
    runTestAssert[Stat]("'{ Int.MinValue }")(
      Term.QuotedMacroExpr(Term.Block(List(Term.Select(tname("Int"), tname("MinValue")))))
    )
    runTestAssert[Stat]("'{ (x: T) => 'x }")(
      Term.QuotedMacroExpr(
        Term.Block(List(Term.Function(List(tparam("x", "T")), Term.QuotedMacroExpr(tname("x")))))
      )
    )
    runTestAssert[Stat]("f('{ 2 })")(
      Term.Apply(tname("f"), List(Term.QuotedMacroExpr(Term.Block(List(Lit.Int(2))))))
    )
    runTestAssert[Stat]("if (b) '{ true } else '{ false }")(
      Term.If(
        tname("b"),
        Term.QuotedMacroExpr(Term.Block(List(Lit.Boolean(true)))),
        Term.QuotedMacroExpr(Term.Block(List(Lit.Boolean(false))))
      )
    )
    runTestAssert[Stat]("'{ val y = a; ${ env } }", assertLayout = None)(
      Term.QuotedMacroExpr(
        Term.Block(
          List(
            Defn.Val(Nil, List(Pat.Var(tname("y"))), None, tname("a")),
            Term.SplicedMacroExpr(Term.Block(List(tname("env"))))
          )
        )
      )
    )
    val layoutMatchSimple = "x match {\n  case 'c => 1\n}"
    runTestAssert[Stat]("x match { case 'c => 1 }", assertLayout = Some(layoutMatchSimple))(
      Term.Match(
        tname("x"),
        List(Case(Lit.Symbol('c), None, Lit.Int(1))),
        Nil
      )
    )
    val layoutMatchComplex = "x match {\n  case '{ a } => 1\n}"
    runTestAssert[Stat]("x match { case '{ a } => 1 }", assertLayout = Some(layoutMatchComplex))(
      Term.Match(
        tname("x"),
        List(Case(Pat.Macro(Term.QuotedMacroExpr(Term.Block(List(tname("a"))))), None, Lit.Int(1)))
      )
    )
  }

  test("macro-brackets") {
    runTestAssert[Stat](
      """|tpr.asType match {
         |  case '[ t ] =>
         |    getTypeTree[t]
         |}""".stripMargin
    )(
      Term.Match(
        Term.Select(Term.Name("tpr"), Term.Name("asType")),
        List(
          Case(
            Pat.Macro(Term.QuotedMacroType(Type.Name("t"))),
            None,
            Term.ApplyType(Term.Name("getTypeTree"), List(Type.Name("t")))
          )
        ),
        Nil
      )
    )

  }

  test("macro-quote-multiline") {
    val code = """|'{
                  |  1 + 3
                  |  'x + 3
                  |  zzz
                  |}""".stripMargin
    runTestAssert[Stat](code)(
      Term.QuotedMacroExpr(
        Term.Block(
          List(
            Term.ApplyInfix(Lit.Int(1), tname("+"), Nil, List(Lit.Int(3))),
            Term.ApplyInfix(Term.QuotedMacroExpr(tname("x")), tname("+"), Nil, List(Lit.Int(3))),
            tname("zzz")
          )
        )
      )
    )
  }

  test("macro-quote-type") {
    runTestAssert[Stat]("'[ Map[Int, String] ]")(
      Term.QuotedMacroType(Type.Apply(pname("Map"), List(pname("Int"), pname("String"))))
    )
    runTestAssert[Stat]("'[ List[${ summon[Type[T]] }] ]")(
      Term.QuotedMacroType(
        Type.Apply(
          pname("List"),
          List(
            Type.Macro(
              Term.SplicedMacroExpr(
                Term.Block(
                  List(
                    Term
                      .ApplyType(tname("summon"), List(Type.Apply(pname("Type"), List(pname("T")))))
                  )
                )
              )
            )
          )
        )
      )
    )
    runTestAssert[Stat]("'[ Show[$tp] ]")(
      Term.QuotedMacroType(
        Type.Apply(Type.Name("Show"), List(Type.Macro(Term.SplicedMacroExpr(Term.Name("tp")))))
      )
    )
  }

  test("macro-splice") {
    runTestAssert[Stat]("${ $x }")(
      Term.SplicedMacroExpr(Term.Block(List(Term.SplicedMacroExpr(tname("x")))))
    )
    runTestAssert[Stat]("$ { $x }", assertLayout = Some("${ $x }"))(
      Term.SplicedMacroExpr(Term.Block(List(Term.SplicedMacroExpr(tname("x")))))
    )
    runTestAssert[Stat]("${ powerCode('x) }")(
      Term.SplicedMacroExpr(
        Term.Block(List(Term.Apply(tname("powerCode"), List(Term.QuotedMacroExpr(tname("x"))))))
      )
    )
    val multilineSpliceLayout = "${\n  val x = 'y\n  println(d)\n  1\n}"
    runTestAssert[Stat](
      "${ val x = 'y; println(d); 1 }",
      assertLayout = Some(multilineSpliceLayout)
    )(
      Term.SplicedMacroExpr(
        Term.Block(
          List(
            Defn.Val(Nil, List(Pat.Var(tname("x"))), None, Term.QuotedMacroExpr(tname("y"))),
            Term.Apply(tname("println"), List(tname("d"))),
            Lit.Int(1)
          )
        )
      )
    )
    runTestAssert[Stat]("${ assertImpl('{ x != $y }) }", Some("${ assertImpl('{ x != $y }) }"))(
      Term.SplicedMacroExpr(
        Term.Block(
          List(
            Term.Apply(
              tname("assertImpl"),
              List(
                Term.QuotedMacroExpr(
                  Term.Block(
                    List(
                      Term.ApplyInfix(
                        tname("x"),
                        tname("!="),
                        Nil,
                        List(Term.SplicedMacroExpr(tname("y")))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("macro-splice-multiline") {
    val code = """|${
                  |  val x = 'y
                  |  println(d)
                  |  1
                  |}""".stripMargin
    runTestAssert[Stat](code)(
      Term.SplicedMacroExpr(
        Term.Block(
          List(
            Defn.Val(Nil, List(Pat.Var(tname("x"))), None, Term.QuotedMacroExpr(tname("y"))),
            Term.Apply(tname("println"), List(tname("d"))),
            Lit.Int(1)
          )
        )
      )
    )
  }

  test("non-macro-dollar-ident") {
    val code = "a.map($d => $d.a)"
    runTestAssert[Stat](code)(
      Term.Apply(
        Term.Select(Term.Name("a"), Term.Name("map")),
        List(
          Term.Function(
            List(Term.Param(Nil, Term.Name("$d"), None, None)),
            Term.Select(Term.Name("$d"), Term.Name("a"))
          )
        )
      )
    )
  }

  test("non-macro-dollar-type") {
    val code = "type $F2 = [$T] => $T => Option[$T]"
    runTestAssert[Stat](code)(
      Defn.Type(
        Nil,
        Type.Name("$F2"),
        Nil,
        Type.PolyFunction(
          List(Type.Param(Nil, Type.Name("$T"), Nil, Type.Bounds(None, None), Nil, Nil)),
          Type
            .Function(List(Type.Name("$T")), Type.Apply(Type.Name("Option"), List(Type.Name("$T"))))
        ),
        Type.Bounds(None, None)
      )
    )
  }

  test("macro-quote-complex") {
    runTestAssert[Stat]("'{ ClassTag[T](${ Expr(ct.runtimeClass.asInstanceOf[Class[T]]) }) }")(
      Term.QuotedMacroExpr(
        Term.Block(
          List(
            Term.Apply(
              Term.ApplyType(tname("ClassTag"), List(pname("T"))),
              List(
                Term.SplicedMacroExpr(
                  Term.Block(
                    List(
                      Term.Apply(
                        tname("Expr"),
                        List(
                          Term.ApplyType(
                            Term.Select(
                              Term.Select(tname("ct"), tname("runtimeClass")),
                              tname("asInstanceOf")
                            ),
                            List(Type.Apply(pname("Class"), List(pname("T"))))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    runTestAssert[Stat](
      "'{ ${ summon[H].toExpr(tup.head) } *: ${ summon[T].toExpr(tup.tail) } }"
    )(
      Term.QuotedMacroExpr(
        Term.Block(
          List(
            Term.ApplyInfix(
              Term.SplicedMacroExpr(
                Term.Block(
                  List(
                    Term.Apply(
                      Term
                        .Select(Term.ApplyType(tname("summon"), List(pname("H"))), tname("toExpr")),
                      List(Term.Select(tname("tup"), tname("head")))
                    )
                  )
                )
              ),
              tname("*:"),
              Nil,
              List(
                Term.SplicedMacroExpr(
                  Term.Block(
                    List(
                      Term.Apply(
                        Term.Select(
                          Term.ApplyType(tname("summon"), List(pname("T"))),
                          tname("toExpr")
                        ),
                        List(Term.Select(tname("tup"), tname("tail")))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("simpler") {
    runTestAssert[Stat](
      "'{ val x: Int = ${ (q2) ?=> a } }",
      assertLayout = Some("'{ val x: Int = ${ q2 ?=> a } }")
    )(
      Term.QuotedMacroExpr(
        Term.Block(
          List(
            Defn.Val(
              Nil,
              List(Pat.Var(Term.Name("x"))),
              Some(Type.Name("Int")),
              Term.SplicedMacroExpr(
                Term.Block(
                  List(
                    Term.ContextFunction(
                      List(Term.Param(Nil, Term.Name("q2"), None, None)),
                      Term.Name("a")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  }
}
