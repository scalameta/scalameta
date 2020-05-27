package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class MacroSuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Dotty)

  /** All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/metaprogramming/toc.html
   *  https://dotty.epfl.ch/docs/reference/metaprogramming/macros.html
   *  https://dotty.epfl.ch/docs/reference/metaprogramming/tasty-reflect.html
   *
   *  '{ ... } OR 'ident - QuotedMacroExpr
   *  '[ ... ] - QuotedMacroType
   *  ${ ... } OR $ident - SplicedMacroExpr
   *  symbols 'ident are not supported in dotty
   */
  test("parse-single-quote-character") {
    runTestAssert[Stat]("val a = 'c'")(
      Defn.Val(Nil, List(Pat.Var(tname("a"))), None, Lit.Char('c'))
    )
  }

  test("macro-quote-expr") {
    runTestAssert[Stat]("'{ Int.MinValue }")(
      Term.QuotedMacroExpr(List(Term.Select(tname("Int"), tname("MinValue"))))
    )
    runTestAssert[Stat]("'{ (x: T) => 'x }")(
      Term.QuotedMacroExpr(List(Term.Function(List(tparam("x", "T")), Lit.Symbol('x))))
    )
    runTestAssert[Stat]("f('{ 2 })")(
      Term.Apply(tname("f"), List(Term.QuotedMacroExpr(List(Lit.Int(2)))))
    )
    runTestAssert[Stat]("if (b) '{ true } else '{ false }")(
      Term.If(
        tname("b"),
        Term.QuotedMacroExpr(List(Lit.Boolean(true))),
        Term.QuotedMacroExpr(List(Lit.Boolean(false)))
      )
    )
    runTestAssert[Stat]("'{ val y = a; ${ env } }", assertLayout = false)(
      Term.QuotedMacroExpr(
        List(
          Defn.Val(Nil, List(Pat.Var(tname("y"))), None, tname("a")),
          Term.SplicedMacroExpr(List(tname("env")))
        )
      )
    )
    runTestAssert[Stat]("x match { case '{ a } => 1 }", assertLayout = false)(
      Term.Match(
        tname("x"),
        List(Case(Pat.Macro(Term.QuotedMacroExpr(List(tname("a")))), None, Lit.Int(1)))
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
                List(
                  Term.ApplyType(tname("summon"), List(Type.Apply(pname("Type"), List(pname("T")))))
                )
              )
            )
          )
        )
      )
    )
    runTestAssert[Stat]("'[ Show[$tp] ]")(
      Term.QuotedMacroType(Type.Apply(pname("Show"), List(pname("$tp"))))
    )
  }

  test("macro-splice") {
    runTestAssert[Stat]("${ powerCode('x) }")(
      Term.SplicedMacroExpr(List(Term.Apply(tname("powerCode"), List(Lit.Symbol('x)))))
    )
    runTestAssert[Stat]("${ x = 'y; 1 }", assertLayout = false)(
      Term.SplicedMacroExpr(List(Term.Assign(tname("x"), Lit.Symbol('y)), Lit.Int(1)))
    )
    runTestAssert[Stat]("${ assertImpl('{ x != $y }) }")(
      Term.SplicedMacroExpr(
        List(
          Term.Apply(
            tname("assertImpl"),
            List(
              Term.QuotedMacroExpr(
                List(Term.ApplyInfix(tname("x"), tname("!="), Nil, List(tname("$y"))))
              )
            )
          )
        )
      )
    )
  }

  test("macro-quote-complex") {
    runTestAssert[Stat]("'{ ClassTag[T](${ Expr(ct.runtimeClass.asInstanceOf[Class[T]]) }) }")(
      Term.QuotedMacroExpr(
        List(
          Term.Apply(
            Term.ApplyType(tname("ClassTag"), List(pname("T"))),
            List(
              Term.SplicedMacroExpr(
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

    // TODO: it generates () around postfix argument, check if it is necessary
    runTestAssert[Stat](
      "'{ ${ summon[H].toExpr(tup.head) } *: ${ summon[T].toExpr(tup.tail) } }",
      assertLayout = false
    )(
      Term.QuotedMacroExpr(
        List(
          Term.ApplyInfix(
            Term.SplicedMacroExpr(
              List(
                Term.Apply(
                  Term.Select(Term.ApplyType(tname("summon"), List(pname("H"))), tname("toExpr")),
                  List(Term.Select(tname("tup"), tname("head")))
                )
              )
            ),
            tname("*:"),
            Nil,
            List(
              Term.SplicedMacroExpr(
                List(
                  Term.Apply(
                    Term.Select(Term.ApplyType(tname("summon"), List(pname("T"))), tname("toExpr")),
                    List(Term.Select(tname("tup"), tname("tail")))
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
