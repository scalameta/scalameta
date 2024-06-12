package scala.meta.tests.parsers.dotty

import scala.meta._

class MacroSuite extends BaseDottySuite {

  implicit val parseBlock: String => Stat = code => blockStat(code)(dialects.Scala3)

  /**
   * All examples based on dotty documentation:
   *   - [[https://dotty.epfl.ch/docs/reference/metaprogramming/toc.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/metaprogramming/macros.html]]
   *   - [[https://dotty.epfl.ch/docs/reference/metaprogramming/tasty-reflect.html]]
   *
   * Variants:
   *   - `QuotedMacroType`: `'[ ... ]`
   *   - `QuotedMacroExpr`: `'{ ... }` OR `'ident`
   *   - `SplicedMacroExpr`: `${ ... }` OR `$ident`
   */
  test("parse-single-quote-character") {
    runTestAssert[Stat]("val a = 'c'")(Defn.Val(Nil, List(Pat.Var(tname("a"))), None, Lit.Char('c')))
  }

  test("parse-single-quote-space") {
    runTestAssert[Stat]("' { 'ax }", assertLayout = Some("'{ 'ax }"))(Term.QuotedMacroExpr(blk(
      Term.QuotedMacroExpr(tname("ax"))
    )))
  }

  test("macro-quote-expr: '{ 'ax }") {
    runTestAssert[Stat]("'{ 'ax }")(Term.QuotedMacroExpr(blk(Term.QuotedMacroExpr(tname("ax")))))
  }

  test("macro-quote-expr: '{ 'a + 'b }") {
    runTestAssert[Stat]("'{ 'a + 'b }")(Term.QuotedMacroExpr(blk(Term.ApplyInfix(
      Term.QuotedMacroExpr(tname("a")),
      tname("+"),
      Nil,
      List(Term.QuotedMacroExpr(tname("b")))
    ))))
  }

  test("macro-quote-expr: '{ 'a + 'b + 'c }") {
    runTestAssert[Stat]("'{ 'a + 'b + 'c }")(Term.QuotedMacroExpr(blk(Term.ApplyInfix(
      Term.ApplyInfix(
        Term.QuotedMacroExpr(tname("a")),
        tname("+"),
        Nil,
        List(Term.QuotedMacroExpr(tname("b")))
      ),
      tname("+"),
      Nil,
      List(Term.QuotedMacroExpr(tname("c")))
    ))))
  }

  test("macro-quote-expr: '{ Int.MinValue }") {
    runTestAssert[Stat]("'{ Int.MinValue }")(Term.QuotedMacroExpr(blk(
      Term.Select(tname("Int"), tname("MinValue"))
    )))
  }

  test("macro-quote-expr: '{ (x: T) => 'x }") {
    runTestAssert[Stat]("'{ (x: T) => 'x }")(Term.QuotedMacroExpr(blk(
      Term.Function(List(tparam("x", "T")), Term.QuotedMacroExpr(tname("x")))
    )))
  }

  test("macro-quote-expr: f('{ 2 })") {
    runTestAssert[Stat]("f('{ 2 })")(Term.Apply(tname("f"), List(Term.QuotedMacroExpr(blk(int(2))))))
  }

  test("macro-quote-expr: if (b) '{ true } else '{ false }") {
    runTestAssert[Stat]("if (b) '{ true } else '{ false }")(
      Term
        .If(tname("b"), Term.QuotedMacroExpr(blk(bool(true))), Term.QuotedMacroExpr(blk(bool(false))))
    )
  }

  test("macro-quote-expr: '{ val y = a; ${ env } }") {
    runTestAssert[Stat]("'{ val y = a; ${ env } }", assertLayout = None)(Term.QuotedMacroExpr(blk(
      Defn.Val(Nil, List(Pat.Var(tname("y"))), None, tname("a")),
      Term.SplicedMacroExpr(blk(tname("env")))
    )))
  }

  test("macro-quote-expr: x match { case 'c => 1 }") {
    val layoutMatchSimple = "x match {\n  case 'c => 1\n}"
    runTestAssert[Stat]("x match { case 'c => 1 }", assertLayout = Some(layoutMatchSimple))(
      Term.Match(tname("x"), List(Case(Lit.Symbol('c), None, int(1))), Nil)
    )
  }

  test("macro-quote-expr: x match { case '{ a } => 1 }") {
    val layoutMatchComplex = "x match {\n  case '{ a } => 1\n}"
    runTestAssert[Stat]("x match { case '{ a } => 1 }", assertLayout = Some(layoutMatchComplex))(
      Term
        .Match(tname("x"), List(Case(Pat.Macro(Term.QuotedMacroExpr(blk(tname("a")))), None, int(1))))
    )
  }

  test("macro-quote-expr: x match { case '{ $${bind@'{a}} } => 1 }") {
    val layoutMatchWithSplice = s"x match {\n  case '{ $${ bind @ '{ a } } } => 1\n}"
    runTestAssert[Stat](
      s"x match { case '{ $${bind@'{a}} } => 1 }",
      assertLayout = Some(layoutMatchWithSplice)
    )(Term.Match(
      tname("x"),
      Case(
        Pat.Macro(Term.QuotedMacroExpr(blk(Term.SplicedMacroPat(
          Pat.Bind(Pat.Var(tname("bind")), Pat.Macro(Term.QuotedMacroExpr(blk(tname("a")))))
        )))),
        None,
        int(1)
      ) :: Nil
    ))
  }

  test("macro-brackets") {
    runTestAssert[Stat](
      """|tpr.asType match {
         |  case '[ t ] =>
         |    getTypeTree[t]
         |}""".stripMargin
    )(Term.Match(
      Term.Select(tname("tpr"), tname("asType")),
      Case(
        Pat.Macro(Term.QuotedMacroType(pname("t"))),
        None,
        Term.ApplyType(tname("getTypeTree"), List(pname("t")))
      ) :: Nil,
      Nil
    ))

  }

  test("macro-quote-multiline") {
    val code = """|'{
                  |  1 + 3
                  |  'x + 3
                  |  zzz
                  |}""".stripMargin
    runTestAssert[Stat](code)(Term.QuotedMacroExpr(blk(
      Term.ApplyInfix(int(1), tname("+"), Nil, List(int(3))),
      Term.ApplyInfix(Term.QuotedMacroExpr(tname("x")), tname("+"), Nil, List(int(3))),
      tname("zzz")
    )))
  }

  test("macro-quote-type: '[ Map[Int, String] ]") {
    runTestAssert[Stat]("'[ Map[Int, String] ]")(Term.QuotedMacroType(
      Type.Apply(pname("Map"), List(pname("Int"), pname("String")))
    ))
  }

  test("macro-quote-type: '[ List[${ summon[Type[T]] }] ]") {
    runTestAssert[Stat]("'[ List[${ summon[Type[T]] }] ]")(Term.QuotedMacroType(Type.Apply(
      pname("List"),
      Type.Macro(Term.SplicedMacroExpr(blk(
        Term.ApplyType(tname("summon"), List(Type.Apply(pname("Type"), List(pname("T")))))
      ))) :: Nil
    )))
  }

  test("macro-quote-type: '[ Show[$tp] ]") {
    runTestAssert[Stat]("'[ Show[$tp] ]")(Term.QuotedMacroType(
      Type.Apply(pname("Show"), List(Type.Macro(Term.SplicedMacroExpr(tname("tp")))))
    ))
  }

  test("macro-splice: ${ $x }") {
    val code = "${ $x }"
    val tree = Term.SplicedMacroExpr(blk(Term.SplicedMacroExpr(tname("x"))))
    runTestAssert[Stat](code)(tree)
    runTestAssert[Stat](code, assertLayout = Some(code))(tree)
    val backquoted = Term.SplicedMacroExpr(blk(tname("$x")))
    parseAndCheckTree[Stat]("${ `$x` }", code)(backquoted)
  }

  test("macro-splice: ${ powerCode('x) }") {
    runTestAssert[Stat]("${ powerCode('x) }")(Term.SplicedMacroExpr(blk(
      Term.Apply(tname("powerCode"), List(Term.QuotedMacroExpr(tname("x"))))
    )))
  }

  test("macro-splice: ${ val x = 'y; println(d); 1 }") {
    val multilineSpliceLayout = "${\n  val x = 'y\n  println(d)\n  1\n}"
    runTestAssert[Stat]("${ val x = 'y; println(d); 1 }", multilineSpliceLayout)(
      Term.SplicedMacroExpr(blk(
        Defn.Val(Nil, List(Pat.Var(tname("x"))), None, Term.QuotedMacroExpr(tname("y"))),
        Term.Apply(tname("println"), List(tname("d"))),
        int(1)
      ))
    )
  }

  test("macro-splice: ${ assertImpl('{ x != $y }) }") {
    runTestAssert[Stat]("${ assertImpl('{ x != $y }) }")(Term.SplicedMacroExpr(blk(Term.Apply(
      tname("assertImpl"),
      Term.QuotedMacroExpr(blk(
        Term.ApplyInfix(tname("x"), tname("!="), Nil, List(Term.SplicedMacroExpr(tname("y"))))
      )) :: Nil
    ))))
  }

  test("macro-splice-multiline") {
    val code = """|${
                  |  val x = 'y
                  |  println(d)
                  |  1
                  |}""".stripMargin
    runTestAssert[Stat](code)(Term.SplicedMacroExpr(blk(
      Defn.Val(Nil, List(Pat.Var(tname("x"))), None, Term.QuotedMacroExpr(tname("y"))),
      Term.Apply(tname("println"), List(tname("d"))),
      int(1)
    )))
  }

  test("non-macro-dollar-ident") {
    val code = "a.map($d => $d.a)"
    runTestAssert[Stat](code)(Term.Apply(
      Term.Select(tname("a"), tname("map")),
      List(Term.Function(List(tparam("$d")), Term.Select(tname("$d"), tname("a"))))
    ))
  }

  test("non-macro-dollar-type") {
    val code = "type $F2 = [$T] => $T => Option[$T]"
    runTestAssert[Stat](code)(Defn.Type(
      Nil,
      pname("$F2"),
      Nil,
      Type.PolyFunction(
        List(pparam("$T")),
        Type.Function(List(pname("$T")), Type.Apply(pname("Option"), List(pname("$T"))))
      ),
      noBounds
    ))
  }

  test("macro-quote-complex: 1") {
    runTestAssert[Stat]("'{ ClassTag[T](${ Expr(ct.runtimeClass.asInstanceOf[Class[T]]) }) }")(
      Term.QuotedMacroExpr(blk(Term.Apply(
        Term.ApplyType(tname("ClassTag"), List(pname("T"))),
        Term.SplicedMacroExpr(blk(Term.Apply(
          tname("Expr"),
          Term.ApplyType(
            Term.Select(Term.Select(tname("ct"), tname("runtimeClass")), tname("asInstanceOf")),
            List(Type.Apply(pname("Class"), List(pname("T"))))
          ) :: Nil
        ))) :: Nil
      )))
    )
  }

  test("macro-quote-complex: 2") {
    runTestAssert[Stat]("'{ ${ summon[H].toExpr(tup.head) } *: ${ summon[T].toExpr(tup.tail) } }")(
      Term.QuotedMacroExpr(blk(Term.ApplyInfix(
        Term.SplicedMacroExpr(blk(Term.Apply(
          Term.Select(Term.ApplyType(tname("summon"), List(pname("H"))), tname("toExpr")),
          List(Term.Select(tname("tup"), tname("head")))
        ))),
        tname("*:"),
        Nil,
        Term.SplicedMacroExpr(blk(Term.Apply(
          Term.Select(Term.ApplyType(tname("summon"), List(pname("T"))), tname("toExpr")),
          List(Term.Select(tname("tup"), tname("tail")))
        ))) :: Nil
      )))
    )
  }

  test("simpler") {
    runTestAssert[Stat](
      "'{ val x: Int = ${ (q2) ?=> a } }",
      assertLayout = Some("'{ val x: Int = ${ q2 ?=> a } }")
    )(Term.QuotedMacroExpr(blk(Defn.Val(
      Nil,
      List(Pat.Var(tname("x"))),
      Some(pname("Int")),
      Term.SplicedMacroExpr(blk(Term.ContextFunction(List(tparam("q2")), tname("a"))))
    ))))
  }

  test("no-name") {
    runTestAssert[Stat]("'{ val x: Int = $ }")(Term.QuotedMacroExpr(blk(
      Defn.Val(Nil, List(Pat.Var(tname("x"))), Some(pname("Int")), tname("$"))
    )))
    runTestAssert[Stat]("${ val x: Int = $ }")(Term.SplicedMacroExpr(blk(
      Defn.Val(Nil, List(Pat.Var(tname("x"))), Some(pname("Int")), tname("$"))
    )))
  }

  test("#3610 1") {
    val code = "'[ List[Int] ]"
    val tree = Term.QuotedMacroType(Type.Apply(pname("List"), List(pname("Int"))))
    runTestAssert[Stat](code)(tree)
  }

  test("#3610 2") {
    val code = "'[ type t = Int; List[t] ]"
    val tree = Term.QuotedMacroType(Type.Block(
      List(Defn.Type(Nil, pname("t"), Nil, pname("Int"), noBounds)),
      Type.Apply(pname("List"), List(pname("t")))
    ))
    runTestAssert[Stat](code)(tree)
  }

  test("#3610 3") {
    val code = "'[ type tail <: Tuple; *:[Int, tail] ]"
    val tree = Term.QuotedMacroType(Type.Block(
      List(Decl.Type(Nil, pname("tail"), Nil, bounds(hi = "Tuple"))),
      Type.Apply(pname("*:"), List(pname("Int"), pname("tail")))
    ))
    runTestAssert[Stat](code)(tree)
  }

  test("#6483-metals splice") {
    val code = "${ fn }[Int](0)"
    val codeInParens = "( ${ fn } )[Int](0)"
    val tree = Term
      .Apply(Term.ApplyType(Term.SplicedMacroExpr(blk(tname("fn"))), List(pname("Int"))), List(lit(0)))
    runTestAssert[Stat](code)(tree)
    runTestAssert[Stat](codeInParens, code)(tree)
  }

  test("#6483-metals splice in braces") {
    val code = """|{
                  |  ${ fn }
                  |}[Int](0)""".stripMargin
    val tree = Term.Apply(
      Term.ApplyType(blk(Term.SplicedMacroExpr(blk(tname("fn")))), List(pname("Int"))),
      List(lit(0))
    )
    runTestAssert[Stat](code)(tree)
  }

  test("#6483-metals poly in parens") {
    val code = "([B] => (c: B) => fn[B](c))[Int](0)"
    val tree = Term.Apply(
      Term.ApplyType(
        Term.PolyFunction(
          List(pparam("B")),
          Term.Function(
            List(tparam("c", "B")),
            Term.Apply(Term.ApplyType(tname("fn"), List(pname("B"))), List(tname("c")))
          )
        ),
        List(pname("Int"))
      ),
      List(lit(0))
    )
    runTestAssert[Stat](code)(tree)
  }

  test("#6483-metals poly in braces") {
    val code = """|{
                  |  [B] => (c: B) => fn[B](c)
                  |}[Int](0)
                  |""".stripMargin
    val tree = Term.Apply(
      Term.ApplyType(
        blk(Term.PolyFunction(
          List(pparam("B")),
          Term.Function(
            List(tparam("c", "B")),
            Term.Apply(Term.ApplyType(tname("fn"), List(pname("B"))), List(tname("c")))
          )
        )),
        List(pname("Int"))
      ),
      List(lit(0))
    )
    runTestAssert[Stat](code)(tree)
  }

}
