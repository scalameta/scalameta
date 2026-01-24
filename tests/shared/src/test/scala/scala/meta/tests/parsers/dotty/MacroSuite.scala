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
  test("parse-single-quote-character")(
    runTestAssert[Stat]("val a = 'c'")(Defn.Val(Nil, List(patvar("a")), None, lit('c')))
  )

  test("parse-single-quote-space")(runTestAssert[Stat]("' { 'ax }", assertLayout = Some("'{ 'ax }"))(
    Term.QuotedMacroExpr(blk(Term.QuotedMacroExpr(tname("ax"))))
  ))

  test("macro-quote-expr: '{ 'ax }")(
    runTestAssert[Stat]("'{ 'ax }")(Term.QuotedMacroExpr(blk(Term.QuotedMacroExpr(tname("ax")))))
  )

  test("macro-quote-expr: '{ 'a + 'b }")(runTestAssert[Stat]("'{ 'a + 'b }")(Term.QuotedMacroExpr(
    blk(tinfix(Term.QuotedMacroExpr(tname("a")), "+", Term.QuotedMacroExpr(tname("b"))))
  )))

  test("macro-quote-expr: '{ 'a + 'b + 'c }")(
    runTestAssert[Stat]("'{ 'a + 'b + 'c }")(Term.QuotedMacroExpr(blk(tinfix(
      tinfix(Term.QuotedMacroExpr(tname("a")), "+", Term.QuotedMacroExpr(tname("b"))),
      "+",
      Term.QuotedMacroExpr(tname("c"))
    ))))
  )

  test("macro-quote-expr: '{ Int.MinValue }")(
    runTestAssert[Stat]("'{ Int.MinValue }")(Term.QuotedMacroExpr(blk(tselect("Int", "MinValue"))))
  )

  test("macro-quote-expr: '{ (x: T) => 'x }")(runTestAssert[Stat]("'{ (x: T) => 'x }")(
    Term.QuotedMacroExpr(blk(tfunc(tparam("x", "T"))(Term.QuotedMacroExpr(tname("x")))))
  ))

  test("macro-quote-expr: f('{ 2 })")(
    runTestAssert[Stat]("f('{ 2 })")(tapply(tname("f"), Term.QuotedMacroExpr(blk(int(2)))))
  )

  test("macro-quote-expr: if (b) '{ true } else '{ false }")(
    runTestAssert[Stat]("if (b) '{ true } else '{ false }")(
      Term.If(tname("b"), Term.QuotedMacroExpr(blk(bool(true))), Term.QuotedMacroExpr(blk(bool(false))))
    )
  )

  test("macro-quote-expr: '{ val y = a; ${ env } }")(
    runTestAssert[Stat]("'{ val y = a; ${ env } }", assertLayout = None)(Term.QuotedMacroExpr(
      blk(Defn.Val(Nil, List(patvar("y")), None, tname("a")), Term.SplicedMacroExpr(blk(tname("env"))))
    ))
  )

  test("macro-quote-expr: x match { case 'c => 1 }") {
    val layoutMatchSimple = "x match {\n  case 'c => 1\n}"
    runTestAssert[Stat]("x match { case 'c => 1 }", assertLayout = Some(layoutMatchSimple))(
      tmatch(tname("x"), Case(Pat.Macro(Term.QuotedMacroExpr(tname("c"))), None, int(1)))
    )
  }

  test("macro-quote-expr: x match { case '{ a } => 1 }") {
    val layoutMatchComplex = "x match {\n  case '{ a } => 1\n}"
    runTestAssert[Stat]("x match { case '{ a } => 1 }", assertLayout = Some(layoutMatchComplex))(
      Term.Match(tname("x"), List(Case(Pat.Macro(Term.QuotedMacroExpr(blk(tname("a")))), None, int(1))))
    )
  }

  test("macro-quote-expr: x match { case '{ $${bind@'{a}} } => 1 }") {
    val layoutMatchWithSplice = s"x match {\n  case '{ $${ bind @ '{ a } } } => 1\n}"
    runTestAssert[Stat](
      s"x match { case '{ $${bind@'{a}} } => 1 }",
      assertLayout = Some(layoutMatchWithSplice)
    )(tmatch(
      tname("x"),
      Case(
        Pat.Macro(Term.QuotedMacroExpr(blk(
          Term.SplicedMacroPat(Pat.Bind(patvar("bind"), Pat.Macro(Term.QuotedMacroExpr(blk(tname("a"))))))
        ))),
        None,
        int(1)
      )
    ))
  }

  test("macro-brackets")(
    runTestAssert[Stat](
      """|tpr.asType match {
         |  case '[ t ] =>
         |    getTypeTree[t]
         |}""".stripMargin
    )(tmatch(
      tselect("tpr", "asType"),
      Case(
        Pat.Macro(Term.QuotedMacroType(pname("t"))),
        None,
        tapplytype(tname("getTypeTree"), pname("t"))
      )
    ))
  )

  test("macro-quote-multiline") {
    val code =
      """|'{
         |  1 + 3
         |  'x + 3
         |  zzz
         |}""".stripMargin
    runTestAssert[Stat](code)(Term.QuotedMacroExpr(blk(
      tinfix(int(1), "+", int(3)),
      tinfix(Term.QuotedMacroExpr(tname("x")), "+", int(3)),
      tname("zzz")
    )))
  }

  test("macro-quote-type: '[ Map[Int, String] ]")(
    runTestAssert[Stat]("'[ Map[Int, String] ]")(Term.QuotedMacroType(papply("Map", "Int", "String")))
  )

  test("macro-quote-type: '[ List[${ summon[Type[T]] }] ]")(
    runTestAssert[Stat]("'[ List[${ summon[Type[T]] }] ]")(Term.QuotedMacroType(
      papply("List", Type.Macro(Term.SplicedMacroExpr(blk(tapplytype("summon", papply("Type", "T"))))))
    ))
  )

  test("macro-quote-type: '[ Show[$tp] ]")(runTestAssert[Stat]("'[ Show[$tp] ]")(
    Term.QuotedMacroType(papply("Show", Type.Macro(Term.SplicedMacroExpr(tname("tp")))))
  ))

  test("macro-quote-id-by-itself: 'x") {
    val code = " 'x "
    val tree = Term.QuotedMacroExpr(tname("x"))
    runTestAssert[Stat](code)(tree)
    runTestError[Stat](
      "' x",
      """|<input>:1: error: unclosed character literal
         |' x
         |  ^""".stripMargin
    )
    runTestError[Stat](
      "'`x`",
      """|<input>:1: error: unclosed character literal
         |'`x`
         |  ^""".stripMargin
    )
    runTestError[Stat](
      "' `x`",
      """|<input>:1: error: unclosed character literal
         |' `x`
         |  ^""".stripMargin
    )
    runTestAssert[Stat]("`'x`")(tname("'x"))
  }

  test("macro-quote-id-within-quote: '{ 'x }") {
    val code = "'{ 'x }"
    val tree = Term.QuotedMacroExpr(blk(Term.QuotedMacroExpr(tname("x"))))
    runTestAssert[Stat](code)(tree)
    runTestAssert[Stat]("' { 'x }", code)(tree)
    runTestError[Stat](
      "' { ' x }",
      """|<input>:1: error: unclosed character literal
         |' { ' x }
         |      ^""".stripMargin
    )
    runTestError[Stat](
      "'{ '`x` }",
      """|<input>:1: error: unclosed character literal
         |'{ '`x` }
         |     ^""".stripMargin
    )
    runTestError[Stat](
      "'{ ' `x` }",
      """|<input>:1: error: unclosed character literal
         |'{ ' `x` }
         |     ^""".stripMargin
    )
    val backquoted = Term.QuotedMacroExpr(blk(tname("'x")))
    runTestAssert[Stat]("'{ `'x` }")(backquoted)
  }

  test("macro-quote-id-within-splice: ${ 'x }") {
    val code = "${ 'x }"
    val tree = Term.SplicedMacroExpr(blk(Term.QuotedMacroExpr(tname("x"))))
    runTestAssert[Stat](code)(tree)
    runTestAssert[Stat]("$ { 'x }", code)(tree)
    runTestError[Stat](
      "$ { ' x }",
      """|<input>:1: error: unclosed character literal
         |$ { ' x }
         |      ^""".stripMargin
    )
    runTestError[Stat](
      "${ '`x` }",
      """|<input>:1: error: unclosed character literal
         |${ '`x` }
         |     ^""".stripMargin
    )
    runTestError[Stat](
      "${ ' `x` }",
      """|<input>:1: error: unclosed character literal
         |${ ' `x` }
         |     ^""".stripMargin
    )
    val backquoted = Term.SplicedMacroExpr(blk(tname("'x")))
    runTestAssert[Stat]("${ `'x` }")(backquoted)
  }

  test("macro-splice-id-within-quote: '{ $x }") {
    val code = "'{ $x }"
    val tree = Term.QuotedMacroExpr(blk(Term.SplicedMacroExpr(tname("x"))))
    runTestAssert[Stat](code)(tree)
    runTestAssert[Stat]("' { $x }", code)(tree)
    val spaceTree = Term.QuotedMacroExpr(blk(tpostfix("$", "x")))
    runTestAssert[Stat]("' { $ x }", "'{ $ x }")(spaceTree)
    runTestAssert[Stat]("'{ $`x` }", "'{ $ x }")(spaceTree)
    runTestAssert[Stat]("'{ $ `x` }", "'{ $ x }")(spaceTree)
    val backquoted = Term.QuotedMacroExpr(blk(tname("$x")))
    runTestAssert[Stat]("'{ `$x` }")(backquoted)
  }

  test("macro-splice-id-within-splice: ${ $x }") {
    val code = "${ $x }"
    val tree = Term.SplicedMacroExpr(blk(tname("$x")))
    runTestAssert[Stat](code)(tree)
    runTestAssert[Stat]("$ { $x }", code)(tree)
    val spaceTree = Term.SplicedMacroExpr(blk(tpostfix("$", "x")))
    runTestAssert[Stat]("$ { $ x }", "${ $ x }")(spaceTree)
    runTestAssert[Stat]("${ $`x` }", "${ $ x }")(spaceTree)
    runTestAssert[Stat]("${ $ `x` }", "${ $ x }")(spaceTree)
    val backquoted = Term.SplicedMacroExpr(blk(tname("$x")))
    runTestAssert[Stat]("${ `$x` }", code)(backquoted)
  }

  test("macro-splice: ${ powerCode('x) }")(runTestAssert[Stat]("${ powerCode('x) }")(
    Term.SplicedMacroExpr(blk(tapply(tname("powerCode"), Term.QuotedMacroExpr(tname("x")))))
  ))

  test("macro-splice: ${ val x = 'y; println(d); 1 }") {
    val multilineSpliceLayout = "${\n  val x = 'y\n  println(d)\n  1\n}"
    runTestAssert[Stat]("${ val x = 'y; println(d); 1 }", multilineSpliceLayout)(
      Term.SplicedMacroExpr(blk(
        Defn.Val(Nil, List(patvar("x")), None, Term.QuotedMacroExpr(tname("y"))),
        tapply(tname("println"), tname("d")),
        int(1)
      ))
    )
  }

  test("macro-splice: ${ assertImpl('{ x != $y }) }")(
    runTestAssert[Stat]("${ assertImpl('{ x != $y }) }")(Term.SplicedMacroExpr(blk(tapply(
      tname("assertImpl"),
      Term.QuotedMacroExpr(blk(tinfix(tname("x"), "!=", Term.SplicedMacroExpr(tname("y")))))
    ))))
  )

  test("macro-splice-multiline") {
    val code =
      """|${
         |  val x = 'y
         |  println(d)
         |  1
         |}""".stripMargin
    runTestAssert[Stat](code)(Term.SplicedMacroExpr(blk(
      Defn.Val(Nil, List(patvar("x")), None, Term.QuotedMacroExpr(tname("y"))),
      tapply(tname("println"), tname("d")),
      int(1)
    )))
  }

  test("non-macro-dollar-ident") {
    val code = "a.map($d => $d.a)"
    runTestAssert[Stat](code)(tapply(tselect("a", "map"), tfunc(tparam("$d"))(tselect("$d", "a"))))
  }

  test("non-macro-dollar-type") {
    val code = "type $F2 = [$T] => $T => Option[$T]"
    runTestAssert[Stat](code)(Defn.Type(
      Nil,
      pname("$F2"),
      Nil,
      ppolyfunc(pparam("$T"))(pfunc(pname("$T"))(papply("Option", "$T"))),
      noBounds
    ))
  }

  test("macro-quote-complex: 1")(
    runTestAssert[Stat]("'{ ClassTag[T](${ Expr(ct.runtimeClass.asInstanceOf[Class[T]]) }) }")(
      Term.QuotedMacroExpr(blk(tapply(
        tapplytype(tname("ClassTag"), pname("T")),
        Term.SplicedMacroExpr(blk(tapply(
          tname("Expr"),
          tapplytype(tselect("ct", "runtimeClass", "asInstanceOf"), papply("Class", "T"))
        )))
      )))
    )
  )

  test("macro-quote-complex: 2") {
    runTestAssert[Stat]("'{ ${ summon[H].toExpr(tup.head) } *: ${ summon[T].toExpr(tup.tail) } }")(
      Term.QuotedMacroExpr(blk(tinfix(
        Term.SplicedMacroExpr(blk(
          tapply(tselect(tapplytype(tname("summon"), pname("H")), "toExpr"), tselect("tup", "head"))
        )),
        "*:",
        Term.SplicedMacroExpr(blk(
          tapply(tselect(tapplytype(tname("summon"), pname("T")), "toExpr"), tselect("tup", "tail"))
        ))
      )))
    )
  }

  test("simpler")(
    runTestAssert[Stat](
      "'{ val x: Int = ${ (q2) ?=> a } }",
      assertLayout = Some("'{ val x: Int = ${ q2 ?=> a } }")
    )(Term.QuotedMacroExpr(blk(Defn.Val(
      Nil,
      List(patvar("x")),
      Some(pname("Int")),
      Term.SplicedMacroExpr(blk(tctxfunc(tparam("q2"))(tname("a"))))
    ))))
  )

  test("no-name") {
    runTestAssert[Stat]("'{ val x: Int = $ }")(Term.QuotedMacroExpr(blk(
      Defn.Val(Nil, List(patvar("x")), Some(pname("Int")), tname("$"))
    )))
    runTestAssert[Stat]("${ val x: Int = $ }")(Term.SplicedMacroExpr(blk(
      Defn.Val(Nil, List(patvar("x")), Some(pname("Int")), tname("$"))
    )))
  }

  test("#3610 1") {
    val code = "'[ List[Int] ]"
    val tree = Term.QuotedMacroType(papply("List", "Int"))
    runTestAssert[Stat](code)(tree)
  }

  test("#3610 2") {
    val code = "'[ type t = Int; List[t] ]"
    val tree = Term.QuotedMacroType(
      Type.Block(List(Defn.Type(Nil, pname("t"), Nil, pname("Int"), noBounds)), papply("List", "t"))
    )
    runTestAssert[Stat](code)(tree)
  }

  test("#3610 3") {
    val code = "'[ type tail <: Tuple; *:[Int, tail] ]"
    val tree = Term.QuotedMacroType(Type.Block(
      List(Decl.Type(Nil, pname("tail"), Nil, bounds(hi = "Tuple"))),
      papply("*:", "Int", "tail")
    ))
    runTestAssert[Stat](code)(tree)
  }

  test("#6483-metals splice") {
    val code = "${ fn }[Int](0)"
    val codeInParens = "( ${ fn } )[Int](0)"
    val tree = tapply(tapplytype(Term.SplicedMacroExpr(blk(tname("fn"))), pname("Int")), lit(0))
    runTestAssert[Stat](code)(tree)
    runTestAssert[Stat](codeInParens, code)(tree)
  }

  test("#6483-metals splice in braces") {
    val code =
      """|{
         |  ${ fn }
         |}[Int](0)""".stripMargin
    val tree = tapply(tapplytype(blk(Term.SplicedMacroExpr(blk(tname("fn")))), pname("Int")), lit(0))
    runTestAssert[Stat](code)(tree)
  }

  test("#6483-metals poly in parens") {
    val code = "([B] => (c: B) => fn[B](c))[Int](0)"
    val tree = tapply(
      tapplytype(
        tpolyfunc(
          pparam("B")
        )(tfunc(tparam("c", "B"))(tapply(tapplytype(tname("fn"), pname("B")), tname("c")))),
        pname("Int")
      ),
      lit(0)
    )
    runTestAssert[Stat](code)(tree)
  }

  test("#6483-metals poly in braces") {
    val code =
      """|{
         |  [B] => (c: B) => fn[B](c)
         |}[Int](0)
         |""".stripMargin
    val tree = tapply(
      tapplytype(
        blk(tpolyfunc(pparam("B"))(
          tfunc(tparam("c", "B"))(tapply(tapplytype(tname("fn"), pname("B")), tname("c")))
        )),
        pname("Int")
      ),
      lit(0)
    )
    runTestAssert[Stat](code)(tree)
  }

}
