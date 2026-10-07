package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.internal.prettyprinters.TreeSyntax

class CommentSuite extends BaseDottySuite {

  test("class: comment after name, extends on the next line") {
    val code =
      """|class A // c
         |  extends B
         |""".stripMargin
    val layout =
      """|class A // c
         |  extends B
         |""".stripMargin
    val tree = Defn.Class(
      Nil,
      Type.Name.newBuilder("A").endComment(Seq("// c")).result(),
      Type.ParamClause(Nil),
      EmptyCtor(),
      tpl(List(init("B")), Nil),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("if: comment after thenp") {
    val code =
      """|if (a) b // c
         |else d
         |""".stripMargin
    val layout =
      """|if (a) b // c
         |else d
         |""".stripMargin
    val tree = Term.If(tname("a"), tnameComments("b")()("// c"), tname("d"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("try: comment after expr") {
    val code =
      """|try a // c
         |catch { case _ => b }
         |""".stripMargin
    val layout =
      """|try a // c
         |catch {
         |  case _ => b
         |}
         |""".stripMargin
    val tree = Term
      .Try(tnameComments("a")()("// c"), List(Case(Pat.Wildcard(), None, tname("b"))), None)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("try: comment after catch block") {
    val code =
      """|try a
         |catch { case _ => b } // c
         |finally d
         |""".stripMargin
    val layout =
      """|try a catch {
         |  case _ => b
         |} // c
         |finally d
         |""".stripMargin
    val tree = Term.Try(
      tname("a"),
      Some(
        Term.CasesBlock.newBuilder(List(Case(Pat.Wildcard(), None, tname("b"))))
          .endComment(Seq("// c")).result(),
      ),
      Some(tname("d")),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("do: comment after body") {
    val code =
      """|do a // c
         |while (b)
         |""".stripMargin
    val layout =
      """|do a // c
         |while (b)
         |""".stripMargin
    val tree = Term.Do(tnameComments("a")()("// c"), tname("b"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("match: comment after expr") {
    val code =
      """|foo // c
         |  match { case _ => 1 }
         |""".stripMargin
    val layout =
      """|foo // c
         |match {
         |  case _ => 1
         |}
         |""".stripMargin
    val tree = tmatch(tnameComments("foo")()("// c"), Case(Pat.Wildcard(), None, int(1)))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("type infix: comment after op") {
    val code =
      """|type T = A & // c
         |  B
         |""".stripMargin
    val layout =
      """|type T = A & // c
         |  B
         |""".stripMargin
    val tree = Defn.Type(
      Nil,
      pname("T"),
      Nil,
      Type.ApplyInfix(
        pname("A"),
        Type.Name.newBuilder("&").endComment(Seq("// c")).result(),
        pname("B"),
      ),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("val: comment after =") {
    val code =
      """|val x = // c
         |  1
         |""".stripMargin
    val layout =
      """|val x = // c
         |  1
         |""".stripMargin
    val tree = Defn
      .Val(Nil, List(patvar("x")), None, Lit.Int.newBuilder(1).begComment(Seq("// c")).result())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("throw: comment after keyword") {
    val code =
      """|def f = throw // c
         |  e
         |""".stripMargin
    val layout =
      """|def f = throw // c
         |  e
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, Nil, None, Term.Throw(tnameComments("e")("// c")()))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("function: comment after arrow") {
    val code =
      """|(x: Int) => // c
         |  x
         |""".stripMargin
    val layout =
      """|(x: Int) => // c
         |  x
         |""".stripMargin
    val tree = tfunc(tparam("x", "Int"))(tnameComments("x")("// c")())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("case: comment after arrow, one-liner") {
    val code =
      """|x match {
         |  case 1 => // c
         |    2
         |}
         |""".stripMargin
    val layout =
      """|x match {
         |  case 1 => // c
         |    2
         |}
         |""".stripMargin
    val tree =
      tmatch(tname("x"), Case(int(1), None, Lit.Int.newBuilder(2).begComment(Seq("// c")).result()))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("return: comment after keyword, expr on the next line") {
    val code =
      """|def f = return // c
         |  1
         |""".stripMargin
    val layout =
      """|def f = return // c
         |  1
         |""".stripMargin
    val body = Term.Return(Lit.Int.newBuilder(1).begComment(Seq("// c")).result())
    val tree = Defn.Def(Nil, tname("f"), Nil, Nil, None, body)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("return: comment after keyword, expr on the same line") {
    val code = "def f = return /* c */ 1"
    val layout =
      """|def f = return /* c */ 1
         |""".stripMargin
    val body = Term.Return(Lit.Int.newBuilder(1).begComment(Seq("/* c */")).result())
    val tree = Defn.Def(Nil, tname("f"), Nil, Nil, None, body)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("if: block comment after cond, body on the same line") {
    val code = "if (a) /* c */ b"
    val layout =
      """|if (a) /* c */ b
         |""".stripMargin
    val tree = Term.If(tname("a"), tnameComments("b")("/* c */")(), Lit.Unit())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("while: block comment after cond, body on the same line") {
    val code = "while (a) /* c */ b"
    val layout =
      """|while (a) /* c */ b
         |""".stripMargin
    val tree = Term.While(tname("a"), tnameComments("b")("/* c */")())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("if: comment after cond, body on the next line") {
    val code =
      """|if (a) // c
         |  b
         |else d
         |""".stripMargin
    val layout =
      """|if (a) // c
         |  b else d
         |""".stripMargin
    val tree = Term.If(tname("a"), tnameComments("b")("// c")(), tname("d"))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("while: comment after cond, body on the next line") {
    val code =
      """|while (a) // c
         |  b
         |""".stripMargin
    val layout =
      """|while (a) // c
         |  b
         |""".stripMargin
    val tree = Term.While(tname("a"), tnameComments("b")("// c")())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("line comment at end of input") {
    val x = term("x // X")
    assertEquals(x.endComment.get.values.last.newlinesAfter, 1)
    assertSyntax("func(x // X\n)")(Term.Apply(tname("func"), Term.ArgClause(List(x))))
  }

  test("block comment at end of input") {
    val x = term("x /* X */")
    assertEquals(x.endComment.get.values.last.newlinesAfter, 1)
    assertSyntax("func(x /* X */\n)")(Term.Apply(tname("func"), Term.ArgClause(List(x))))
  }

  test("line comment at start of input") {
    val x = term("// X\nx")
    assertEquals(x.begComment.get.newlinesBefore, 0)
    assertSyntax("val y = // X\n  x")(Defn.Val(Nil, List(patvar("y")), None, x))
  }

  test("block comment at start of input") {
    val x = term("/* X */ x")
    assertEquals(x.begComment.get.newlinesBefore, 0)
    assertSyntax("val y = /* X */ x")(Defn.Val(Nil, List(patvar("y")), None, x))
  }

  test("infix: comment on its own line after op") {
    val code =
      """|a op
         |  // foo
         |  b
         |""".stripMargin
    val layout =
      """|a op
         |  // foo
         |  b
         |""".stripMargin
    val tree = Term.ApplyInfix(
      tname("a"),
      tname("op"),
      Type.ArgClause(Nil),
      Term.ArgClause.newBuilder(List(tname("b"))).begComment(detachedComments("// foo")).result(),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix: block comment on its own line after op") {
    val code =
      """|a op
         |  /* foo */ b
         |""".stripMargin
    val layout =
      """|a op
         |  /* foo */ b
         |""".stripMargin
    val tree = Term.ApplyInfix(
      tname("a"),
      tname("op"),
      Type.ArgClause(Nil),
      Term.ArgClause.newBuilder(List(tname("b"))).begComment(detachedComments("/* foo */")).result(),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("infix: comment inside parens after op") {
    val code =
      """|a op (
         |  // foo
         |  b
         |)
         |""".stripMargin
    val layout =
      """|a op
         |  // foo
         |  b
         |""".stripMargin
    val tree = Term.ApplyInfix(tname("a"), tname("op"), Nil, List(tnameComments("b")("// foo")()))
    parseAndCheckTree[Stat](code, layout)(tree)
    val reparsed = Term.ApplyInfix(
      tname("a"),
      tname("op"),
      Type.ArgClause(Nil),
      Term.ArgClause.newBuilder(List(tname("b"))).begComment(detachedComments("// foo")).result(),
    )
    runTestAssert[Stat](layout)(reparsed)
  }

  test("comment before a blank line at the start of input") {
    val code =
      """|// c
         |
         |x
         |""".stripMargin
    val tree = tname("x")
    runTestAssert[Stat](code, "x")(tree)
  }

  test("comment before a brace body") {
    val code =
      """|def f = // c
         |  {
         |    x
         |  }
         |""".stripMargin
    val layout =
      """|def f = {
         |  x
         |}
         |""".stripMargin
    val tree = Defn.Def(Nil, tname("f"), Nil, None, blk(tname("x")))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("comment inside a parameter clause") {
    val code =
      """|def f(
         |  // c
         |  x: Int
         |) = 1
         |""".stripMargin
    val layout = "def f(x: Int) = 1"
    val tree = Defn.Def(
      Nil,
      tname("f"),
      List(Member.ParamClauseGroup(
        Type.ParamClause(Nil),
        List(List(tparam("x", "Int").toBuilder.begComment(Seq("// c")).result())),
      )),
      None,
      int(1),
    )
    parseAndCheckTree[Stat](code, layout)(tree)
    val reparsed = Defn.Def(Nil, tname("f"), Nil, List(List(tparam("x", pname("Int")))), None, int(1))
    runTestAssert[Stat](layout)(reparsed)
  }

  test("braceless def body: DSLC at the body's indentation, definition follows") {
    val code =
      """|def f =
         |  x
         |  // c
         |def g = 1
         |""".stripMargin
    val layout =
      """|def f = {
         |  x
         |  // c
         |}
         |
         |// c
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(Nil, tname("f"), Nil, Nil, None, blk(tnameComments("x")()("// c"))),
      Defn.Def.newBuilder(Nil, tname("g"), Nil, None, int(1)).begComment(Seq("// c")).result(),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon template body: DSLC at the body's indentation, definition follows") {
    val code =
      """|object O:
         |  x
         |  // c
         |object P
         |""".stripMargin
    val layout =
      """|object O {
         |  x
         |  // c
         |}
         |
         |// c
         |object P
         |""".stripMargin
    val tree = Source(List(
      Defn.Object(Nil, tname("O"), tpl(List(tnameComments("x")()("// c")))),
      Defn.Object.newBuilder(Nil, tname("P"), tplNoBody()).begComment(Seq("// c")).result(),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon template body: DSLC at the outer indentation, definition follows") {
    val code =
      """|object O:
         |  x
         |// c
         |object P
         |""".stripMargin
    val layout =
      """|object O { x }
         |
         |// c
         |object P
         |""".stripMargin
    val tree = Source(List(
      Defn.Object(Nil, tname("O"), tpl(List(tname("x")))),
      Defn.Object.newBuilder(Nil, tname("P"), tplNoBody()).begComment(Seq("// c")).result(),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon template body: DSLC at the body's indentation, blank line, definition") {
    val code =
      """|object O:
         |  x
         |  // c
         |
         |object P
         |""".stripMargin
    val layout =
      """|object O {
         |  x
         |  // c
         |
         |}
         |object P
         |""".stripMargin
    val tree = Source(List(
      Defn.Object(Nil, tname("O"), tpl(List(tnameComments("x")()("// c")))),
      Defn.Object(Nil, tname("P"), tplNoBody()),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("import: ASLC, then a DSLC at the end of input") {
    val code =
      """|import p3.a3 // ct3
         |// ct4
         |""".stripMargin
    val src = source(code)
    val imp = src.stats.head.asInstanceOf[Import]
    val importer = imp.importers.head
    assertEquals(imp.endComment.get.values.map(_.syntax), List("// ct3", "// ct4"))
    assertEquals(importer.endComment.get.values.map(_.syntax), List("// ct3"))
    assert(importer.endComment.get ne imp.endComment.get)
    assertNotEquals(importer.endComment.get.values.head, imp.endComment.get.values.head)
    assertEquals(imp.endComment.get.values.head.parent, Some(imp.endComment.get))
    assertSyntax(
      """|import p3.a3 // ct3
         |  // ct3
         |  // ct4
         |""".stripMargin,
    )(Source(src.stats))
  }

  test("block: ASLC on a select, then a DSLC before the closing brace") {
    val code =
      """|{
         |  a.b // c1
         |  // c2
         |}
         |""".stripMargin
    val block = stat(code).asInstanceOf[Term.Block]
    val select = block.stats.head.asInstanceOf[Term.Select]
    assertEquals(select.endComment.get.values.map(_.syntax), List("// c1", "// c2"))
    assertEquals(select.name.endComment.get.values.map(_.syntax), List("// c1"))
    assert(select.name.endComment.get ne select.endComment.get)
    assertNotEquals(select.name.endComment.get.values.head, select.endComment.get.values.head)
    assertSyntax(
      """|{
         |  a.b // c1
         |    // c1
         |    // c2
         |}
         |""".stripMargin,
    )(Term.Block(block.stats))
  }

  test("block: DSLC before an infix statement") {
    val code =
      """|{
         |  // c
         |  a + b
         |}
         |""".stripMargin
    val block = stat(code).asInstanceOf[Term.Block]
    val infix = block.stats.head.asInstanceOf[Term.ApplyInfix]
    assertEquals(infix.begComment.get.values.map(_.syntax), List("// c"))
    assertEquals(infix.lhs.begComment.get, infix.begComment.get)
  }

  test("block: MLC then the infix statement on its line") {
    val code =
      """|{
         |  /* c */ a + b
         |}
         |""".stripMargin
    val block = stat(code).asInstanceOf[Term.Block]
    val infix = block.stats.head.asInstanceOf[Term.ApplyInfix]
    assertEquals(infix.begComment.get.values.map(_.syntax), List("/* c */"))
    assertEquals(infix.lhs.begComment.get, infix.begComment.get)
  }

  test("block: DSLC after a statement, blank line, statement") {
    val code =
      """|{
         |  x
         |  // c
         |
         |  y
         |}
         |""".stripMargin
    val layout =
      """|{
         |  x
         |  y
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tname("x"), tname("y")))
  }

  test("block: DSLCs between two statements, separated by blank lines") {
    val code =
      """|{
         |  s1
         |  // c1
         |
         |  // c2
         |
         |  // c3
         |  s2
         |}
         |""".stripMargin
    val layout =
      """|{
         |  s1
         |  // c3
         |  s2
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tname("s1"), tnameComments("s2")("// c3")()))
  }

  test("source: DSLC, blank line, first statement") {
    val code =
      """|// c
         |
         |import a.b
         |""".stripMargin
    val layout =
      """|import a.b
         |""".stripMargin
    runTestAssert[Source](code, layout)(Source(List(Import(List(Importer("a", List("b")))))))
  }

  test("source: last statement, blank line, DSLC") {
    val code =
      """|import a.b
         |
         |// c
         |""".stripMargin
    val layout =
      """|import a.b
         |""".stripMargin
    runTestAssert[Source](code, layout)(Source(List(Import(List(Importer("a", List("b")))))))
  }

  test("block: DSLC, blank line, first statement") {
    val code =
      """|{
         |  // c
         |
         |  x
         |}
         |""".stripMargin
    val layout =
      """|{
         |  x
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tname("x")))
  }

  test("block: last statement, blank line, DSLC") {
    val code =
      """|{
         |  x
         |
         |  // c
         |}
         |""".stripMargin
    val layout =
      """|{
         |  x
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tname("x")))
  }

  test("empty template: ASLC after the brace") {
    val code =
      """|class A { // c
         |}
         |""".stripMargin
    val layout =
      """|class A
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tplNoBody()))
  }

  test("empty block: ASLC after the brace") {
    val code =
      """|val x = { // c
         |}
         |""".stripMargin
    val layout = "val x = {}"
    val body = blk()
    runTestAssert[Stat](code, layout)(Defn.Val(Nil, List(Pat.Var(tname("x"))), None, body))
  }

  test("empty package body: ASLC after the brace") {
    val code =
      """|package p { // c
         |}
         |""".stripMargin
    val layout = "package p"
    val body = Pkg.Body(Nil)
    runTestAssert[Source](code, layout)(Source(List(Pkg(tname("p"), body))))
  }

  test("package with braces: DSLC before the brace") {
    val code =
      """|package p
         |// c
         |{
         |  object O
         |}
         |""".stripMargin
    val layout =
      """|package p
         |object O
         |""".stripMargin
    val obj = Defn.Object(Nil, tname("O"), tplNoBody())
    val body = Pkg.Body.newBuilder(List(obj)).begComment(Seq("// c")).result()
    parseAndCheckTree[Source](code, layout)(Source(List(Pkg(tname("p"), body))))
  }

  test("def: ASLC after =, brace body on the next line") {
    val code =
      """|def f = // c
         |  {
         |    x
         |  }
         |""".stripMargin
    val layout =
      """|def f = {
         |  x
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("x"))))
  }

  test("block: ASLC after a semicolon") {
    val code =
      """|{
         |  x; // c
         |}
         |""".stripMargin
    val layout =
      """|{
         |  x
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tname("x")))
  }

  test("block: semicolon, then a DSLC") {
    val code =
      """|{
         |  x;
         |  // c
         |}
         |""".stripMargin
    val layout =
      """|{
         |  x
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tname("x")))
  }

  test("args: trailing comma, then a DSLC before the paren") {
    val code =
      """|f(
         |  a,
         |  b,
         |  // c
         |)
         |""".stripMargin
    val layout =
      """|f(a, b)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tapply(tname("f"), tname("a"), tname("b")))
  }

  test("args: last arg, then a DSLC before the paren") {
    val code =
      """|f(
         |  a,
         |  b
         |  // c
         |)
         |""".stripMargin
    val layout =
      """|f(a, b)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tapply(tname("f"), tname("a"), tname("b")))
  }

  test("args: last arg, blank line, DSLC before the paren") {
    val code =
      """|f(
         |  a,
         |  b
         |
         |  // c
         |)
         |""".stripMargin
    val layout =
      """|f(a, b)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tapply(tname("f"), tname("a"), tname("b")))
  }

  test("args: DSLC, blank line, first arg") {
    val code =
      """|f(
         |  // c
         |
         |  a,
         |  b
         |)
         |""".stripMargin
    val layout =
      """|f(a, b)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tapply(tname("f"), tname("a"), tname("b")))
  }

  test("args: DSLC after an arg, blank line, arg") {
    val code =
      """|f(
         |  a,
         |  // c
         |
         |  b
         |)
         |""".stripMargin
    val layout =
      """|f(a, b)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tapply(tname("f"), tname("a"), tname("b")))
  }

  test("tuple: last element, then a DSLC before the paren") {
    val code =
      """|(
         |  a,
         |  b
         |  // c
         |)
         |""".stripMargin
    val layout =
      """|(a, b)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Term.Tuple(List(tname("a"), tname("b"))))
  }

  test("tuple: last element, blank line, DSLC before the paren") {
    val code =
      """|(
         |  a,
         |  b
         |
         |  // c
         |)
         |""".stripMargin
    val layout =
      """|(a, b)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Term.Tuple(List(tname("a"), tname("b"))))
  }

  test("tuple: DSLC, blank line, first element") {
    val code =
      """|(
         |  // c
         |
         |  a,
         |  b
         |)
         |""".stripMargin
    val layout =
      """|(a, b)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Term.Tuple(List(tname("a"), tname("b"))))
  }

  test("params: last param, then a DSLC before the paren") {
    val code =
      """|def f(
         |  a: Int,
         |  b: Int
         |  // c
         |) = a
         |""".stripMargin
    val layout =
      """|def f(a: Int, b: Int) = a
         |""".stripMargin
    val params = List(tparam("a", "Int"), tparam("b", "Int"))
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, List(params), None, tname("a")))
  }

  test("params: last param, blank line, DSLC before the paren") {
    val code =
      """|def f(
         |  a: Int,
         |  b: Int
         |
         |  // c
         |) = a
         |""".stripMargin
    val layout =
      """|def f(a: Int, b: Int) = a
         |""".stripMargin
    val params = List(tparam("a", "Int"), tparam("b", "Int"))
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, List(params), None, tname("a")))
  }

  test("params: DSLC, blank line, first param") {
    val code =
      """|def f(
         |  // c
         |
         |  a: Int,
         |  b: Int
         |) = a
         |""".stripMargin
    val layout =
      """|def f(a: Int, b: Int) = a
         |""".stripMargin
    val params = List(tparam("a", "Int"), tparam("b", "Int"))
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, List(params), None, tname("a")))
  }

  test("source: import, DSLC, blank line, object") {
    val code =
      """|import y.Y
         |// c
         |
         |object O
         |""".stripMargin
    val layout =
      """|import y.Y
         |object O
         |""".stripMargin
    val tree =
      Source(List(Import(List(Importer("y", List("Y")))), Defn.Object(Nil, tname("O"), tplNoBody())))
    runTestAssert[Source](code, layout)(tree)
  }

  test("package: DSLC between blank lines before the first statement") {
    val code =
      """|package p
         |
         |// c
         |
         |object O
         |""".stripMargin
    val layout =
      """|package p
         |object O
         |""".stripMargin
    val tree = Source(List(Pkg(tname("p"), Pkg.Body(List(Defn.Object(Nil, tname("O"), tplNoBody()))))))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon body: DSLC after a statement, blank line, statement") {
    val code =
      """|object O:
         |  x
         |  // c
         |
         |  y
         |""".stripMargin
    val layout =
      """|object O {
         |  x
         |  y
         |}
         |""".stripMargin
    runTestAssert[Source](code, layout)(Source(
      List(Defn.Object(Nil, tname("O"), tpl(List(tname("x"), tname("y"))))),
    ))
  }

  test("colon body: last statement, blank line, DSLC, outdent") {
    val code =
      """|object O:
         |  x
         |
         |  // c
         |object P
         |""".stripMargin
    val layout =
      """|object O { x }
         |
         |// c
         |object P
         |""".stripMargin
    val tree = Source(List(
      Defn.Object(Nil, tname("O"), tpl(List(tname("x")))),
      Defn.Object.newBuilder(Nil, tname("P"), tplNoBody()).begComment(detachedComments("// c"))
        .result(),
    ))
    runTestAssert[Source](code, layout)(tree)
  }

  test("colon body: DSLC, blank line, first statement") {
    val code =
      """|object O:
         |  // c
         |
         |  x
         |""".stripMargin
    val layout =
      """|object O { x }
         |""".stripMargin
    runTestAssert[Source](code, layout)(Source(List(Defn.Object(Nil, tname("O"), tpl(List(tname("x")))))))
  }

  test("template: last statement, blank line, DSLC") {
    val code =
      """|class A {
         |  x
         |
         |  // c
         |}
         |""".stripMargin
    val layout =
      """|class A { x }
         |""".stripMargin
    runTestAssert[Stat](code, layout)(
      Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tpl(List(tname("x")))),
    )
  }

  test("template: DSLC, blank line, first statement") {
    val code =
      """|class A {
         |  // c
         |
         |  x
         |}
         |""".stripMargin
    val layout =
      """|class A { x }
         |""".stripMargin
    runTestAssert[Stat](code, layout)(
      Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tpl(List(tname("x")))),
    )
  }

  test("package with braces: last statement, blank line, DSLC") {
    val code =
      """|package p {
         |  object O
         |
         |  // c
         |}
         |""".stripMargin
    val layout =
      """|package p
         |object O
         |""".stripMargin
    val tree = Source(List(Pkg(tname("p"), Pkg.Body(List(Defn.Object(Nil, tname("O"), tplNoBody()))))))
    runTestAssert[Source](code, layout)(tree)
  }

  test("braceless package: last statement, blank line, DSLC") {
    val code =
      """|package p
         |object O
         |
         |// c
         |""".stripMargin
    val layout =
      """|package p
         |object O
         |  // c
         |""".stripMargin
    val templ = tplNoBody().toBuilder.begComment(detachedComments("// c")).result()
    val obj = Defn.Object(Nil, tname("O"), templ)
    parseAndCheckTree[Source](code, layout)(Source(List(Pkg(tname("p"), Pkg.Body(List(obj))))))
  }

  test("block: ASLC after a semicolon, statement follows") {
    val code =
      """|{
         |  x; // c
         |  y
         |}
         |""".stripMargin
    val layout =
      """|{
         |  x
         |  // c
         |  y
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tname("x"), tnameComments("y")("// c")()))
  }

  test("block: semicolon, MLC, statement on the same line") {
    val code =
      """|{
         |  x; /* c */ y
         |}
         |""".stripMargin
    val layout =
      """|{
         |  x
         |  /* c */ y
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tname("x"), tnameComments("y")("/* c */")()))
  }

  test("source: a DSLC only") {
    val code =
      """|// c
         |""".stripMargin
    val layout =
      """|// c
         |""".stripMargin
    runTestAssert[Source](code, layout)(Source.newBuilder(Nil).begComment(Seq("// c")).result())
  }

  test("args: MLC between an arg and the comma") {
    val code = "f(a /* c */, b)"
    runTestAssert[Stat](code)(tapply(tname("f"), tnameComments("a")()("/* c */"), tname("b")))
  }

  test("params: MLC between a param and the comma") {
    val code = "def f(a: Int /* c */, b: Int) = a"
    val a = tparam("a", "Int").toBuilder.endComment(Seq("/* c */")).result()
    val params = List(a, tparam("b", "Int"))
    val layout = "def f(a: Int, b: Int) = a"
    parseAndCheckTree[Stat](code, layout)(
      Defn.Def(Nil, tname("f"), Nil, List(params), None, tname("a")),
    )
  }

  test("empty refinement: ASLC after the brace") {
    val code =
      """|type T = A { // c
         |}
         |""".stripMargin
    val layout = "type T = A {}"
    val body = Stat.Block(Nil)
    runTestAssert[Stat](code, layout)(
      Defn.Type(Nil, pname("T"), Nil, Type.Refine(Some(pname("A")), body)),
    )
  }

  test("template: self, DSLC, blank line, first statement") {
    val code =
      """|class A { self =>
         |  // c
         |
         |  x
         |}
         |""".stripMargin
    val layout = "class A { self => x }"
    val x = tname("x")
    val tree = Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tpl(Nil, self(tname("self")), x))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("built self with an end comment, printed and parsed again") {
    val commentedSelf = Self.newBuilder(tname("self"), None).endComment(Seq("// c")).result()
    val tree = Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tpl(Nil, commentedSelf, tname("x")))
    val printed = tree.reprint
    assertNoDiff(
      printed,
      """|class A { self => // c
         |  x }
         |""".stripMargin,
    )
    val reparsed = Defn.Class(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tpl(Nil, self(tname("self")), tnameComments("x")("// c")()),
    )
    assertEquals(templStat(printed).structure, reparsed.structure)
  }

  test("secondary ctor: statement, DSLC, blank line, statement") {
    val code =
      """|class A {
         |  def this() = {
         |    this()
         |    // c
         |
         |    x
         |  }
         |}
         |""".stripMargin
    val ctor = templStat(code).collect { case t: Ctor.Block => t }.head
    assertEquals(ctor.stats.head.begComment.map(_.values.map(_.syntax)), None)
  }

  test("braceless def body: DSLC, blank line, first statement") {
    val code =
      """|def f =
         |  // c
         |
         |  x
         |  y
         |""".stripMargin
    val layout =
      """|def f = {
         |  x
         |  y
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, blk(tname("x"), tname("y"))))
  }

  test("empty block: MLC inside the braces") {
    val code = "val x = { /* c */ }"
    val layout = "val x = {}"
    val body = blk()
    runTestAssert[Stat](code, layout)(Defn.Val(Nil, List(Pat.Var(tname("x"))), None, body))
  }

  test("empty template: DSLC inside the braces") {
    val code =
      """|class A {
         |  // c
         |}
         |""".stripMargin
    val layout = "class A"
    runTestAssert[Stat](code, layout)(Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tplNoBody()))
  }

  test("empty block: DSLC inside the braces") {
    val code =
      """|val x = {
         |  // c
         |}
         |""".stripMargin
    val layout = "val x = {}"
    runTestAssert[Stat](code, layout)(Defn.Val(Nil, List(Pat.Var(tname("x"))), None, blk()))
  }

  test("empty package body: DSLC inside the braces") {
    val code =
      """|package p {
         |  // c
         |}
         |""".stripMargin
    val layout = "package p"
    runTestAssert[Source](code, layout)(Source(List(Pkg(tname("p"), Pkg.Body(Nil)))))
  }

  test("block: statement, blank line, DSLC, statement") {
    val code =
      """|{
         |  x
         |
         |  // c
         |  y
         |}
         |""".stripMargin
    val layout =
      """|{
         |  x
         |  // c
         |  y
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tname("x"), tnameComments("y")("// c")()))
  }

  test("type args: ASLC after a comma") {
    val code =
      """|f[
         |  A, // c
         |  B
         |]
         |""".stripMargin
    val layout =
      """|f[A // c
         |, B]
         |""".stripMargin
    val a = Type.Name.newBuilder("A").endComment(Seq("// c")).result()
    runTestAssert[Stat](code, layout)(Term.ApplyType(tname("f"), List(a, pname("B"))))
  }

  test("type params: ASLC after a comma") {
    val code =
      """|class C[
         |  A, // c
         |  B
         |]
         |""".stripMargin
    val layout =
      """|class C[A // c
         |, B]
         |""".stripMargin
    val a = pparam("A").toBuilder.endComment(Seq("// c")).result()
    runTestAssert[Stat](code, layout)(
      Defn.Class(Nil, pname("C"), List(a, pparam("B")), EmptyCtor(), tplNoBody()),
    )
  }

  test("for: ASLC after an enumerator") {
    val code =
      """|for {
         |  a <- b // c
         |  d <- e
         |} yield a
         |""".stripMargin
    val layout =
      """|for (a <- b // c
         |; d <- e) yield a
         |""".stripMargin
    val enums = List(
      Enumerator.Generator.newBuilder(patvar("a"), tname("b")).endComment(Seq("// c")).result(),
      Enumerator.Generator(patvar("d"), tname("e")),
    )
    runTestAssert[Stat](code, layout)(Term.ForYield(enums, tname("a")))
  }

  test("import: ASLC after an importer's comma") {
    val code =
      """|import a.b, // c
         |  d.e
         |""".stripMargin
    val layout =
      """|import a.b // c
         |, d.e
         |""".stripMargin
    val ab = Importer("a", List("b")).toBuilder.endComment(Seq("// c")).result()
    runTestAssert[Stat](code, layout)(Import(List(ab, Importer("d", List("e")))))
  }

  test("export: ASLC after a selector's comma, DSLC, next selector") {
    val code =
      """|object O {
         |  export a.{
         |    b, // c1
         |    // c2
         |    c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  export a.{
         |    b // c1
         |,    // c2
         |    c
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("end Importee.Name, Name.Indeterminate: // c1"),
    )("end Importee.Name, Name.Indeterminate: // c1", "beg Importee.Name, Name.Indeterminate: // c2")
  }

  test("enum case params: DSLC indented after a comma, next param") {
    val code =
      """|enum E {
         |  case C(
         |    a: A,
         |      // c
         |    b: B
         |  )
         |}
         |""".stripMargin
    checkComments(
      code,
      """|enum E { case C(a: A, b: B) }
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Term.Param, Term.Name: // c")
  }

  test("block: last statement, then an MLC and a SLC on one line") {
    val code =
      """|{
         |  x
         |  /* c */ // d
         |}
         |""".stripMargin
    val layout =
      """|{
         |  x
         |  /* c */
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tnameComments("x")()("/* c */")))
  }

  test("block: semicolon, then an MLC before the brace") {
    val code = "{ x; /* c */ }"
    val layout =
      """|{
         |  x
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tname("x")))
  }

  test("select chain: DSLC before the dot") {
    val code =
      """|x.foo(a)
         |  // c
         |  .bar(b)
         |""".stripMargin
    val layout = "x.foo(a).bar(b)"
    runTestAssert[Stat](code, layout)(
      tapply(tselect(tapply(tselect("x", "foo"), tname("a")), "bar"), tname("b")),
    )
  }

  test("if: DSLC before else") {
    val code =
      """|if (a) b
         |// c
         |else d
         |""".stripMargin
    val layout = "if (a) b else d"
    runTestAssert[Stat](code, layout)(Term.If(tname("a"), tname("b"), tname("d")))
  }

  test("class: ASLC after extends, DSLC before with") {
    val code =
      """|class A extends B // c1
         |  // c2
         |  with C
         |""".stripMargin
    val layout =
      """|class A extends B // c1
         | with C
         |""".stripMargin
    val b = Type.Name.newBuilder("B").endComment(Seq("// c1")).result()
    val anon = Name.Anonymous.newBuilder().begComment(detachedComments("// c2")).result()
    parseAndCheckTree[Stat](code, layout)(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      EmptyCtor(),
      tplNoBody(Init(b, anon, List.empty[Term.ArgClause]), init("C")),
    ))
  }

  test("class: MLC after extends") {
    val code =
      """|class A extends /* c1 */ B
         |""".stripMargin
    checkComments(
      code,
      """|class A /* c1 */ extends B
         |""".stripMargin,
      reprinted = Seq("end Type.Name: /* c1 */"),
    )("beg Template, Init, Type.Name: /* c1 */")
  }

  test("class: DSLC before extends") {
    val code =
      """|class A
         |  // c1
         |  extends B
         |""".stripMargin
    checkComments(
      code,
      """|class A
         |// c1
         |// c1
         |  // c1
         |  extends B
         |""".stripMargin,
      reprinted = Seq(
        "beg Type.ParamClause: // c1\n// c1\n  // c1",
        "beg Ctor.Primary, Name.Anonymous: // c1\n// c1\n  // c1",
        "beg Template: // c1\n// c1\n  // c1",
      ),
    )(
      "beg Type.ParamClause: // c1",
      "beg Ctor.Primary, Name.Anonymous: // c1",
      "beg Template: // c1",
    )
  }

  test("secondary ctor: DSLC, blank line, init") {
    val code =
      """|class A {
         |  def this() = {
         |    // c
         |
         |    this()
         |  }
         |}
         |""".stripMargin
    val ctor = templStat(code).collect { case t: Ctor.Block => t }.head
    assertEquals(ctor.endComment, None)
  }

  test("try: DSLC before catch") {
    val code =
      """|try x
         |// c
         |catch { case _ => y }
         |""".stripMargin
    val layout =
      """|try x catch {
         |  case _ => y
         |}
         |""".stripMargin
    val tree = Term.Try(tname("x"), List(Case(Pat.Wildcard(), None, tname("y"))), None)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("try: DSLC before finally") {
    val code =
      """|try x
         |// c
         |finally y
         |""".stripMargin
    val layout = "try x finally y"
    runTestAssert[Stat](code, layout)(Term.Try(tname("x"), Nil, Some(tname("y"))))
  }

  test("braceless try: DSLC at the outer indentation, catch") {
    val code =
      """|def f =
         |  try
         |    x
         |  // c
         |  catch
         |    case _ => y
         |""".stripMargin
    val layout =
      """|def f = try x catch {
         |  case _ => y
         |}
         |""".stripMargin
    val body = Term.Try(tname("x"), List(Case(Pat.Wildcard(), None, tname("y"))), None)
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, body))
  }

  test("braceless try: DSLC at the indentation of the try body, catch") {
    val code =
      """|def f =
         |  try
         |    x
         |    // c
         |  catch
         |    case _ => y
         |""".stripMargin
    val layout =
      """|def f = try {
         |  x
         |  // c
         |} catch {
         |  case _ => y
         |}
         |""".stripMargin
    val body = Term
      .Try(blk(tnameComments("x")()("// c")), List(Case(Pat.Wildcard(), None, tname("y"))), None)
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, body))
  }

  test("braceless if: DSLC after the outdent, else") {
    val code =
      """|def f =
         |  if a then
         |    b
         |// c
         |  else d
         |""".stripMargin
    val layout = "def f = if (a) b else d"
    val body = Term.If(tname("a"), tname("b"), tname("d"))
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, body))
  }

  test("case body: DSLC, blank line, first statement") {
    val code =
      """|x match {
         |  case 1 =>
         |    // c
         |
         |    a
         |    b
         |}
         |""".stripMargin
    val layout =
      """|x match {
         |  case 1 =>
         |    a
         |    b
         |}
         |""".stripMargin
    val tree = Term.Match(tname("x"), List(Case(int(1), None, blk(tname("a"), tname("b")))), Nil)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("braceless if: DSLC before else, block body") {
    val code =
      """|def f =
         |  if a then b
         |  // c
         |  else
         |    d
         |    e
         |""".stripMargin
    val layout =
      """|def f = if (a) b else {
         |  d
         |  e
         |}
         |""".stripMargin
    val body = Term.If(tname("a"), tname("b"), blk(tname("d"), tname("e")))
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, body))
  }

  test("braceless if: DSLC before else, statement on its own line") {
    val code =
      """|def f =
         |  if a then
         |    b
         |  // c
         |  else
         |    d
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a) b else d
         |""".stripMargin,
    )()
  }

  test("braceless if: DSLC, MLC before else on its line, statement on its own line") {
    val code =
      """|def f =
         |  if a then
         |    b
         |  // c1
         |  /* c2 */ else
         |    d
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a) b else d
         |""".stripMargin,
    )()
  }

  test("braceless if: DSLC under the then branch, else, statement on its own line") {
    val code =
      """|def f =
         |  if a then
         |    b
         |    // c
         |  else
         |    d
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a) {
         |  b
         |  // c
         |} else d
         |""".stripMargin,
    )("end Term.Name: // c")
  }

  test("braceless if: DSLC, blank line, else, statement on its own line") {
    val code =
      """|def f =
         |  if a then
         |    b
         |  // c
         |
         |  else
         |    d
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a) b else d
         |""".stripMargin,
    )()
  }

  test("braceless try: DSLC before finally, statement on its own line") {
    val code =
      """|def f =
         |  try
         |    b
         |  // c
         |  finally
         |    d
         |""".stripMargin
    checkComments(
      code,
      """|def f = try b finally d
         |""".stripMargin,
    )()
  }

  test("braceless for: DSLC before yield, statement on its own line") {
    val code =
      """|def f =
         |  for
         |    x <- xs
         |  // c
         |  yield
         |    f(x)
         |""".stripMargin
    checkComments(
      code,
      """|def f = for (x <- xs) yield f(x)
         |""".stripMargin,
    )()
  }

  test("for in parens: DSLC before yield, statement on its own line") {
    val code =
      """|def f =
         |  for (x <- xs)
         |  // c
         |  yield
         |    f(x)
         |""".stripMargin
    checkComments(
      code,
      """|def f = for (x <- xs) yield f(x)
         |""".stripMargin,
    )()
  }

  test("braceless if: DSLC before else, two statements on their own lines") {
    val code =
      """|def f =
         |  if a then
         |    b
         |  // c
         |  else
         |    d
         |    e
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a) b else {
         |  d
         |  e
         |}
         |""".stripMargin,
    )()
  }

  test("braceless if: DSLC before else, DSLC, two statements on their own lines") {
    val code =
      """|def f =
         |  if a then
         |    b
         |  // c1
         |  else
         |    // c2
         |    d
         |    e
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a) b else {
         |  // c2
         |  d
         |  e
         |}
         |""".stripMargin,
    )("beg Term.Name: // c2")
  }

  test("braceless try: DSLC before catch on its own line") {
    val code =
      """|def f =
         |  try b
         |  // c
         |  catch
         |    case _ => c
         |""".stripMargin
    checkComments(
      code,
      """|def f = try b catch {
         |  case _ => c
         |}
         |""".stripMargin,
    )()
  }

  test("braceless try: DSLC before catch on its own line, DSLC before the case") {
    val code =
      """|def f =
         |  try b
         |  // c1
         |  catch
         |    // c2
         |    case _ => c
         |""".stripMargin
    checkComments(
      code,
      """|def f = try b catch {
         |  // c2
         |  case _ => c
         |}
         |""".stripMargin,
    )("beg Case: // c2")
  }

  test("braceless match: DSLC before match on its own line") {
    val code =
      """|def f =
         |  x
         |  // c
         |  match
         |    case _ => c
         |""".stripMargin
    checkComments(
      code,
      """|def f = x match {
         |  case _ => c
         |}
         |""".stripMargin,
    )()
  }

  test("braceless match: DSLC before match on its own line, DSLC before the case") {
    val code =
      """|def f =
         |  x
         |  // c1
         |  match
         |    // c2
         |    case _ => c
         |""".stripMargin
    checkComments(
      code,
      """|def f = x match {
         |  // c2
         |  case _ => c
         |}
         |""".stripMargin,
    )("beg Case: // c2")
  }

  test("braceless if: DSLC before then, statement on the same line") {
    val code =
      """|def f =
         |  if a
         |  // c
         |  then b
         |  else d
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a) b else d
         |""".stripMargin,
    )()
  }

  test("braceless if: DSLC before then, statement on its own line") {
    val code =
      """|def f =
         |  if a
         |  // c
         |  then
         |    b
         |  else d
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a) b else d
         |""".stripMargin,
    )()
  }

  test("braceless while: DSLC before do, statement on the same line") {
    val code =
      """|def f =
         |  while a
         |  // c
         |  do b
         |""".stripMargin
    checkComments(
      code,
      """|def f = while (a) b
         |""".stripMargin,
    )()
  }

  test("braceless while: DSLC before do, statement on its own line") {
    val code =
      """|def f =
         |  while a
         |  // c
         |  do
         |    b
         |""".stripMargin
    checkComments(
      code,
      """|def f = while (a) b
         |""".stripMargin,
    )()
  }

  test("braceless for: DSLC before do, statement on the same line") {
    val code =
      """|def f =
         |  for x <- xs
         |  // c
         |  do f(x)
         |""".stripMargin
    checkComments(
      code,
      """|def f = for (x <- xs) f(x)
         |""".stripMargin,
    )()
  }

  test("braceless match: DSLC before the guard") {
    val code =
      """|def f =
         |  x match
         |    case y
         |    // c
         |    if y > 0 => 1
         |""".stripMargin
    checkComments(
      code,
      """|def f = x match {
         |  case y if y > 0 => 1
         |}
         |""".stripMargin,
    )()
  }

  test("braceless match: DSLC before .match") {
    val code =
      """|def f =
         |  x
         |    // c
         |    .match
         |      case _ => 1
         |""".stripMargin
    checkComments(
      code,
      """|def f = x.match {
         |  case _ => 1
         |}
         |""".stripMargin,
    )()
  }

  test("braceless if: DSLC before then, DSLC, statement on its own line") {
    val code =
      """|def f =
         |  if a
         |  // c1
         |  then
         |    // c2
         |    b
         |  else d
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a)
         |  // c2
         |  b else d
         |""".stripMargin,
    )("beg Term.Name: // c2")
  }

  test("braceless while: DSLC before do, DSLC, statement on its own line") {
    val code =
      """|def f =
         |  while a
         |  // c1
         |  do
         |    // c2
         |    b
         |""".stripMargin
    checkComments(
      code,
      """|def f = while (a)
         |  // c2
         |  b
         |""".stripMargin,
    )("beg Term.Name: // c2")
  }

  test("braceless match: DSLC before .match, DSLC before the case") {
    val code =
      """|def f =
         |  x
         |    // c1
         |    .match
         |      // c2
         |      case _ => 1
         |""".stripMargin
    checkComments(
      code,
      """|def f = x.match {
         |  // c2
         |  case _ => 1
         |}
         |""".stripMargin,
    )("beg Case: // c2")
  }

  test("braceless if: MLC before else on its line") {
    val code =
      """|def f =
         |  if a then b
         |  /* c */ else d
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a) b else d
         |""".stripMargin,
    )()
  }

  test("braceless if: MLC before then on its line") {
    val code =
      """|def f =
         |  if a
         |  /* c */ then b
         |  else d
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (a) b else d
         |""".stripMargin,
    )()
  }

  test("braceless try: MLC before catch on its line") {
    val code =
      """|def f =
         |  try b
         |  /* c */ catch
         |    case _ => c
         |""".stripMargin
    checkComments(
      code,
      """|def f = try b catch {
         |  case _ => c
         |}
         |""".stripMargin,
    )()
  }

  test("braceless try: MLC before finally on its line") {
    val code =
      """|def f =
         |  try b
         |  /* c */ finally d
         |""".stripMargin
    checkComments(
      code,
      """|def f = try b finally d
         |""".stripMargin,
    )()
  }

  test("braceless for: MLC before yield on its line") {
    val code =
      """|def f =
         |  for x <- xs
         |  /* c */ yield f(x)
         |""".stripMargin
    checkComments(
      code,
      """|def f = for (x <- xs) yield f(x)
         |""".stripMargin,
    )()
  }

  test("braceless match: MLC before match on its line") {
    val code =
      """|def f =
         |  x
         |  /* c */ match
         |    case _ => 1
         |""".stripMargin
    checkComments(
      code,
      """|def f = x match {
         |  case _ => 1
         |}
         |""".stripMargin,
    )()
  }

  test("braceless if: DSLC before else, ASLC after else") {
    val code =
      """|def f =
         |  if foo then
         |    bar
         |  // c2
         |  else // c3
         |    baz
         |""".stripMargin
    checkComments(
      code,
      """|def f = if (foo) bar else // c3
         |  baz
         |""".stripMargin,
    )("beg Term.Name: // c3")
  }

  test("braceless class: modifier, DSLC on the next line, definition") {
    val code =
      """|class A:
         |  inline
         |  // c1
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|class A { inline def f = 1 }
         |""".stripMargin,
    )()
  }

  test("colon argument: braces at both ends, ASLC after the last") {
    val code =
      """|def f =
         |  foo:
         |    { bar }.baz { qux } // c
         |""".stripMargin
    val layout =
      """|def f = foo {
         |  {
         |    bar
         |  }.baz {
         |    qux
         |  } // c
         |}
         |""".stripMargin
    val stat = tapply(tselect(blk(tname("bar")), "baz"), blk(tname("qux"))).toBuilder
      .endComment(Seq("// c")).result()
    runTestAssert[Stat](code, layout)(
      Defn.Def(Nil, tname("f"), Nil, None, tapply(tname("foo"), blk(stat))),
    )
  }

  test("colon argument: braces at both ends, DSLC after the last") {
    val code =
      """|def f =
         |  foo:
         |    { bar }.baz { qux }
         |    // c
         |""".stripMargin
    val layout =
      """|def f = foo {
         |  {
         |    bar
         |  }.baz {
         |    qux
         |  }
         |  // c
         |}
         |""".stripMargin
    val stat = tapply(tselect(blk(tname("bar")), "baz"), blk(tname("qux"))).toBuilder
      .endComment(detachedComments("// c")).result()
    runTestAssert[Stat](code, layout)(
      Defn.Def(Nil, tname("f"), Nil, None, tapply(tname("foo"), blk(stat))),
    )
  }

  test("match type: DSLC before the first case") {
    val code =
      """|type T[X] = X match
         |  // c1
         |  case Int => A
         |""".stripMargin
    checkComments(
      code,
      """|type T[X] = X match {
         |  // c1
         |  case Int => A
         |}
         |""".stripMargin,
    )("beg TypeCase: // c1")
  }

  test("braceless for: DSLC before the first enumerator") {
    val code =
      """|def f =
         |  for
         |    // c1
         |    x <- xs
         |  do g(x)
         |""".stripMargin
    checkComments(
      code,
      """|def f = for (
         |// c1
         |x <- xs) g(x)
         |""".stripMargin,
    )("beg Enumerator.Generator, Pat.Var, Term.Name: // c1")
  }

  test("braceless secondary ctor: DSLC before the self call") {
    val code =
      """|class A:
         |  def this(x: Int) =
         |    // c1
         |    this()
         |    f
         |""".stripMargin
    checkComments(
      code,
      """|class A {
         |  def this(x: Int) = {
         |    // c1
         |    this()
         |    f
         |  }
         |}
         |""".stripMargin,
    )("beg Init, Type.Singleton, Term.This, Name.Anonymous: // c1")
  }

  test("given with: DSLC before the first statement") {
    val code =
      """|given A with
         |  // c1
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|given A with {
         |  // c1
         |  def f = 1
         |}
         |""".stripMargin,
    )("beg Defn.Def: // c1")
  }

  test("refinement with: DSLC before the first declaration") {
    val code =
      """|type T = A with
         |  // c1
         |  def f: Int
         |""".stripMargin
    checkComments(
      code,
      """|type T = A {
         |  // c1
         |  def f: Int
         |}
         |""".stripMargin,
    )("beg Decl.Def: // c1")
  }

  test("braceless match: empty case body, ASLC, next case") {
    val code =
      """|def f =
         |  x match
         |    case 1 => // c1
         |    case 2 => b
         |""".stripMargin
    checkComments(
      code,
      """|def f = x match {
         |  case 1 => // c1
         |  // c1
         |  case 2 => b
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Block: // c1", "beg Case: // c1\n  // c1"),
    )("end Case: // c1", "beg Case: // c1")
  }

  test("braceless match: empty case body, DSLC indented under it, next case") {
    val code =
      """|def f =
         |  x match
         |    case 1 =>
         |      // c1
         |    case 2 => b
         |""".stripMargin
    checkComments(
      code,
      """|def f = x match {
         |  case 1 =>
         |    // c1
         |    {}
         |  // c1
         |  case 2 => b
         |}
         |""".stripMargin,
    )("beg Term.Block: // c1", "beg Case: // c1")
  }

  test("braceless match: case body, DSLC indented under it, next case") {
    val code =
      """|def f =
         |  foo match
         |    case a =>
         |      x
         |      // a1
         |    case b => y
         |""".stripMargin
    checkComments(
      code,
      """|def f = foo match {
         |  case a =>
         |    x
         |    // a1
         |  // a1
         |  case b =>
         |    y
         |}
         |""".stripMargin,
      reprinted = Seq("beg Case: // a1\n  // a1"),
    )("end Term.Name: // a1", "beg Case: // a1")
  }

  test("braceless class: def body, DSLC indented under it, next def") {
    val code =
      """|class A:
         |  def f =
         |    x
         |    // c
         |  def g = 1
         |""".stripMargin
    checkComments(
      code,
      """|class A {
         |  def f = {
         |    x
         |    // c
         |  }
         |  // c
         |  def g = 1
         |}
         |""".stripMargin,
    )("end Term.Name: // c", "beg Defn.Def: // c")
  }

  test("match: case body in braces, DSLC before and after it, next case") {
    val code =
      """|def f =
         |  foo match {
         |    case a =>
         |      // c1
         |      foo
         |      // c2
         |    case b =>
         |  }
         |""".stripMargin
    checkComments(
      code,
      """|def f = foo match {
         |  case a =>
         |    // c1
         |    foo
         |  // c2
         |  case b =>
         |}
         |""".stripMargin,
    )("beg Term.Name: // c1", "beg Case: // c2")
  }

  test("braceless match: two-statement case body, DSLC indented under it, next case") {
    val code =
      """|def f =
         |  x match
         |    case 1 =>
         |      a
         |      b
         |      // c1
         |    case 2 => c
         |""".stripMargin
    checkComments(
      code,
      """|def f = x match {
         |  case 1 =>
         |    a
         |    b
         |    // c1
         |  // c1
         |  case 2 =>
         |    c
         |}
         |""".stripMargin,
      reprinted = Seq("beg Case: // c1\n  // c1"),
    )("end Term.Name: // c1", "beg Case: // c1")
  }

  test("params: DSLC before using") {
    val code =
      """|def f(a: Int)(
         |    // c1
         |    using b: B,
         |) = 1
         |""".stripMargin
    checkComments(
      code,
      """|def f(a: Int)(
         |// c1
         |using b: B) = 1
         |""".stripMargin,
    )("beg Term.Param, Mod.Using, Mod.Using: // c1")
  }

  test("params: MLC before using") {
    val code =
      """|def f(a: Int)(/* c1 */ using b: B) = 1
         |""".stripMargin
    checkComments(
      code,
      """|def f(a: Int)(/* c1 */ using b: B) = 1
         |""".stripMargin,
    )("beg Term.Param, Mod.Using, Mod.Using: /* c1 */")
  }

  test("given: MLC before the name") {
    val code =
      """|object O {
         |  given /* c1 */ x: Int = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given x: Int = 1 }
         |""".stripMargin,
    )()
  }

  test("derives: MLC before the class") {
    val code =
      """|case class A(x: Int) derives /* c1 */ Eq
         |""".stripMargin
    checkComments(
      code,
      """|case class A(x: Int) derives Eq
         |""".stripMargin,
    )()
  }

  test("derives: DSLC before derives") {
    val code =
      """|class A extends B
         |  // c1
         |  derives C
         |""".stripMargin
    checkComments(
      code,
      """|class A extends B derives C
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Name.Anonymous: // c1")
  }

  test("derives: ASLC before derives") {
    val code =
      """|class A extends B // c1
         |  derives C
         |""".stripMargin
    checkComments(code)("end Init, Type.Name: // c1")
  }

  test("derives: DSLC before derives, no extends") {
    val code =
      """|class A
         |  // c1
         |  derives C
         |""".stripMargin
    checkComments(
      code,
      """|class A
         |// c1
         |// c1
         |  // c1
         |  derives C
         |""".stripMargin,
      reprinted = Seq(
        "beg Type.ParamClause: // c1\n// c1\n  // c1",
        "beg Ctor.Primary, Name.Anonymous: // c1\n// c1\n  // c1",
        "beg Template: // c1\n// c1\n  // c1",
      ),
    )(
      "beg Type.ParamClause: // c1",
      "beg Ctor.Primary, Name.Anonymous: // c1",
      "beg Template: // c1",
    )
  }

  test("given with: DSLC before with") {
    val code =
      """|given intOrd: Ord[Int]
         |  // c1
         |  with {
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|given intOrd: Ord[Int] with { def f = 1 }
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Name.Anonymous: // c1")
  }

  test("given with: ASLC before with") {
    val code =
      """|given intOrd: Ord[Int] // c1
         |  with {
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|given intOrd: Ord[Int] // c1
         |  with { def f = 1 }
         |""".stripMargin,
    )("end Init, Type.Apply, Type.ArgClause: // c1")
  }

  test("given with: MLC after with") {
    val code =
      """|given intOrd: Ord[Int] with /* c1 */ {
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|given intOrd: Ord[Int] with /* c1 */ { def f = 1 }
         |""".stripMargin,
    )("beg Template.Body: /* c1 */")
  }

  test("given with: MLC before an empty body") {
    val code =
      """|given intOrd: Ord[Int] with /* c1 */ {}
         |""".stripMargin
    checkComments(
      code,
      """|given intOrd: Ord[Int] with /* c1 */
         |""".stripMargin,
      reprintError = "<input>:1: error: `identifier` expected but `end of file` found",
    )("beg Template.Body: /* c1 */")
  }

  test("given with: DSLC before with, empty body") {
    val code =
      """|given intOrd: Ord[Int]
         |  // c1
         |  with {}
         |""".stripMargin
    checkComments(
      code,
      """|given intOrd: Ord[Int] with {}
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Name.Anonymous: // c1")
  }

  test("given: parameter clause, MLC, arrow") {
    val code =
      """|object O {
         |  given f: (x: Int) /* c1 */ => Foo = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given f: (x: Int) => Foo = ??? }
         |""".stripMargin,
      reprinted = Seq(),
    )("end Term.ParamClause: /* c1 */")
  }

  test("import: given, MLC, type") {
    val code =
      """|object O {
         |  import a.{given /* c1 */ Int}
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { import a.given Int }
         |""".stripMargin,
    )()
  }

  test("match: given pattern, MLC, type") {
    val code =
      """|object O {
         |  x match {
         |    case given /* c1 */ Int => 1
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case given Int => 1
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("braceless trait: only a DSLC, end marker") {
    val code =
      """|trait T extends B:
         |  // c
         |end T
         |""".stripMargin
    checkComments(
      code,
      """|trait T extends B
         |
         |// c
         |end T
         |""".stripMargin,
    )("beg Term.EndMarker: // c")
  }

  test("braceless cases: DSLC before the first case") {
    val code =
      """|object A:
         |  val p: Receive =
         |    // c
         |    case 1 => a
         |    case 2 => b
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val p: Receive = {
         |    // c
         |    case 1 => a
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("beg Case: // c")
  }

  test("braceless try: DSLC after the body, catch") {
    val code =
      """|object A:
         |  def f =
         |    try
         |      a
         |    // c
         |    catch case _: E => ()
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = try a catch {
         |    case _: E => ()
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("extension: ASLC after the clause, DSLC, def") {
    val code =
      """|object A:
         |  extension (x: Int) // c1
         |    // c2
         |    def double = x * 2
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  extension (x: Int) // c1
         |    {
         |      // c1
         |      // c2
         |      def double = x * 2
         |    }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Defn.Def: // c1\n      // c2"),
    )("end Member.ParamClauseGroup, Term.ParamClause: // c1", "beg Defn.Def: // c1\n    // c2")
  }

  test("extension: DSLC, one def, printed without comments") {
    val code =
      """|object A:
         |  extension (x: Int)
         |    // c
         |    def double = x * 2
         |""".stripMargin
    val printed = TreeSyntax.reprint(source(code), comments = false).toString
    assertNoDiff(
      printed,
      """|object A {
         |  extension (x: Int) {
         |    def double = x * 2
         |  }
         |}
         |""".stripMargin,
    )
    val bodies = source(printed).collect { case t: Defn.ExtensionGroup => t.body.productPrefix }
    assertEquals(bodies, List("Term.Block"))
  }

  test("braceless try: DSLC, body, finally") {
    val code =
      """|object A:
         |  def f: Unit =
         |    try
         |      // c
         |      g
         |    finally h
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f: Unit = try
         |    // c
         |    g finally h
         |}
         |""".stripMargin,
      reprintError = "<input>:4: error: `outdent` expected but `finally` found",
    )("beg Term.Name: // c")
  }

  test("inline match: ASLC after the equals") {
    val code =
      """|object A:
         |  inline def f = // c
         |    inline x match
         |      case 1 => a
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  inline def f = // c
         |    inline x match {
         |      case 1 => a
         |    }
         |}
         |""".stripMargin,
    )("beg Term.Match, Mod.Inline: // c")
  }

  test("inline match: DSLC before it") {
    val code =
      """|object A:
         |  def f =
         |    g
         |    // c
         |    inline x match
         |      case 1 => a
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = {
         |    g
         |    // c
         |    inline x match {
         |      case 1 => a
         |    }
         |  }
         |}
         |""".stripMargin,
    )("beg Term.Match, Mod.Inline: // c")
  }

  test("inline match: inline at the end of the line, DSLC, scrutinee") {
    val code =
      """|object A:
         |  def f =
         |    inline
         |    // c
         |    x match
         |      case 1 => a
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f =
         |    // c
         |    inline x match {
         |      case 1 => a
         |    }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Match, Mod.Inline: // c"),
    )("beg Term.Match, Term.Name: // c")
  }

  test("if: DSLC before the thenp, else if, ASLC after its condition") {
    val code =
      """|object A {
         |  def f = {
         |    if (a)
         |      // t
         |      x
         |    else if (b) // c
         |      y
         |    else z
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = {
         |    if (a)
         |      // t
         |      x else if (b) // c
         |      y else z
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Name: // t"),
    )("beg Term.Name: // t", "beg Term.Name: // c")
  }

  test("if: ASLC after the condition, else if, DSLC before its thenp") {
    val code =
      """|object A {
         |  def f = {
         |    if (a) // c
         |      x
         |    else if (b)
         |      // t
         |      y
         |    else z
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = {
         |    if (a) // c
         |      x else if (b)
         |      // t
         |      y else z
         |  }
         |}
         |""".stripMargin,
    )("beg Term.Name: // c", "beg Term.Name: // t")
  }

  test("if: else if mid-line, ASLC after its condition, thenp at the column of the line") {
    val code =
      """|object A {
         |  if (a)
         |    f() else if (b) // c
         |    g()
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { if (a) f() else if (b) g() }
         |""".stripMargin,
    )()
  }

  test("braceless def body: DSLC under the body, DSLC at the def's indentation") {
    val code =
      """|object A:
         |  def f =
         |    a
         |    // c1
         |  // c2
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = {
         |    a
         |    // c1
         |  }
         |  // c2
         |}
         |""".stripMargin,
    )("end Defn.Def: // c2", "end Term.Name: // c1")
  }

  test("braceless nested class: DSLC at the outer body's indentation") {
    val code =
      """|class A:
         |  class B:
         |    def f = 1
         |  // c2
         |""".stripMargin
    checkComments(
      code,
      """|class A {
         |  class B { def f = 1 }
         |  // c2
         |}
         |""".stripMargin,
    )("end Defn.Class: // c2")
  }

  test("lambda in braces: DSLC between the params and the arrow") {
    val code =
      """|object A:
         |  xs.map { (x: Int)
         |    // c1
         |    =>
         |      b
         |  }
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  xs.map {
         |    (x: Int) => b
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("infix: MLC after a parenthesized operand") {
    val code =
      """|object A {
         |  val x = (a + b) /* c1 */ * c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val x = (a + b) * c }
         |""".stripMargin,
    )()
  }

  test("if: MLC between the condition and a brace body") {
    val code =
      """|object A {
         |  if (x) /* c1 */ { a }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  if (x) {
         |    a
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("apply: MLC after a parenthesized argument") {
    val code =
      """|object A {
         |  f((a + b) /* c1 */)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { f(a + b) }
         |""".stripMargin,
    )()
  }

  test("val: MLC after a parenthesized type") {
    val code =
      """|object A {
         |  val x: (Int) /* c1 */ = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val x: Int = 1 }
         |""".stripMargin,
    )()
  }

  test("case: MLC after a parenthesized guard") {
    val code =
      """|object A {
         |  x match {
         |    case a if (a > 0) /* c1 */ => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case a if a > 0 => b
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("while: MLC between the condition and a brace body") {
    val code =
      """|object A {
         |  while (x) /* c1 */ { a }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  while (x) {
         |    a
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("if: MLC between the condition and a brace body, else") {
    val code =
      """|object A {
         |  if (x) /* c1 */ { a } else b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  if (x) {
         |    a
         |  } else b
         |}
         |""".stripMargin,
    )()
  }

  test("if: MLC between the condition and then") {
    val code =
      """|object A {
         |  if (x) /* c1 */ then a else b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { if (x) a else b }
         |""".stripMargin,
    )()
  }

  test("if: ASLC after the condition, then on the next line") {
    val code =
      """|object A:
         |  if (x) // c1
         |  then a
         |""".stripMargin
    checkComments(
      code,
      """|object A { if (x) a }
         |""".stripMargin,
    )()
  }

  test("while: MLC between the condition and do") {
    val code =
      """|object A {
         |  while (x) /* c1 */ do a
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { while (x) a }
         |""".stripMargin,
    )()
  }

  test("quoted type pattern: type definition, type, DSLC before the bracket") {
    val code =
      """|object A:
         |  def f(x: Type[?]) = x match
         |    case '[
         |      type t
         |      List[t]
         |      // c
         |    ] => 1
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f(x: Type[?]) = x match {
         |    case '[ type t; List[t] ] => 1
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("quoted type: type, DSLC before the bracket") {
    val code =
      """|object A {
         |  val t = '[
         |    List[Int]
         |    // c
         |  ]
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val t = '[ List[Int] ] }
         |""".stripMargin,
    )()
  }

  test("import: MLC after as") {
    val code =
      """|import a.{d as /* c1 */ e}
         |""".stripMargin
    checkComments(
      code,
      """|import a.d as e
         |""".stripMargin,
    )()
  }

  test("import: MLC after as, no braces") {
    val code =
      """|import a.d as /* c1 */ e
         |""".stripMargin
    checkComments(
      code,
      """|import a.d as e
         |""".stripMargin,
    )()
  }

  test("import: MLC after as, wildcard") {
    val code =
      """|import a.{d as /* c1 */ _}
         |""".stripMargin
    checkComments(
      code,
      """|import a.d as _
         |""".stripMargin,
    )()
  }

  test("import: MLC after the arrow") {
    val code =
      """|import a.{d => /* c1 */ e}
         |""".stripMargin
    checkComments(
      code,
      """|import a.d as /* c1 */ e
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Name.Indeterminate: /* c1 */")
  }

  test("export: MLC after as") {
    val code =
      """|object A {
         |  export a.{d as /* c1 */ e}
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { export a.d as e }
         |""".stripMargin,
    )()
  }

  test("extension: MLC after the keyword") {
    val code =
      """|object A:
         |  extension /* c1 */ (x: Int) def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|object A { extension (x: Int) def f = 1 }
         |""".stripMargin,
    )()
  }

  test("super: MLC before the qualifier") {
    val code =
      """|object A {
         |  super /* c1 */ [A].f
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { super[A].f }
         |""".stripMargin,
    )()
  }

  test("for: MLC after the generator, do, MLC") {
    val code =
      """|object A:
         |  for x <- xs /* c1 */ do /* c2 */ a
         |""".stripMargin
    checkComments(
      code,
      """|object A { for (x <- xs /* c1 */) /* c2 */ a }
         |""".stripMargin,
      reprinted =
        Seq("end Term.EnumeratorsBlock: /* c2 */", "end Enumerator.Generator, Term.Name: /* c1 */"),
    )("end Enumerator.Generator, Term.Name: /* c1 */", "beg Term.Name: /* c2 */")
  }

  test("case body: DSLC under a one-stat body, the tree after the reprint") {
    val code =
      """|object A:
         |  x match
         |    case 1 =>
         |      a
         |      // c1
         |    case 2 => b
         |""".stripMargin
    def bodies(tree: Tree) = tree.collect { case c: Case => c.body.productPrefix }
    val parsed = source(code)
    assertEquals(bodies(parsed), List("Term.Block", "Term.Name"))
    assertEquals(bodies(source(TreeSyntax.reprint(parsed).toString)), List("Term.Name", "Term.Name"))
  }

  test("case body: DSLC at the body's indentation, DSLC at the case's indentation") {
    val code =
      """|object A:
         |  x match
         |    case 1 =>
         |      a
         |      // c1
         |    // c2
         |    case 2 => b
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 =>
         |      a
         |      // c1
         |    // c1
         |    // c2
         |    case 2 =>
         |      b
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Case: // c1\n    // c1\n    // c2"),
    )("end Term.Name: // c1", "beg Case: // c1\n    // c2")
  }

  test("if: DSLC before else on its own line, parsed without comments") {
    val code =
      """|object A:
         |  if x then a
         |  // c1
         |  else
         |    b
         |""".stripMargin
    val options = scala.meta.parsers.ParserOptions.default.withCaptureComments(false)
    val tree = implicitly[scala.meta.parsers.Parse[Source]]
      .apply(Input.String(code))(dialect, options).get
    val elsep = tree.collect { case t: Term.If => t.elsep.productPrefix }
    assertEquals(elsep, List("Term.Name"))
  }

  test("then, do, yield, finally, =, =>: DSLC before it on its own line, parsed without comments") {
    val options = scala.meta.parsers.ParserOptions.default.withCaptureComments(false)
    def body(code: String)(f: PartialFunction[Tree, Tree]) =
      implicitly[scala.meta.parsers.Parse[Source]].apply(Input.String(code))(dialect, options).get
        .collect(f).map(_.productPrefix).mkString(",")
    val bodies = List(
      body(
        """|object A:
           |  if x
           |  // c1
           |  then
           |    a
           |""".stripMargin,
      ) { case t: Term.If => t.thenp },
      body(
        """|object A:
           |  while x
           |  // c1
           |  do
           |    a
           |""".stripMargin,
      ) { case t: Term.While => t.body },
      body(
        """|object A:
           |  for x <- xs
           |  // c1
           |  yield
           |    a
           |""".stripMargin,
      ) { case t: Term.ForYield => t.body },
      body(
        """|object A:
           |  try a
           |  // c1
           |  finally
           |    b
           |""".stripMargin,
      ) { case t: Term.Try => t.finallyp.get },
      body(
        """|object A:
           |  def f: Int
           |  // c1
           |  =
           |    a
           |""".stripMargin,
      ) { case t: Defn.Def => t.body },
      body(
        """|object A:
           |  xs.map: x
           |  // c1
           |  =>
           |    a
           |""".stripMargin,
      ) { case t: Term.Function => t.body },
    )
    assertEquals(bodies, List.fill(6)("Term.Name"))
  }

  test("extension: clause, DSLC indented under it, def") {
    val code =
      """|extension (x: Int)
         |  // c1
         |  def f = x
         |""".stripMargin
    checkComments(
      code,
      """|extension (x: Int) {
         |  // c1
         |  def f = x
         |}
         |""".stripMargin,
    )("beg Defn.Def: // c1")
  }

  test("braceless match: last case, body on its own line, DSLC at the case's indentation") {
    val code =
      """|object O:
         |  x match
         |    case 1 =>
         |      b
         |    // c1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 1 => b
         |    // c1
         |  }
         |}
         |""".stripMargin,
    )("end Case: // c1")
  }

  test("braceless try: ASLC after try, body, catch") {
    val code =
      """|object O:
         |  try // c1
         |    a
         |  catch
         |    case _ => b
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  try // c1
         |    a catch {
         |    case _ => b
         |  }
         |}
         |""".stripMargin,
      reprintError = "<input>:3: error: `outdent` expected but `catch` found",
    )("beg Term.Name: // c1")
  }

  test("if then: nested if, ASLC after then, else") {
    val code =
      """|object O:
         |  if a then
         |    if b then // c1
         |      c
         |    else d
         |  else e
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  if (a) if (b) // c1
         |    c else d else e
         |}
         |""".stripMargin,
      reprintError = "<input>:3: error: `outdent` expected but `else` found",
    )("beg Term.Name: // c1")
  }

  test("if then: nested while, ASLC after do, else") {
    val code =
      """|object O:
         |  if a then
         |    while b do // c1
         |      c
         |  else e
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  if (a) while (b) // c1
         |    c else e
         |}
         |""".stripMargin,
      reprintError = "<input>:3: error: `outdent` expected but `else` found",
    )("beg Term.Name: // c1")
  }

  test("while: DSLC, blank line, do") {
    val code =
      """|object O:
         |  while a
         |  // c1
         |
         |  do b
         |""".stripMargin
    checkComments(
      code,
      """|object O { while (a) b }
         |""".stripMargin,
    )()
  }

  test("end marker: MLC after end") {
    val code =
      """|object O:
         |  def f =
         |    1
         |  end /* c1 */ f
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f = 1
         |  end f
         |}
         |""".stripMargin,
    )()
  }

  test("dependent function type: param, MLC, colon") {
    val code =
      """|object O:
         |  type F = (x /* c1 */ : Int) => x.type
         |""".stripMargin
    checkComments(
      code,
      """|object O { type F = (x: Int) => x.type }
         |""".stripMargin,
      reprinted = Seq(),
    )("end Type.Name: /* c1 */")
  }

  test("case class: param, DSLC, colon") {
    val code =
      """|case class C(
         |  x
         |  // c1
         |  : Int)
         |""".stripMargin
    checkComments(
      code,
      """|case class C(x: Int)
         |""".stripMargin,
    )()
  }

  test("quote: MLC after the quote") {
    val code =
      """|object O:
         |  def f(using Quotes) = ' /* c1 */ x
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f(using Quotes) = '/* c1 */ x }
         |""".stripMargin,
      reprintError = "<input>:1: error: Macro quote must be followed by brace or bracket",
    )("beg Term.Name: /* c1 */")
  }

  test("annotation: DSLC, blank line, modifier, def") {
    val code =
      """|object O:
         |  @deprecated
         |  // c1
         |
         |  final def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O { @deprecated final def f = 1 }
         |""".stripMargin,
    )()
  }

  test("annotation: ASLC, DSLC, def") {
    val code =
      """|object O:
         |  @deprecated // c1
         |  // c2
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  @deprecated // c1
         |    def f = 1
         |}
         |""".stripMargin,
    )("end Mod.Annot, Init, Type.Name: // c1")
  }

  test("source: line comment at the start") {
    val code =
      """|// c1
         |trait A
         |""".stripMargin
    val first = source(code).reprint
    val second = source(first).reprint
    val expected =
      """|// c1
         |trait A""".stripMargin
    assertEquals((first, second), (expected, expected))
  }

  test("object: procedure syntax, blank line, MLC, call") {
    val code =
      """|object A {
         |  def f() {}
         |
         |  /* c1 */ f()
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f(): Unit = {}
         |    /* c1 */ f (())
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Apply, Term.Name: /* c1 */"),
    )("beg Term.Name: /* c1 */")
  }

  test("object: procedure syntax, MLC, call") {
    val code =
      """|object A {
         |  def f() {}
         |  /* c1 */ f()
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f(): Unit = {}
         |    /* c1 */ f (())
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Apply, Term.Name: /* c1 */"),
    )("beg Term.Name: /* c1 */")
  }

  test("object: procedure syntax, blank line, DSLC, call") {
    val code =
      """|object A {
         |  def f() {}
         |
         |  // c1
         |  f()
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f(): Unit = {}
         |    // c1
         |    f (())
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Apply, Term.Name: // c1"),
    )("beg Term.Name: // c1")
  }

  test("object: procedure syntax, blank line, MLC, symbolic call") {
    val code =
      """|object A {
         |  def f() {}
         |
         |  /* c1 */ + 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f(): Unit = {}
         |    /* c1 */ + 1
         |}
         |""".stripMargin,
    )("beg Term.Name: /* c1 */")
  }

  test("object: block, blank line, MLC, call") {
    val code =
      """|object A {
         |  val x = {}
         |
         |  /* c1 */ f()
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val x = {}
         |  /* c1 */ f()
         |}
         |""".stripMargin,
    )("beg Term.Apply, Term.Name: /* c1 */")
  }

  test("if then: DSLC before else, colon lambda body, printed without comments") {
    val code =
      """|object A:
         |  def f =
         |    if a then
         |      b
         |    // c1
         |    else
         |      c.so:
         |        d
         |""".stripMargin
    val printed = scala.meta.internal.prettyprinters.TreeSyntax
      .reprint(source(code), comments = false)
    val uncommented = code.replace("    // c1\n", "")
    assertNoDiff(
      printed.toString,
      """|object A {
         |  def f = if (a) b else c.so {
         |    d
         |  }
         |}
         |""".stripMargin,
    )
    assertNoDiff(
      scala.meta.internal.prettyprinters.TreeSyntax.reprint(source(uncommented), comments = false)
        .toString,
      """|object A {
         |  def f = if (a) b else c.so {
         |    d
         |  }
         |}
         |""".stripMargin,
    )
  }

  test("if then: MLC after each parenthesized group of the condition") {
    val code =
      """|object A:
         |  if (a op b) /* c1 */ (c op d) /* c2 */ (e op f) /* c3 */ then g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { if ((a op b) /* c1 */ (c op d) /* c2 */ (e op f) /* c3 */ ) g }
         |""".stripMargin,
      reprinted = Seq(
        "end Term.Apply, Term.ArgClause: /* c3 */",
        "end Term.Apply, Term.ArgClause: /* c2 */",
        "end Term.ApplyInfix: /* c1 */",
      ),
    )(
      "end Term.Apply, Term.ArgClause: /* c3 */",
      "end Term.Apply, Term.ArgClause, Term.ApplyInfix: /* c2 */",
      "beg Term.ArgClause, Term.ApplyInfix: /* c1 */",
    ).reprint
    assertNoDiff(
      source(first).reprint,
      "object A { if ((a op b /* c1 */ )(c op d) /* c2 */ (e op f) /* c3 */ ) g }",
    )
  }

  test("while do: MLC after each parenthesized group of the condition") {
    val code =
      """|object A:
         |  while (a op b) /* c1 */ (c op d) /* c2 */ (e op f) /* c3 */ do g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { while ((a op b) /* c1 */ (c op d) /* c2 */ (e op f) /* c3 */ ) g }
         |""".stripMargin,
      reprinted = Seq(
        "end Term.Apply, Term.ArgClause: /* c3 */",
        "end Term.Apply, Term.ArgClause: /* c2 */",
        "end Term.ApplyInfix: /* c1 */",
      ),
    )(
      "end Term.Apply, Term.ArgClause: /* c3 */",
      "end Term.Apply, Term.ArgClause, Term.ApplyInfix: /* c2 */",
      "beg Term.ArgClause, Term.ApplyInfix: /* c1 */",
    ).reprint
    assertNoDiff(
      source(first).reprint,
      "object A { while ((a op b /* c1 */ )(c op d) /* c2 */ (e op f) /* c3 */ ) g }",
    )
  }

  test("if: MLC after each parenthesized group, then a body") {
    val code =
      """|object A:
         |  if (a op b) /* c1 */ (c op d) /* c2 */ (e op f) /* c3 */ g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { if (a op b) /* c1 */ (c op d /* c2 */ )(e op f) /* c3 */ g }
         |""".stripMargin,
      reprinted = Seq(
        "beg Term.SelectPostfix, Term.Apply, Term.ApplyInfix: /* c1 */",
        "end Term.Apply, Term.ArgClause: /* c3 */",
        "end Term.ApplyInfix, Term.ArgClause, Term.Name: /* c2 */",
      ),
    )(
      "beg Term.SelectPostfix, Term.Apply, Term.ApplyInfix: /* c1 */",
      "end Term.Apply, Term.ArgClause: /* c3 */",
      "end Term.ApplyInfix: /* c2 */",
    ).reprint
    assertNoDiff(
      source(first).reprint,
      "object A { if (a op b) /* c1 */ (c op d /* c2 */ )(e op f) /* c3 */ g }",
    )
  }

  test("if: MLC after each parenthesized group, then a brace body") {
    val code =
      """|object A:
         |  if (a op b) /* c1 */ (c op d) /* c2 */ (e op f) /* c3 */ { g }
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A {
         |  if (a op b) /* c1 */ (c op d /* c2 */ )(e op f) /* c3 */ {
         |    g
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq(
        "beg Term.Apply, Term.Apply, Term.ApplyInfix: /* c1 */",
        "end Term.Apply, Term.ArgClause: /* c3 */",
        "end Term.ApplyInfix, Term.ArgClause, Term.Name: /* c2 */",
      ),
    )(
      "beg Term.Apply, Term.Apply, Term.ApplyInfix: /* c1 */",
      "end Term.Apply, Term.ArgClause: /* c3 */",
      "end Term.ApplyInfix: /* c2 */",
    ).reprint
    assertNoDiff(
      source(first).reprint,
      """|object A {
         |  if (a op b) /* c1 */ (c op d /* c2 */ )(e op f) /* c3 */ {
         |    g
         |  }
         |}
         |""".stripMargin,
    )
  }

  test("if then: MLC after a tuple that starts the condition") {
    val code =
      """|object A:
         |  if (a, b) /* c1 */ == x then g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { if ((a, b) /* c1 */ /* c1 */ == x) g }
         |""".stripMargin,
      reprinted = Seq("end Term.Tuple: /* c1 */ /* c1 */"),
    )("end Term.Tuple: /* c1 */", "beg Term.Name: /* c1 */").reprint
    assertNoDiff(source(first).reprint, "object A { if ((a, b) /* c1 */ /* c1 */ == x) g }")
  }

  test("while do: MLC after a tuple that starts the condition") {
    val code =
      """|object A:
         |  while (a, b) /* c1 */ == x do g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { while ((a, b) /* c1 */ /* c1 */ == x) g }
         |""".stripMargin,
      reprinted = Seq("end Term.Tuple: /* c1 */ /* c1 */"),
    )("end Term.Tuple: /* c1 */", "beg Term.Name: /* c1 */").reprint
    assertNoDiff(source(first).reprint, "object A { while ((a, b) /* c1 */ /* c1 */ == x) g }")
  }

  test("if then: MLC after an infix operand that starts the condition") {
    val code =
      """|object A:
         |  if (a op b) /* c1 */ == x then g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { if ((a op b) /* c1 */ == x) g }
         |""".stripMargin,
      reprinted = Seq("end Term.ApplyInfix: /* c1 */"),
    )("beg Term.Name: /* c1 */").reprint
    assertNoDiff(source(first).reprint, "object A { if ((a op b /* c1 */ ) == x) g }")
  }

  test("if then: MLC after an infix operand that starts the condition, then on its line") {
    val code =
      """|object A:
         |  if (a op b) /* c1 */ == x
         |  then g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { if ((a op b) /* c1 */ == x) g }
         |""".stripMargin,
      reprinted = Seq("end Term.ApplyInfix: /* c1 */"),
    )("beg Term.Name: /* c1 */").reprint
    assertNoDiff(source(first).reprint, "object A { if ((a op b /* c1 */ ) == x) g }")
  }

  test("if then: MLC before a select on a group that starts the condition") {
    val code =
      """|object A:
         |  if (a op b) /* c1 */ .c then g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { if ((a op b).c) g }
         |""".stripMargin,
    )().reprint
    assertNoDiff(source(first).reprint, "object A { if ((a op b).c) g }")
  }

  test("if then: MLC before and after a group that starts the condition") {
    val code =
      """|object A:
         |  if /* c0 */ (a op b) /* c1 */ (c op d) then g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { if ( /* c0 */ (a op b) /* c1 */ (c op d)) g }
         |""".stripMargin,
      reprinted = Seq("beg Term.Apply, Term.ApplyInfix: /* c0 */", "end Term.ApplyInfix: /* c1 */"),
    )(
      "beg Term.Apply, Term.ApplyInfix, Term.Name: /* c0 */",
      "beg Term.ArgClause, Term.ApplyInfix: /* c1 */",
    ).reprint
    assertNoDiff(source(first).reprint, "object A { if ( /* c0 */ (a op b /* c1 */ )(c op d)) g }")
  }

  test("if: MLC after a tuple condition, body on the same line") {
    val code =
      """|object A:
         |  if (a, b) /* c */ b
         |""".stripMargin
    checkComments(
      code,
      """|object A { if ((a, b) /* c */ ) /* c */ b }
         |""".stripMargin,
    )("end Term.Tuple: /* c */", "beg Term.Name: /* c */")
  }

  test("if then: semicolon, MLC, else on the same line") {
    val code =
      """|object A {
         |  if a then b; /* c1 */ else c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { if (a) b else c }
         |""".stripMargin,
    )()
  }

  test("if then: semicolon, ASLC, else on the next line") {
    val code =
      """|object A {
         |  if a then b; // c1
         |  else c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  if (a) b // c1
         |  else c
         |}
         |""".stripMargin,
    )("end Term.Name: // c1")
  }
}
