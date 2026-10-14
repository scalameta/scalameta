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
         |  b
         |else d
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
    assertSyntax("func(\n  x // X\n)")(Term.Apply(tname("func"), Term.ArgClause(List(x))))
  }

  test("block comment at end of input") {
    val x = term("x /* X */")
    assertEquals(x.endComment.get.values.last.newlinesAfter, 1)
    assertSyntax("func(\n  x /* X */\n)")(Term.Apply(tname("func"), Term.ArgClause(List(x))))
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
    val tree = tnameComments("x")("// c")()
    val layout =
      """|// c
         |
         |x
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tree)
  }

  test("comment before a brace body") {
    val code =
      """|def f = // c
         |  {
         |    x
         |  }
         |""".stripMargin
    val layout =
      """|def f = // c
         |  {
         |    x
         |  }
         |""".stripMargin
    val body = Term.Block.newBuilder(List(tname("x"))).begComment(Seq("// c")).result()
    val tree = Defn.Def(Nil, tname("f"), Nil, None, body)
    runTestAssert[Stat](code, layout)(tree)
  }

  test("comment inside a parameter clause") {
    val code =
      """|def f(
         |  // c
         |  x: Int
         |) = 1
         |""".stripMargin
    val layout =
      """|def f(
         |  // c
         |  x: Int
         |) = 1
         |""".stripMargin
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
    runTestAssert[Stat](code, layout)(tree)
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
         |def g = 1
         |""".stripMargin
    val tree = Source(List(
      Defn.Def(Nil, tname("f"), Nil, Nil, None, blk(tnameComments("x")()("// c"))),
      Defn.Def(Nil, tname("g"), Nil, Nil, None, int(1)),
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
         |object P
         |""".stripMargin
    val tree = Source(List(
      Defn.Object(Nil, tname("O"), tpl(List(tnameComments("x")()("// c")))),
      Defn.Object(Nil, tname("P"), tplNoBody()),
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
    assertNotEquals(importer.endComment.get, imp.endComment.get)
    assertEquals(importer.endComment.get.values.head, imp.endComment.get.values.head)
    assertEquals(imp.endComment.get.values.head.parent, None)
    assertSyntax(
      """|import p3.a3 // ct3
         |// ct4
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
    assertEquals(select.name.endComment.get.values.map(_.syntax), List("// c1", "// c2"))
    assertEquals(select.name.endComment.get, select.endComment.get)
    assertEquals(select.name.endComment.get.values.head, select.endComment.get.values.head)
    assertSyntax(
      """|{
         |  a.b // c1
         |  // c2
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
         |  // c
         |
         |  y
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tnameComments("x")()("// c"), tname("y")))
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
         |  // c1
         |
         |  // c2
         |
         |  // c3
         |  s2
         |}
         |""".stripMargin
    val tree = blk(tnameComments("s1")()("// c1"), tnameComments("s2")("// c2", "// c3")())
    runTestAssert[Stat](code, layout)(tree)
  }

  test("source: DSLC, blank line, first statement") {
    val code =
      """|// c
         |
         |import a.b
         |""".stripMargin
    val layout =
      """|// c
         |
         |import a.b
         |""".stripMargin
    val imp = Import.newBuilder(List(Importer("a", "b"))).begComment(Seq("// c")).result()
    val tree = Source(List(imp))
    runTestAssert[Source](code, layout)(tree)
  }

  test("source: last statement, blank line, DSLC") {
    val code =
      """|import a.b
         |
         |// c
         |""".stripMargin
    val layout =
      """|import a.b
         |
         |// c
         |""".stripMargin
    val imp = Import.newBuilder(List(Importer("a", "b"))).endComment(Seq("// c")).result()
    val tree = Source(List(imp))
    runTestAssert[Source](code, layout)(tree)
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
         |  // c
         |
         |  x
         |}
         |""".stripMargin
    val tree = blk(tnameComments("x")("// c")())
    runTestAssert[Stat](code, layout)(tree)
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
         |
         |  // c
         |}
         |""".stripMargin
    val tree = blk(tnameComments("x")()("// c"))
    runTestAssert[Stat](code, layout)(tree)
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
         |// c
         |{
         |  object O
         |}
         |""".stripMargin
    val obj = Defn.Object(Nil, tname("O"), tplNoBody())
    val body = Pkg.Body.newBuilder(List(obj)).begComment(detachedComments("// c")).result()
    runTestAssert[Source](code, layout)(Source(List(Pkg(tname("p"), body))))
  }

  test("def: ASLC after =, brace body on the next line") {
    val code =
      """|def f = // c
         |  {
         |    x
         |  }
         |""".stripMargin
    val layout =
      """|def f = // c
         |  {
         |    x
         |  }
         |""".stripMargin
    val body = Term.Block.newBuilder(List(tname("x"))).begComment(Seq("// c")).result()
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, body))
  }

  test("block: ASLC after a semicolon") {
    val code =
      """|{
         |  x; // c
         |}
         |""".stripMargin
    val layout =
      """|{
         |  x // c
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tnameComments("x")()("// c")))
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
         |  // c
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tnameComments("x")()("// c")))
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
      """|f(
         |  a, b
         |  // c
         |)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tapply(tname("f"), tname("a"), tnameComments("b")()("// c")))
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
      """|f(
         |  a, b
         |  // c
         |)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tapply(tname("f"), tname("a"), tnameComments("b")()("// c")))
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
      """|f(
         |  a, b
         |
         |  // c
         |)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tapply(tname("f"), tname("a"), tnameComments("b")()("// c")))
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
      """|f(
         |  // c
         |
         |  a, b
         |)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tapply(tname("f"), tnameComments("a")("// c")(), tname("b")))
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
      """|f(
         |  a,
         |  // c
         |
         |  b
         |)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(tapply(tname("f"), tnameComments("a")()("// c"), tname("b")))
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
      """|(
         |  a, b
         |  // c
         |)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Term.Tuple(List(tname("a"), tnameComments("b")()("// c"))))
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
      """|(
         |  a, b
         |
         |  // c
         |)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Term.Tuple(List(tname("a"), tnameComments("b")()("// c"))))
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
      """|(
         |  // c
         |
         |  a, b
         |)
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Term.Tuple(List(tnameComments("a")("// c")(), tname("b"))))
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
      """|def f(
         |  a: Int, b: Int
         |  // c
         |) = a
         |""".stripMargin
    val b = tparam("b", "Int").toBuilder.endComment(Seq("// c")).result()
    val params = List(tparam("a", "Int"), b)
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
      """|def f(
         |  a: Int, b: Int
         |
         |  // c
         |) = a
         |""".stripMargin
    val b = tparam("b", "Int").toBuilder.endComment(Seq("// c")).result()
    val params = List(tparam("a", "Int"), b)
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
      """|def f(
         |  // c
         |
         |  a: Int, b: Int
         |) = a
         |""".stripMargin
    val a = tparam("a", "Int").toBuilder.begComment(Seq("// c")).result()
    val params = List(a, tparam("b", "Int"))
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
         |// c
         |
         |object O
         |""".stripMargin
    val tree = Source(List(
      Import.newBuilder(List(Importer("y", "Y"))).endComment(Seq("// c")).result(),
      Defn.Object(Nil, tname("O"), tplNoBody()),
    ))
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
         |
         |// c
         |
         |object O
         |""".stripMargin
    val obj = Defn.Object.newBuilder(Nil, tname("O"), tplNoBody()).begComment(Seq("// c")).result()
    val body = Pkg.Body(List(obj))
    val tree = Source(List(Pkg(tname("p"), body)))
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
         |  // c
         |
         |  y
         |}
         |""".stripMargin
    val tree =
      Source(List(Defn.Object(Nil, tname("O"), tpl(List(tnameComments("x")()("// c"), tname("y"))))))
    runTestAssert[Source](code, layout)(tree)
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
      """|object O {
         |  x
         |
         |  // c
         |}
         |object P
         |""".stripMargin
    val body = tplBody(tnameComments("x")()("// c"))
    val tree = Source(
      List(Defn.Object(Nil, tname("O"), tpl(Nil, body)), Defn.Object(Nil, tname("P"), tplNoBody())),
    )
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
      """|object O {
         |  // c
         |
         |  x
         |}
         |""".stripMargin
    val body = tplBody(tnameComments("x")("// c")())
    runTestAssert[Source](code, layout)(Source(List(Defn.Object(Nil, tname("O"), tpl(Nil, body)))))
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
      """|class A {
         |  x
         |
         |  // c
         |}
         |""".stripMargin
    val body = tplBody(tnameComments("x")()("// c"))
    val tree = Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tpl(Nil, body))
    runTestAssert[Stat](code, layout)(tree)
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
      """|class A {
         |  // c
         |
         |  x
         |}
         |""".stripMargin
    val body = tplBody(tnameComments("x")("// c")())
    val tree = Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tpl(Nil, body))
    runTestAssert[Stat](code, layout)(tree)
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
         |object O {}
         |
         |// c
         |""".stripMargin
    val obj = Defn.Object.newBuilder(Nil, tname("O"), tplNoBody()).endComment(Seq("// c")).result()
    val body = Pkg.Body(List(obj))
    val tree = Source(List(Pkg(tname("p"), body)))
    parseAndCheckTree[Source](code, layout)(tree)
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
         |// c
         |{}
         |
         |// c
         |
         |""".stripMargin
    val templ = tplNoBody().toBuilder.begComment(detachedComments("// c")).result()
    val obj = Defn.Object(Nil, tname("O"), templ)
    val pkg = Pkg.newBuilder(tname("p"), Pkg.Body(List(obj))).endComment(Seq("// c")).result()
    parseAndCheckTree[Source](code, layout)(Source(List(pkg)))
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
         |  x // c
         |  y
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tnameComments("x")()("// c"), tname("y")))
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
    runTestAssert[Stat](code, "f(a /* c */ , b)")(
      tapply(tname("f"), tnameComments("a")()("/* c */"), tname("b")),
    )
  }

  test("params: MLC between a param and the comma") {
    val code = "def f(a: Int /* c */, b: Int) = a"
    val a = tparam("a", "Int").toBuilder.endComment(Seq("/* c */")).result()
    val params = List(a, tparam("b", "Int"))
    runTestAssert[Stat](code, "def f(a: Int /* c */ , b: Int) = a")(
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
    val layout =
      """|class A {
         |  self =>
         |  // c
         |
         |  x
         |}
         |""".stripMargin
    val x = tname("x")
    val commentedSelf = Self.newBuilder(tname("self"), None).endComment(Seq("// c")).result()
    val tree = Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tpl(Nil, commentedSelf, x))
    runTestAssert[Stat](code, layout)(tree)
  }

  test("built self with an end comment, printed and parsed again") {
    val commentedSelf = Self.newBuilder(tname("self"), None).endComment(Seq("// c")).result()
    val tree = Defn.Class(Nil, pname("A"), Nil, EmptyCtor(), tpl(Nil, commentedSelf, tname("x")))
    val printed = tree.reprint
    assertNoDiff(
      printed,
      """|class A { self => // c
         |  x
         |}
         |""".stripMargin,
    )
    assertEquals(templStat(printed).structure, tree.structure)
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
         |  // c
         |
         |  x
         |  y
         |}
         |""".stripMargin
    val body = blk(tnameComments("x")("// c")(), tname("y"))
    runTestAssert[Stat](code, layout)(Defn.Def(Nil, tname("f"), Nil, None, body))
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
         |
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
      """|f[
         |  A, // c
         |  B
         |]
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
      """|class C[
         |  A, // c
         |  B
         |]
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
      """|for {
         |  a <- b // c
         |  d <- e
         |} yield a
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
      """|import a.b, // c
         |  d.e
         |""".stripMargin
    val ab = Importer("a", "b").toBuilder.endComment(Seq("// c")).result()
    runTestAssert[Stat](code, layout)(Import(List(ab, Importer("d", "e"))))
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
         |    b, // c1
         |    // c2
         |    c
         |  }
         |}
         |""".stripMargin,
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
      """|enum E {
         |  case C(
         |    a: A,
         |    // c
         |    b: B
         |  )
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Param, Term.Name: // c"),
    )("end Term.Param, Type.Name: // c")
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
         |  /* c */ // d
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tnameComments("x")()("/* c */", "// d")))
  }

  test("block: semicolon, then an MLC before the brace") {
    val code = "{ x; /* c */ }"
    val layout =
      """|{
         |  x /* c */
         |}
         |""".stripMargin
    runTestAssert[Stat](code, layout)(blk(tnameComments("x")()("/* c */")))
  }

  test("select chain: DSLC before the dot") {
    val code =
      """|x.foo(a)
         |  // c
         |  .bar(b)
         |""".stripMargin
    val layout =
      """|x.foo(a)
         |// c
         |.bar(b)
         |""".stripMargin
    val sel = Term.Select(tapply(tselect("x", "foo"), tname("a")), tnameComments("bar")("// c")())
    runTestAssert[Stat](code, layout)(tapply(sel, tname("b")))
  }

  test("if: DSLC before else") {
    val code =
      """|if (a) b
         |// c
         |else d
         |""".stripMargin
    val layout =
      """|if (a) b
         |// c
         |else d
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Term.If(tname("a"), tname("b"), tnameComments("d")("// c")()))
  }

  test("class: ASLC after extends, DSLC before with") {
    val code =
      """|class A extends B // c1
         |  // c2
         |  with C
         |""".stripMargin
    checkComments(code)(
      "end Type.Name: // c1",
      "beg Name.Anonymous: // c2",
      "beg Init, Type.Name: // c2",
    )
  }

  test("class: MLC after extends") {
    val code =
      """|class A extends /* c1 */ B
         |""".stripMargin
    checkComments(code)("beg Template, Init, Type.Name: /* c1 */")
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
        "beg Template, Init, Type.Name: // c1\n// c1\n  // c1",
      ),
      twice =
        """|class A
           |// c1
           |// c1
           |// c1
           |// c1
           |// c1
           |// c1
           |  // c1
           |  // c1
           |  // c1
           |  extends B
           |""".stripMargin,
    )(
      "beg Type.ParamClause: // c1",
      "beg Ctor.Primary, Name.Anonymous: // c1",
      "beg Template, Init, Type.Name: // c1",
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
      """|try x
         |// c
         |catch {
         |  case _ => y
         |}
         |""".stripMargin
    val cases = Term.CasesBlock.newBuilder(List(Case(Pat.Wildcard(), None, tname("y"))))
      .begComment(Seq("// c")).result()
    runTestAssert[Stat](code, layout)(Term.Try(tname("x"), Some(cases), None))
  }

  test("try: DSLC before finally") {
    val code =
      """|try x
         |// c
         |finally y
         |""".stripMargin
    val layout =
      """|try x
         |// c
         |finally y
         |""".stripMargin
    runTestAssert[Stat](code, layout)(Term.Try(tname("x"), Nil, Some(tnameComments("y")("// c")())))
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
    val layout =
      """|def f = if (a) b
         |// c
         |else d
         |""".stripMargin
    val body = Term.If(tname("a"), tname("b"), tnameComments("d")("// c")())
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
         |    // c
         |
         |    a
         |    b
         |}
         |""".stripMargin
    val body = blk(tnameComments("a")("// c")(), tname("b"))
    val tree = Term.Match(tname("x"), List(Case(int(1), None, body)), Nil)
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
      """|def f = if (a)
         |  // c
         |  b
         |else d
         |""".stripMargin,
    )("beg Term.Name: // c")
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
      """|def f = while (a)
         |  // c
         |  b
         |""".stripMargin,
    )("beg Term.Name: // c")
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
      """|def f = for (x <- xs)
         |  // c
         |  f(x)
         |""".stripMargin,
    )("beg Term.Apply, Term.Name: // c")
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
         |  case y
         |  // c
         |  if y > 0 => 1
         |}
         |""".stripMargin,
    )("beg Term.ApplyInfix, Term.Name: // c")
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
         |  b
         |else d
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
      """|def f = if (a) b
         |/* c */ else d
         |""".stripMargin,
    )("beg Term.Name: /* c */")
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
      """|def f = if (a)
         |  /* c */ b
         |else d
         |""".stripMargin,
    )("beg Term.Name: /* c */")
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
      """|def f = try b
         |/* c */ finally d
         |""".stripMargin,
    )("beg Term.Name: /* c */")
  }

  test("braceless for: MLC before yield on its line") {
    val code =
      """|def f =
         |  for x <- xs
         |  /* c */ yield f(x)
         |""".stripMargin
    checkComments(
      code,
      """|def f = for (x <- xs)
         |/* c */ yield f(x)
         |""".stripMargin,
    )("beg Term.Apply, Term.Name: /* c */")
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
      """|class A {
         |  inline
         |  // c1
         |  def f = 1
         |}
         |""".stripMargin,
    )("end Mod.Inline: // c1")
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
      """|def f = for {
         |  // c1
         |  x <- xs
         |} g(x)
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
         |    {}
         |  case 2 => b
         |}
         |""".stripMargin,
    )("beg Term.Block: // c1")
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
         |    // c1
         |  case 2 => b
         |}
         |""".stripMargin,
      reprinted = Seq("end Case, Term.Block: // c1", "beg Term.Block: // c1"),
      twice =
        """|def f = x match {
           |  case 1 =>
           |    // c1
           |    {}
           |    // c1
           |  case 2 =>
           |    b
           |}
           |""".stripMargin,
    )("end Case: // c1", "beg Term.Block: // c1")
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
         |  case b =>
         |    y
         |}
         |""".stripMargin,
      reprinted = Seq("end Case, Term.Name: // a1"),
    )("end Term.Name: // a1")
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
         |  def g = 1
         |}
         |""".stripMargin,
    )("end Term.Name: // c")
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
         |    // c2
         |  case b =>
         |}
         |""".stripMargin,
    )("end Case, Term.Name: // c2", "beg Term.Name: // c1")
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
         |  case 2 =>
         |    c
         |}
         |""".stripMargin,
    )("end Term.Name: // c1")
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
         |  // c1
         |  using b: B
         |) = 1
         |""".stripMargin,
    )("beg Mod.Using, Mod.Using: // c1")
  }

  test("params: MLC before using") {
    val code =
      """|def f(a: Int)(/* c1 */ using b: B) = 1
         |""".stripMargin
    checkComments(
      code,
      """|def f(a: Int)( /* c1 */ using b: B) = 1
         |""".stripMargin,
    )("beg Mod.Using, Mod.Using: /* c1 */")
  }

  test("given: MLC before the name") {
    val code =
      """|object O {
         |  given /* c1 */ x: Int = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given /* c1 */ x: Int = 1 }
         |""".stripMargin,
    )("beg Term.Name: /* c1 */")
  }

  test("derives: MLC before the class") {
    val code =
      """|case class A(x: Int) derives /* c1 */ Eq
         |""".stripMargin
    checkComments(
      code,
      """|case class A(x: Int) derives /* c1 */ Eq
         |""".stripMargin,
    )("beg Type.Name: /* c1 */")
  }

  test("derives: DSLC before derives") {
    val code =
      """|class A extends B
         |  // c1
         |  derives C
         |""".stripMargin
    checkComments(code)("beg Name.Anonymous: // c1", "beg Type.Name: // c1")
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
        "beg Template, Type.Name: // c1\n// c1\n  // c1",
      ),
      twice =
        """|class A
           |// c1
           |// c1
           |// c1
           |// c1
           |// c1
           |// c1
           |  // c1
           |  // c1
           |  // c1
           |  derives C
           |""".stripMargin,
    )(
      "beg Type.ParamClause: // c1",
      "beg Ctor.Primary, Name.Anonymous: // c1",
      "beg Template, Type.Name: // c1",
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
      """|given intOrd: Ord[Int]
         |  // c1
         |  with { def f = 1 }
         |""".stripMargin,
    )("beg Name.Anonymous: // c1", "beg Template.Body: // c1")
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
    checkComments(code)("beg Template.Body: /* c1 */")
  }

  test("given with: DSLC before with, empty body") {
    val code =
      """|given intOrd: Ord[Int]
         |  // c1
         |  with {}
         |""".stripMargin
    checkComments(code)("beg Name.Anonymous: // c1", "beg Template.Body: // c1")
  }

  test("given: parameter clause, MLC, arrow") {
    val code =
      """|object O {
         |  given f: (x: Int) /* c1 */ => Foo = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given f: (x: Int) /* c1 */ => Foo = ??? }
         |""".stripMargin,
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
      """|object O { import a.{given /* c1 */ Int} }
         |""".stripMargin,
    )("beg Type.Name: /* c1 */")
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
         |    case given /* c1 */ Int => 1
         |  }
         |}
         |""".stripMargin,
    )("beg Type.Name: /* c1 */")
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
         |    // c
         |    case _: E => ()
         |  }
         |}
         |""".stripMargin,
    )("beg Case: // c")
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
         |      // c2
         |      def double = x * 2
         |    }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Block: // c1", "beg Defn.Def: // c2"),
    )("end Member.ParamClauseGroup, Term.ParamClause: // c1", "beg Defn.Def: // c2")
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
         |  def f: Unit = try (
         |    // c
         |    g
         |  ) finally h
         |}
         |""".stripMargin,
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
         |      x
         |    else if (b) // c
         |      y
         |    else z
         |  }
         |}
         |""".stripMargin,
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
         |      x
         |    else if (b)
         |      // t
         |      y
         |    else z
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
      """|object A {
         |  if (a) f() else if (b) // c
         |    g()
         |}
         |""".stripMargin,
    )("beg Term.Apply, Term.Name: // c")
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
      reprinted = Seq("end Defn.Def, Term.Block: // c2", "end Term.Name: // c1"),
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
      reprinted = Seq("end Defn.Class, Template, Template.Body: // c2"),
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
         |    (x: Int)
         |      // c1
         |      => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.ParamClause: // c1")
  }

  test("lambda in braces: DSLC between the params and the arrow, body on the arrow's line") {
    val code =
      """|object A:
         |  xs.map { (x: Int)
         |    // c1
         |    => b
         |  }
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  xs.map {
         |    (x: Int)
         |      // c1
         |      => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.ParamClause: // c1")
  }

  test("lambda in braces: two DSLCs between the params and the arrow") {
    val code =
      """|object A:
         |  xs.map { (x: Int)
         |    // c1
         |    // c2
         |    =>
         |      b
         |  }
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  xs.map {
         |    (x: Int)
         |      // c1
         |      // c2
         |      => b
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("end Term.ParamClause: // c1\n      // c2"),
    )("end Term.ParamClause: // c1\n    // c2")
  }

  test("case: DSLC between the pattern and the arrow") {
    val code =
      """|object A:
         |  x match
         |    case 1
         |      // c1
         |      =>
         |        b
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1
         |      // c1
         |      => b
         |  }
         |}
         |""".stripMargin,
    )("end Lit.Int: // c1")
  }

  test("case: DSLC between the pattern and the arrow, body on the arrow's line") {
    val code =
      """|object A:
         |  x match
         |    case 1
         |      // c1
         |      => b
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1
         |      // c1
         |      => b
         |  }
         |}
         |""".stripMargin,
    )("end Lit.Int: // c1")
  }

  test("case: DSLC between the guard and the arrow") {
    val code =
      """|object A:
         |  x match
         |    case 1 if c
         |      // c1
         |      =>
         |        b
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 if c
         |      // c1
         |      => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: // c1")
  }

  test("match type: DSLC between the pattern and the arrow") {
    val code =
      """|object A:
         |  type T[X] = X match
         |    case Int
         |      // c1
         |      => String
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  type T[X] = X match {
         |    case Int
         |      // c1
         |      => String
         |  }
         |}
         |""".stripMargin,
    )("end Type.Name: // c1")
  }

  test("type lambda: DSLC between the params and the arrow") {
    val code =
      """|object A:
         |  type T = [X]
         |    // c1
         |    =>> List[X]
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  type T = [X]
         |    // c1
         |    =>> List[X]
         |}
         |""".stripMargin,
    )("end Type.ParamClause: // c1")
  }

  test("context function type: DSLC between the param type and the arrow") {
    val code =
      """|object A:
         |  type T = A
         |    // c1
         |    ?=> B
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  type T = A
         |    // c1
         |    ?=> B
         |}
         |""".stripMargin,
    )("end Type.FuncParamClause, Type.Name: // c1")
  }

  test("context lambda: DSLC between the params and the arrow") {
    val code =
      """|object A:
         |  val f = {
         |    (x: Int)
         |      // c1
         |      ?=> x
         |  }
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val f = {
         |    (x: Int)
         |      // c1
         |      ?=> x
         |  }
         |}
         |""".stripMargin,
    )("end Term.ParamClause: // c1")
  }

  test("val: DSLC between the pattern and the equals") {
    val code =
      """|object A:
         |  val x
         |    // c1
         |    = 1
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val x
         |    // c1
         |    = 1
         |}
         |""".stripMargin,
    )("end Pat.Var, Term.Name: // c1")
  }

  test("given alias: DSLC between the type and the equals") {
    val code =
      """|object A:
         |  given x: Int
         |    // c1
         |    = 1
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  given x: Int
         |    // c1
         |    = 1
         |}
         |""".stripMargin,
    )("end Type.Name: // c1")
  }

  test("class: DSLC between the name and derives") {
    val code =
      """|object A:
         |  case class C(x: Int)
         |    // c1
         |    derives Eq
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  case class C(x: Int)
         |    // c1
         |    derives Eq
         |}
         |""".stripMargin,
    )("beg Template, Type.Name: // c1")
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
         |    case '[ type t; List[t]
         |    // c
         |    ] => 1
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("end Type.Apply: // c"),
    )("end Type.Apply, Type.ArgClause: // c")
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
      """|object A {
         |  val t = '[ List[Int]
         |  // c
         |  ]
         |}
         |""".stripMargin,
      reprinted = Seq("end Type.Apply: // c"),
    )("end Type.Apply, Type.ArgClause: // c")
  }

  test("import: MLC after as") {
    val code =
      """|import a.{d as /* c1 */ e}
         |""".stripMargin
    checkComments(
      code,
      """|import a.{d as /* c1 */ e}
         |""".stripMargin,
    )("beg Name.Indeterminate: /* c1 */")
  }

  test("import: MLC after as, no braces") {
    val code =
      """|import a.d as /* c1 */ e
         |""".stripMargin
    checkComments(
      code,
      """|import a.d as /* c1 */ e
         |""".stripMargin,
    )("beg Name.Indeterminate: /* c1 */")
  }

  test("import: MLC after as, wildcard") {
    val code =
      """|import a.{d as /* c1 */ _}
         |""".stripMargin
    checkComments(
      code,
      """|import a.{d as _}
         |""".stripMargin,
    )()
  }

  test("import: MLC after the arrow") {
    val code =
      """|import a.{d => /* c1 */ e}
         |""".stripMargin
    checkComments(
      code,
      """|import a.{d as /* c1 */ e}
         |""".stripMargin,
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
      """|object A { export a.{d as /* c1 */ e} }
         |""".stripMargin,
    )("beg Name.Indeterminate: /* c1 */")
  }

  test("extension: MLC after the keyword") {
    val code =
      """|object A:
         |  extension /* c1 */ (x: Int) def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|object A { extension /* c1 */ (x: Int) def f = 1 }
         |""".stripMargin,
    )("beg Member.ParamClauseGroup, Type.ParamClause, Term.ParamClause: /* c1 */")
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
      """|object A {
         |  for {
         |    x <- xs /* c1 */
         |  }
         |    /* c2 */ a
         |}
         |""".stripMargin,
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
    val printed = TreeSyntax.reprint(parsed).toString
    assertEquals(bodies(source(printed)), List("Term.Name", "Term.Name"))
    assertNoDiff(TreeSyntax.reprint(source(printed)).toString, printed)
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
         |    // c2
         |    case 2 =>
         |      b
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("end Case, Term.Name: // c1", "beg Case: // c2"),
    )("end Term.Name: // c1", "beg Case: // c2")
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
         |    case 1 =>
         |      b
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
         |  try (
         |    // c1
         |    a
         |  ) catch {
         |    case _ => b
         |  }
         |}
         |""".stripMargin,
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
         |    c
         |  else d else e
         |}
         |""".stripMargin,
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
         |    c
         |  else e
         |}
         |""".stripMargin,
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
      """|object O {
         |  while (a)
         |    // c1
         |
         |    b
         |}
         |""".stripMargin,
    )("beg Term.Name: // c1")
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
         |  end /* c1 */ f
         |}
         |""".stripMargin,
    )("beg Term.Name: /* c1 */")
  }

  test("dependent function type: param, MLC, colon") {
    val code =
      """|object O:
         |  type F = (x /* c1 */ : Int) => x.type
         |""".stripMargin
    checkComments(
      code,
      """|object O { type F = (x /* c1 */ : Int) => x.type }
         |""".stripMargin,
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
      """|case class C(
         |  x
         |    // c1
         |    : Int
         |)
         |""".stripMargin,
    )("end Term.Name: // c1")
  }

  test("quote: MLC after the quote") {
    val code =
      """|object O:
         |  def f(using Quotes) = ' /* c1 */ x
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f(using Quotes) = ' /* c1 */ x }
         |""".stripMargin,
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
      """|object O {
         |  @deprecated
         |  // c1
         |
         |  final def f = 1
         |}
         |""".stripMargin,
    )("end Mod.Annot: // c1")
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
         |  // c2
         |  def f = 1
         |}
         |""".stripMargin,
    )("end Mod.Annot, Init, Type.Name: // c1\n  // c2")
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
         |trait A""".stripMargin.lf2nl
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
         |
         |  /* c1 */ f()
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Apply, Term.Name: /* c1 */"),
    )("beg Term.Apply, Term.Name: /* c1 */")
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
         |  /* c1 */ f()
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Apply, Term.Name: /* c1 */"),
    )("beg Term.Apply, Term.Name: /* c1 */")
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
         |
         |  // c1
         |  f()
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Apply, Term.Name: // c1"),
    )("beg Term.Apply, Term.Name: // c1")
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
         |
         |  /* c1 */ 1
         |}
         |""".stripMargin,
      reprinted = Seq("beg Lit.Int: /* c1 */"),
    )("beg Lit.Int: /* c1 */")
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
         |
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
      """|object A { if ((a op b)(c op d) /* c2 */ (e op f) /* c3 */ ) g }
         |""".stripMargin,
      reprinted =
        Seq("end Term.Apply, Term.ArgClause: /* c3 */", "end Term.Apply, Term.ArgClause: /* c2 */"),
    )("end Term.Apply, Term.ArgClause: /* c3 */", "end Term.Apply: /* c2 */").reprint
    assertNoDiff(
      source(first).reprint,
      "object A { if ((a op b)(c op d) /* c2 */ (e op f) /* c3 */ ) g }",
    )
  }

  test("while do: MLC after each parenthesized group of the condition") {
    val code =
      """|object A:
         |  while (a op b) /* c1 */ (c op d) /* c2 */ (e op f) /* c3 */ do g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { while ((a op b)(c op d) /* c2 */ (e op f) /* c3 */ ) g }
         |""".stripMargin,
      reprinted =
        Seq("end Term.Apply, Term.ArgClause: /* c3 */", "end Term.Apply, Term.ArgClause: /* c2 */"),
    )("end Term.Apply, Term.ArgClause: /* c3 */", "end Term.Apply: /* c2 */").reprint
    assertNoDiff(
      source(first).reprint,
      "object A { while ((a op b)(c op d) /* c2 */ (e op f) /* c3 */ ) g }",
    )
  }

  test("if: MLC after each parenthesized group, then a body") {
    val code =
      """|object A:
         |  if (a op b) /* c1 */ (c op d) /* c2 */ (e op f) /* c3 */ g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { if (a op b) /* c1 */ (c op d)(e op f) /* c3 */ g }
         |""".stripMargin,
    )("beg Term.SelectPostfix, Term.Apply: /* c1 */", "end Term.Apply, Term.ArgClause: /* c3 */")
      .reprint
    assertNoDiff(
      source(first).reprint,
      "object A { if (a op b) /* c1 */ (c op d)(e op f) /* c3 */ g }",
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
         |  if (a op b) /* c1 */ (c op d)(e op f) /* c3 */ {
         |    g
         |  }
         |}
         |""".stripMargin,
    )("beg Term.Apply, Term.Apply: /* c1 */", "end Term.Apply, Term.ArgClause: /* c3 */").reprint
    assertNoDiff(
      source(first).reprint,
      """|object A {
         |  if (a op b) /* c1 */ (c op d)(e op f) /* c3 */ {
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
      """|object A { if ((a, b) /* c1 */ == x) g }
         |""".stripMargin,
    )("end Term.Tuple: /* c1 */").reprint
    assertNoDiff(source(first).reprint, "object A { if ((a, b) /* c1 */ == x) g }")
  }

  test("while do: MLC after a tuple that starts the condition") {
    val code =
      """|object A:
         |  while (a, b) /* c1 */ == x do g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { while ((a, b) /* c1 */ == x) g }
         |""".stripMargin,
    )("end Term.Tuple: /* c1 */").reprint
    assertNoDiff(source(first).reprint, "object A { while ((a, b) /* c1 */ == x) g }")
  }

  test("if then: MLC after an infix operand that starts the condition") {
    val code =
      """|object A:
         |  if (a op b) /* c1 */ == x then g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { if ((a op b) == x) g }
         |""".stripMargin,
    )().reprint
    assertNoDiff(source(first).reprint, "object A { if ((a op b) == x) g }")
  }

  test("if then: MLC after an infix operand that starts the condition, then on its line") {
    val code =
      """|object A:
         |  if (a op b) /* c1 */ == x
         |  then g
         |""".stripMargin
    val first = checkComments(
      code,
      """|object A { if ((a op b) == x) g }
         |""".stripMargin,
    )().reprint
    assertNoDiff(source(first).reprint, "object A { if ((a op b) == x) g }")
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
      """|object A { if ( /* c0 */ (a op b)(c op d)) g }
         |""".stripMargin,
      reprinted = Seq("beg Term.Apply: /* c0 */"),
    )("beg Term.Apply, Term.ApplyInfix, Term.Name: /* c0 */").reprint
    assertNoDiff(source(first).reprint, "object A { if ( /* c0 */ (a op b)(c op d)) g }")
  }

  test("if: MLC after a tuple condition, body on the same line") {
    val code =
      """|object A:
         |  if (a, b) /* c */ b
         |""".stripMargin
    checkComments(
      code,
      """|object A { if ((a, b)) /* c */ b }
         |""".stripMargin,
    )("beg Term.Name: /* c */")
  }

  test("if then: semicolon, MLC, else on the same line") {
    val code =
      """|object A {
         |  if a then b; /* c1 */ else c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  if (a) b
         |  /* c1 */ else c
         |}
         |""".stripMargin,
    )("beg Term.Name: /* c1 */")
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

  test("if: newline, DSLC, condition") {
    val code =
      """|object A {
         |  if
         |  // c
         |  (a) b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  if (
         |  // c
         |  a) b
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("args: last arg, DSLC, blank line, its args") {
    val code =
      """|object A {
         |  f(a
         |  // c
         |
         |  (b))
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  f(
         |    a
         |    // c
         |
         |      (b)
         |  )
         |}
         |""".stripMargin,
    )("beg Term.ArgClause: // c")
  }

  test("block: statement, DSLC, blank line, infix operator") {
    val code =
      """|object A {
         |  {
         |    a
         |    // c
         |
         |    && b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  {
         |    a
         |    // c
         |
         |    &&b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: // c")
  }

  test("def body: outdent, DSLC, blank line, DSLC, statement") {
    val code =
      """|object A:
         |  def f =
         |    a
         |  // c1
         |
         |  // c2
         |  b
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = a
         |  // c1
         |
         |  // c2
         |  b
         |}
         |""".stripMargin,
    )("end Defn.Def: // c1", "beg Term.Name: // c2")
  }

  test("if block: outdent, DSLC, blank line, DSLC, statement") {
    val code =
      """|object A:
         |  if x then
         |    a
         |  // c1
         |
         |  // c2
         |  b
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  if (x) a
         |  // c1
         |
         |  // c2
         |  b
         |}
         |""".stripMargin,
    )("end Term.If: // c1", "beg Term.Name: // c2")
  }

  test("match: outdent, DSLC, blank line, DSLC, statement") {
    val code =
      """|object A:
         |  x match
         |    case 1 => a
         |  // c1
         |
         |  // c2
         |  b
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a
         |  }
         |  // c1
         |
         |  // c2
         |  b
         |}
         |""".stripMargin,
    )("end Term.Match: // c1", "beg Term.Name: // c2")
  }

  test("class body: outdent, DSLC, blank line, DSLC, statement") {
    val code =
      """|class A:
         |  def f = a
         |// c1
         |
         |// c2
         |val b = 1
         |""".stripMargin
    checkComments(
      code,
      """|class A { def f = a }
         |// c1
         |
         |// c2
         |val b = 1
         |""".stripMargin,
    )("end Defn.Class: // c1", "beg Defn.Val: // c2")
  }

  test("splice: MLC between the dollar and the block") {
    val code =
      """|object A {
         |  def f = '{ $ /* c */ { a } }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = '{ ${ a } } }
         |""".stripMargin,
    )()
  }

  test("splice: MLC between the dollar and the name") {
    val code =
      """|object A {
         |  def f = '{ $ /* c */ a }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = '{ $a } }
         |""".stripMargin,
    )()
  }

  test("type quote: MLC between the quote and the bracket") {
    val code =
      """|object A {
         |  def f = ' /* c */ [ T ]
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = '[ T ] }
         |""".stripMargin,
    )()
  }

  test("type splice: MLC between the dollar and the block") {
    val code =
      """|object A {
         |  def f = '[ List[$ /* c */ { t }] ]
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = '[ List[${ t }] ] }
         |""".stripMargin,
    )()
  }

  test("quote: MLC between the quote and the name") {
    val code =
      """|object A {
         |  def f = ' /* c */ a
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = ' /* c */ a }
         |""".stripMargin,
    )("beg Term.Name: /* c */")
  }

  test("quote: MLC between the quote and the block") {
    val code =
      """|object A {
         |  def f = ' /* c */ { a }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = ' /* c */ { a } }
         |""".stripMargin,
    )("beg Term.Block: /* c */")
  }

  test("object: blank line, DSLC, blank line, colon body") {
    val code =
      """|object A
         |
         |// c
         |
         |:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|object A
         |// c
         |{
         |  def f = 1
         |}
         |""".stripMargin,
    )("beg Template, Template.Body: // c")
  }

  test("object: blank line, MLC, colon body") {
    val code =
      """|object A
         |
         |/* c */ :
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|object A
         |/* c */
         |{
         |  def f = 1
         |}
         |""".stripMargin,
    )("beg Template, Template.Body: /* c */")
  }

  test("class: blank line, DSLC, blank line, colon body") {
    val code =
      """|class A
         |
         |// c
         |
         |:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|class A
         |
         |// c
         |
         |// c
         |
         |// c
         |{
         |  def f = 1
         |}
         |""".stripMargin,
      reprintError = "illegal start of definition `{`",
    )(
      "beg Type.ParamClause: // c",
      "beg Ctor.Primary, Name.Anonymous: // c",
      "beg Template, Template.Body: // c",
    )
  }

  test("trait: blank line, DSLC, blank line, colon body") {
    val code =
      """|trait A
         |
         |// c
         |
         |:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|trait A
         |
         |// c
         |
         |// c
         |
         |// c
         |{
         |  def f = 1
         |}
         |""".stripMargin,
      reprintError = "illegal start of definition `{`",
    )(
      "beg Type.ParamClause: // c",
      "beg Ctor.Primary, Name.Anonymous: // c",
      "beg Template, Template.Body: // c",
    )
  }

  test("enum derives: blank line, DSLC, blank line, colon body") {
    val code =
      """|enum A derives Eq
         |
         |// c
         |
         |:
         |  case B
         |""".stripMargin
    checkComments(
      code,
      """|enum A derives Eq
         |// c
         |{
         |  case B
         |}
         |""".stripMargin,
    )("beg Template.Body: // c")
  }

  test("package: blank line, DSLC, blank line, colon body") {
    val code =
      """|package a
         |
         |// c
         |
         |:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|package a
         |// c
         |{
         |  def f = 1
         |}
         |""".stripMargin,
      reprinted = Seq("beg Pkg.Body: // c"),
    )("beg Pkg.Body: // c")
  }

  test("object: DSLC, colon body") {
    val code =
      """|object A
         |// c
         |:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|object A
         |// c
         |{
         |  def f = 1
         |}
         |""".stripMargin,
      reprinted = Seq("beg Template, Template.Body: // c"),
    )("beg Template, Template.Body: // c")
  }

  test("if then: DSLC, two blank lines, else") {
    val code =
      """|object O {
         |  def f =
         |    if a then
         |      b
         |    // c
         |
         |
         |    else
         |      c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = if (a) b else c }
         |""".stripMargin,
    )()
  }

  test("if then: DSLC, one blank line, else") {
    val code =
      """|object O {
         |  def f =
         |    if a then
         |      b
         |    // c
         |
         |    else
         |      c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = if (a) b else c }
         |""".stripMargin,
    )()
  }

  test("if: DSLC, two blank lines, then") {
    val code =
      """|object O {
         |  def f =
         |    if a
         |    // c
         |
         |
         |    then
         |      b
         |    else
         |      c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = if (a) b else c }
         |""".stripMargin,
    )()
  }

  test("try: DSLC, two blank lines, catch") {
    val code =
      """|object O {
         |  def f =
         |    try
         |      b
         |    // c
         |
         |
         |    catch
         |      case _ => c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f = try b catch {
         |    case _ => c
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("for: DSLC, two blank lines, yield") {
    val code =
      """|object O {
         |  def f =
         |    for
         |      x <- xs
         |    // c
         |
         |
         |    yield
         |      x
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = for (x <- xs) yield x }
         |""".stripMargin,
    )()
  }

  test("for: DSLC, two blank lines, do") {
    val code =
      """|object O {
         |  def f =
         |    for
         |      x <- xs
         |    // c
         |
         |
         |    do
         |      g(x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = for (x <- xs) g(x) }
         |""".stripMargin,
    )()
  }

  test("match: DSLC, two blank lines, match") {
    val code =
      """|object O {
         |  def f =
         |    x
         |    // c
         |
         |
         |    match
         |      case _ => c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f = x match {
         |    case _ => c
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("if then: DSLC, three blank lines, else") {
    val code =
      """|object O {
         |  def f =
         |    if a then
         |      b
         |    // c
         |
         |
         |
         |    else
         |      c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = if (a) b else c }
         |""".stripMargin,
    )()
  }

  test("for: condition in parens, blank line, DSLC, blank line, body") {
    val code =
      """|object O {
         |  for (x <- xs)
         |
         |    // c
         |
         |    println(x)
         |}
         |""".stripMargin
    checkComments(code)("beg Term.Apply, Term.Name: // c")
  }

  test("if: condition in parens, blank line, DSLC, blank line, body") {
    val code =
      """|object O {
         |  if (a)
         |
         |    // c
         |
         |    println(x)
         |}
         |""".stripMargin
    checkComments(code)("beg Term.Apply, Term.Name: // c")
  }

  test("extension: params, blank line, DSLC, blank line, def") {
    val code =
      """|object O {
         |  extension (x: Int)
         |
         |    // c
         |
         |    def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  extension (x: Int) {
         |
         |    // c
         |
         |    def f = 1
         |  }
         |}
         |""".stripMargin,
    )("beg Defn.Def: // c")
  }

  test("annotation: args, blank line, DSLC, blank line, def") {
    val code =
      """|object O {
         |  @deprecated("x")
         |
         |  // c
         |
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(code)("end Mod.Annot: // c")
  }

  test("pattern: MLC between the wildcard and the star") {
    val code =
      """|object O {
         |  x match { case B(_ /* c */ *) => 1 }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case B(_*) => 1
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("pattern: MLC between the name and the star") {
    val code =
      """|object O {
         |  x match { case B(xs /* c */ *) => 1 }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case B(xs /* c */ *) => 1
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c */")
  }

  test("args: MLC between the name and the star") {
    val code =
      """|object O {
         |  f(xs /* c */ *)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { f(xs /* c */ *) }
         |""".stripMargin,
    )("end Term.Name: /* c */")
  }

  test("try: DSLC, two blank lines, finally") {
    val code =
      """|object O {
         |  def f =
         |    try
         |      b
         |    // c
         |
         |
         |    finally
         |      c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = try b finally c }
         |""".stripMargin,
    )()
  }

  test("while: DSLC, two blank lines, do") {
    val code =
      """|object O {
         |  def f =
         |    while
         |      a
         |    // c
         |
         |
         |    do
         |      c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = while (a) c }
         |""".stripMargin,
    )()
  }

  test("inline match: blank line, DSLC, blank line, scrutinee") {
    val code =
      """|object O {
         |  inline def f(x: Int) =
         |    inline
         |
         |    // c
         |
         |    x match { case _ => 1 }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  inline def f(x: Int) =
         |
         |    // c
         |
         |    inline x match {
         |      case _ => 1
         |    }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Match, Mod.Inline: // c"),
    )("beg Term.Match, Term.Name: // c")
  }

  test("inline match: DSLC, scrutinee") {
    val code =
      """|object O {
         |  inline def f(x: Int) =
         |    inline
         |    // c
         |    x match { case _ => 1 }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  inline def f(x: Int) =
         |    // c
         |    inline x match {
         |      case _ => 1
         |    }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Match, Mod.Inline: // c"),
    )("beg Term.Match, Term.Name: // c")
  }

  test("inline match: MLC, scrutinee") {
    val code =
      """|object O {
         |  inline def f(x: Int) =
         |    inline /* c */ x match { case _ => 1 }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  inline def f(x: Int) = inline /* c */ x match {
         |    case _ => 1
         |  }
         |}
         |""".stripMargin,
    )("end Mod.Inline: /* c */")
  }

  test("inline match: DSLC before inline") {
    val code =
      """|object O {
         |  inline def f(x: Int) =
         |    // c
         |    inline x match { case _ => 1 }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  inline def f(x: Int) =
         |    // c
         |    inline x match {
         |      case _ => 1
         |    }
         |}
         |""".stripMargin,
    )("beg Term.Match, Mod.Inline: // c")
  }

  test("inline if: blank line, DSLC, blank line, if") {
    val code =
      """|object O {
         |  inline def f(x: Int) =
         |    inline
         |
         |    // c
         |
         |    if x > 0 then 1 else 2
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  inline def f(x: Int) = {
         |    inline
         |
         |    // c
         |
         |    if (x > 0) 1 else 2
         |  }
         |}
         |""".stripMargin,
    )("beg Term.If: // c")
  }

  test("inline if: MLC, if") {
    val code =
      """|object O {
         |  inline def f(x: Int) =
         |    inline /* c */ if x > 0 then 1 else 2
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { inline def f(x: Int) = inline /* c */ if (x > 0) 1 else 2 }
         |""".stripMargin,
    )("end Mod.Inline: /* c */")
  }

  test("function type: using, blank line, DSLC, blank line, type") {
    val code =
      """|object O {
         |  type F = (using
         |
         |  // c
         |
         |  Int) ?=> Int
         |}
         |""".stripMargin
    runTestError[Source](code, "`identifier` expected but `)` found")
  }

  test("class param: val, blank line, DSLC, blank line, name") {
    val code =
      """|class A(val
         |
         |// c
         |
         | x: Int)
         |""".stripMargin
    checkComments(
      code,
      """|class A(
         |  val
         |
         |    // c
         |
         |    x: Int
         |)
         |""".stripMargin,
    )("end Mod.ValParam: // c")
  }

  test("enum param: val, blank line, DSLC, blank line, name") {
    val code =
      """|enum E(val
         |
         |// c
         |
         | x: Int):
         |  case A extends E(1)
         |""".stripMargin
    checkComments(
      code,
      """|enum E(
         |  val
         |
         |    // c
         |
         |    x: Int
         |) { case A extends E(1) }
         |""".stripMargin,
    )("end Mod.ValParam: // c")
  }

  test("extends: comma, blank line, DSLC, blank line, parent") {
    val code =
      """|class C extends B,
         |
         |  // c
         |
         |  D:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|class C extends B with
         |
         |  // c
         |
         |  D { def f = 1 }
         |""".stripMargin,
    )("beg Init, Type.Name: // c")
  }

  test("derives: comma, blank line, DSLC, blank line, type class") {
    val code =
      """|class C derives A,
         |
         |  // c
         |
         |  B:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|class C derives A,
         |
         |  // c
         |
         |  B { def f = 1 }
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("enum: case comma, blank line, DSLC, blank line, case") {
    val code =
      """|enum E:
         |  case A,
         |
         |    // c
         |
         |    B
         |""".stripMargin
    checkComments(
      code,
      """|enum E {
         |  case A,
         |
         |    // c
         |
         |    B
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("args: comma, blank line, DSLC, blank line, arg") {
    val code =
      """|object O {
         |  f(
         |    a,
         |
         |    // c
         |
         |    b
         |  )
         |}
         |""".stripMargin
    checkComments(code)("beg Term.Name: // c")
  }

  test("extends: comma, DSLC, parent") {
    val code =
      """|class C extends B,
         |  // c
         |  D:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|class C extends B with
         |  // c
         |  D { def f = 1 }
         |""".stripMargin,
    )("beg Init, Type.Name: // c")
  }

  test("enum case: extends comma, blank line, DSLC, blank line, parent") {
    val code =
      """|enum E:
         |  case A extends B,
         |
         |    // c
         |
         |    C
         |""".stripMargin
    checkComments(
      code,
      """|enum E {
         |  case A extends B,
         |
         |    // c
         |
         |    C
         |}
         |""".stripMargin,
    )("beg Init, Type.Name: // c")
  }

  test("enum case: extends comma, DSLC, parent") {
    val code =
      """|enum E:
         |  case A extends B,
         |    // c
         |    C
         |""".stripMargin
    checkComments(
      code,
      """|enum E {
         |  case A extends B,
         |    // c
         |    C
         |}
         |""".stripMargin,
    )("beg Init, Type.Name: // c")
  }

  test("derives: comma, DSLC, type class") {
    val code =
      """|class C derives A,
         |  // c
         |  B:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|class C derives A,
         |  // c
         |  B { def f = 1 }
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("enum: case comma, DSLC, case") {
    val code =
      """|enum E:
         |  case A,
         |    // c
         |    B
         |""".stripMargin
    checkComments(
      code,
      """|enum E {
         |  case A,
         |    // c
         |    B
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("given alias: DSLC before the closing bracket") {
    val code =
      """|object O {
         |  given Ord[Int
         |  // c
         |  ] = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given Ord[Int] = ??? }
         |""".stripMargin,
    )()
  }

  test("given: colon body, DSLC before the closing bracket") {
    val code =
      """|object O {
         |  given Ord[Int
         |  // c
         |  ]:
         |    def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given Ord[Int] { def f = 1 } }
         |""".stripMargin,
    )()
  }

  test("given: named, DSLC before the closing bracket") {
    val code =
      """|object O {
         |  given x: Ord[Int
         |  // c
         |  ] with {}
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given x: Ord[
         |    Int
         |    // c
         |  ] with {}
         |}
         |""".stripMargin,
    )("end Type.Name: // c")
  }

  test("type args: DSLC before the closing bracket") {
    val code =
      """|object O {
         |  val x: Ord[Int
         |  // c
         |  ] = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val x: Ord[
         |    Int
         |    // c
         |  ] = ???
         |}
         |""".stripMargin,
    )("end Type.Name: // c")
  }

  test("given: tuple type, DSLC before the closing paren") {
    val code =
      """|object O {
         |  given (Int, String
         |  // c
         |  ) = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given (
         |    Int, String
         |    // c
         |  ) = ???
         |}
         |""".stripMargin,
    )("end Type.Name: // c")
  }

  test("given: arrow, tuple type, DSLC before the closing paren") {
    val code =
      """|object O {
         |  given (Int, String
         |  // c
         |  ) => Ord[Int] = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given (
         |    Int, String
         |    // c
         |  ) => Ord[Int] = ???
         |}
         |""".stripMargin,
    )("end Term.Param, Type.Name: // c")
  }

  test("given: arrow, type params, DSLC before the closing bracket") {
    val code =
      """|object O {
         |  given [T
         |  // c
         |  ] => Ord[T] = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given [
         |    T
         |    // c
         |    // c
         |    // c
         |  ]: Ord[T] = ???
         |}
         |""".stripMargin,
      reprinted = Seq("end Type.Param, Type.Name: // c\n    // c\n    // c"),
    )("end Type.Param: // c", "beg Type.ParamClause: // c", "beg Type.Bounds: // c")
  }

  test("given: type args, MLC before the closing bracket") {
    val code =
      """|object O {
         |  given Ord[Int /* c */ ] with {}
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given Ord[Int /* c */ ] with {} }
         |""".stripMargin,
    )("end Type.Name: /* c */")
  }

  test("if: condition in parens, MLC, args, then") {
    val code =
      """|object O {
         |  if (a) /* c */ (b) then 1 else 2
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { if (a(b)) 1 else 2 }
         |""".stripMargin,
    )()
  }

  test("if: condition in parens, MLC, body in parens") {
    val code =
      """|object O {
         |  if (a) /* c */ (b) else c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { if (a) b else c }
         |""".stripMargin,
    )()
  }

  test("if: condition in parens, DSLC, body in braces") {
    val code =
      """|object O {
         |  if (a) // c
         |  { b } else c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  if (a) // c
         |    {
         |      b
         |    }
         |  else c
         |}
         |""".stripMargin,
    )("beg Term.Block: // c")
  }

  test("while: condition in parens, MLC, body in parens") {
    val code =
      """|object O {
         |  while (a) /* c */ (b)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { while (a) b }
         |""".stripMargin,
    )()
  }

  test("while: condition in parens, MLC, args, do") {
    val code =
      """|object O {
         |  while (a) /* c */ (b) do c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { while (a(b)) c }
         |""".stripMargin,
    )()
  }

  test("for: tuple pattern in parens, MLC, generator") {
    val code =
      """|object O {
         |  for (a, b) /* c */ <- xs yield a
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { for ((a, b) /* c */ <- xs) yield a }
         |""".stripMargin,
    )("end Pat.Tuple: /* c */")
  }

  test("for: tuple pattern in parens, MLC inside") {
    val code =
      """|object O {
         |  for (a, /* c */ b) <- xs yield a
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { for ((a, /* c */ b) <- xs) yield a }
         |""".stripMargin,
    )("beg Pat.Var, Term.Name: /* c */")
  }

  test("for: enumerators in parens, DSLC, body") {
    val code =
      """|object O {
         |  for (x <- xs) // c
         |    f(x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  for (x <- xs)
         |    // c
         |    f(x)
         |}
         |""".stripMargin,
    )("beg Term.Apply, Term.Name: // c")
  }

  test("pattern: wildcard, star, MLC, infix argument") {
    val code =
      """|object O {
         |  x match { case _ * /* c */ y => 1 }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case _ * /* c */ y => 1
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c */")
  }

  test("pattern: wildcard, star, DSLC, infix argument") {
    val code =
      """|object O {
         |  x match {
         |    case _ *
         |      // c
         |      y => 1
         |  }
         |}
         |""".stripMargin
    checkComments(code)("beg Pat.ArgClause, Pat.Var, Term.Name: // c")
  }

  test("given: named, type in parens, DSLC before the closing paren") {
    val code =
      """|object O {
         |  given x: (Int
         |  // c
         |  ) = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given x: Int
         |    // c
         |    = ???
         |}
         |""".stripMargin,
    )("end Type.Name: // c")
  }

  test("given: named, type in parens, MLC before the closing paren") {
    val code =
      """|object O {
         |  given x: (Int /* c */ ) = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given x: Int /* c */ = ??? }
         |""".stripMargin,
    )("end Type.Name: /* c */")
  }

  test("given: named, tuple type, DSLC before the closing paren") {
    val code =
      """|object O {
         |  given x: (Int, String
         |  // c
         |  ) = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given x: (
         |    Int, String
         |    // c
         |  ) = ???
         |}
         |""".stripMargin,
    )("end Type.Name: // c")
  }

  test("given: named, typed param in parens, DSLC before the closing paren") {
    val code =
      """|object O {
         |  given x: (a: Int
         |  // c
         |  ) = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given x: (
         |    a: Int
         |    // c
         |  ) = ???
         |}
         |""".stripMargin,
    )("end Type.TypedParam, Type.Name: // c")
  }

  test("given: type params, arrow, type in parens, DSLC before the closing paren") {
    val code =
      """|object O {
         |  given [T] => (Int
         |  // c
         |  ) = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given [T]: Int
         |    // c
         |    = ???
         |}
         |""".stripMargin,
    )("end Type.Name: // c")
  }

  test("given: type params, arrow, type in parens, MLC before the closing paren") {
    val code =
      """|object O {
         |  given [T] => (Int /* c */ ) = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given [T]: Int /* c */ = ??? }
         |""".stripMargin,
    )("end Type.Name: /* c */")
  }

  test("context bounds in braces: MLC after a comma") {
    val code =
      """|object O {
         |  def f[T: {A, /* c */ B}] = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f[T: {A, /* c */ B}] = 1 }
         |""".stripMargin,
    )("beg Type.Name: /* c */")
  }

  test("context bounds in braces: comma, DSLC, bound") {
    val code =
      """|object O {
         |  def f[T: {A,
         |    // c
         |    B}] = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f[
         |    T: {A,
         |      // c
         |      B}
         |  ] = 1
         |}
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("context bounds in braces: comma, blank line, DSLC, blank line, bound") {
    val code =
      """|object O {
         |  def f[T: {A,
         |
         |    // c
         |
         |    B}] = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f[
         |    T: {A,
         |
         |      // c
         |
         |      B}
         |  ] = 1
         |}
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("context bounds: MLC before the second colon") {
    val code =
      """|object O {
         |  def f[T: A /* c */ : B] = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f[T: A /* c */: B] = 1 }
         |""".stripMargin,
    )("end Type.Name: /* c */")
  }

  test("context bounds: DSLC after the second colon") {
    val code =
      """|object O {
         |  def f[T: A: // c
         |    B] = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f[
         |    T: A: // c
         |      B
         |  ] = 1
         |}
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("capture set: MLC after a comma") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object O {
         |  val l: Logger^{fs, /* c */ io} = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { val l: Logger^{fs, /* c */ io} = ??? }
         |""".stripMargin,
    )("beg Term.Name: /* c */")
  }

  test("capture set: comma, DSLC, capability") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object O {
         |  val l: Logger^{fs,
         |    // c
         |    io} = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val l: Logger^{fs,
         |    // c
         |    io} = ???
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("capture set: comma, blank line, DSLC, blank line, capability") {
    implicit val dialect: Dialect = dialects.Scala3Future
    val code =
      """|object O {
         |  val l: Logger^{fs,
         |
         |    // c
         |
         |    io} = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val l: Logger^{fs,
         |
         |    // c
         |
         |    io} = ???
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("quoted type block: MLC after a semicolon") {
    val code =
      """|object O {
         |  '[ type t = Int; /* c */ List[t] ]
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { '[ type t = Int; /* c */ List[t] ] }
         |""".stripMargin,
    )("beg Type.Apply, Type.Name: /* c */")
  }

  test("quoted type block: semicolon, DSLC, type") {
    val code =
      """|object O {
         |  '[ type t = Int;
         |    // c
         |    List[t] ]
         |}
         |""".stripMargin
    checkComments(code)("beg Type.Apply, Type.Name: // c")
  }

  test("end marker: if, DSLC, blank line, statement") {
    val code =
      """|object O:
         |  if a then
         |    b
         |  end if // c
         |
         |  val x = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  if (a) b
         |  end if // c
         |
         |  val x = 1
         |}
         |""".stripMargin,
    )("end Term.EndMarker, Term.Name: // c")
  }

  test("end marker: if, DSLC, statement") {
    val code =
      """|object O:
         |  if a then
         |    b
         |  end if // c
         |  val x = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  if (a) b
         |  end if // c
         |  val x = 1
         |}
         |""".stripMargin,
    )("end Term.EndMarker, Term.Name: // c")
  }

  test("end marker: if, MLC, statement") {
    val code =
      """|object O:
         |  if a then
         |    b
         |  end if /* c */
         |  val x = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  if (a) b
         |  end if /* c */
         |  val x = 1
         |}
         |""".stripMargin,
    )("end Term.EndMarker, Term.Name: /* c */")
  }

  test("end marker: while, DSLC, blank line, statement") {
    val code =
      """|object O:
         |  while a do
         |    b
         |  end while // c
         |
         |  val x = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  while (a) b
         |  end while // c
         |
         |  val x = 1
         |}
         |""".stripMargin,
    )("end Term.EndMarker, Term.Name: // c")
  }

  test("end marker: match, DSLC, blank line, statement") {
    val code =
      """|object O:
         |  x match
         |    case _ => 1
         |  end match // c
         |
         |  val y = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case _ => 1
         |  }
         |  end match // c
         |
         |  val y = 1
         |}
         |""".stripMargin,
    )("end Term.EndMarker, Term.Name: // c")
  }

  test("end marker: try, DSLC, blank line, statement") {
    val code =
      """|object O:
         |  try
         |    b
         |  finally
         |    c
         |  end try // c
         |
         |  val x = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  try b finally c
         |  end try // c
         |
         |  val x = 1
         |}
         |""".stripMargin,
    )("end Term.EndMarker, Term.Name: // c")
  }

  test("end marker: for, DSLC, blank line, statement") {
    val code =
      """|object O:
         |  for
         |    x <- xs
         |  do
         |    f(x)
         |  end for // c
         |
         |  val y = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  for (x <- xs) f(x)
         |  end for // c
         |
         |  val y = 1
         |}
         |""".stripMargin,
    )("end Term.EndMarker, Term.Name: // c")
  }

  test("end marker: extension, DSLC, blank line, statement") {
    val code =
      """|object O:
         |  extension (x: Int)
         |    def f = 1
         |  end extension // c
         |
         |  val y = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  extension (x: Int) def f = 1
         |  end extension // c
         |
         |  val y = 1
         |}
         |""".stripMargin,
    )("end Term.EndMarker, Term.Name: // c")
  }

  test("end marker: def, DSLC, blank line, statement") {
    val code =
      """|object O:
         |  def f =
         |    1
         |  end f // c
         |
         |  val y = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f = 1
         |  end f // c
         |
         |  val y = 1
         |}
         |""".stripMargin,
    )("end Term.EndMarker, Term.Name: // c")
  }

  test("match type: MLC before the brace") {
    val code =
      """|object O {
         |  type M[X] = X match /* c */ { case Int => String }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  type M[X] = X match /* c */ {
         |    case Int => String
         |  }
         |}
         |""".stripMargin,
    )("beg Type.CasesBlock: /* c */")
  }

  test("match type: MLC on its own line before the brace") {
    val code =
      """|object O {
         |  type M[X] = X match
         |  /* c */ {
         |    case Int => String
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  type M[X] = X match
         |    /* c */ {
         |      case Int => String
         |    }
         |}
         |""".stripMargin,
    )("beg Type.CasesBlock: /* c */")
  }

  test("return: DSLC, blank line, deeper expression") {
    val code =
      """|object O {
         |  def f: Int = {
         |    return // c
         |
         |      1
         |  }
         |}
         |""".stripMargin
    checkComments(code)("beg Lit.Int: // c")
  }

  test("context function type: MLC on the next line") {
    val code =
      """|object O {
         |  val f: Int ?=>
         |  /* c */  Int = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val f: Int ?=>
         |  /* c */ Int = ???
         |}
         |""".stripMargin,
    )("beg Type.Name: /* c */")
  }

  test("function type: MLC on the next line") {
    val code =
      """|object O {
         |  val f: Int =>
         |  /* c */  Int = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val f: Int =>
         |  /* c */ Int = ???
         |}
         |""".stripMargin,
    )("beg Type.Name: /* c */")
  }

  test("import: rename arrow, MLC on the next line") {
    val code =
      """|import a.{x =>
         |/* c */  y}
         |""".stripMargin
    checkComments(
      code,
      """|import a.{x =>
         |/* c */ y}
         |""".stripMargin,
    )("beg Name.Indeterminate: /* c */")
  }

  test("for: generator, DSLC, guard") {
    val code =
      """|object O {
         |  for {
         |    x <- // c
         |      xs
         |    if x > 1
         |  } yield x
         |}
         |""".stripMargin
    checkComments(code)("beg Term.Name: // c")
  }

  test("object: DSLC, blank line, colon body") {
    val code =
      """|object O // c
         |
         |:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|object O // c
         |  { def f = 1 }
         |""".stripMargin,
    )("end Term.Name: // c")
  }

  test("given: using, colon, MLC on the next line, type") {
    val code =
      """|object O {
         |  given [T](using Ord[T]):
         |  /* c */  Ord[List[T]] = ???
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given [T](using Ord[T]):
         |  /* c */ Ord[List[T]] = ???
         |}
         |""".stripMargin,
    )("beg Type.Apply, Type.Name: /* c */")
  }

  test("given: named, colon, MLC on the next line, type with body") {
    val code =
      """|object O {
         |  given intOrd:
         |  /* c */  Ord[Int] with
         |    def compare = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given intOrd:
         |  /* c */ Ord[Int] with { def compare = 1 }
         |}
         |""".stripMargin,
    )("beg Template, Init, Type.Apply, Type.Name: /* c */")
  }

  test("by-name param: MLC on the next line, type") {
    val code =
      """|object O {
         |  def f(x: =>
         |  /* c */  Int) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f(
         |    x: =>
         |    /* c */ Int
         |  ) = 1
         |}
         |""".stripMargin,
    )("beg Type.Name: /* c */")
  }

  test("by-name param: type on the next line") {
    val code =
      """|object O {
         |  def f(x: =>
         |    Int) = 1
         |}
         |""".stripMargin
    runTestError[Source](code, "`identifier` expected but `indent` found")
  }

  test("extends: blank line, DSLC, blank line, comma, parent") {
    val code =
      """|class A extends B
         |
         |// c
         |
         |, C
         |""".stripMargin
    checkComments(
      code,
      """|class A extends B
         |
         |  // c
         |
         |  with C
         |""".stripMargin,
    )("beg Name.Anonymous: // c", "beg Init, Type.Name: // c")
  }

  test("extends: comma, DSLC, blank line, parent") {
    val code =
      """|class A extends B,
         |// c
         |
         |  C
         |""".stripMargin
    checkComments(
      code,
      """|class A extends B
         |// c
         |
         |  with C
         |""".stripMargin,
      reprinted = Seq("beg Name.Anonymous: // c", "beg Init, Type.Name: // c"),
      twice =
        """|class A extends B
           |  // c
           |
           |  with C
           |""".stripMargin,
    )("end Init: // c")
  }

  test("derives: blank line, DSLC, blank line, comma") {
    val code =
      """|case class A(x: Int) derives Eq
         |
         |// c
         |
         |, Show
         |""".stripMargin
    checkComments(
      code,
      """|case class A(x: Int) derives Eq,
         |
         |  // c
         |
         |  Show
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("enum: case, blank line, DSLC, blank line, comma, case") {
    val code =
      """|enum E {
         |  case A
         |
         |// c
         |
         |, B
         |}
         |""".stripMargin
    checkComments(
      code,
      """|enum E {
         |  case A,
         |
         |    // c
         |
         |    B
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("context bounds in braces: blank line, DSLC, blank line, comma") {
    val code =
      """|object O {
         |  def f[T: {A
         |
         |// c
         |
         |, B}] = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f[
         |    T: {A,
         |
         |      // c
         |
         |      B}
         |  ] = 1
         |}
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("args: DSLC before the colon and the wildcard") {
    val code =
      """|object O {
         |  g(xs
         |    // c
         |    : _*)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  g(
         |    xs
         |    // c
         |    *
         |  )
         |}
         |""".stripMargin,
      reprinted = Seq(),
      twice =
        """|object O { g(xs*) }
           |""".stripMargin,
    )("end Term.Name: // c")
  }

  test("args: DSLC before the star") {
    val code =
      """|object O {
         |  def g(xs: Int
         |  // c
         |  *) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def g(xs: Int*) = 1 }
         |""".stripMargin,
    )()
  }

  test("import: given, blank line, DSLC, blank line, comma, wildcard") {
    val code =
      """|import a.{given
         |
         |// c
         |
         |, *}
         |""".stripMargin
    checkComments(
      code,
      """|import a.{
         |  given,
         |
         |  // c
         |
         |  *
         |}
         |""".stripMargin,
    )("beg Importee.Wildcard: // c")
  }

  test("import: rename arrow, DSLC, new name") {
    val code =
      """|import a.{x => // c
         |  y}
         |""".stripMargin
    runTestError[Source](code, "`identifier` expected but `indent` found")
  }

  test("import: unimport arrow, MLC on the next line") {
    val code =
      """|import a.{x =>
         |/* c */  _}
         |""".stripMargin
    checkComments(
      code,
      """|import a.{x as _}
         |""".stripMargin,
    )()
  }

  test("select match: MLC before the brace") {
    val code =
      """|object O {
         |  x.match /* c */ { case _ => 1 }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x.match /* c */ {
         |    case _ => 1
         |  }
         |}
         |""".stripMargin,
    )("beg Term.CasesBlock: /* c */")
  }

  test("select match: MLC on its own line before the brace") {
    val code =
      """|object O {
         |  x.match
         |  /* c */ {
         |    case _ => 1
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x.match
         |    /* c */ {
         |      case _ => 1
         |    }
         |}
         |""".stripMargin,
    )("beg Term.CasesBlock: /* c */")
  }

  test("match: MLC before the brace") {
    val code =
      """|object O {
         |  x match /* c */ { case _ => 1 }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match /* c */ {
         |    case _ => 1
         |  }
         |}
         |""".stripMargin,
    )("beg Term.CasesBlock: /* c */")
  }

  test("derives: DSLC after the last parent, blank line, derives") {
    val code =
      """|class A extends B
         |  // c
         |
         |  derives C
         |""".stripMargin
    checkComments(code)("beg Name.Anonymous: // c", "beg Type.Name: // c")
  }

  test("derives: last parent, DSLC on its own line, derives") {
    val code =
      """|class A extends B
         |// c
         |derives C
         |""".stripMargin
    checkComments(
      code,
      """|class A extends B
         |  // c
         |  derives C
         |""".stripMargin,
    )("beg Name.Anonymous: // c", "beg Type.Name: // c")
  }

  test("derives: comma, DSLC, blank line, type class") {
    val code =
      """|class C derives A,
         |// c
         |
         |  B:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|class C derives A,
         |// c
         |
         |  B { def f = 1 }
         |""".stripMargin,
    )("end Type.Name: // c")
  }

  test("enum: case comma, DSLC, blank line, case") {
    val code =
      """|enum E:
         |  case A,
         |  // c
         |
         |    B
         |""".stripMargin
    checkComments(
      code,
      """|enum E {
         |  case A,
         |  // c
         |
         |    B
         |}
         |""".stripMargin,
    )("end Term.Name: // c")
  }

  test("context bounds in braces: comma, DSLC, blank line, bound") {
    val code =
      """|object O {
         |  def f[T: {A,
         |  // c
         |
         |    B}] = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f[
         |    T: {A,
         |    // c
         |
         |      B}
         |  ] = 1
         |}
         |""".stripMargin,
    )("end Type.Name: // c")
  }

  test("enum case: extends comma, DSLC, blank line, parent") {
    val code =
      """|enum E:
         |  case A extends B,
         |  // c
         |
         |    C
         |""".stripMargin
    checkComments(
      code,
      """|enum E {
         |  case A extends B,
         |  // c
         |
         |    C
         |}
         |""".stripMargin,
    )("end Init: // c")
  }

  test("given alias: DSLC after the equals, body") {
    val code =
      """|object O {
         |  given A = // c
         |    a
         |}
         |""".stripMargin
    checkComments(code)("beg Term.Name: // c")
  }

  test("given alias: equals, DSLC on its own line, body") {
    val code =
      """|object O {
         |  given A =
         |    // c
         |    a
         |}
         |""".stripMargin
    checkComments(code)("beg Term.Name: // c")
  }

  test("given alias: equals, MLC on its own line, body") {
    val code =
      """|object O {
         |  given A =
         |    /* c */
         |    a
         |}
         |""".stripMargin
    checkComments(code)("beg Term.Name: /* c */")
  }

  test("braceless if: MLC at the start of the line before else, body on the next line") {
    val code =
      """|object O:
         |  def f =
         |    if a then b
         |    /* c */ else
         |      x
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = if (a) b else x }
         |""".stripMargin,
    )()
  }

  test("braceless if: two MLCs at the start of the line before else, body on the next line") {
    val code =
      """|object O:
         |  def f =
         |    if a then b
         |    /* c */ /* d */ else
         |      x
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = if (a) b else x }
         |""".stripMargin,
    )()
  }

  test("braceless if: MLC at the start of the line before then, body on the next line") {
    val code =
      """|object O:
         |  def f =
         |    if a
         |    /* c */ then
         |      x
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = if (a) x }
         |""".stripMargin,
    )()
  }

  test("braceless while: MLC at the start of the line before do, body on the next line") {
    val code =
      """|object O:
         |  def f =
         |    while a
         |    /* c */ do
         |      x
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = while (a) x }
         |""".stripMargin,
    )()
  }

  test("braceless for: MLC at the start of the line before do, body on the next line") {
    val code =
      """|object O:
         |  def f =
         |    for x <- xs
         |    /* c */ do
         |      x
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = for (x <- xs) x }
         |""".stripMargin,
    )()
  }

  test("braceless for: MLC at the start of the line before yield, body on the next line") {
    val code =
      """|object O:
         |  def f =
         |    for x <- xs
         |    /* c */ yield
         |      x
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = for (x <- xs) yield x }
         |""".stripMargin,
    )()
  }

  test("braceless try: MLC at the start of the line before finally, body on the next line") {
    val code =
      """|object O:
         |  def f =
         |    try a
         |    /* c */ finally
         |      x
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = try a finally x }
         |""".stripMargin,
    )()
  }

  test("braceless if: parens, MLC at the start of the line before else, body on the next line") {
    val code =
      """|object O:
         |  def f =
         |    if (a) b
         |    /* c */ else
         |      x
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = if (a) b else x }
         |""".stripMargin,
    )()
  }

  test("braceless while: block condition, MLC at the start of the line before do") {
    val code =
      """|object O:
         |  def w(): Unit =
         |    while
         |      val done = poll()
         |      !done
         |    /* p3 */ do
         |      Thread.sleep(10)
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def w(): Unit = while ({
         |    val done = poll()
         |    !done
         |  }) Thread.sleep(10)
         |}
         |""".stripMargin,
    )()
  }

  test("braceless if: MLC at the start of the line before else, two statements") {
    val code =
      """|object O:
         |  def f =
         |    if a then b
         |    /* c */ else
         |      x
         |      y
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f = if (a) b else {
         |    x
         |    y
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("package object: MLC between package and object") {
    val code =
      """|package /* c */ object util:
         |  type Id[A] = A
         |""".stripMargin
    checkComments(
      code,
      """|package object util { type Id[A] = A }
         |""".stripMargin,
    )()
  }

  test("given: DSLC before the closing bracket") {
    val code =
      """|object O {
         |  given Ord[Int
         |  // c
         |  ] with {}
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given Ord[Int] with {} }
         |""".stripMargin,
    )()
  }

  test("class: DSLC before extends, DSLC before the colon") {
    val code =
      """|class A
         |  // c1
         |  extends B
         |// c2
         |:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|class A
         |// c1
         |// c1
         |  // c1
         |  extends B
         |  // c2
         |  {
         |    def f = 1
         |  }
         |""".stripMargin,
      reprinted = Seq(
        "beg Type.ParamClause: // c1\n// c1\n  // c1",
        "beg Ctor.Primary, Name.Anonymous: // c1\n// c1\n  // c1",
        "beg Template, Init, Type.Name: // c1\n// c1\n  // c1",
        "beg Name.Anonymous: // c2",
        "beg Template.Body: // c2",
      ),
      twice =
        """|class A
           |// c1
           |// c1
           |// c1
           |// c1
           |// c1
           |// c1
           |  // c1
           |  // c1
           |  // c1
           |  extends B
           |  // c2
           |  {
           |    def f = 1
           |  }
           |""".stripMargin,
    )(
      "beg Type.ParamClause: // c1",
      "beg Ctor.Primary, Name.Anonymous: // c1",
      "beg Template, Init, Type.Name: // c1",
      "beg Name.Anonymous: // c2",
      "beg Template.Body: // c2",
    )
  }

  test("class: DSLC before derives, DSLC before the colon") {
    val code =
      """|class A extends B
         |  // c1
         |  derives D
         |// c2
         |:
         |  def f = 1
         |""".stripMargin
    checkComments(
      code,
      """|class A extends B
         |  // c1
         |  derives D
         |// c2
         |{
         |  def f = 1
         |}
         |""".stripMargin,
    )("beg Name.Anonymous: // c1", "beg Template.Body: // c2", "beg Type.Name: // c1")
  }

  test("val: DSLC, colon at the end of its line, type") {
    val code =
      """|object O {
         |  val x
         |  // c
         |  :
         |    Int = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val x:
         |    // c
         |    Int = 1
         |}
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("def: DSLC, colon at the end of its line, type") {
    val code =
      """|object O {
         |  def f
         |  // c
         |  :
         |    Int = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f:
         |    // c
         |    Int = 1
         |}
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("decl: DSLC, colon at the end of its line, type") {
    val code =
      """|trait O {
         |  def f
         |  // c
         |  :
         |    Int
         |}
         |""".stripMargin
    checkComments(
      code,
      """|trait O {
         |  def f:
         |    // c
         |    Int
         |}
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("param: DSLC, colon at the end of its line, type") {
    val code =
      """|class O(x
         |  // c
         |  :
         |    Int)
         |""".stripMargin
    checkComments(
      code,
      """|class O(
         |  x:
         |    // c
         |    Int
         |)
         |""".stripMargin,
    )("beg Type.Name: // c")
  }

  test("ascription: DSLC, colon at the end of its line, fewer braces") {
    val code =
      """|object O {
         |  val y = (x
         |  // c
         |  :
         |    Int)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val y = x
         |    // c
         |    {
         |      Int
         |    }
         |}
         |""".stripMargin,
    )("beg Term.ArgClause, Term.Block: // c")
  }

  test("fewer braces: DSLC, colon at the end of its line, lambda") {
    val code =
      """|object O {
         |  xs.foreach
         |  // c
         |  :
         |    x => println(x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  xs.foreach
         |    // c
         |    {
         |      x => println(x)
         |    }
         |}
         |""".stripMargin,
    )("beg Term.ArgClause, Term.Block: // c")
  }

  test("given: DSLC, colon at the end of its line, body") {
    val code =
      """|object O {
         |  given A
         |  // c
         |  :
         |    def f = 1
         |}
         |""".stripMargin
    checkComments(code)("beg Name.Anonymous: // c", "beg Template.Body: // c")
  }

  test("val: DSLC, colon and type on one line") {
    val code =
      """|object O {
         |  val x
         |  // c
         |  : Int = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val x
         |    // c
         |    : Int = 1
         |}
         |""".stripMargin,
    )("end Pat.Var, Term.Name: // c")
  }

  test("given: parameters, DSLC, colon at the end of its line, body") {
    val code =
      """|object O {
         |  given [T] => C[T]
         |  // c
         |  :
         |    def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  given [T]: C[T] with
         |    // c
         |    { def f = 1 }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Template.Body: // c"),
    )("beg Name.Anonymous: // c", "beg Template.Body: // c")
  }

  test("given: named, DSLC, colon at the end of its line, body") {
    val code =
      """|object O {
         |  given x: A
         |  // c
         |  :
         |    def f = 1
         |}
         |""".stripMargin
    checkComments(code)("beg Name.Anonymous: // c", "beg Template.Body: // c")
  }

  test("given: ASLC before the brace") {
    val code =
      """|object O {
         |  given A /* c */ {
         |    def f = 1
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given A /* c */ { def f = 1 } }
         |""".stripMargin,
    )("end Init, Type.Name: /* c */")
  }

  test("given: parameters, ASLC before the brace") {
    val code =
      """|object O {
         |  given [T] => C[T] /* c */ {
         |    def f = 1
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given [T]: C[T] /* c */ with { def f = 1 } }
         |""".stripMargin,
    )("end Init, Type.Apply, Type.ArgClause: /* c */")
  }

  test("given: ASLC before an empty body") {
    val code =
      """|object O {
         |  given A /* c */ {}
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { given A /* c */ {} }
         |""".stripMargin,
    )("end Init, Type.Name: /* c */")
  }

  test("given: DSLC before the brace") {
    val code =
      """|object O {
         |  given A // c
         |  {
         |    def f = 1
         |  }
         |}
         |""".stripMargin
    checkComments(code)("end Decl.GivenAnonymous, Type.Name: // c")
  }

  test("object: ASLC before the brace") {
    val code =
      """|object O /* c */ {
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O /* c */ { def f = 1 }
         |""".stripMargin,
    )("end Term.Name: /* c */")
  }

  test("object: ASLC before an empty body") {
    val code =
      """|object O /* c */ {}
         |""".stripMargin
    checkComments(
      code,
      """|object O /* c */
         |""".stripMargin,
      reprinted = Seq("end Defn.Object, Term.Name: /* c */"),
    )("end Term.Name: /* c */")
  }

  test("given: context parameters, DSLC, colon at the end of its line, body") {
    val code =
      """|object O {
         |  given [T] => (o: Ord[T]) => C[T]
         |  // c
         |  :
         |    def f = 1
         |}
         |""".stripMargin
    checkComments(code)("beg Name.Anonymous: // c", "beg Template.Body: // c")
  }

  test("given: using clause, DSLC, colon at the end of its line, body") {
    val code =
      """|object O {
         |  given [T](using o: Ord[T]): C[T]
         |  // c
         |  :
         |    def f = 1
         |}
         |""".stripMargin
    runTestError[Source](code, "`;` expected but `:` found")
  }

  test("given: DSLC, colon at the end of its line, self, body") {
    val code =
      """|object O {
         |  given A
         |  // c
         |  :
         |    self =>
         |    def f = 1
         |}
         |""".stripMargin
    checkComments(code)("beg Name.Anonymous: // c", "beg Template.Body: // c")
  }

  test("given: DSLC, colon at the end of its line, self") {
    val code =
      """|object O {
         |  given A
         |  // c
         |  :
         |    self =>
         |}
         |""".stripMargin
    checkComments(code)("beg Name.Anonymous: // c", "beg Template.Body: // c")
  }

}
