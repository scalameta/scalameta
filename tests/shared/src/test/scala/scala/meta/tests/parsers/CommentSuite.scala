package scala.meta.tests.parsers

import scala.meta._

class CommentSuite extends ParseSuite {
  implicit val dialect: Dialect = dialects.Scala213

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

  test("case: MLC after a body in parens") {
    val code =
      """|object A {
         |  x match {
         |    case 1 => (a) /* c1 */
         |    case 2 => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a /* c1 */
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c1 */")
  }

  test("case: ASLC after a body in parens") {
    val code =
      """|object A {
         |  x match {
         |    case 1 => (a) // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: // c1")
  }

  test("case: MLC inside and after a body in parens") {
    val code =
      """|object A {
         |  x match {
         |    case 1 => (a /* c1 */) /* c2 */
         |    case 2 => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a /* c1 */ /* c2 */
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("end Term.Name: /* c1 */ /* c2 */"),
    )("end Term.Name: /* c1 */) /* c2 */")
  }

  test("case: MLC after a body in parens, last case") {
    val code =
      """|object A {
         |  x match {
         |    case 1 => a
         |    case 2 => (b) /* c1 */
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a
         |    case 2 => b /* c1 */
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c1 */")
  }

  test("case: MLC after a semicolon, next case on the line") {
    val code =
      """|object A {
         |  x match { case 1 => a; /* c1 */ case 2 => b }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a /* c1 */
         |    /* c1 */ case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c1 */", "beg Case: /* c1 */")
  }

  test("case: MLC after a semicolon, body in parens") {
    val code =
      """|object A {
         |  x match { case 1 => (a); /* c1 */ case 2 => b }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a /* c1 */
         |    /* c1 */ case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c1 */", "beg Case: /* c1 */")
  }

  test("case: MLC after a semicolon, last case") {
    val code =
      """|object A {
         |  x match { case 1 => a; /* c1 */ }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a /* c1 */
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c1 */")
  }

  test("case: MLC after a semicolon, two statements") {
    val code =
      """|object A {
         |  x match { case 1 => a; b; /* c1 */ case 2 => c }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 =>
         |      {
         |        a
         |        b
         |      } /* c1 */
         |    /* c1 */ case 2 =>
         |      c
         |  }
         |}
         |""".stripMargin,
    )("end Term.Block: /* c1 */", "beg Case: /* c1 */")
  }

  test("case: MLC after a semicolon, partial function") {
    val code =
      """|object A {
         |  f { case 1 => a; /* c1 */ case 2 => b }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  f {
         |    case 1 => a /* c1 */
         |    /* c1 */ case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c1 */", "beg Case: /* c1 */")
  }

  test("case: MLC after a semicolon, catch") {
    val code =
      """|object A {
         |  try a catch { case e: E => b; /* c1 */ case _ => c }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  try a catch {
         |    case e: E => b /* c1 */
         |    /* c1 */ case _ => c
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c1 */", "beg Case: /* c1 */")
  }

  test("case: MLC before a semicolon, next case on the line") {
    val code =
      """|object A {
         |  x match { case 1 => a /* c1 */; case 2 => b }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a /* c1 */
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c1 */")
  }

  test("if: semicolon, MLC, else on the same line") {
    val code =
      """|object A {
         |  if (a) b; /* c1 */ else c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { if (a) b else c }
         |""".stripMargin,
    )()
  }

  test("do: semicolon, MLC, while on the same line") {
    val code =
      """|object A {
         |  do b; /* c1 */ while (c)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { do b while (c) }
         |""".stripMargin,
    )()
  }

  test("if: semicolon, ASLC, else on the next line") {
    val code =
      """|object A {
         |  if (a) b; // c1
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

  test("if: MLC after thenp, else on the same line") {
    val code =
      """|object A {
         |  if (a) b /* c1 */ else c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { if (a) b /* c1 */ else c }
         |""".stripMargin,
    )("end Term.Name: /* c1 */")
  }

  test("do: semicolon, ASLC, while on the next line") {
    val code =
      """|object A {
         |  do b; // c1
         |  while (c)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  do b // c1
         |  while (c)
         |}
         |""".stripMargin,
    )("end Term.Name: // c1")
  }

  test("do: MLC after the body, while on the same line") {
    val code =
      """|object A {
         |  do b /* c1 */ while (c)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { do b /* c1 */ while (c) }
         |""".stripMargin,
    )("end Term.Name: /* c1 */")
  }

  test("case: ASLC after a semicolon, next case on the next line") {
    val code =
      """|object A {
         |  x match {
         |    case 1 => a; // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: // c1")
  }

  test("block: semicolon, MLC, semicolon, statement") {
    val code =
      """|object A {
         |  { a; /* c1 */ ; b }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  {
         |    a /* c1 */
         |    b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c1 */")
  }

  test("case: MLC between two semicolons, next case on the line") {
    val code =
      """|object A {
         |  x match { case 1 => a; /* c1 */ ; case 2 => b }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case 1 => a /* c1 */
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("end Term.Name: /* c1 */")
  }

  test("args: comma, MLC, next arg on the same line") {
    val code =
      """|object A {
         |  f(a, /* c1 */ b)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { f(a, /* c1 */ b) }
         |""".stripMargin,
    )("beg Term.Name: /* c1 */")
  }

  test("return: comment after keyword, expr on the next line") {
    val code =
      """|def f = { return // c
         |  1 }
         |""".stripMargin
    val layout =
      """|def f = {
         |  return // c
         |  1
         |}
         |""".stripMargin
    val tree = Defn.Def(
      Nil,
      tname("f"),
      Nil,
      Nil,
      None,
      blk(Term.Return.newBuilder(Lit.Unit()).endComment(Seq("// c")).result(), int(1)),
    )
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

  test("return: semicolon, ASLC, next statement") {
    val code =
      """|object O {
         |  def f: Unit = {
         |    return; // c1
         |    foo
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f: Unit = {
         |    return
         |    // c1
         |    foo
         |  }
         |}
         |""".stripMargin,
    )("beg Term.Name: // c1")
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

  test("import: one importee without braces, DSLC, blank line, statement") {
    val code =
      """|object A {
         |  import a.b
         |  // c
         |
         |  val x = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  import a.b
         |  val x = 1
         |}
         |""".stripMargin,
    )()
  }

  test("import: one importee without braces, ASLC, DSLC, blank line, DSLC, statement") {
    val code =
      """|object A {
         |  import a.b // c1
         |  // c2
         |
         |  // c3
         |  val x = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  import a.b // c1
         |  // c3
         |  val x = 1
         |}
         |""".stripMargin,
    )("end Import, Importer, Importee.Name, Name.Indeterminate: // c1", "beg Defn.Val: // c3")
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

  test("select chain: ASLC after a call, DSLC, next call") {
    val code =
      """|object O {
         |  xs
         |    .map(f) // c1
         |    // c2
         |    .filter(g)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  xs.map(f) // c1
         |  .filter(g)
         |}
         |""".stripMargin,
    )("end Term.Apply, Term.ArgClause: // c1")
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

  test("early initializer: DSLC before with") {
    val code =
      """|class A extends {
         |  val x = 1
         |}
         |  // c1
         |  with B
         |""".stripMargin
    checkComments(
      code,
      """|class A extends {
         |  val x = 1
         |} with B
         |""".stripMargin,
    )()
  }

  test("early initializer: ASLC before with") {
    val code =
      """|class A extends {
         |  val x = 1
         |} // c1
         |  with B
         |""".stripMargin
    checkComments(
      code,
      """|class A extends {
         |  val x = 1
         |} // c1
         | with B
         |""".stripMargin,
    )("end Stat.Block: // c1")
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

  test("do: DSLC before while") {
    val code =
      """|object O {
         |  def f =
         |    do x
         |    // c
         |    while (a)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = do x while (a) }
         |""".stripMargin,
    )()
  }

  test("do: DSLC indented under the body, while") {
    val code =
      """|object O {
         |  def f =
         |    do
         |      x
         |      // c
         |    while (a)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = do x while (a) }
         |""".stripMargin,
    )()
  }

  test("do: while, DSLC inside the parentheses") {
    val code =
      """|object O {
         |  def f =
         |    do x
         |    while (
         |      // c
         |      a)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f = do x while (
         |  // c
         |  a)
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("if: MLC before else on its line") {
    val code =
      """|object O {
         |  def f =
         |    if (a) b
         |    /* c */ else d
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = if (a) b else d }
         |""".stripMargin,
    )()
  }

  test("do: while, MLC before the parentheses") {
    val code =
      """|object O {
         |  def f =
         |    do x
         |    while /* c */ (a)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = do x while (a) }
         |""".stripMargin,
    )()
  }

  test("select: MLC before the dot on its line") {
    val code =
      """|object O {
         |  def f =
         |    x
         |      /* c */ .foo
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = x.foo }
         |""".stripMargin,
    )()
  }

  test("select: MLC after the qualifier, dot on the same line") {
    val code =
      """|object O {
         |  def f =
         |    x /* c */ .foo
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = x /* c */.foo }
         |""".stripMargin,
    )("end Term.Name: /* c */")
  }

  test("if: DSLC before else, ASLC after else") {
    val code =
      """|object O {
         |  def f =
         |    if (foo)
         |      bar
         |    // c2
         |    else // c3
         |      baz
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f = if (foo) bar else // c3
         |    baz
         |}
         |""".stripMargin,
    )("beg Term.Name: // c3")
  }

  test("built if: detached comment on the statement after else") {
    val elsep = tname("d").toBuilder.begComment(detachedComments("// c")).result()
    assertNoDiff(
      Term.If(tname("a"), tname("b"), elsep).reprint,
      """|if (a) b else
         |  // c
         |  d
         |""".stripMargin,
    )
  }

  test("annotation: DSLC on the next line, definition") {
    val code =
      """|object O {
         |  @deprecated
         |  // c1
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { @deprecated def f = 1 }
         |""".stripMargin,
    )()
  }

  test("annotation: ASLC, definition on the next line") {
    val code =
      """|object O {
         |  @deprecated // c1
         |  def f = 1
         |}
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

  test("annotation: ASLC, val on the next line") {
    val code =
      """|object O {
         |  @deprecated // c1
         |  val f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  @deprecated // c1
         |    val f = 1
         |}
         |""".stripMargin,
    )("end Mod.Annot, Init, Type.Name: // c1")
  }

  test("annotation: ASLC, class on the next line") {
    val code =
      """|object O {
         |  @deprecated // c1
         |  class A
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  @deprecated // c1
         |    class A
         |}
         |""".stripMargin,
    )("end Mod.Annot, Init, Type.Name: // c1")
  }

  test("annotation: DSLC between annotations, definition") {
    val code =
      """|object O {
         |  @a
         |  // c1
         |  @b
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  @a
         |    // c1
         |    @b def f = 1
         |}
         |""".stripMargin,
    )("beg Mod.Annot: // c1")
  }

  test("modifier: DSLC on the next line, definition") {
    val code =
      """|object O {
         |  private
         |  // c1
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  private[
         |  // c1
         |  ] def f = 1
         |}
         |""".stripMargin,
      reprintError = "<input>:4: error: `identifier` expected but `]` found",
    )("beg Name.Anonymous: // c1")
  }

  test("modifier: DSLC between modifiers, definition") {
    val code =
      """|object O {
         |  private
         |  // c1
         |  final def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  private[
         |  // c1
         |  ]
         |    // c1
         |    final def f = 1
         |}
         |""".stripMargin,
      reprintError = "<input>:4: error: `identifier` expected but `]` found",
    )("beg Name.Anonymous: // c1", "beg Mod.Final: // c1")
  }

  test("annotation: DSLC before it and after it, definition") {
    val code =
      """|object O {
         |  // c0
         |  @deprecated
         |  // c1
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  // c0
         |  @deprecated def f = 1
         |}
         |""".stripMargin,
    )("beg Defn.Def, Mod.Annot: // c0")
  }

  test("modifier: MLC before it on its line, definition") {
    val code =
      """|object O {
         |  /* c1 */ final def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  /* c1 */ final def f = 1
         |}
         |""".stripMargin,
    )("beg Defn.Def, Mod.Final: /* c1 */")
  }

  test("annotation: ASLC, DSLC, definition") {
    val code =
      """|object O {
         |  @deprecated // c1
         |  // c2
         |  def f = 1
         |}
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

  test("modifier: scaladoc between modifiers, definition") {
    val code =
      """|object O {
         |  final
         |  /** c1 */
         |  protected
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  final
         |    /** c1 */
         |    protected def f = 1
         |}
         |""".stripMargin,
    )("beg Mod.Protected: /** c1 */")
  }

  test("annotation: DSLC indented under it, definition") {
    val code =
      """|object O {
         |  @deprecated
         |    // c1
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { @deprecated def f = 1 }
         |""".stripMargin,
    )()
  }

  test("trait: annotation, DSLC, declaration") {
    val code =
      """|trait T {
         |  @deprecated
         |  // c1
         |  def f: Int
         |}
         |""".stripMargin
    checkComments(
      code,
      """|trait T { @deprecated def f: Int }
         |""".stripMargin,
    )()
  }

  test("annotation: DSLC, class") {
    val code =
      """|object O {
         |  @deprecated
         |  // c1
         |  class C
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { @deprecated class C }
         |""".stripMargin,
    )()
  }

  test("modifier: DSLC, val") {
    val code =
      """|object O {
         |  private
         |  // c1
         |  val x = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  private[
         |  // c1
         |  ] val x = 1
         |}
         |""".stripMargin,
      reprintError = "<input>:4: error: `identifier` expected but `]` found",
    )("beg Name.Anonymous: // c1")
  }

  test("params: annotation, DSLC, parameter") {
    val code =
      """|object O {
         |  def f(
         |      @a
         |      // c1
         |      x: Int) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f(@a
         |    // c1
         |    x: Int) = 1
         |}
         |""".stripMargin,
    )("beg Name.Anonymous: // c1", "beg Term.Name: // c1")
  }

  test("params: annotation, ASLC, parameter") {
    val code =
      """|object O {
         |  def f(
         |      @a // c1
         |      x: Int) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f(@a // c1
         |    x: Int) = 1
         |}
         |""".stripMargin,
    )("end Mod.Annot, Init, Type.Name: // c1")
  }

  test("params: DSLC before implicit, two parameters") {
    val code =
      """|object O {
         |  def f(
         |      // c1
         |      implicit x: Int, y: Int) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f(
         |  // c1
         |  implicit x: Int, y: Int) = 1
         |}
         |""".stripMargin,
    )("beg Term.Param, Mod.Implicit, Term.Param, Mod.Implicit, Mod.Implicit: // c1")
  }

  test("params: implicit, DSLC, two parameters") {
    val code =
      """|object O {
         |  def f(implicit
         |      // c1
         |      x: Int, y: Int) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f(implicit
         |    // c1
         |    x: Int, y: Int) = 1
         |}
         |""".stripMargin,
    )("beg Term.Name: // c1")
  }

  test("params: second clause, annotation, DSLC, parameter") {
    val code =
      """|object O {
         |  def f(x: Int)(
         |      @a
         |      // c1
         |      y: Int) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f(x: Int)(@a
         |    // c1
         |    y: Int) = 1
         |}
         |""".stripMargin,
    )("beg Name.Anonymous: // c1", "beg Term.Name: // c1")
  }

  test("type params: annotation, DSLC, parameter") {
    val code =
      """|object O {
         |  def f[
         |      @a
         |      // c1
         |      T] = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f[@a
         |    // c1
         |    T] = 1
         |}
         |""".stripMargin,
    )("beg Name.Anonymous: // c1", "beg Type.Name: // c1")
  }

  test("lambda in braces: ASLC after the arrow, empty body") {
    val code =
      """|object O {
         |  def g = f { x => // c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def g = f { x =>
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq(),
    )("end Term.Function: // c")
  }

  test("lambda in braces: DSLC under the arrow, empty body") {
    val code =
      """|object O {
         |  def g = f { x =>
         |    // c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def g = f { x =>
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq(),
    )("end Term.Function: // c", "beg Term.Block: // c")
  }

  test("lambda in braces: ASLC after the arrow, body") {
    val code =
      """|object O {
         |  def g = f { x => // c
         |    y
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def g = f {
         |    x => // c
         |      y
         |  }
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("lambda in braces: body, DSLC under it") {
    val code =
      """|object O {
         |  def g = f { x =>
         |    y
         |    // c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def g = f {
         |    x => y
         |    // c
         |  }
         |}
         |""".stripMargin,
    )("end Term.Function: // c")
  }

  test("empty argument clause: DSLC inside") {
    val code =
      """|object O {
         |  f(
         |    // c1
         |  )
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { f() }
         |""".stripMargin,
    )()
  }

  test("empty parameter clause: DSLC inside") {
    val code =
      """|object O {
         |  def f(
         |    // c1
         |  ) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f() = 1 }
         |""".stripMargin,
    )()
  }

  test("import: DSLC between selectors, blank lines around it") {
    val code =
      """|object O {
         |  import a.{
         |    b,
         |
         |    // c1
         |
         |    c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  import a.{
         |    b,
         |    c
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("import: ASLC after a selector's comma, DSLC, next selector") {
    val code =
      """|object O {
         |  import a.{
         |    b, // c1
         |    // c2
         |    c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  import a.{
         |    b // c1
         |,    // c2
         |    c
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("end Importee.Name, Name.Indeterminate: // c1"),
    )("end Importee.Name, Name.Indeterminate: // c1", "beg Importee.Name, Name.Indeterminate: // c2")
  }

  test("secondary ctor: DSLC, blank line, self call") {
    val code =
      """|class A {
         |  def this() = {
         |    // c1
         |
         |    this()
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { def this() = this() }
         |""".stripMargin,
    )()
  }

  test("semicolons: two, then an ASLC") {
    val code =
      """|object O {
         |  a;; // c1
         |  b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  a
         |  // c1
         |  b
         |}
         |""".stripMargin,
    )("beg Term.Name: // c1")
  }

  test("package: only a DSLC") {
    val code =
      """|package p
         |// c1
         |""".stripMargin
    checkComments(
      code,
      """|package p
         |// c1
         |
         |""".stripMargin,
    )("end Pkg: // c1", "beg Pkg.Body: // c1")
  }

  test("match: DSLC before the guard") {
    val code =
      """|object O {
         |  def f =
         |    x match {
         |      case y
         |      // c
         |      if y > 0 => 1
         |    }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f = x match {
         |    case y if y > 0 => 1
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("select: DSLC before the dot") {
    val code =
      """|object O {
         |  def f =
         |    x
         |      // c
         |      .foo
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f = x.foo }
         |""".stripMargin,
    )()
  }

  test("match: empty case body, ASLC, next case") {
    val code =
      """|object O {
         |  x match {
         |    case 1 => // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 1 => // c1
         |    // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Block: // c1", "beg Case: // c1\n    // c1"),
    )("end Case: // c1", "beg Case: // c1")
  }

  test("match: empty case body, DSLC indented under it, next case") {
    val code =
      """|object O {
         |  x match {
         |    case 1 =>
         |      // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 1 =>
         |      // c1
         |      {}
         |    // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("beg Term.Block: // c1", "beg Case: // c1")
  }

  test("lambda: empty body, ASLC after the arrow") {
    val code =
      """|object O {
         |  xs.map { x => // c1
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  xs.map { x =>
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq(),
    )("end Term.Function: // c1")
  }

  test("secondary ctor: MLC before the self call") {
    val code =
      """|class A {
         |  def this(x: Int) = /* c1 */ this()
         |}
         |""".stripMargin
    checkComments(code, "class A { def this(x: Int) = /* c1 */ this() }")(
      "beg Init, Type.Singleton, Term.This, Name.Anonymous: /* c1 */",
    )
  }

  test("match: empty case body, MLC, next case on the same line") {
    val code =
      """|object O {
         |  x match { case 1 => /* c1 */ case 2 => b }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 1 => /* c1 */
         |    /* c1 */ case 2 => b
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Block: /* c1 */", "beg Case: /* c1 */\n    /* c1 */"),
    )("end Case: /* c1 */", "beg Case: /* c1 */")
  }

  test("catch: empty case body, ASLC, next case") {
    val code =
      """|object O {
         |  try a
         |  catch {
         |    case _: E => // c1
         |    case _ => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  try a catch {
         |    case _: E => // c1
         |    // c1
         |    case _ => b
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Block: // c1", "beg Case: // c1\n    // c1"),
    )("end Case: // c1", "beg Case: // c1")
  }

  test("match: empty case body, unicode arrow, ASLC, next case") {
    val code =
      """|object O {
         |  x match {
         |    case 1 ⇒ // c1
         |    case 2 ⇒ b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 1 => // c1
         |    // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Block: // c1", "beg Case: // c1\n    // c1"),
    )("end Case: // c1", "beg Case: // c1")
  }

  test("match: empty case body, DSLC at the case indentation, next case") {
    val code =
      """|object O {
         |  x match {
         |    case 1 =>
         |    // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 1 =>
         |      // c1
         |      {}
         |    // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("beg Term.Block: // c1", "beg Case: // c1")
  }

  test("match: empty case body, DSLC indented under it, DSLC at the case indentation") {
    val code =
      """|object O {
         |  x match {
         |    case 1 =>
         |      // c1
         |    // c2
         |    case 2 => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 1 =>
         |      // c1
         |      // c2
         |      {}
         |    // c1
         |    // c2
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Block: // c1\n      // c2", "beg Case: // c1\n    // c2"),
    )("beg Term.Block: // c1\n    // c2", "beg Case: // c1\n    // c2")
  }

  test("match: empty case body, blank line, DSLC indented under it, next case") {
    val code =
      """|object O {
         |  x match {
         |    case 1 =>
         |
         |      // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 1 =>
         |      // c1
         |      {}
         |    // c1
         |    case 2 => b
         |  }
         |}
         |""".stripMargin,
    )("beg Term.Block: // c1", "beg Case: // c1")
  }

  test("for: DSLC before the first enumerator") {
    val code =
      """|object O {
         |  for {
         |    // c1
         |    x <- xs
         |  } g(x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  for (
         |  // c1
         |  x <- xs) g(x)
         |}
         |""".stripMargin,
    )("beg Enumerator.Generator, Pat.Var, Term.Name: // c1")
  }

  test("for: DSLC between enumerators") {
    val code =
      """|object O {
         |  for {
         |    x <- xs
         |    // c1
         |    y <- ys
         |  } yield x
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  for (x <- xs; 
         |  // c1
         |  y <- ys) yield x
         |}
         |""".stripMargin,
    )("beg Enumerator.Generator, Pat.Var, Term.Name: // c1")
  }

  test("match: empty case body, DSLC indented under it, DSLC, MLC before the next case") {
    val code =
      """|object O {
         |  foo match {
         |    case a =>
         |      // a1
         |    // b1
         |    /* b2 */ case b =>
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo match {
         |    case a =>
         |      // a1
         |      // b1
         |      /* b2 */ {}
         |    // a1
         |    // b1
         |    /* b2 */ case b =>
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq(
        "beg Term.Block: // a1\n      // b1\n      /* b2 */",
        "beg Case: // a1\n    // b1\n    /* b2 */",
      ),
    )("beg Term.Block: // a1\n    // b1\n    /* b2 */", "beg Case: // a1\n    // b1\n    /* b2 */")
  }

  test("match: empty case body, two DSLCs, next case at their indentation") {
    val code =
      """|object O {
         |  foo match {
         |    case a =>
         |      // a1
         |      // b1
         |      case b =>
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo match {
         |    case a =>
         |      // a1
         |      // b1
         |      {}
         |    // a1
         |    // b1
         |    case b =>
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Block: // a1\n      // b1", "beg Case: // a1\n    // b1"),
    )("beg Term.Block: // a1\n      // b1", "beg Case: // a1\n      // b1")
  }

  test("match: case body, DSLC indented under it, next case") {
    val code =
      """|object O {
         |  foo match {
         |    case a =>
         |      x
         |      // a1
         |    case b => y
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo match {
         |    case a => x
         |    // a1
         |    case b => y
         |  }
         |}
         |""".stripMargin,
    )("beg Case: // a1")
  }

  test("match: case body, blank line, DSLC indented under it, next case") {
    val code =
      """|object O {
         |  foo match {
         |    case a =>
         |      x
         |
         |      // a1
         |    case b => y
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo match {
         |    case a => x
         |    // a1
         |    case b => y
         |  }
         |}
         |""".stripMargin,
    )("beg Case: // a1")
  }

  test("match: case body, DSLC at the case indentation, next case") {
    val code =
      """|object O {
         |  foo match {
         |    case a =>
         |      x
         |    // b1
         |    case b => y
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo match {
         |    case a => x
         |    // b1
         |    case b => y
         |  }
         |}
         |""".stripMargin,
    )("beg Case: // b1")
  }

  test("match: case body, ASLC, DSLC indented under it, next case") {
    val code =
      """|object O {
         |  foo match {
         |    case a => x // a1
         |      // a2
         |    case b => y
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo match {
         |    case a => x // a1
         |    // a2
         |    case b => y
         |  }
         |}
         |""".stripMargin,
    )("end Case, Term.Name: // a1", "beg Case: // a2")
  }

  test("match: case body after the arrow, on two lines, ASLC, DSLC indented under it") {
    val code =
      """|object O {
         |  y match {
         |    case a => foo(
         |        1) // c1
         |      // c2
         |    case b => z
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  y match {
         |    case a =>
         |      foo(1) // c1
         |    // c2
         |    case b =>
         |      z
         |  }
         |}
         |""".stripMargin,
    )("end Case, Term.Apply, Term.ArgClause: // c1", "beg Case: // c2")
  }

  test("match: ASLC after the arrow, DSLC, statement, ASLC, DSLC, next case") {
    val code =
      """|object O {
         |  y match {
         |    case a => // c1
         |      // c2
         |      foo // c3
         |      // c4
         |    case b => z
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  y match {
         |    case a => // c1
         |    // c2
         |      foo // c3
         |    // c4
         |    case b => z
         |  }
         |}
         |""".stripMargin,
      reprinted =
        Seq("end Case, Term.Name: // c3", "beg Term.Name: // c1\n    // c2", "beg Case: // c4"),
    )("end Case, Term.Name: // c3", "beg Term.Name: // c1\n      // c2", "beg Case: // c4")
  }

  test("match: empty case body, ASLC, DSLC indented under it, next case") {
    val code =
      """|object O {
         |  foo match {
         |    case a => // a1
         |      // a2
         |    case b => y
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo match {
         |    case a =>
         |      // a2
         |      {}
         |    // a1
         |    // a2
         |    case b => y
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.Block: // a2", "beg Case: // a1\n    // a2"),
    )("beg Term.Block: // a2", "beg Case: // a1\n      // a2")
  }

  test("match: last case body, DSLC at the case indentation") {
    val code =
      """|object O {
         |  foo match {
         |    case a => x
         |    // c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo match {
         |    case a => x
         |    // c
         |  }
         |}
         |""".stripMargin,
    )("end Case: // c")
  }

  test("match: last case, empty body, DSLC at the case indentation") {
    val code =
      """|object O {
         |  foo match {
         |    case a =>
         |    // c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo match {
         |    case a =>
         |      // c
         |      {}
         |    // c
         |  }
         |}
         |""".stripMargin,
    )("end Case: // c", "beg Term.Block: // c")
  }

  test("class: def body, DSLC indented under it, next def") {
    val code =
      """|class A {
         |  def f =
         |    x
         |    // c
         |  def g = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A {
         |  def f = x
         |  // c
         |  def g = 1
         |}
         |""".stripMargin,
    )("beg Defn.Def: // c")
  }

  test("if: DSLC under each branch, DSLC before else and after the if") {
    val code =
      """|object O {
         |  if (foo)
         |    bar
         |    // c1
         |  // c2
         |  else
         |    baz
         |    // c3
         |  // c4
         |  qux
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  if (foo) bar else baz
         |  // c3
         |  // c4
         |  qux
         |}
         |""".stripMargin,
    )("beg Term.Name: // c3\n  // c4")
  }

  test("block: statement, DSLC indented under it, next statement") {
    val code =
      """|object O {
         |  foo(
         |    a)
         |    // c
         |  bar
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo(a)
         |  // c
         |  bar
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("block: statement, DSLC, blank line, next statement") {
    val code =
      """|object O {
         |  foo
         |  // c
         |
         |  bar
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo
         |  bar
         |}
         |""".stripMargin,
    )()
  }

  test("val: DSLC indented under the equals, body") {
    val code =
      """|object O {
         |  val x =
         |      // c
         |    y
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val x =
         |    // c
         |    y
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("if: DSLC indented under the condition, thenp") {
    val code =
      """|object O {
         |  if (foo)
         |      // c
         |    bar
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  if (foo)
         |    // c
         |    bar
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("lambda: DSLC indented under the arrow, body") {
    val code =
      """|object O {
         |  xs.map { x =>
         |      // c
         |    f(x)
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  xs.map {
         |    x =>
         |      // c
         |      f(x)
         |  }
         |}
         |""".stripMargin,
    )("beg Term.Apply, Term.Name: // c")
  }

  test("args: DSLC indented after a comma, next arg") {
    val code =
      """|object O {
         |  f(
         |    a,
         |      // c
         |    b
         |  )
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  f(a, 
         |  // c
         |  b)
         |}
         |""".stripMargin,
    )("beg Term.Name: // c")
  }

  test("params: DSLC indented after a comma, next param") {
    val code =
      """|object O {
         |  def f(
         |    a: A,
         |      // c
         |    b: B
         |  ) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f(a: A, b: B) = 1 }
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Term.Param, Term.Name: // c")
  }

  test("class params: DSLC indented after a comma, next param") {
    val code =
      """|object O {
         |  case class C(
         |    a: A,
         |      // c
         |    b: B
         |  )
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { case class C(a: A, b: B) }
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Term.Param, Term.Name: // c")
  }

  test("match: two-statement case body, DSLC indented under it, next case") {
    val code =
      """|object O {
         |  x match {
         |    case 1 =>
         |      a
         |      b
         |      // c1
         |    case 2 => c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 1 =>
         |      a
         |      b
         |    // c1
         |    case 2 =>
         |      c
         |  }
         |}
         |""".stripMargin,
    )("beg Case: // c1")
  }

  test("match: two-statement case body, blank line, DSLC indented under it, next case") {
    val code =
      """|object O {
         |  x match {
         |    case 1 =>
         |      a
         |      b
         |
         |      // c1
         |    case 2 => c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 1 =>
         |      a
         |      b
         |    // c1
         |    case 2 =>
         |      c
         |  }
         |}
         |""".stripMargin,
    )("beg Case: // c1")
  }

  test("class: self, ASLC, statement") {
    val code =
      """|class A { self => // c1
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { self => // c1
         |  // c1
         |  def f = 1
         |}
         |""".stripMargin,
      reprinted = Seq("end Self: // c1", "beg Defn.Def: // c1\n  // c1"),
    )("end Self: // c1", "beg Defn.Def: // c1")
  }

  test("class: self, DSLC, no statement") {
    val code =
      """|class A { self =>
         |  // c1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { self =>
         |// c1
         | }
         |""".stripMargin,
    )("end Self: // c1")
  }

  test("params: DSLC before implicit") {
    val code =
      """|object O {
         |  def f(a: Int)(
         |      // c1
         |      implicit b: B,
         |  ) = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f(a: Int)(
         |  // c1
         |  implicit b: B) = 1
         |}
         |""".stripMargin,
    )("beg Term.Param, Mod.Implicit, Mod.Implicit: // c1")
  }

  test("class: self, DSLC indented deeper, statement") {
    val code =
      """|class A { self =>
         |      // c1
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { self =>
         |  // c1
         |  def f = 1
         |}
         |""".stripMargin,
    )("beg Defn.Def: // c1")
  }

  test("class: self, MLC, statement on the same line") {
    val code =
      """|class A { self => /* c1 */ def f = 1 }
         |""".stripMargin
    checkComments(
      code,
      """|class A { self => /* c1 */ /* c1 */ def f = 1 }
         |""".stripMargin,
      reprinted = Seq("end Self: /* c1 */ /* c1 */", "beg Defn.Def: /* c1 */ /* c1 */"),
    )("end Self: /* c1 */", "beg Defn.Def: /* c1 */")
  }

  test("class: self, MLC, statement on the next line") {
    val code =
      """|class A { self => /* c1 */
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { self => /* c1 */
         |  /* c1 */
         |  def f = 1
         |}
         |""".stripMargin,
      reprinted = Seq("end Self: /* c1 */", "beg Defn.Def: /* c1 */\n  /* c1 */"),
    )("end Self: /* c1 */", "beg Defn.Def: /* c1 */")
  }

  test("class: self, SLC, no statement") {
    val code =
      """|class A { self => // c1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { self => // c1
         | }
         |""".stripMargin,
    )("end Self: // c1")
  }

  test("class: self, MLC, no statement") {
    val code =
      """|class A { self => /* c1 */ }
         |""".stripMargin
    checkComments(
      code,
      """|class A { self => /* c1 */ }
         |""".stripMargin,
    )("end Self: /* c1 */")
  }

  test("class: DSLC, self, statement") {
    val code =
      """|class A {
         |  // c1
         |  self =>
         |  def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A {
         |  // c1
         |  self => def f = 1 }
         |""".stripMargin,
    )("beg Self, Term.Name: // c1")
  }

  test("class: MLC, self, statement") {
    val code =
      """|class A { /* c1 */ self => def f = 1 }
         |""".stripMargin
    checkComments(
      code,
      """|class A { /* c1 */ self => def f = 1 }
         |""".stripMargin,
    )("beg Self, Term.Name: /* c1 */")
  }

  test("secondary ctor: self call, DSLC indented deeper, statement") {
    val code =
      """|class A {
         |  def this(x: Int) = {
         |    this()
         |        // c1
         |    f
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A {
         |  def this(x: Int) = {
         |    this()
         |    // c1
         |    f
         |  }
         |}
         |""".stripMargin,
    )("beg Term.Name: // c1")
  }

  test("block: statement in braces, DSLC, blank line, next statement") {
    val code =
      """|object O {
         |  foo { x }
         |  // c
         |
         |  bar
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  foo {
         |    x
         |  }
         |  bar
         |}
         |""".stripMargin,
    )()
  }

  test("infix in parens: ASLC inside, then an operator") {
    val code =
      """|object O {
         |  val x = (
         |    a + b // c1
         |  ) * c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val x = a + b // c1
         |    * c
         |}
         |""".stripMargin,
      reprinted = Seq("end Defn.Val, Term.ApplyInfix, Term.ArgClause, Term.Name: // c1"),
    )("end Term.ApplyInfix, Term.ArgClause, Term.Name: // c1")
  }

  test("infix in parens: MLC inside, then an operator") {
    val code =
      """|object O {
         |  val x = (a + b /* c1 */) * c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { val x = a + b /* c1 */ * c }
         |""".stripMargin,
      reprinted = Seq("end Term.Name: /* c1 */"),
    )("end Term.ApplyInfix, Term.ArgClause, Term.Name: /* c1 */")
  }

  test("match in parens: MLC inside, then a select") {
    val code =
      """|object O {
         |  val x = (a match { case _ => 1 } /* c1 */).foo
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val x = a match {
         |    case _ => 1
         |  } /* c1 */.foo
         |}
         |""".stripMargin,
      reprintError = "<input>:4: error: `;` expected but `.` found",
    )("end Term.Match, Term.CasesBlock: /* c1 */")
  }

  test("params: implicit, MLC, parameter") {
    val code =
      """|object O {
         |  def f(implicit /* c1 */ x: Int) = x
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { def f(implicit /* c1 */ /* c1 */ x: Int) = x }
         |""".stripMargin,
      reprinted =
        Seq("end Mod.Implicit, Mod.Implicit: /* c1 */ /* c1 */", "beg Term.Name: /* c1 */ /* c1 */"),
    )("end Mod.Implicit, Mod.Implicit: /* c1 */", "beg Term.Name: /* c1 */")
  }

  test("params: implicit, ASLC, parameter") {
    val code =
      """|object O {
         |  def f(implicit // c1
         |      x: Int) = x
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f(implicit // c1
         |    // c1
         |    x: Int) = x
         |}
         |""".stripMargin,
      reprinted = Seq("end Mod.Implicit, Mod.Implicit: // c1", "beg Term.Name: // c1\n    // c1"),
    )("end Mod.Implicit, Mod.Implicit: // c1", "beg Term.Name: // c1")
  }

  test("class: private constructor, MLC, parameters") {
    val code =
      """|class A private /* c1 */ (x: Int)
         |""".stripMargin
    checkComments(
      code,
      """|class A private[/* c1 */ ] (x: Int)
         |""".stripMargin,
      reprintError = "<input>:1: error: `identifier` expected but `]` found",
    )(
      "beg Name.Anonymous: /* c1 */",
      "beg Name.Anonymous: /* c1 */",
      "beg Term.ParamClause: /* c1 */",
    )
  }

  test("lambda in braces: MLC before the arrow") {
    val code =
      """|object O {
         |  xs.map { x /* c1 */ =>
         |    x + 1
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  xs.map {
         |    x => x + 1
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq(),
    )("end Term.ParamClause, Term.Param, Term.Name: /* c1 */")
  }

  test("lambda in parens: MLC before the arrow") {
    val code =
      """|object O {
         |  xs.map(x /* c1 */ => x + 1)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { xs.map(x => x + 1) }
         |""".stripMargin,
      reprinted = Seq(),
    )("end Term.ParamClause, Term.Param, Term.Name: /* c1 */")
  }

  test("for: ASLC after the enumerators") {
    val code =
      """|object O {
         |  for (x <- xs) // c1
         |    println(x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { for (x <- xs) println(x) }
         |""".stripMargin,
      reprinted = Seq(),
    )("end Term.EnumeratorsBlock: // c1")
  }

  test("for: MLC before the enumerators") {
    val code =
      """|object O {
         |  for /* c1 */ (x <- xs) println(x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { for (x <- xs) println(x) }
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Term.EnumeratorsBlock: /* c1 */")
  }

  test("for: braces, ASLC after the enumerators, yield") {
    val code =
      """|object O {
         |  for { x <- xs } // c1
         |  yield x
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { for (x <- xs) yield x }
         |""".stripMargin,
      reprinted = Seq(),
    )("end Term.EnumeratorsBlock: // c1")
  }

  test("match: alternative, ASLC after the bar") {
    val code =
      """|object O {
         |  x match {
         |    case A | // c1
         |        B => 1
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case A | B => 1
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("match: alternative, MLC after the bar") {
    val code =
      """|object O {
         |  x match {
         |    case _: A | /* c1 */ _: B => 1
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case _: A | _: B => 1
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("if: MLC before the condition") {
    val code =
      """|object O {
         |  if /* c1 */ (a) b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { if (a) b }
         |""".stripMargin,
    )()
  }

  test("while: MLC before the condition") {
    val code =
      """|object O {
         |  while /* c1 */ (a) b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { while (a) b }
         |""".stripMargin,
    )()
  }

  test("unit: DSLC inside the parens") {
    val code =
      """|object O {
         |  val x = (
         |    // c1
         |  )
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { val x = () }
         |""".stripMargin,
    )()
  }

  test("package with braces: DSLC before and after a class") {
    val code =
      """|package a {
         |  // c1
         |  class A
         |  // c2
         |}
         |""".stripMargin
    checkComments(
      code,
      """|package a
         |// c1
         |class A
         |// c2
         |
         |""".stripMargin,
      reprinted = Seq(
        "end Pkg: // c2",
        "beg Defn.Class: // c1",
        "beg Type.ParamClause: // c2",
        "beg Ctor.Primary, Name.Anonymous: // c2",
        "beg Template, Template.Body: // c2",
      ),
    )("beg Defn.Class: // c1", "end Defn.Class: // c2")
  }

  test("match: parenthesized case body, ASLC") {
    val code =
      """|object A {
         |  d match {
         |    case -1 => (None: Option[Long]) // c
         |    case x => (Some(x): Option[Long])
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  d match {
         |    case -1 =>
         |      None: Option[Long] // c
         |    case x =>
         |      Some(x): Option[Long]
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("end Case, Term.Ascribe, Type.Apply, Type.ArgClause: // c"),
    )("end Case: // c")
  }

  test("class: ASLC after the brace, blank line, statement") {
    val code =
      """|class A { // c
         |
         |  val x = 0
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { val x = 0 }
         |""".stripMargin,
    )()
  }

  test("def: block, ASLC after the brace, blank line, statements") {
    val code =
      """|object A {
         |  def f = { // c
         |
         |    g()
         |    h()
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = {
         |    g()
         |    h()
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("def: DSLC after the equals, blank line, body") {
    val code =
      """|object A {
         |  def f =
         |    // c
         |
         |    g
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = g }
         |""".stripMargin,
    )()
  }

  test("if: DSLC after the condition, blank line, thenp") {
    val code =
      """|object A {
         |  if (a)
         |    // c
         |
         |    b
         |  else c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { if (a) b else c }
         |""".stripMargin,
    )()
  }

  test("lambda in braces: ASLC after the arrow, blank line, body") {
    val code =
      """|object A {
         |  g { x => // c
         |
         |    h
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  g {
         |    x => h
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("val: DSLC after the equals, parenthesized body") {
    val code =
      """|object A {
         |  val x =
         |    // c
         |    (a + b)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val x = a + b }
         |""".stripMargin,
    )()
  }

  test("apply: DSLC before a brace argument on its own line") {
    val code =
      """|object A {
         |  locally
         |    // q
         |    {
         |      println("foo")
         |    }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  locally {
         |    println("foo")
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Term.ArgClause, Term.Block: // q")
  }

  test("infix in parens: blank line, DSLC, rhs") {
    val code =
      """|object A {
         |  val x = (
         |    a ||
         |
         |      // c
         |      b
         |  )
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val x = a ||
         |  // c
         |    // c
         |    b
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.ArgClause, Term.Name: // c\n    // c"),
    )("beg Type.ArgClause: // c", "beg Term.ArgClause, Term.Name: // c")
  }

  test("match: DSLC before a guard, parenthesized operand") {
    val code =
      """|object A {
         |  x match {
         |    case A
         |        // c
         |        if (a && (b || c)) => 1
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case A if a && (b || c) => 1
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("secondary ctor: DSLC before the block") {
    val code =
      """|class A {
         |  def this(i: Float)
         |    // c
         |    {
         |      this(i.toLong)
         |    }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A {
         |  def this(i: Float) =
         |    // c
         |    this(i.toLong)
         |}
         |""".stripMargin,
      reprinted = Seq("beg Init, Type.Singleton, Term.This, Name.Anonymous: // c"),
    )("beg Ctor.Block: // c")
  }

  test("if: ASLC after the else-if condition, body on the next line") {
    val code =
      """|object A {
         |  if (a) b
         |  else if (c) // c1
         |    d
         |  else e
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  if (a) b else if (c) // c1
         |    d else e
         |}
         |""".stripMargin,
    )("beg Term.Name: // c1")
  }

  test("infix: paren operand, DSLC inside") {
    val code =
      """|object A {
         |  val x = a || (
         |    // c
         |    b
         |  )
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val x = a ||
         |    // c
         |    b
         |}
         |""".stripMargin,
      reprinted = Seq("beg Term.ArgClause, Term.Name: // c"),
    )("beg Term.Name: // c")
  }

  test("infix in parens: ASLC on the last operand") {
    val code =
      """|object A {
         |  def f = (
         |    a
         |      || b // c
         |  )
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = a || b // c
         |}
         |""".stripMargin,
      reprinted = Seq("end Defn.Def, Term.ApplyInfix, Term.ArgClause, Term.Name: // c"),
    )("end Term.ApplyInfix, Term.ArgClause, Term.Name: // c")
  }

  test("class: self, DSLC at column 0, closing brace") {
    val code =
      """|class A {
         |  self: X =>
         |// c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { self: X =>
         |// c
         | }
         |""".stripMargin,
    )("end Self: // c")
  }

  test("match in braces: case body of one term, DSLC under it") {
    val code =
      """|object A {
         |  x match {
         |    case a =>
         |      b
         |      // c
         |    case d => e
         |  }
         |}
         |""".stripMargin
    val parsed = checkComments(
      code,
      """|object A {
         |  x match {
         |    case a => b
         |    // c
         |    case d => e
         |  }
         |}
         |""".stripMargin,
    )("beg Case: // c")
    assertEquals(
      parsed.collect { case t: Case => t.body.productPrefix },
      List("Term.Name", "Term.Name"),
    )
  }

  test("lambda in braces: body of one term, DSLC under it") {
    val code =
      """|object A {
         |  xs.map { x =>
         |    b
         |    // c
         |  }
         |}
         |""".stripMargin
    val parsed = checkComments(
      code,
      """|object A {
         |  xs.map {
         |    x => b
         |    // c
         |  }
         |}
         |""".stripMargin,
    )("end Term.Function: // c")
    assertEquals(parsed.collect { case t: Term.Function => t.body.productPrefix }, List("Term.Name"))
  }

  test("package: DSLC indented under it, next package") {
    val code =
      """|package a
         |  // c1
         |package b
         |""".stripMargin
    checkComments(
      code,
      """|package a
         |// c1
         |package b
         |""".stripMargin,
    )("beg Pkg: // c1")
  }

  test("def: implicit, DSLC, params") {
    val code =
      """|object O {
         |  def f(implicit
         |    // c1
         |    x: Int) = x
         |}
         |""".stripMargin
    checkComments(code)("beg Term.Name: // c1")
  }

  test("match: last case, body on its own line, DSLC at the case's indentation") {
    val code =
      """|object O {
         |  x match {
         |    case 1 =>
         |      b
         |    // c1
         |  }
         |}
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

  test("catch: last case, body on its own line, DSLC at the case's indentation") {
    val code =
      """|object O {
         |  try a
         |  catch {
         |    case _ =>
         |      b
         |    // c1
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  try a catch {
         |    case _ => b
         |    // c1
         |  }
         |}
         |""".stripMargin,
    )("end Case: // c1")
  }

  test("type param: DSLC before the bounds") {
    val code =
      """|object O {
         |  def g[
         |    T
         |    // c1
         |    <: B
         |  ] = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def g[T
         |  // c1
         |    <: B] = 1
         |}
         |""".stripMargin,
    )("beg Type.ParamClause: // c1", "beg Type.Bounds: // c1")
  }

  test("block: empty, DSLC inside, ASLC after the brace") {
    val code =
      """|object O {
         |  def f = {
         |    // c1
         |  } // c2
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f = {} // c2
         |}
         |""".stripMargin,
    )("end Defn.Def, Term.Block: // c2")
  }

  test("class: empty body, DSLC inside, ASLC after the brace") {
    val code =
      """|class A {
         |  // c1
         |} // c2
         |""".stripMargin
    checkComments(
      code,
      """|class A // c2
         |
         |""".stripMargin,
      reprinted = Seq("end Defn.Class, Type.Name: // c2"),
    )("end Defn.Class, Template, Template.Body: // c2")
  }

  test("new: empty body, DSLC inside, ASLC after the brace") {
    val code =
      """|object O {
         |  val x = new B {
         |    // c1
         |  } // c2
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  val x = new B {} // c2
         |}
         |""".stripMargin,
    )("end Defn.Val, Term.NewAnonymous, Template, Template.Body: // c2")
  }

  test("args: empty, MLC inside, ASLC after the paren") {
    val code =
      """|object O {
         |  f( /* c1 */ ) // c2
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  f() // c2
         |}
         |""".stripMargin,
    )("end Term.Apply, Term.ArgClause: // c2")
  }

  test("if: DSLC, blank line, else") {
    val code =
      """|object O {
         |  if (a) b
         |  // c1
         |
         |  else c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { if (a) b else c }
         |""".stripMargin,
    )()
  }

  test("try: DSLC, blank line, catch") {
    val code =
      """|object O {
         |  try a
         |  // c1
         |
         |  catch { case _ => b }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  try a catch {
         |    case _ => b
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("try: DSLC, blank line, finally") {
    val code =
      """|object O {
         |  try a
         |  // c1
         |
         |  finally b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { try a finally b }
         |""".stripMargin,
    )()
  }

  test("for: DSLC, blank line, yield") {
    val code =
      """|object O {
         |  for (x <- xs)
         |  // c1
         |
         |  yield x
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { for (x <- xs) yield x }
         |""".stripMargin,
    )()
  }

  test("select: DSLC, blank line, dot") {
    val code =
      """|object O {
         |  a
         |  // c1
         |
         |  .b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { a.b }
         |""".stripMargin,
    )()
  }

  test("class: DSLC, blank line, extends") {
    val code =
      """|class A
         |// c1
         |
         |extends B
         |""".stripMargin
    checkComments(
      code,
      """|class A extends B
         |""".stripMargin,
    )()
  }

  test("annotation: DSLC, blank line, modifier, def") {
    val code =
      """|object O {
         |  @deprecated
         |  // c1
         |
         |  final def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { @deprecated final def f = 1 }
         |""".stripMargin,
    )()
  }

  test("args: DSLC, blank line, last arg") {
    val code =
      """|object O {
         |  f(a,
         |    // c1
         |
         |    b)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { f(a, b) }
         |""".stripMargin,
    )()
  }

  test("class param: val, MLC, name") {
    val code =
      """|class A(val /* c1 */ x: Int)
         |""".stripMargin
    checkComments(
      code,
      """|class A(val /* c1 */ /* c1 */ x: Int)
         |""".stripMargin,
      reprinted = Seq("end Mod.ValParam: /* c1 */ /* c1 */", "beg Term.Name: /* c1 */ /* c1 */"),
    )("end Mod.ValParam: /* c1 */", "beg Term.Name: /* c1 */")
  }

  test("match: pattern, DSLC, alternative") {
    val code =
      """|object O {
         |  x match {
         |    case A
         |    // c1
         |    | B => c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case A | B => c
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("type: MLC after the keyword") {
    val code =
      """|object O {
         |  type /* c1 */ T = Int
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { type T = Int }
         |""".stripMargin,
    )()
  }

  test("existential: wildcard, MLC, bound") {
    val code =
      """|object O {
         |  type T = A[_ /* c1 */ <: B]
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { type T = A[_ <: B] }
         |""".stripMargin,
    )()
  }

  test("args: colon, MLC, splice") {
    val code =
      """|object O {
         |  f(xs: /* c1 */ _*)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { f(xs: _*) }
         |""".stripMargin,
    )()
  }

  test("secondary ctor: MLC after def") {
    val code =
      """|class A(x: Int) {
         |  def /* c1 */ this() = this(1)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A(x: Int) { def this() = this(1) }
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Name.This: /* c1 */")
  }

  test("annotation: MLC after the at sign") {
    val code =
      """|object O {
         |  @ /* c1 */ inline def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { @inline def f = 1 }
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Init, Type.Name: /* c1 */")
  }

  test("annotation: ASLC, DSLC, def") {
    val code =
      """|object O {
         |  @deprecated // c1
         |  // c2
         |  def f = 1
         |}
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

  test("braceless package: DSLC at the end, parsed without comments") {
    val code =
      """|package a
         |
         |object B
         |// c1
         |""".stripMargin
    val options = scala.meta.parsers.ParserOptions.default.withCaptureComments(false)
    val tree = implicitly[scala.meta.parsers.Parse[Source]]
      .apply(Input.String(code))(dialect, options).get
    val pkg =
      """|package a
         |
         |object B""".stripMargin
    assertEquals(tree.collect { case t: Pkg => t.pos.text }, List(pkg))
  }

  test("block: empty block statement, DSLC inside, ASLC after the brace, statement") {
    val code =
      """|object O {
         |  def f = {
         |    {
         |      // c1
         |    } // c2
         |    a
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  def f = {
         |    {} // c2
         |    a
         |  }
         |}
         |""".stripMargin,
    )("end Term.Block: // c2")
  }

  test("if: DSLC before else, literal else branch") {
    val code =
      """|object O {
         |  val x = if (a) "b"
         |  // c1
         |  else "c"
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O { val x = if (a) "b" else "c" }
         |""".stripMargin,
    )()
  }

  test("match: literal pattern, DSLC, literal alternative") {
    val code =
      """|object O {
         |  x match {
         |    case 'a'
         |    // c1
         |    | 'b' => c
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object O {
         |  x match {
         |    case 'a' | 'b' => c
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("callee in parens: DSLC before the paren, call") {
    val code =
      """|object A {
         |  def f = (
         |    a
         |    // c1
         |  )(x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = a(x) }
         |""".stripMargin,
    )()
  }

  test("callee in parens: ASLC before the paren, call") {
    val code =
      """|object A {
         |  def f = (
         |    a // c1
         |  )(x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = a // c1
         |    (x)
         |}
         |""".stripMargin,
      reprinted = Seq("end Defn.Def, Term.Name: // c1"),
    )("end Term.Name: // c1")
  }

  test("callee in parens: MLC before the paren, call") {
    val code =
      """|object A {
         |  def f = (
         |    a /* c1 */
         |  )(x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = a /* c1 */
         |    (x)
         |}
         |""".stripMargin,
      reprinted = Seq("end Defn.Def, Term.Name: /* c1 */"),
    )("end Term.Name: /* c1 */")
  }

  test("operand in parens: ASLC inside, infix") {
    val code =
      """|object A {
         |  val y = (a // c1
         |  ) + b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val y = a // c1
         |    + b
         |}
         |""".stripMargin,
      reprinted = Seq("end Defn.Val, Term.Name: // c1"),
    )("end Term.Name: // c1")
  }

  test("operand in parens: ASLC inside, infix on the right") {
    val code =
      """|object A {
         |  val z = b + (a // c1
         |  ) * c
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val z = b + a // c1
         |    * c
         |}
         |""".stripMargin,
      reprinted = Seq("end Defn.Val, Term.ApplyInfix, Term.ArgClause, Term.Name: // c1"),
    )("end Term.Name: // c1")
  }

  test("operand in parens: ASLC inside, eta") {
    val code =
      """|object A {
         |  val y = (f // c1
         |  ) _
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val y = f // c1
         |    _
         |}
         |""".stripMargin,
      reprinted = Seq("end Defn.Val, Term.Name: // c1"),
    )("end Term.Name: // c1")
  }

  test("operand in parens: ASLC inside, type args") {
    val code =
      """|object A {
         |  val y = (a // c1
         |  )[Int]
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val y = a // c1
         |    [Int]
         |}
         |""".stripMargin,
      reprintError = "<input>:3: error: illegal start of definition `[`",
    )("end Term.Name: // c1")
  }

  test("callee in parens: DSLC at a lower indent before the paren, call") {
    val code =
      """|object A {
         |  def f = (
         |      a op b
         |    // c1
         |    )(x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = (a op b)(x) }
         |""".stripMargin,
    )()
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

  test("source: blank lines, line comment at the start") {
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

  test("unary minus: MLC before a literal") {
    val code =
      """|object A {
         |  val x = - /* c1 */ 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val x = -1 }
         |""".stripMargin,
    )()
  }

  test("unary minus: MLC before a double, no spaces") {
    val code =
      """|object A {
         |  val y = -/* c1 */1.0
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val y = -1.0 }
         |""".stripMargin,
    )()
  }

  test("pattern: unary minus, MLC, literal") {
    val code =
      """|object A {
         |  x match {
         |    case - /* c1 */ 1 =>
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case -1 =>
         |  }
         |}
         |""".stripMargin,
    )()
  }

  test("access modifier: MLC before the qualifier") {
    val code =
      """|class A {
         |  private /* c1 */ [A] def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { private[A] def f = 1 }
         |""".stripMargin,
    )()
  }

  test("access modifier: MLC before this") {
    val code =
      """|class A {
         |  protected /* c1 */ [this] def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { protected[this] def f = 1 }
         |""".stripMargin,
    )()
  }

  test("unary plus: MLC before a literal") {
    val code =
      """|object A {
         |  val x = + /* c1 */ 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val x = 1 }
         |""".stripMargin,
    )()
  }

  test("unary tilde: MLC before a literal") {
    val code =
      """|object A {
         |  val x = ~ /* c1 */ 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val x = -2 }
         |""".stripMargin,
    )()
  }

  test("unary not: MLC before a boolean") {
    val code =
      """|object A {
         |  val x = ! /* c1 */ true
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val x = false }
         |""".stripMargin,
    )()
  }

  test("unary minus: ASLC before a literal") {
    val code =
      """|object A {
         |  val x = - // c1
         |    1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val x = - // c1
         |  1
         |}
         |""".stripMargin,
    )("end Defn.Val, Term.Name: // c1")
  }

  test("unary minus: MLC after a literal") {
    val code =
      """|object A {
         |  val x = -1 /* c1 */
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val x = -1 /* c1 */
         |}
         |""".stripMargin,
    )("end Defn.Val, Lit.Int: /* c1 */")
  }

  test("access modifier: MLC inside the brackets") {
    val code =
      """|class A {
         |  private[ /* c1 */ A /* c2 */ ] def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { private[/* c1 */ A /* c2 */] def f = 1 }
         |""".stripMargin,
    )("beg Name.Indeterminate: /* c1 */", "end Name.Indeterminate: /* c2 */")
  }

  test("access modifier: MLC before the qualifier, after a modifier") {
    val code =
      """|class A {
         |  final private /* c1 */ [A] def f = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|class A { final private[A] def f = 1 }
         |""".stripMargin,
    )()
  }

  test("callee in parens: MLC after the paren, call") {
    val code =
      """|object A {
         |  def f = (a op b) /* c1 */ (x)
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = (a op b)(x) }
         |""".stripMargin,
    )()
  }

  test("callee in parens: DSLC at a lower indent before the paren") {
    val code =
      """|object A {
         |  def f = (
         |      a op b
         |    // c1
         |    )
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { def f = a op b }
         |""".stripMargin,
    )()
  }

  test("arg clause: DSLC at a lower indent before the paren") {
    val code =
      """|object A {
         |  f(
         |      a
         |    // c1
         |    )
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { f(a) }
         |""".stripMargin,
    )()
  }

  test("def: ASLC after the body, DSLC at column 0 before the brace") {
    val code =
      """|object A {
         |  def f = g(x) // c1
         |// c2
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  def f = g(x) // c1
         |    // c1
         |    // c2
         |}
         |""".stripMargin,
      reprinted = Seq("end Defn.Def, Term.Apply, Term.ArgClause: // c1\n    // c1\n    // c2"),
    )("end Defn.Def, Term.Apply, Term.ArgClause: // c1\n// c2")
  }

  test("case class: MLC, scaladoc, definition") {
    val code =
      """|object A {
         |  /* Tags */
         |  /** doc */
         |  case class X(a: Int)
         |}
         |""".stripMargin
    checkComments(code)("beg Defn.Class, Mod.Case: /* Tags */\n  /** doc */")
  }

  test("class params: scaladoc before each param") {
    val code =
      """|class A(
         |    /** c1 */
         |    x: Int,
         |    /** c2 */
         |    y: Int,
         |)
         |""".stripMargin
    checkComments(
      code,
      """|class A(x: Int, y: Int)
         |""".stripMargin,
      reprinted = Seq(),
    )("beg Term.Param, Term.Name: /** c1 */", "beg Term.Param, Term.Name: /** c2 */")
  }

  test("type args: DSLC before the first, ASLC after the last") {
    val code =
      """|object A {
         |  val m: Map[
         |    // c1
         |    Int, String // c2
         |  ] = null
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val m: Map[
         |  // c1
         |  Int, String // c2
         |  ] = null
         |}
         |""".stripMargin,
    )("beg Type.Name: // c1", "end Type.Name: // c2")
  }

  test("val: ASLC after =, DSLC, body deeper") {
    val code =
      """|object A {
         |  val x = // c1
         |    // c2
         |      1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val x = // c1
         |  // c2
         |    1
         |}
         |""".stripMargin,
      reprinted = Seq("beg Lit.Int: // c1\n  // c2"),
    )("beg Lit.Int: // c1\n    // c2")
  }

  test("infix: MLC after a brace argument") {
    val code =
      """|object A {
         |  val y = x op { a } /* c */
         |  val z = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val y = x op {
         |    a
         |  } /* c */
         |  val z = 1
         |}
         |""".stripMargin,
    )("end Defn.Val, Term.ApplyInfix, Term.ArgClause, Term.Block: /* c */")
  }

  test("infix: MLC after a brace argument, operator") {
    val code =
      """|object A {
         |  val y = x op { a } /* c */ + b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val y = x op {
         |    a
         |  } /* c */ + b
         |}
         |""".stripMargin,
    )("end Term.Block: /* c */")
  }

  test("infix: ASLC after a brace argument") {
    val code =
      """|object A {
         |  val y = x op { a } // c
         |  val z = 1
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val y = x op {
         |    a
         |  } // c
         |  val z = 1
         |}
         |""".stripMargin,
    )("end Defn.Val, Term.ApplyInfix, Term.ArgClause, Term.Block: // c")
  }

  test("infix: MLC after an argument in parens, operator") {
    val code =
      """|object A {
         |  val y = x op (a) /* c */ + b
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val y = x op a + b }
         |""".stripMargin,
    )()
  }

  test("infix: MLC after an argument in parens") {
    val code =
      """|object A {
         |  val y = x op (a) /* c */
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  val y = x op a /* c */
         |}
         |""".stripMargin,
      reprinted = Seq("end Defn.Val, Term.ApplyInfix, Term.ArgClause, Term.Name: /* c */"),
    )("end Defn.Val, Term.ApplyInfix, Term.ArgClause: /* c */")
  }

  test("infix: MLC after a tuple argument, operator") {
    val code =
      """|object A {
         |  val y = x op (a, b) /* c */ + d
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A { val y = x op (a, b) /* c */ + d }
         |""".stripMargin,
    )("end Term.Tuple: /* c */")
  }

  test("pattern: MLC after an alternative in parens") {
    val code =
      """|object A {
         |  x match {
         |    case (a) /* c */ | b =>
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case a /* c */ | b =>
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("end Pat.Var, Term.Name: /* c */"),
    )("end Pat.Var: /* c */")
  }

  test("pattern: MLC after an infix operand in parens") {
    val code =
      """|object A {
         |  x match {
         |    case (a) /* c */ :: b =>
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case a /* c */ :: b =>
         |  }
         |}
         |""".stripMargin,
      reprinted = Seq("end Pat.Var, Term.Name: /* c */"),
    )("end Pat.Var: /* c */")
  }

  test("pattern: MLC after a tuple alternative") {
    val code =
      """|object A {
         |  x match {
         |    case (a, b) /* c */ | c =>
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case (a, b) /* c */ | c =>
         |  }
         |}
         |""".stripMargin,
    )("end Pat.Tuple: /* c */")
  }

  test("pattern: MLC after an alternative") {
    val code =
      """|object A {
         |  x match {
         |    case a /* c */ | b =>
         |  }
         |}
         |""".stripMargin
    checkComments(
      code,
      """|object A {
         |  x match {
         |    case a /* c */ | b =>
         |  }
         |}
         |""".stripMargin,
    )("end Pat.Var, Term.Name: /* c */")
  }
}
