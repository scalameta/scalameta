// TODO: uncomment once we reimplement Tree.transform

// package scala.meta.tests
// package tokens

// import org.scalatest._
// import scala.meta._
// import scala.meta.dialects.Scala211
// import scala.meta.internal.prettyprinters.inferTokens

// import org.scalatest.FunSuite

// class InferSimpleSuite extends FunSuite {

//   private def trimTokens(tks: Tokens) = tks.filterNot(tk => tk.isInstanceOf[Token.BOF] || tk.isInstanceOf[Token.EOF])

//   private def compareTokenCodes(a: Tree, b: Tree, ignoreSpaces: Boolean = false): Unit = {
//     def getCodes(t: Tree) = {
//       val codes = trimTokens(t.tokens).map(_.show[Syntax])
//       if (ignoreSpaces) codes.filter(_ != " ")
//       else codes
//     }
//     val t1 = getCodes(a)
//     val t2 = getCodes(b)
//     if (t1 != t2) {
//       println(a.show[Structure] + "\n" + b.show[Structure])
//       println(t1 + "\n" + t2)
//     }
//     assert(t1 == t2)
//   }

//   private def inferedShouldEqual(t: Tree, s: String): Unit = {
//     val t1 = trimTokens(t.tokens).map(_.show[Syntax]).mkString
//     if (t1 != s)
//       println(t1 + "\n" + s)
//     assert(t1 == s)
//   }

//   private def findFirst(t: Tree)(p: Tree => Boolean): Tree = {
//     import scala.meta.tql._
//     val find = collect {
//       case l if p(l) => l
//     }.topDownBreak
//     val lst = find(t)
//     assert(!lst.isEmpty)
//     lst.head
//   }

//   private def debug(trees: Tree*): Unit = {
//     trees.foreach { tree =>
//       println
//       println(tree.show[Structure])
//       println(tree.tokens)
//     }
//   }

//   private implicit class XtensionAntimonadicParse(code: String) {
//     def parse_![U](implicit parse: Parse[U], dialect: Dialect): U = code.parse[U].get
//   }

//   /* Infer Names */
//   /* -----------------------------------------------------------------------*/

//   test("InferNames") {
//     inferedShouldEqual(Name.Anonymous(), "_")
//     inferedShouldEqual(Name.Indeterminate("`test`"), "`test`")
//     inferedShouldEqual(Name.Indeterminate("test"), "test")
//   }

//   /* Infer Terms */
//   /* -----------------------------------------------------------------------*/

//   test("InferThis1") {
//     val tree = """rrr.this"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.This]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferThis2") {
//     val tree = """this"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.This]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferSuper1") {
//     inferedShouldEqual(Term.Super(Name.Indeterminate("rrr"), Name.Anonymous()), "rrr.super")
//     inferedShouldEqual(Term.Super(Name.Anonymous(), Name.Anonymous()), "super")
//     inferedShouldEqual(Term.Super(Name.Anonymous(), Name.Indeterminate("AA")), "super[AA]")
//   }
//   test("InferSelect1") {
//     val tree = """A.b"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Select]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferInterpolate1") {
//     val str = """ s"the world is blue ${like.an} $orange." """.trim
//     val tree = str.parse_![Term].asInstanceOf[Term.Interpolate]
//     inferedShouldEqual(tree.copy(), str)
//   }
//   test("InferApply1") {
//     val tree = """A(b, c)"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Apply]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferApply2") {
//     val tree = """A[B, C](a, b)"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Apply]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferApplyType1") {
//     val tree = """A[B, C]"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.ApplyType]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferApplyInfix1") {
//     val tree = """str mkString "," """.trim
//       .parse_![Term].asInstanceOf[Term.ApplyInfix]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferApplyInfix2") {
//     val tree = """str mkString ("[", ",", "]")""".trim
//       .parse_![Term].asInstanceOf[Term.ApplyInfix]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferApplyUnary1") {
//     val tree = """!str"""
//       .parse_![Term].asInstanceOf[Term.ApplyUnary]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferApplyUnary2") {
//     val tree = """!{
//                  |  val y = 23
//                  | y
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.ApplyUnary]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferApplyAssign1") {
//     val tree = """x = 23312"""
//       .parse_![Term].asInstanceOf[Term.Assign]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferApplyAssign2") {
//     val tree = """x = {
//                  |  val test = true
//                  |  test
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Assign]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferApplyUpdate1") {
//     val tree = """x(213) = {
//                  |  val test = true
//                  |  test
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Update]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferApplyUpdate2") {
//     val tree = """x(213) = 23312"""
//       .parse_![Term].asInstanceOf[Term.Update]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferReturn1") {
//     val tree = """return 31"""
//       .parse_![Term].asInstanceOf[Term.Return]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferThrow1") {
//     val tree = """throw DummyException"""
//       .parse_![Term].asInstanceOf[Term.Throw]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferAscribe1") {
//     val tree = """x: Int"""
//       .parse_![Term].asInstanceOf[Term.Ascribe]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferAnnotate1") {
//     val tree = """x: @Test @Via"""
//       .parse_![Term].asInstanceOf[Term.Annotate]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTuple1") {
//     val tree = """(1, 2, 3)"""
//       .parse_![Term].asInstanceOf[Term.Tuple]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferBlock1") {
//     val tree = """{
//                  |  val test = 1234
//                  |  test
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Block]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferBlock2") {
//     val tree = """{ x: Int =>
//                  |  val test = 1234
//                  |  test
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Block]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferBlock3") {
//     val tree = """{ implicit conn =>
//                  |  val test = 1234
//                  |  test
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Block]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferBlock4") {
//     val tree = """{ conn =>
//                  |  val test = 1234
//                  |  test
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Block]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferBlock5") {
//     val tree = """{ (x: Int, y: Int) =>
//                  |  val test = 1234
//                  |  test
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Block]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferIf1") {
//     val tree = """if (x == 0) {
//                  |  println("hi")
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.If]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferIf2") {
//     val tree = """if (x == 0) {
//                  |  println("hi")
//                  |} else println("hi2")"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.If]
//     compareTokenCodes(tree, tree.copy())
//   }
//   // TODO: fixme
//   // test("InferMatch1") {
//   //   val tree = """x match {
//   //                |  case u: Int => {
//   //                |    println("hi") /* this is a comment */
//   //                |  }
//   //                |  case u: String => println(u)
//   //                |}"""
//   //     .stripMargin.parse_![Term].asInstanceOf[Term.Match]
//   //   compareTokenCodes(tree, tree.copy())
//   // }
//   // test("InferTryWithCases1") {
//   //   val tree = """try {
//   //                |  val x = "this is a string"
//   //                |  x
//   //                |} catch {
//   //                |  case NonFatal(err) => {
//   //                |    println("hi") /* this is a comment */
//   //                |  }
//   //                |  case _ => println("Small error...")
//   //                |}"""
//   //     .stripMargin.parse_![Term].asInstanceOf[Term.TryWithCases]
//   //   compareTokenCodes(tree, tree.copy())
//   // }
//   // test("InferTryWithCases2") {
//   //   val tree = """try {
//   //                |  val x = "this is a string"
//   //                |  x
//   //                |} catch {
//   //                |  case NonFatal(err) => {
//   //                |    println("hi") /* this is a comment */
//   //                |  }
//   //                |  case _ => println("Small error...")
//   //                |} finally {
//   //                |  println("This is the end")
//   //                |}"""
//   //     .stripMargin.parse_![Term].asInstanceOf[Term.TryWithCases]
//   //   compareTokenCodes(tree, tree.copy())
//   // }
//   test("InferTryWithTerm1") {
//     val tree = """try {
//                  |  val x = "this is a string"
//                  |  x
//                  |} catch {
//                  |  println("dummy catch!")
//                  |} finally {
//                  |  println("This is the end")
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.TryWithTerm]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTryWithTerm2") {
//     val tree = """try {
//                  |  val x = "this is a string"
//                  |  x
//                  |} catch {
//                  |  println("dummy catch!")
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.TryWithTerm]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPartialFunction1") {
//     val tree = """{
//                  |  case x: Int => x * x
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.PartialFunction]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPartialFunction2") {
//     val tree = """{
//                  |  case x: Int => x.toString
//                  |  case y: String => y
//                  |  case _ => "not something I could print as I want."
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.PartialFunction]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferWhile1") {
//     val tree = """while (true) {
//                  |  println("never ending")
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.While]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferWhile2") {
//     val tree = """while (true) println("never ending")"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.While]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDo1") {
//     val tree = """do {
//                  |  println("never ending")
//                  |} while (true)"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Do]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDo2") {
//     val tree = """do println("never ending") while (true)"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.Do]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferFor1") {
//     val tree = """for (x <- 0 to 10) println(3)"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.For]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferFor2") {
//     val tree = """for (x <- 0 to 10; if x == 2) {
//                  |  println(3)
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.For]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferForYield1") {
//     val tree = """for (x <- 0 to 10) yield x"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.ForYield]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferForYield2") {
//     val tree = """for (x <- 0 to 10; if x == 2) yield {
//                  |  println(3)
//                  |  x
//                  |}"""
//       .stripMargin.parse_![Term].asInstanceOf[Term.ForYield]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferNew1") {
//     val tree = """new A"""
//       .stripMargin.parse_![Stat].asInstanceOf[Term.New]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferNew2") {
//     val tree = """new A(4 /* this is a comment */, 56)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Term.New]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferEta1") {
//     val tree = """Test _"""
//       .stripMargin.parse_![Stat].asInstanceOf[Term.Eta]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferArgs1") {
//     val tree = """A(x = 34, 343)(x :: y: _*)""".parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Term.Arg.Named]).asInstanceOf[Term.Arg.Named]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("InferArgs2") {
//     val tree = """A(x = 34, 343)(x :: y: _*)""".parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Term.Arg.Repeated]).asInstanceOf[Term.Arg.Repeated]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("InferParam1") {
//     val tree = """def test(a: Int) = ???""".parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Term.Param]).asInstanceOf[Term.Param]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("InferParam2") {
//     val tree = """def test(a: Int = 0) = ???""".parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Term.Param]).asInstanceOf[Term.Param]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("InferParam3") {
//     val tree = """def test(implicit a: Int = 0) = ???""".parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Term.Param]).asInstanceOf[Term.Param]
//     compareTokenCodes(t1, t1.copy())
//   }

//   /* Infer types */
//   /* -----------------------------------------------------------------------*/

//   test("InferTypeSelect1") {
//     val tree = """A.B"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Select]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeProject1") {
//     val tree = """A#B"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Project]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeSingleton1") {
//     val tree = """A.type"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Singleton]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeApply1") {
//     val tree = """A[B]"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Apply]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeApply2") {
//     val tree = """A[B, C]"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Apply]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeApplyInfix1") {
//     val tree = """A op B"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.ApplyInfix]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeFunction1") {
//     val tree = """Int => Int"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Function]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeFunction2") {
//     val tree = """(Int, String) => Int => String"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Function]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeTuple1") {
//     val tree = """(A, B, C)"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Tuple]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeCompound1") {
//     val tree = """A with B"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Compound]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeCompound2") {
//     val tree = """A with B with C"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Compound]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeCompound3") {
//     val tree = """A with B { def x: Int => Int }"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Compound]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeExistential1") {
//     val tree = """A forSome { type A }"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Existential]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeExistential2") {
//     val tree = """A forSome { type A; type B }"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Existential]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeAnnotate1") {
//     val tree = """A @test"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Annotate]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeAnnotate2") {
//     val tree = """A @test(url = scala.meta)"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Annotate]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypePlaceHolder1") {
//     val tree = """_ <: A"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Placeholder]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypePlaceHolder2") {
//     val tree = """_ >: A"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Placeholder]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypePlaceHolder3") {
//     val tree = """_ >: B <: A"""
//       .stripMargin.parse_![Type].asInstanceOf[Type.Placeholder]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeBounds1") {
//     val tree = """_ <: A""".parse_![Type]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Type.Bounds]).asInstanceOf[Type.Bounds]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("InferTypeBounds2") {
//     val tree = """_ >: B <: A """.parse_![Type]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Type.Bounds]).asInstanceOf[Type.Bounds]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("InferTypeBounds3") {
//     val tree = """_ >: B""".parse_![Type]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Type.Bounds]).asInstanceOf[Type.Bounds]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("InferTypeArgRepeated1") {
//     val tree = """A*"""
//       .parse_![Type.Arg].asInstanceOf[Type.Arg.Repeated]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeParam1") {
//     val tree = """A"""
//       .parse_![Type.Param].asInstanceOf[Type.Param]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeParam2") {
//     val tree = """+A"""
//       .parse_![Type.Param].asInstanceOf[Type.Param]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeParam3") {
//     val tree = """+A <% C"""
//       .parse_![Type.Param].asInstanceOf[Type.Param]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeParam4") {
//     val tree = """+A <% C: D: E"""
//       .parse_![Type.Param].asInstanceOf[Type.Param]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferTypeParam5") {
//     val tree = """-A: C"""
//       .parse_![Type.Param].asInstanceOf[Type.Param]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infer Pats (terms) */
//   /* -----------------------------------------------------------------------*/

//   test("InferPatBind1") {
//     val tree = """t @ Test(x: Int, y: String /* This is a comment */)"""
//       .parse_![Pat].asInstanceOf[Pat.Bind]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatAlternative1") {
//     val tree = """Int | String"""
//       .parse_![Pat].asInstanceOf[Pat.Alternative]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTuple1") {
//     val tree = """(x, y)"""
//       .parse_![Pat].asInstanceOf[Pat.Tuple]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTuple2") {
//     val tree = """(x: Int, y: String)"""
//       .parse_![Pat].asInstanceOf[Pat.Tuple]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatExtract1") {
//     val tree = """Test(x: Int, y: String)"""
//       .parse_![Pat].asInstanceOf[Pat.Extract]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatExtract2") {
//     val tree = """Test[String](x: Int, y: String)"""
//       .parse_![Pat].asInstanceOf[Pat.Extract]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatExtractInfix1") {
//     val tree = """x :: xs"""
//       .parse_![Pat].asInstanceOf[Pat.ExtractInfix]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatExtractInfix2") {
//     val tree = """x :: (xx, y)"""
//       .parse_![Pat].asInstanceOf[Pat.ExtractInfix]
//     compareTokenCodes(tree, tree.copy())
//   }
//   /* TODO: Those test pass properly, but add a character at the end of each name. */
//   /*test("inferPatInterpolate1") {
//     val tree = """ mys"$something is blue like $an $orange." """.trim
//       .parse_![Pat].asInstanceOf[Pat.Interpolate]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferPatInterpolate2") {
//     val tree = """ mys"$something is blue like ${an.orange}." """.trim
//       .parse_![Pat].asInstanceOf[Pat.Interpolate]
//     compareTokenCodes(tree, tree.copy())
//   }*/
//   test("InferPatTyped1") {
//     val tree = """x: Int"""
//       .parse_![Pat].asInstanceOf[Pat.Typed]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infer Pat (types) */
//   /* -----------------------------------------------------------------------*/

//   test("InferPatTypeProject1") {
//     val tree = """A#B"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Project]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTypeApply1") {
//     val tree = """A[B]"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Apply]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTypeApply2") {
//     val tree = """A[B, C]"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Apply]
//     compareTokenCodes(tree, tree.copy())
//   }
//   // TODO: looks like in those cases Type is used directly, not Pat.Type.
//   /*test("InferPatTypeApplyInfix1") {
//     val tree = """A op B"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.ApplyInfix]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTypeFunction1") {
//     val tree = """Int => Int"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Function]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTypeFunction2") {
//     val tree = """(Int, String) => Int => String"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Function]
//     compareTokenCodes(tree, tree.copy())
//   }*/
//   test("InferPatTypeTuple1") {
//     val tree = """(A, B, C)"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Tuple]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTypeCompound1") {
//     val tree = """A with B"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Compound]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTypeCompound2") {
//     val tree = """A with B with C"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Compound]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTypeCompound3") {
//     val tree = """A with B { def x: Int => Int }"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Compound]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTypeAnnotate1") {
//     val tree = """A @test"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Annotate]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferPatTypeAnnotate2") {
//     val tree = """A @test(url = scala.meta)"""
//       .stripMargin.parse_![Pat.Type].asInstanceOf[Pat.Type.Annotate]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infer literals */
//   /* -----------------------------------------------------------------------*/

//   test("InferLit1") {
//     val tree = """42"""
//       .stripMargin.parse_![Term].asInstanceOf[Lit]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferLit2") {
//     val tree = """2121421L"""
//       .stripMargin.parse_![Term].asInstanceOf[Lit]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferLit3") {
//     val tree = """23.231F"""
//       .stripMargin.parse_![Term].asInstanceOf[Lit]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferLit4") {
//     val tree = """23.231"""
//       .stripMargin.parse_![Term].asInstanceOf[Lit]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferLit5") {
//     val tree = """'c'"""
//       .stripMargin.parse_![Term].asInstanceOf[Lit]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferLit6") {
//     val tree = """'aabbs"""
//       .stripMargin.parse_![Term].asInstanceOf[Lit]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferLit7") {
//     val tree = "\"Hello world\""
//       .stripMargin.parse_![Term].asInstanceOf[Lit]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Decl.Val */
//   /* -----------------------------------------------------------------------*/

//   test("InferDeclVal1") {
//     val tree = """val a: Int"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Val]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDeclVal2") {
//     val tree = """protected[Test] lazy val a: Int"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Val]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDeclVal3") {
//     val tree = """@deprecated val a: (A => B)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Val]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Decl.Var */
//   /* -----------------------------------------------------------------------*/

//   test("InferDeclVar1") {
//     val tree = """var a: Int"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Var]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDeclVar2") {
//     val tree = """protected[Test] lazy var a: Int"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Var]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDeclVar3") {
//     val tree = """@deprecated var a: (A => B)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Var]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Decl.Type */
//   /* -----------------------------------------------------------------------*/

//   test("InferDeclType1") {
//     val tree = """type a"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Type]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDeclType2") {
//     val tree = """type a >: Int"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Type]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDeclType3") {
//     val tree = """type a <: Int"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Type]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDeclType4") {
//     val tree = """@test protected[Test] type a[A, B] <: (A => B)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Type]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Decl.Def */
//   /* -----------------------------------------------------------------------*/

//   test("inferDeclDef1") {
//     val tree = """def aaa: Int"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDeclDef2") {
//     val tree = """def aaa(): (A => B)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDeclDef3") {
//     val tree = """def aaa[T](b: T): String"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDeclDef5") {
//     val tree = """def aaa[T, B](b: T)(c: Int = 0): (T => B)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDeclDef6") {
//     val tree = """implicit def aaa[T, B](b: T)(c: Int = 0): (T /* This is a comment */ => B)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDeclDef7") {
//     val tree = """private[test] def aaa[T, B, E](b/*This is a comment*/: T, a: String)(c: Int = 0): (T => B)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Decl.Def]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Defn.Val */
//   /* -----------------------------------------------------------------------*/

//   test("InferDefnVal1") {
//     val tree = """val a = 123"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Val]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVal2") {
//     val tree = """val a: String = 123"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Val]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVal3") {
//     val tree = """lazy val a = 123"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Val]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVal4") {
//     val tree = """protected[test] lazy val a = 123"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Val]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVal5") {
//     val tree = """@test override val a: Int = {
//                  |  val x = 231
//                  |  21323 + x
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Val]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVal6") {
//     val tree = """val (aa, bb) = (12, 23)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Val]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVal7") {
//     val tree = """val (aa, bb, (cc, dd)) = (12, 23, (11, 23))"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Val]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Defn.Var */
//   /* -----------------------------------------------------------------------*/

//   test("InferDefnVar1") {
//     val tree = """var a = 123"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Var]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVar2") {
//     val tree = """var a: String = 123"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Var]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVar3") {
//     val tree = """lazy var a = 123"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Var]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVar4") {
//     val tree = """protected[test] lazy var a = 123"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Var]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVar5") {
//     val tree = """@test override var a: Int = {
//                  |  val x = 231
//                  |  21323 + x
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Var]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVar6") {
//     val tree = """var (aa, bb) = (12, 23)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Var]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnVar7") {
//     val tree = """var (aa, bb, (cc, dd)) = (12, 23, (11, 23))"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Var]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Defn.Def */
//   /* -----------------------------------------------------------------------*/

//   test("inferDefnDef1") {
//     val tree = """def aaa = 2132"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDef2") {
//     val tree = """def aaa() = 2132"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDef3") {
//     val tree = """def aaa[T](b: T) = 2132"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDef4") {
//     val tree = """def aaa[T](b: T): Int = 2132"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDef5") {
//     val tree = """def aaa[T, B](b: T)(c: Int = 0): (T => B) = { 2132 /* This is a comment */ }"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDef6") {
//     val tree = """implicit def aaa[T, B](b: T)(c: Int = 0): (T => B) = 2132"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDef7") {
//     val tree = """private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = 2132"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Def]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDef8") {
//     val tree = """private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = {
//                  |  def yy = "hi" // new comment
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Def]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering macros */
//   /* -----------------------------------------------------------------------*/

//   test("InferDefnMacro1") {
//     val tree = """def aaa: Int = macro test"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Macro]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnMacro2") {
//     val tree = """def aaa(): Int = macro { x = 4 /* this is wrong! */}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Macro]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnMacro3") {
//     val tree = """def aaa[T](b: T): (A => B) = macro impl"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Macro]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnMacro4") {
//     val tree = """def aaa[T, B](b: T)(c: Int = 0): (T => B) = macro { impl /* This is a comment */ }"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Macro]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnMacro5") {
//     val tree = """private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = macro test"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Macro]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnMacro6") {
//     val tree = """private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = macro {
//                  |  def yy = "hi" // this macro will never work
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Macro]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Defn.Type */
//   /* -----------------------------------------------------------------------*/

//   test("InferDefnType1") {
//     val tree = """type a = Int"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Type]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnType2") {
//     val tree = """@test type a = Int"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Type]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnType3") {
//     val tree = """type a[T, E, C] = Int"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Type]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("InferDefnType4") {
//     val tree = """@test protected[Test] type a[A, B] = (A => B)"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Type]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Defn.Class */
//   /* -----------------------------------------------------------------------*/

//   test("inferDefnDefnClass1") {
//     val tree = """class Test {
//                  |  def test = 1234
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     val treeCp =
//       compareTokenCodes(tree, tree.copy())
//   }

//   test("inferDefnDefnClass2") {
//     val tree = """@ast class Test {
//                  |  def test = 1234
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     val treeCp = tree.copy()
//     compareTokenCodes(tree, tree.copy())
//   }

//   test("inferDefnDefnClass3") {
//     val tree = """class Test extends Any {
//                  |  def test = 1234
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     val treeCp = tree.copy()
//     compareTokenCodes(tree, tree.copy())
//   }

//   test("inferDefnDefnClass4") {
//     val tree = """@test class Test extends (A => B) with C {
//                  |  def test = 1234
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     compareTokenCodes(tree, tree.copy())
//   }

//   test("inferDefnDefnClass5") {
//     val tree = """@test case class Test() extends (A => B) with C {
//                  |  def test = 1234
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     compareTokenCodes(tree, tree.copy())
//   }

//   test("inferDefnDefnClass6") {
//     val tree = """@test case class Test(a: Int) extends (A => B) with C {
//                  |  def test = 1234
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     compareTokenCodes(tree, tree.copy())
//   }

//   test("inferDefnDefnClass7") {
//     val tree = """@test case class Test(a: Int, b: String) extends (A => B) with C {
//                  |  def test = 1234
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnClass8") {
//     val tree = """@test case class Test(var a: Int, val b: String) extends (A => B) with C {
//                  |  def test = 1234
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnClass9") {
//     val tree = """@test case class Test[B](var a: Int, val b: String) extends (A => B) with C {
//                  |  def test = 1234
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnClass10") {
//     val tree = """@test case class Test[A, B](var a: Int, val b: String) extends (A => B) with C {
//                  |  def test = 1234
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnClass11") {
//     val tree = """@test @abc case class Test[A, B](var a: Int, val b: String) extends (A => B) with C {
//                |  def test = 1234
//                |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnClass12") {
//     val tree = """@test @abc case class Test"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Class]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Defn.Trait */
//   /* -----------------------------------------------------------------------*/

//   test("inferDefnDefnTrait1") {
//     val tree = """trait Test"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Trait]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnTrait2") {
//     val tree = """trait Test { }"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Trait]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnTrait3") {
//     val tree = """@deprecated trait Test {
//                  | val y = test /* hi! */
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Trait]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnTrait4") {
//     val tree = """@deprecated sealed trait Test extends AA {
//                  | val y = test /* hi! */
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Trait]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnTrait5") {
//     val tree = """@deprecated protected[this] sealed trait Test extends (AA => BB) with CC {
//                  | val y = test /* hi! */
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Trait]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Defn.Object */
//   /* -----------------------------------------------------------------------*/

//   test("inferDefnDefnObject1") {
//     val tree = """object Test {
//                  |  def test = 1234
//                  |  case object AA
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Object]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnObject2") {
//     val tree = """@ast @tt object Test {
//                  |  def test = 1234
//                  |  case object AA
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Object]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnObject3") {
//     val tree = """@ast @tt case object Test {
//                  |  def test = 1234
//                  |  case object AA
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Object]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("inferDefnDefnObject4") {
//     val tree = """@ast @tt case object Test"""
//       .stripMargin.parse_![Stat].asInstanceOf[Defn.Object]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Pkg */
//   /* -----------------------------------------------------------------------*/

//   test("inferPkg1") {
//     val tree = """package test
//                  |case class test
//                  |"""
//       .stripMargin.parse_![Source]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Pkg]).asInstanceOf[Pkg]
//     compareTokenCodes(t1, t1.copy())
//   }
//   /* Infering Pkg.Object */
//   /* -----------------------------------------------------------------------*/

//   test("inferPkgObject1") {
//     val tree = """package object Test {
//                  |  def test = 1234
//                  |  case object AA
//                  |}"""
//       .stripMargin.parse_![Stat].asInstanceOf[Pkg.Object]
//     compareTokenCodes(tree, tree.copy())
//   }

//   /* Infering Ctor */
//   /* -----------------------------------------------------------------------*/

//   test("CtorPrimary1") {
//     val tree = "class Test(x: Int, val y: String)".parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Ctor.Primary]).asInstanceOf[Ctor.Primary]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("CtorSecondary1") {
//     val tree = """class Test(x: Int, val y: String) {
//                  |  def this(x: Int, y: Int) = this(x, y.toString)
//                  |}""".stripMargin.parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Ctor.Secondary]).asInstanceOf[Ctor.Secondary]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("CtorSecondary2") {
//     val tree = """class Test(x: Int, val y: String) {
//                  |  def this(x: Int, y: Int) {
//                  |    this(x, y.toString)
//                  |    /* this is a comment */
//                  |  }
//                  |}""".stripMargin.parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Ctor.Secondary]).asInstanceOf[Ctor.Secondary]
//     compareTokenCodes(t1, t1.copy(), true)
//   }

//   /* Infering Mods */
//   /* -----------------------------------------------------------------------*/

//   test("Mods1") {
//     inferedShouldEqual(Mod.Private(Name.Indeterminate("Test")), "private[Test]")
//     inferedShouldEqual(Mod.Private(Name.Anonymous()), "private")
//     inferedShouldEqual(Mod.Protected(Name.Indeterminate("Test")), "protected[Test]")
//     inferedShouldEqual(Mod.Protected(Name.Anonymous()), "protected")
//   }
//   test("Mods2") {
//     val tree = """@test(url = "http://www.something.com") def x = 4""".parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Mod.Annot]).asInstanceOf[Mod.Annot]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("Mods3") {
//     val tree = """@test(url = "http://www.something.com", v = Seq(1,2,3,4)) def x = 4""".parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Mod.Annot]).asInstanceOf[Mod.Annot]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("Mods4") {
//     val tree = """@test case class Test(x: Int)""".parse_![Stat]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Mod.Annot]).asInstanceOf[Mod.Annot]
//     compareTokenCodes(t1, t1.copy())
//   }

//   /* Infering Enumerators */
//   /* -----------------------------------------------------------------------*/

//   test("Enum1") {
//     val tree = "for (x <- 0 to 10) yield x".parse_![Term]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Enumerator.Generator]).asInstanceOf[Enumerator.Generator]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("Enum2") {
//     val tree = "for (x <- 0 to 10; y = 10) yield x".parse_![Term]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Enumerator.Val]).asInstanceOf[Enumerator.Val]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("Enum3") {
//     val tree = "for (x <- 0 to 10 if x == 0) yield x".parse_![Term]
//     //val tree = "for (x <- 0 to 10 if x % 2 == 0) yield x".parse_![Term] // TODO: dunno why this does not pass
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Enumerator.Guard]).asInstanceOf[Enumerator.Guard]
//     compareTokenCodes(t1, t1.copy())
//   }

//   /* Infering Imports and Selector */
//   /* -----------------------------------------------------------------------*/

//   test("Importee1") {
//     val tree = "import scala.meta".parse_![Stat].asInstanceOf[Import]
//     compareTokenCodes(tree, tree.copy())
//   }
//   test("Importee2") {
//     inferedShouldEqual(Importee.Wildcard(), "_")
//   }
//   test("Importee3") {
//     inferedShouldEqual(Importee.Rename(Name.Indeterminate("A"), Name.Indeterminate("B")), "A => B")
//   }
//   test("Importee4") {
//     inferedShouldEqual(Importee.Unimport(Name.Indeterminate("A")), "A => _")
//   }

//   /* Infering case */
//   /* -----------------------------------------------------------------------*/

//   test("InferCase1") {
//     val tree = """x match {
//                  |  case x: Int => x
//                  |}""".stripMargin.parse_![Term]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Case]).asInstanceOf[Case]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("InferCase2") {
//     val tree = """x match {
//                  |  case _ => x
//                  |}""".stripMargin.parse_![Term]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Case]).asInstanceOf[Case]
//     compareTokenCodes(t1, t1.copy())
//   }
//   test("InferCase3") {
//     val tree = """x match {
//                  |  case (x: Int, y: Int) =>
//                  |  val yy = x * y
//                  |  yy
//                  |}""".stripMargin.parse_![Term]
//     val t1 = findFirst(tree)((p: Tree) => p.isInstanceOf[Case]).asInstanceOf[Case]
//     compareTokenCodes(t1, t1.copy())
//   }
//   /* Infering Source */
//   /* -----------------------------------------------------------------------*/

//   test("inferSource") {
//     val tree = """class Test {
//                  |  def test = 1234
//                  |  case object AA
//                  |}"""
//       .stripMargin.parse_![Source].asInstanceOf[Source]
//     compareTokenCodes(tree, tree.copy())
//   }

// }