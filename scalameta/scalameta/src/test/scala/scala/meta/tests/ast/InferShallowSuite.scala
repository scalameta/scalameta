package scala.meta.tests
package tokens

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class InferShallowSuite extends FunSuite {
  /* Infer Names */
  /* -----------------------------------------------------------------------*/

  test("InferNames") {
    assertShallowInfer(Name.Anonymous(), "_")
    assertShallowInfer(Name.Indeterminate("`test`"), "`test`")
    assertShallowInfer(Name.Indeterminate("test"), "test")
  }

  /* Infer Terms */
  /* -----------------------------------------------------------------------*/

  test("InferThis1") {
    assertShallowRoundtrip("""rrr.this""", classOf[Term])
  }
  test("InferThis2") {
    assertShallowRoundtrip("""this""", classOf[Term])
  }
  test("InferSuper1") {
    assertShallowInfer(Term.Super(Name.Indeterminate("rrr"), Name.Anonymous()), "rrr.super")
    assertShallowInfer(Term.Super(Name.Anonymous(), Name.Anonymous()), "super")
    assertShallowInfer(Term.Super(Name.Anonymous(), Name.Indeterminate("AA")), "super[AA]")
  }
  test("InferSelect1") {
    assertShallowRoundtrip("""A.b""", classOf[Term])
  }
  test("InferInterpolate1") {
    // TODO: fixme
    // assertShallowRoundtrip(""" s"the world is blue ${like.an} $orange." """.trim, classOf[Term])
  }
  test("InferApply1") {
    assertShallowRoundtrip("""A(b, c)""", classOf[Term])
  }
  test("InferApply2") {
    assertShallowRoundtrip("""A[B, C](a, b)""", classOf[Term])
  }
  test("InferApplyType1") {
    assertShallowRoundtrip("""A[B, C]""", classOf[Term])
  }
  test("InferApplyInfix1") {
    assertShallowRoundtrip("""str mkString "," """.trim, classOf[Term])
  }
  test("InferApplyInfix2") {
    assertShallowRoundtrip("""str mkString ("[", ",", "]")""".trim, classOf[Term])
  }
  test("InferApplyUnary1") {
    assertShallowRoundtrip("""!str""", classOf[Term])
  }
  test("InferApplyUnary2") {
    assertShallowRoundtrip("""!{
                 |  val y = 23
                 | y
                 |}""", classOf[Term])
  }
  test("InferApplyAssign1") {
    assertShallowRoundtrip("""x = 23312""", classOf[Term])
  }
  test("InferApplyAssign2") {
    assertShallowRoundtrip("""x = {
                 |  val test = true
                 |  test
                 |}""", classOf[Term])
  }
  test("InferApplyUpdate1") {
    assertShallowRoundtrip("""x(213) = {
                 |  val test = true
                 |  test
                 |}""", classOf[Term])
  }
  test("InferApplyUpdate2") {
    assertShallowRoundtrip("""x(213) = 23312""", classOf[Term])
  }
  test("InferReturn1") {
    assertShallowRoundtrip("""return 31""", classOf[Term])
  }
  test("InferThrow1") {
    assertShallowRoundtrip("""throw DummyException""", classOf[Term])
  }
  test("InferAscribe1") {
    assertShallowRoundtrip("""x: Int""", classOf[Term])
  }
  test("InferAnnotate1") {
    assertShallowRoundtrip("""x: @Test @Via""", classOf[Term])
  }
  test("InferTuple1") {
    assertShallowRoundtrip("""(1, 2, 3)""", classOf[Term])
  }
  test("InferBlock1") {
    assertShallowRoundtrip("""{
                 |  val test = 1234
                 |  test
                 |}""", classOf[Term])
  }
  test("InferBlock2") {
    assertShallowRoundtrip("""{ x: Int =>
                 |  val test = 1234
                 |  test
                 |}""", classOf[Term])
  }
  test("InferBlock3") {
    assertShallowRoundtrip("""{ implicit conn =>
                 |  val test = 1234
                 |  test
                 |}""", classOf[Term])
  }
  test("InferBlock4") {
    assertShallowRoundtrip("""{ conn =>
                 |  val test = 1234
                 |  test
                 |}""", classOf[Term])
  }
  test("InferBlock5") {
    assertShallowRoundtrip("""{ (x: Int, y: Int) =>
                 |  val test = 1234
                 |  test
                 |}""", classOf[Term])
  }
  test("InferIf1") {
    assertShallowRoundtrip("""if (x == 0) {
                 |  println("hi")
                 |}""", classOf[Term])
  }
  test("InferIf2") {
    assertShallowRoundtrip("""if (x == 0) {
                 |  println("hi")
                 |} else println("hi2")""", classOf[Term])
  }
  // TODO: fixme
  // test("InferMatch1") {
  //   assertShallowRoundtrip("""x match {
  //                |  case u: Int => {
  //                |    println("hi") /* this is a comment */
  //                |  }
  //                |  case u: String => println(u)
  //                |}""", classOf[Term])
  // }
  // test("InferTryWithCases1") {
  //   assertShallowRoundtrip("""try {
  //                |  val x = "this is a string"
  //                |  x
  //                |} catch {
  //                |  case NonFatal(err) => {
  //                |    println("hi") /* this is a comment */
  //                |  }
  //                |  case _ => println("Small error...")
  //                |}""", classOf[Term])
  // }
  // test("InferTryWithCases2") {
  //   assertShallowRoundtrip("""try {
  //                |  val x = "this is a string"
  //                |  x
  //                |} catch {
  //                |  case NonFatal(err) => {
  //                |    println("hi") /* this is a comment */
  //                |  }
  //                |  case _ => println("Small error...")
  //                |} finally {
  //                |  println("This is the end")
  //                |}""", classOf[Term])
  // }
  test("InferTryWithTerm1") {
    assertShallowRoundtrip("""try {
                 |  val x = "this is a string"
                 |  x
                 |} catch {
                 |  println("dummy catch!")
                 |} finally {
                 |  println("This is the end")
                 |}""", classOf[Term])
  }
  test("InferTryWithTerm2") {
    assertShallowRoundtrip("""try {
                 |  val x = "this is a string"
                 |  x
                 |} catch {
                 |  println("dummy catch!")
                 |}""", classOf[Term])
  }
  test("InferPartialFunction1") {
    assertShallowRoundtrip("""{
                 |  case x: Int => x * x
                 |}""", classOf[Term])
  }
  test("InferPartialFunction2") {
    assertShallowRoundtrip("""{
                 |  case x: Int => x.toString
                 |  case y: String => y
                 |  case _ => "not something I could print as I want."
                 |}""", classOf[Term])
  }
  test("InferWhile1") {
    assertShallowRoundtrip("""while (true) {
                 |  println("never ending")
                 |}""", classOf[Term])
  }
  test("InferWhile2") {
    assertShallowRoundtrip("""while (true) println("never ending")""", classOf[Term])
  }
  test("InferDo1") {
    assertShallowRoundtrip("""do {
                 |  println("never ending")
                 |} while (true)""", classOf[Term])
  }
  test("InferDo2") {
    assertShallowRoundtrip("""do println("never ending") while (true)""", classOf[Term])
  }
  test("InferFor1") {
    assertShallowRoundtrip("""for (x <- 0 to 10) println(3)""", classOf[Term])
  }
  test("InferFor2") {
    assertShallowRoundtrip("""for (x <- 0 to 10; if x == 2) {
                 |  println(3)
                 |}""", classOf[Term])
  }
  test("InferForYield1") {
    assertShallowRoundtrip("""for (x <- 0 to 10) yield x""", classOf[Term])
  }
  test("InferForYield2") {
    assertShallowRoundtrip("""for (x <- 0 to 10; if x == 2) yield {
                 |  println(3)
                 |  x
                 |}""", classOf[Term])
  }
  test("InferNew1") {
    assertShallowRoundtrip("""new A""", classOf[Stat])
  }
  test("InferNew2") {
    assertShallowRoundtrip("""new A(4 /* this is a comment */, 56)""", classOf[Stat])
  }
  test("InferEta1") {
    assertShallowRoundtrip("""Test _""", classOf[Stat])
  }
  test("InferArgs1") {
    val tree = """A(x = 34, 343)(x :: y: _*)""".parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Term.Arg.Named]).asInstanceOf[Term.Arg.Named]
    assertShallowRoundtrip(t1)
  }
  test("InferArgs2") {
    val tree = """A(x = 34, 343)(x :: y: _*)""".parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Term.Arg.Repeated]).asInstanceOf[Term.Arg.Repeated]
    assertShallowRoundtrip(t1)
  }
  test("InferParam1") {
    val tree = """def test(a: Int) = ???""".parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Term.Param]).asInstanceOf[Term.Param]
    assertShallowRoundtrip(t1)
  }
  test("InferParam2") {
    val tree = """def test(a: Int = 0) = ???""".parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Term.Param]).asInstanceOf[Term.Param]
    assertShallowRoundtrip(t1)
  }
  test("InferParam3") {
    val tree = """def test(implicit a: Int = 0) = ???""".parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Term.Param]).asInstanceOf[Term.Param]
    assertShallowRoundtrip(t1)
  }

  /* Infer types */
  /* -----------------------------------------------------------------------*/

  test("InferTypeSelect1") {
    assertShallowRoundtrip("""A.B""", classOf[Type])
  }
  test("InferTypeProject1") {
    assertShallowRoundtrip("""A#B""", classOf[Type])
  }
  test("InferTypeSingleton1") {
    assertShallowRoundtrip("""A.type""", classOf[Type])
  }
  test("InferTypeApply1") {
    assertShallowRoundtrip("""A[B]""", classOf[Type])
  }
  test("InferTypeApply2") {
    assertShallowRoundtrip("""A[B, C]""", classOf[Type])
  }
  test("InferTypeApplyInfix1") {
    assertShallowRoundtrip("""A op B""", classOf[Type])
  }
  test("InferTypeFunction1") {
    assertShallowRoundtrip("""Int => Int""", classOf[Type])
  }
  test("InferTypeFunction2") {
    assertShallowRoundtrip("""(Int, String) => Int => String""", classOf[Type])
  }
  test("InferTypeTuple1") {
    assertShallowRoundtrip("""(A, B, C)""", classOf[Type])
  }
  test("InferTypeCompound1") {
    assertShallowRoundtrip("""A with B""", classOf[Type])
  }
  test("InferTypeCompound2") {
    assertShallowRoundtrip("""A with B with C""", classOf[Type])
  }
  test("InferTypeCompound3") {
    assertShallowRoundtrip("""A with B { def x: Int => Int }""", classOf[Type])
  }
  test("InferTypeExistential1") {
    assertShallowRoundtrip("""A forSome { type A }""", classOf[Type])
  }
  test("InferTypeExistential2") {
    assertShallowRoundtrip("""A forSome { type A; type B }""", classOf[Type])
  }
  test("InferTypeAnnotate1") {
    assertShallowRoundtrip("""A @test""", classOf[Type])
  }
  test("InferTypeAnnotate2") {
    assertShallowRoundtrip("""A @test(url = scala.meta)""", classOf[Type])
  }
  test("InferTypePlaceHolder1") {
    assertShallowRoundtrip("""_ <: A""", classOf[Type])
  }
  test("InferTypePlaceHolder2") {
    assertShallowRoundtrip("""_ >: A""", classOf[Type])
  }
  test("InferTypePlaceHolder3") {
    assertShallowRoundtrip("""_ >: B <: A""", classOf[Type])
  }
  test("InferTypeBounds1") {
    val tree = """_ <: A""".parse[Type].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Type.Bounds]).asInstanceOf[Type.Bounds]
    assertShallowRoundtrip(t1)
  }
  test("InferTypeBounds2") {
    val tree = """_ >: B <: A """.parse[Type].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Type.Bounds]).asInstanceOf[Type.Bounds]
    assertShallowRoundtrip(t1)
  }
  test("InferTypeBounds3") {
    val tree = """_ >: B""".parse[Type].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Type.Bounds]).asInstanceOf[Type.Bounds]
    assertShallowRoundtrip(t1)
  }
  test("InferTypeArgRepeated1") {
    assertShallowRoundtrip("""A*""", classOf[Type.Arg])
  }
  test("InferTypeParam1") {
    assertShallowRoundtrip("""A""", classOf[Type.Param])
  }
  test("InferTypeParam2") {
    assertShallowRoundtrip("""+A""", classOf[Type.Param])
  }
  test("InferTypeParam3") {
    assertShallowRoundtrip("""+A <% C""", classOf[Type.Param])
  }
  test("InferTypeParam4") {
    assertShallowRoundtrip("""+A <% C: D: E""", classOf[Type.Param])
  }
  test("InferTypeParam5") {
    assertShallowRoundtrip("""-A: C""", classOf[Type.Param])
  }

  /* Infer Pats (terms) */
  /* -----------------------------------------------------------------------*/

  test("InferPatBind1") {
    assertShallowRoundtrip("""t @ Test(x: Int, y: String /* This is a comment */)""", classOf[Pat])
  }
  test("InferPatAlternative1") {
    assertShallowRoundtrip("""Int | String""", classOf[Pat])
  }
  test("InferPatTuple1") {
    assertShallowRoundtrip("""(x, y)""", classOf[Pat])
  }
  test("InferPatTuple2") {
    assertShallowRoundtrip("""(x: Int, y: String)""", classOf[Pat])
  }
  test("InferPatExtract1") {
    assertShallowRoundtrip("""Test(x: Int, y: String)""", classOf[Pat])
  }
  test("InferPatExtract2") {
    assertShallowRoundtrip("""Test[String](x: Int, y: String)""", classOf[Pat])
  }
  test("InferPatExtractInfix1") {
    assertShallowRoundtrip("""x :: xs""", classOf[Pat])
  }
  test("InferPatExtractInfix2") {
    assertShallowRoundtrip("""x :: (xx, y)""", classOf[Pat])
  }
  /* TODO: Those test pass properly, but add a character at the end of each name. */
  /*test("inferPatInterpolate1") {
    assertShallowRoundtrip(""" mys"$something is blue like $an $orange." """.trim, classOf[Pat])
  }
  test("inferPatInterpolate2") {
    assertShallowRoundtrip(""" mys"$something is blue like ${an.orange}." """.trim, classOf[Pat])
  }*/
  test("InferPatTyped1") {
    assertShallowRoundtrip("""x: Int""", classOf[Pat])
  }

  /* Infer Pat (types) */
  /* -----------------------------------------------------------------------*/

  test("InferPatTypeProject1") {
    assertShallowRoundtrip("""A#B""", classOf[Pat.Type])
  }
  test("InferPatTypeApply1") {
    assertShallowRoundtrip("""A[B]""", classOf[Pat.Type])
  }
  test("InferPatTypeApply2") {
    assertShallowRoundtrip("""A[B, C]""", classOf[Pat.Type])
  }
  // TODO: looks like in those cases Type is used directly, not Pat.Type.
  /*test("InferPatTypeApplyInfix1") {
    assertShallowRoundtrip("""A op B""", classOf[Pat.Type])
  }
  test("InferPatTypeFunction1") {
    assertShallowRoundtrip("""Int => Int""", classOf[Pat.Type])
  }
  test("InferPatTypeFunction2") {
    assertShallowRoundtrip("""(Int, String) => Int => String""", classOf[Pat.Type])
  }*/
  test("InferPatTypeTuple1") {
    assertShallowRoundtrip("""(A, B, C)""", classOf[Pat.Type])
  }
  test("InferPatTypeCompound1") {
    assertShallowRoundtrip("""A with B""", classOf[Pat.Type])
  }
  test("InferPatTypeCompound2") {
    assertShallowRoundtrip("""A with B with C""", classOf[Pat.Type])
  }
  test("InferPatTypeCompound3") {
    assertShallowRoundtrip("""A with B { def x: Int => Int }""", classOf[Pat.Type])
  }
  test("InferPatTypeAnnotate1") {
    assertShallowRoundtrip("""A @test""", classOf[Pat.Type])
  }
  test("InferPatTypeAnnotate2") {
    assertShallowRoundtrip("""A @test(url = scala.meta)""", classOf[Pat.Type])
  }

  /* Infer literals */
  /* -----------------------------------------------------------------------*/

  test("InferLit1") {
    assertShallowRoundtrip("""42""", classOf[Term])
  }
  test("InferLit2") {
    assertShallowRoundtrip("""2121421L""", classOf[Term])
  }
  test("InferLit3") {
    assertShallowRoundtrip("""23.231F""", classOf[Term])
  }
  test("InferLit4") {
    assertShallowRoundtrip("""23.231""", classOf[Term])
  }
  test("InferLit5") {
    assertShallowRoundtrip("""'c'""", classOf[Term])
  }
  test("InferLit6") {
    assertShallowRoundtrip("""'aabbs""", classOf[Term])
  }
  test("InferLit7") {
    assertShallowRoundtrip("\"Hello world\"", classOf[Term])
  }

  /* Infering Decl.Val */
  /* -----------------------------------------------------------------------*/

  test("InferDeclVal1") {
    assertShallowRoundtrip("""val a: Int""", classOf[Stat])
  }
  test("InferDeclVal2") {
    assertShallowRoundtrip("""protected[Test] lazy val a: Int""", classOf[Stat])
  }
  test("InferDeclVal3") {
    assertShallowRoundtrip("""@deprecated val a: (A => B)""", classOf[Stat])
  }

  /* Infering Decl.Var */
  /* -----------------------------------------------------------------------*/

  test("InferDeclVar1") {
    assertShallowRoundtrip("""var a: Int""", classOf[Stat])
  }
  test("InferDeclVar2") {
    assertShallowRoundtrip("""protected[Test] lazy var a: Int""", classOf[Stat])
  }
  test("InferDeclVar3") {
    assertShallowRoundtrip("""@deprecated var a: (A => B)""", classOf[Stat])
  }

  /* Infering Decl.Type */
  /* -----------------------------------------------------------------------*/

  test("InferDeclType1") {
    assertShallowRoundtrip("""type a""", classOf[Stat])
  }
  test("InferDeclType2") {
    assertShallowRoundtrip("""type a >: Int""", classOf[Stat])
  }
  test("InferDeclType3") {
    assertShallowRoundtrip("""type a <: Int""", classOf[Stat])
  }
  test("InferDeclType4") {
    assertShallowRoundtrip("""@test protected[Test] type a[A, B] <: (A => B)""", classOf[Stat])
  }

  /* Infering Decl.Def */
  /* -----------------------------------------------------------------------*/

  test("inferDeclDef1") {
    assertShallowRoundtrip("""def aaa: Int""", classOf[Stat])
  }
  test("inferDeclDef2") {
    assertShallowRoundtrip("""def aaa(): (A => B)""", classOf[Stat])
  }
  test("inferDeclDef3") {
    assertShallowRoundtrip("""def aaa[T](b: T): String""", classOf[Stat])
  }
  test("inferDeclDef5") {
    assertShallowRoundtrip("""def aaa[T, B](b: T)(c: Int = 0): (T => B)""", classOf[Stat])
  }
  test("inferDeclDef6") {
    assertShallowRoundtrip("""implicit def aaa[T, B](b: T)(c: Int = 0): (T /* This is a comment */ => B)""", classOf[Stat])
  }
  test("inferDeclDef7") {
    assertShallowRoundtrip("""private[test] def aaa[T, B, E](b/*This is a comment*/: T, a: String)(c: Int = 0): (T => B)""", classOf[Stat])
  }

  /* Infering Defn.Val */
  /* -----------------------------------------------------------------------*/

  test("InferDefnVal1") {
    assertShallowRoundtrip("""val a = 123""", classOf[Stat])
  }
  test("InferDefnVal2") {
    assertShallowRoundtrip("""val a: String = 123""", classOf[Stat])
  }
  test("InferDefnVal3") {
    assertShallowRoundtrip("""lazy val a = 123""", classOf[Stat])
  }
  test("InferDefnVal4") {
    assertShallowRoundtrip("""protected[test] lazy val a = 123""", classOf[Stat])
  }
  test("InferDefnVal5") {
    assertShallowRoundtrip("""@test override val a: Int = {
                 |  val x = 231
                 |  21323 + x
                 |}""", classOf[Stat])
  }
  test("InferDefnVal6") {
    assertShallowRoundtrip("""val (aa, bb) = (12, 23)""", classOf[Stat])
  }
  test("InferDefnVal7") {
    assertShallowRoundtrip("""val (aa, bb, (cc, dd)) = (12, 23, (11, 23))""", classOf[Stat])
  }

  /* Infering Defn.Var */
  /* -----------------------------------------------------------------------*/

  test("InferDefnVar1") {
    assertShallowRoundtrip("""var a = 123""", classOf[Stat])
  }
  test("InferDefnVar2") {
    assertShallowRoundtrip("""var a: String = 123""", classOf[Stat])
  }
  test("InferDefnVar3") {
    assertShallowRoundtrip("""lazy var a = 123""", classOf[Stat])
  }
  test("InferDefnVar4") {
    assertShallowRoundtrip("""protected[test] lazy var a = 123""", classOf[Stat])
  }
  test("InferDefnVar5") {
    assertShallowRoundtrip("""@test override var a: Int = {
                 |  val x = 231
                 |  21323 + x
                 |}""", classOf[Stat])
  }
  test("InferDefnVar6") {
    assertShallowRoundtrip("""var (aa, bb) = (12, 23)""", classOf[Stat])
  }
  test("InferDefnVar7") {
    assertShallowRoundtrip("""var (aa, bb, (cc, dd)) = (12, 23, (11, 23))""", classOf[Stat])
  }

  /* Infering Defn.Def */
  /* -----------------------------------------------------------------------*/

  test("inferDefnDef1") {
    assertShallowRoundtrip("""def aaa = 2132""", classOf[Stat])
  }
  test("inferDefnDef2") {
    assertShallowRoundtrip("""def aaa() = 2132""", classOf[Stat])
  }
  test("inferDefnDef3") {
    assertShallowRoundtrip("""def aaa[T](b: T) = 2132""", classOf[Stat])
  }
  test("inferDefnDef4") {
    assertShallowRoundtrip("""def aaa[T](b: T): Int = 2132""", classOf[Stat])
  }
  test("inferDefnDef5") {
    assertShallowRoundtrip("""def aaa[T, B](b: T)(c: Int = 0): (T => B) = { 2132 /* This is a comment */ }""", classOf[Stat])
  }
  test("inferDefnDef6") {
    assertShallowRoundtrip("""implicit def aaa[T, B](b: T)(c: Int = 0): (T => B) = 2132""", classOf[Stat])
  }
  test("inferDefnDef7") {
    assertShallowRoundtrip("""private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = 2132""", classOf[Stat])
  }
  test("inferDefnDef8") {
    assertShallowRoundtrip("""private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = {
                 |  def yy = "hi" // new comment
                 |}""", classOf[Stat])
  }

  /* Infering macros */
  /* -----------------------------------------------------------------------*/

  test("InferDefnMacro1") {
    assertShallowRoundtrip("""def aaa: Int = macro test""", classOf[Stat])
  }
  test("InferDefnMacro2") {
    assertShallowRoundtrip("""def aaa(): Int = macro { x = 4 /* this is wrong! */}""", classOf[Stat])
  }
  test("InferDefnMacro3") {
    assertShallowRoundtrip("""def aaa[T](b: T): (A => B) = macro impl""", classOf[Stat])
  }
  test("InferDefnMacro4") {
    assertShallowRoundtrip("""def aaa[T, B](b: T)(c: Int = 0): (T => B) = macro { impl /* This is a comment */ }""", classOf[Stat])
  }
  test("InferDefnMacro5") {
    assertShallowRoundtrip("""private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = macro test""", classOf[Stat])
  }
  test("InferDefnMacro6") {
    assertShallowRoundtrip("""private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = macro {
                 |  def yy = "hi" // this macro will never work
                 |}""", classOf[Stat])
  }

  /* Infering Defn.Type */
  /* -----------------------------------------------------------------------*/

  test("InferDefnType1") {
    assertShallowRoundtrip("""type a = Int""", classOf[Stat])
  }
  test("InferDefnType2") {
    assertShallowRoundtrip("""@test type a = Int""", classOf[Stat])
  }
  test("InferDefnType3") {
    assertShallowRoundtrip("""type a[T, E, C] = Int""", classOf[Stat])
  }
  test("InferDefnType4") {
    assertShallowRoundtrip("""@test protected[Test] type a[A, B] = (A => B)""", classOf[Stat])
  }

  /* Infering Defn.Class */
  /* -----------------------------------------------------------------------*/

  test("inferDefnDefnClass1") {
    assertShallowRoundtrip("""class Test {
                 |  def test = 1234
                 |}""", classOf[Stat])
  }

  test("inferDefnDefnClass2") {
    assertShallowRoundtrip("""@ast class Test {
                 |  def test = 1234
                 |}""", classOf[Stat])
  }

  test("inferDefnDefnClass3") {
    assertShallowRoundtrip("""class Test extends Any {
                 |  def test = 1234
                 |}""", classOf[Stat])
  }

  test("inferDefnDefnClass4") {
    assertShallowRoundtrip("""@test class Test extends (A => B) with C {
                 |  def test = 1234
                 |}""", classOf[Stat])
  }

  test("inferDefnDefnClass5") {
    assertShallowRoundtrip("""@test case class Test() extends (A => B) with C {
                 |  def test = 1234
                 |}""", classOf[Stat])
  }

  test("inferDefnDefnClass6") {
    assertShallowRoundtrip("""@test case class Test(a: Int) extends (A => B) with C {
                 |  def test = 1234
                 |}""", classOf[Stat])
  }

  test("inferDefnDefnClass7") {
    assertShallowRoundtrip("""@test case class Test(a: Int, b: String) extends (A => B) with C {
                 |  def test = 1234
                 |}""", classOf[Stat])
  }
  test("inferDefnDefnClass8") {
    assertShallowRoundtrip("""@test case class Test(var a: Int, val b: String) extends (A => B) with C {
                 |  def test = 1234
                 |}""", classOf[Stat])
  }
  test("inferDefnDefnClass9") {
    assertShallowRoundtrip("""@test case class Test[B](var a: Int, val b: String) extends (A => B) with C {
                 |  def test = 1234
                 |}""", classOf[Stat])
  }
  test("inferDefnDefnClass10") {
    assertShallowRoundtrip("""@test case class Test[A, B](var a: Int, val b: String) extends (A => B) with C {
                 |  def test = 1234
                 |}""", classOf[Stat])
  }
  test("inferDefnDefnClass11") {
    assertShallowRoundtrip("""@test @abc case class Test[A, B](var a: Int, val b: String) extends (A => B) with C {
               |  def test = 1234
               |}""", classOf[Stat])
  }
  test("inferDefnDefnClass12") {
    assertShallowRoundtrip("""@test @abc case class Test""", classOf[Stat])
  }

  /* Infering Defn.Trait */
  /* -----------------------------------------------------------------------*/

  test("inferDefnDefnTrait1") {
    assertShallowRoundtrip("""trait Test""", classOf[Stat])
  }
  test("inferDefnDefnTrait2") {
    assertShallowRoundtrip("""trait Test { }""", classOf[Stat])
  }
  test("inferDefnDefnTrait3") {
    assertShallowRoundtrip("""@deprecated trait Test {
                 | val y = test /* hi! */
                 |}""", classOf[Stat])
  }
  test("inferDefnDefnTrait4") {
    assertShallowRoundtrip("""@deprecated sealed trait Test extends AA {
                 | val y = test /* hi! */
                 |}""", classOf[Stat])
  }
  test("inferDefnDefnTrait5") {
    assertShallowRoundtrip("""@deprecated protected[this] sealed trait Test extends (AA => BB) with CC {
                 | val y = test /* hi! */
                 |}""", classOf[Stat])
  }

  /* Infering Defn.Object */
  /* -----------------------------------------------------------------------*/

  test("inferDefnDefnObject1") {
    assertShallowRoundtrip("""object Test {
                 |  def test = 1234
                 |  case object AA
                 |}""", classOf[Stat])
  }
  test("inferDefnDefnObject2") {
    assertShallowRoundtrip("""@ast @tt object Test {
                 |  def test = 1234
                 |  case object AA
                 |}""", classOf[Stat])
  }
  test("inferDefnDefnObject3") {
    assertShallowRoundtrip("""@ast @tt case object Test {
                 |  def test = 1234
                 |  case object AA
                 |}""", classOf[Stat])
  }
  test("inferDefnDefnObject4") {
    assertShallowRoundtrip("""@ast @tt case object Test""", classOf[Stat])
  }

  /* Infering Pkg */
  /* -----------------------------------------------------------------------*/

  test("inferPkg1") {
    val tree = """package test
                 |case class test
                 |""".stripMargin.parse[Source].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Pkg]).asInstanceOf[Pkg]
    assertShallowRoundtrip(t1)
  }
  /* Infering Pkg.Object */
  /* -----------------------------------------------------------------------*/

  test("inferPkgObject1") {
    assertShallowRoundtrip("""package object Test {
                 |  def test = 1234
                 |  case object AA
                 |}""", classOf[Stat])
  }

  /* Infering Ctor */
  /* -----------------------------------------------------------------------*/

  test("CtorPrimary1") {
    val tree = "class Test(x: Int, val y: String)".parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Ctor.Primary]).asInstanceOf[Ctor.Primary]
    assertShallowRoundtrip(t1)
  }
  test("CtorSecondary1") {
    val tree = """class Test(x: Int, val y: String) {
                 |  def this(x: Int, y: Int) = this(x, y.toString)
                 |}""".stripMargin.parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Ctor.Secondary]).asInstanceOf[Ctor.Secondary]
    assertShallowRoundtrip(t1)
  }
  test("CtorSecondary2") {
    val tree = """class Test(x: Int, val y: String) {
                 |  def this(x: Int, y: Int) {
                 |    this(x, y.toString)
                 |    /* this is a comment */
                 |  }
                 |}""".stripMargin.parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Ctor.Secondary]).asInstanceOf[Ctor.Secondary]
    assertShallowRoundtrip(t1, ignoreSpaces = true)
  }

  /* Infering Mods */
  /* -----------------------------------------------------------------------*/

  test("Mods1") {
    assertShallowInfer(Mod.Private(Name.Indeterminate("Test")), "private[Test]")
    assertShallowInfer(Mod.Private(Name.Anonymous()), "private")
    assertShallowInfer(Mod.Protected(Name.Indeterminate("Test")), "protected[Test]")
    assertShallowInfer(Mod.Protected(Name.Anonymous()), "protected")
  }
  test("Mods2") {
    val tree = """@test(url = "http://www.something.com") def x = 4""".parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Mod.Annot]).asInstanceOf[Mod.Annot]
    assertShallowRoundtrip(t1)
  }
  test("Mods3") {
    val tree = """@test(url = "http://www.something.com", v = Seq(1,2,3,4)) def x = 4""".parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Mod.Annot]).asInstanceOf[Mod.Annot]
    assertShallowRoundtrip(t1)
  }
  test("Mods4") {
    val tree = """@test case class Test(x: Int)""".parse[Stat].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Mod.Annot]).asInstanceOf[Mod.Annot]
    assertShallowRoundtrip(t1)
  }

  /* Infering Enumerators */
  /* -----------------------------------------------------------------------*/

  test("Enum1") {
    val tree = "for (x <- 0 to 10) yield x".parse[Term].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Enumerator.Generator]).asInstanceOf[Enumerator.Generator]
    assertShallowRoundtrip(t1)
  }
  test("Enum2") {
    val tree = "for (x <- 0 to 10; y = 10) yield x".parse[Term].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Enumerator.Val]).asInstanceOf[Enumerator.Val]
    assertShallowRoundtrip(t1)
  }
  test("Enum3") {
    val tree = "for (x <- 0 to 10 if x == 0) yield x".parse[Term].get
    // val tree = "for (x <- 0 to 10 if x % 2 == 0) yield x".parse[Term].get // TODO: dunno why this does not pass
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Enumerator.Guard]).asInstanceOf[Enumerator.Guard]
    assertShallowRoundtrip(t1)
  }

  /* Infering Imports and Selector */
  /* -----------------------------------------------------------------------*/

  test("Importee1") {
    assertShallowRoundtrip("import scala.meta", classOf[Stat])
  }
  test("Importee2") {
    assertShallowInfer(Importee.Wildcard(), "_")
  }
  test("Importee3") {
    assertShallowInfer(Importee.Rename(Name.Indeterminate("A"), Name.Indeterminate("B")), "A => B")
  }
  test("Importee4") {
    assertShallowInfer(Importee.Unimport(Name.Indeterminate("A")), "A => _")
  }

  /* Infering case */
  /* -----------------------------------------------------------------------*/

  test("InferCase1") {
    val tree = """x match {
                 |  case x: Int => x
                 |}""".stripMargin.parse[Term].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Case]).asInstanceOf[Case]
    assertShallowRoundtrip(t1)
  }
  test("InferCase2") {
    val tree = """x match {
                 |  case _ => x
                 |}""".stripMargin.parse[Term].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Case]).asInstanceOf[Case]
    assertShallowRoundtrip(t1)
  }
  test("InferCase3") {
    val tree = """x match {
                 |  case (x: Int, y: Int) =>
                 |  val yy = x * y
                 |  yy
                 |}""".stripMargin.parse[Term].get
    val t1 = find(tree)((p: Tree) => p.isInstanceOf[Case]).asInstanceOf[Case]
    assertShallowRoundtrip(t1)
  }
  /* Infering Source */
  /* -----------------------------------------------------------------------*/

  test("inferSource") {
    assertShallowRoundtrip("""class Test {
                 |  def test = 1234
                 |  case object AA
                 |}""", classOf[Source])
  }

  /* Helpers */
  /* -----------------------------------------------------------------------*/

  private def trimLetterbox(tks: Tokens) = {
    tks.filterNot(tk => tk.isInstanceOf[Token.BOF] || tk.isInstanceOf[Token.EOF])
  }

  private def assertShallowInfer(tree: Tree, s_expected: String): Unit = {
    val s_actual = trimLetterbox(tree.resetTokens.tokens).map(_.show[Syntax]).mkString
    if (s_actual != s_expected) {
      Console.err.println(s_actual + "\n" + s_expected)
      assert(s_actual == s_expected)
    }
  }

  private def assertShallowRoundtrip(tree: Tree, ignoreSpaces: Boolean = false): Unit = {
    def tokenCodes(t: Tree) = {
      val codes = trimLetterbox(t.tokens).map(_.show[Syntax])
      if (ignoreSpaces) codes.filter(_ != " ")
      else codes
    }
    val tree1 = tree
    val tree2 = tree.resetTokens
    val codes1 = tokenCodes(tree1)
    val codes2 = tokenCodes(tree2)
    if (codes1 != codes2) {
      Console.err.println(tree1.show[Structure] + "\n" + tree2.show[Structure])
      Console.err.println(codes1 + "\n" + codes2)
      assert(codes1 == codes2)
    }
  }

  private def assertShallowRoundtrip[T <: Tree: scala.meta.parsers.Parse](s: String, dummy: Class[T]): Unit = {
    assertShallowRoundtrip(s.stripMargin.parse[T].get)
  }

  private def find(t: Tree)(p: Tree => Boolean): Tree = {
    t.collect { case tree if p(tree) => tree }.headOption.get
  }
}