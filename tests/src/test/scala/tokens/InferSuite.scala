import scala.meta._
import org.scalatest._
import scala.meta.dialects.Scala211

import scala.meta.internal.ui.inferTokens

class InferSuite extends ParseSuite { // TODO

  private def compareTokenCodes(a: Tree, b: Tree): Unit = {
    def trimTokens(tks: Tokens) = tks.filterNot(tk => tk.isInstanceOf[Token.BOF] || tk.isInstanceOf[Token.EOF])
    val t1 = trimTokens(a.tokens).map(_.show[Code])
    val t2 = trimTokens(b.tokens).map(_.show[Code])
    if (t1 != t2)
    println(t1 + "\n" + t2)
    assert(t1 == t2)
  }

  private def debug(trees: Tree*): Unit = {
    trees.foreach {tree => 
      println
      println(tree.show[Raw])
      println(tree.tokens)
    }
  }

  /* Infering Decl.Val */
  /* -----------------------------------------------------------------------*/

  test("InferDeclVal1") {
    val tree = """val a: Int"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Val]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDeclVal2") {
    val tree = """protected[Test] lazy val a: Int"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Val]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDeclVal3") {
    val tree = """@deprecated val a: (A => B)"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Val]
    compareTokenCodes(tree, tree.copy())
  }

  /* Infering Decl.Var */
  /* -----------------------------------------------------------------------*/

  test("InferDeclVar1") {
    val tree = """var a: Int"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Var]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDeclVar2") {
    val tree = """protected[Test] lazy var a: Int"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Var]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDeclVar3") {
    val tree = """@deprecated var a: (A => B)"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Var]
    compareTokenCodes(tree, tree.copy())
  }

  /* Infering Decl.Type */
  /* -----------------------------------------------------------------------*/

  test("InferDeclType1") {
    val tree = """type a"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Type]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDeclType2") {
    val tree = """type a >: Int"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Type]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDeclType3") {
    val tree = """type a <: Int"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Type]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDeclType4") {
    val tree = """@test protected[Test] type a[A, B] <: (A => B)"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Type]
    compareTokenCodes(tree, tree.copy())
  }

  /* Infering Decl.Def */
  /* -----------------------------------------------------------------------*/

  test("inferDeclDef1") {
    val tree = """def aaa: Int"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDeclDef2") {
    val tree = """def aaa(): (A => B)"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDeclDef3") {
    val tree = """def aaa[T](b: T): String"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDeclDef5") {
    val tree = """def aaa[T, B](b: T)(c: Int = 0): (T => B)"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDeclDef6") {
    val tree = """implicit def aaa[T, B](b: T)(c: Int = 0): (T /* This is a comment */ => B)"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDeclDef7") {
    val tree = """private[test] def aaa[T, B, E](b/*This is a comment*/: T, a: String)(c: Int = 0): (T => B)"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Decl.Def]
    compareTokenCodes(tree, tree.copy())
  }

  /* Infering Defn.Val */
  /* -----------------------------------------------------------------------*/

  test("InferDefnVal1") {
    val tree = """val a = 123"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Val]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVal2") {
    val tree = """val a: String = 123"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Val]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVal3") {
    val tree = """lazy val a = 123"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Val]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVal4") {
    val tree = """protected[test] lazy val a = 123"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Val]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVal5") {
    val tree = """@test override val a: Int = {
                 |  val x = 231
                 |  21323 + x
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Val]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVal6") {
    val tree = """val (aa, bb) = (12, 23)"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Val]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVal7") {
    val tree = """val (aa, bb, (cc, dd)) = (12, 23, (11, 23))"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Val]
    compareTokenCodes(tree, tree.copy())
  }

  /* Infering Defn.Var */
  /* -----------------------------------------------------------------------*/
  
  test("InferDefnVar1") {
    val tree = """var a = 123"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Var]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVar2") {
    val tree = """var a: String = 123"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Var]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVar3") {
    val tree = """lazy var a = 123"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Var]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVar4") {
    val tree = """protected[test] lazy var a = 123"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Var]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVar5") {
    val tree = """@test override var a: Int = {
                 |  val x = 231
                 |  21323 + x
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Var]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVar6") {
    val tree = """var (aa, bb) = (12, 23)"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Var]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnVar7") {
    val tree = """var (aa, bb, (cc, dd)) = (12, 23, (11, 23))"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Var]
    compareTokenCodes(tree, tree.copy())
  }
    
  /* Infering Defn.Def */
  /* -----------------------------------------------------------------------*/

  test("inferDefnDef1") {
    val tree = """def aaa = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDef2") {
    val tree = """def aaa() = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDef3") {
    val tree = """def aaa[T](b: T) = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDef4") {
    val tree = """def aaa[T](b: T): Int = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDef5") {
    val tree = """def aaa[T, B](b: T)(c: Int = 0): (T => B) = { 2132 /* This is a comment */ }"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDef6") {
    val tree = """implicit def aaa[T, B](b: T)(c: Int = 0): (T => B) = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDef7") {
    val tree = """private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDef8") {
    val tree = """private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = {
                 |  def yy = "hi" // new comment 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }

  /* Infering macros */
  /* -----------------------------------------------------------------------*/

  // TODO

  /* Infering Defn.Type */
  /* -----------------------------------------------------------------------*/

  test("InferDefnType1") {
    val tree = """type a = Int"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Type]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnType2") {
    val tree = """@test type a = Int"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Type]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnType3") {
    val tree = """type a[T, E, C] = Int"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Type]
    compareTokenCodes(tree, tree.copy())
  }
  test("InferDefnType4") {
    val tree = """@test protected[Test] type a[A, B] = (A => B)"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Type]
    compareTokenCodes(tree, tree.copy())
  }

  /* Infering Defn.Class */
  /* -----------------------------------------------------------------------*/

  test("inferDefnDefnClass1") {
    val tree = """class Test { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    val treeCp = 
    compareTokenCodes(tree, tree.copy())
  }

  test("inferDefnDefnClass2") {
    val tree = """@ast class Test { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    val treeCp = tree.copy()
    compareTokenCodes(tree, tree.copy())
  }

  test("inferDefnDefnClass3") {
    val tree = """class Test extends Any { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    val treeCp = tree.copy()
    compareTokenCodes(tree, tree.copy())
  }

  test("inferDefnDefnClass4") {
    val tree = """@test class Test extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }

  test("inferDefnDefnClass5") {
    val tree = """@test case class Test() extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }

  test("inferDefnDefnClass6") {
    val tree = """@test case class Test(a: Int) extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }

  test("inferDefnDefnClass7") {
    val tree = """@test case class Test(a: Int, b: String) extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnClass8") {
    val tree = """@test case class Test(var a: Int, val b: String) extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnClass9") {
    val tree = """@test case class Test[B](var a: Int, val b: String) extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnClass10") {
    val tree = """@test case class Test[A, B](var a: Int, val b: String) extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnClass11") {
  val tree = """@test @abc case class Test[A, B](var a: Int, val b: String) extends (A => B) with C { 
               |  def test = 1234 
               |}"""
    .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
   compareTokenCodes(tree, tree.copy())
 }
 test("inferDefnDefnClass12") {
  val tree = """@test @abc case class Test"""
    .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
   compareTokenCodes(tree, tree.copy())
 }

  /* Infering Defn.Trait */
  /* -----------------------------------------------------------------------*/

  test("inferDefnDefnTrait1") {
    val tree = """trait Test"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Trait]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnTrait2") {
    val tree = """trait Test { }"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Trait]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnTrait3") {
    val tree = """@deprecated trait Test {
                 | val y = test /* hi! */
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Trait]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnTrait4") {
    val tree = """@deprecated sealed trait Test extends AA {
                 | val y = test /* hi! */
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Trait]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnTrait5") {
    val tree = """@deprecated protected[this] sealed trait Test extends (AA => BB) with CC {
                 | val y = test /* hi! */
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Trait]
    compareTokenCodes(tree, tree.copy())
  }

  /* Infering Defn.Object */
  /* -----------------------------------------------------------------------*/

  test("inferDefnDefnObject1") {
    val tree = """object Test { 
                 |  def test = 1234
                 |  case object AA
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Object]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnObject2") {
    val tree = """@ast @tt object Test { 
                 |  def test = 1234
                 |  case object AA
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Object]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnObject3") {
    val tree = """@ast @tt case object Test { 
                 |  def test = 1234
                 |  case object AA
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Object]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDefnDefnObject4") {
    val tree = """@ast @tt case object Test"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Object]
    compareTokenCodes(tree, tree.copy())
  }

  /* Infering Pkg */
  /* -----------------------------------------------------------------------*/

  // TODO

  /* Infering Pkg.Object */
  /* -----------------------------------------------------------------------*/

  // TODO

  /* Infering Ctor.Primary */
  /* -----------------------------------------------------------------------*/

  // TODO

  /* Infering Ctor.Secondary */
  /* -----------------------------------------------------------------------*/

  // TODO

  /* Infering Source */
  /* -----------------------------------------------------------------------*/

  test("inferSource") {
    val tree = """class Test { 
                 |  def test = 1234 
                 |  case object AA
                 |}"""
      .stripMargin.parse[Source].asInstanceOf[scala.meta.internal.ast.Source]
    compareTokenCodes(tree, tree.copy())
  }

}