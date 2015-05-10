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

  /* Infering classes */

  test("inferSource") {
    val tree = """class Test { 
                 |  def test = 1234 
                 |  case object AA
                 |}"""
      .stripMargin.parse[Source].asInstanceOf[scala.meta.internal.ast.Source]
    compareTokenCodes(tree, tree.copy())
  }

  test("inferClass1") {
    val tree = """class Test { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    val treeCp = 
    compareTokenCodes(tree, tree.copy())
  }

  test("inferClass2") {
    val tree = """@ast class Test { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    val treeCp = tree.copy()
    compareTokenCodes(tree, tree.copy())
  }

  test("inferClass3") {
    val tree = """class Test extends Any { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    val treeCp = tree.copy()
    compareTokenCodes(tree, tree.copy())
  }

  test("inferClass4") {
    val tree = """@test class Test extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }

  test("inferClass5") {
    val tree = """@test case class Test() extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }

  test("inferClass6") {
    val tree = """@test case class Test(a: Int) extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }

  test("inferClass7") {
    val tree = """@test case class Test(a: Int, b: String) extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferClass8") {
    val tree = """@test case class Test(var a: Int, val b: String) extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferClass9") {
    val tree = """@test case class Test[B](var a: Int, val b: String) extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferClass10") {
    val tree = """@test case class Test[A, B](var a: Int, val b: String) extends (A => B) with C { 
                 |  def test = 1234 
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferClass11") {
  val tree = """@test @abc case class Test[A, B](var a: Int, val b: String) extends (A => B) with C { 
               |  def test = 1234 
               |}"""
    .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]
   compareTokenCodes(tree, tree.copy())
 }

  /* Infering objects */

  test("inferObject1") {
    val tree = """object Test { 
                 |  def test = 1234
                 |  case object AA
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Object]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferObject2") {
    val tree = """@ast @tt object Test { 
                 |  def test = 1234
                 |  case object AA
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Object]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferObject3") {
    val tree = """@ast @tt case object Test { 
                 |  def test = 1234
                 |  case object AA
                 |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Object]
    compareTokenCodes(tree, tree.copy())
  }

  /* Infering Defn.defs */

  test("inferDef1") {
    val tree = """def aaa = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDef2") {
    val tree = """def aaa() = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDef3") {
    val tree = """def aaa[T](b: T) = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDef4") {
    val tree = """def aaa[T](b: T): Int = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDef5") {
    val tree = """def aaa[T, B](b: T)(c: Int = 0): (T => B) = { 2132 /* This is a comment */ }"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDef6") {
    val tree = """implicit def aaa[T, B](b: T)(c: Int = 0): (T => B) = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  test("inferDef7") {
    val tree = """private[test] def aaa[T, B, E](b: T, a: String)(c: Int = 0): (T => B) = 2132"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Def]
    compareTokenCodes(tree, tree.copy())
  }
  
}