import scala.meta._
import org.scalatest._
import scala.meta.dialects.Scala211

import scala.meta.internal.ui.inferTokens

class InferSuite extends ParseSuite { // TODO

  private def compareTokensCode(a: Tree, b: Tree): Unit = {
    def trimTokens(tks: Tokens) = tks.filterNot(tk => tk.isInstanceOf[Token.BOF] || tk.isInstanceOf[Token.EOF])
    val t1 = trimTokens(a.tokens).map(_.show[Code])
    val t2 = trimTokens(b.tokens).map(_.show[Code])
    assert(t1 == t2)
  }

  test("inferSource") {
    val tree = """class Test {
                  |  def test = 1234
                  |}"""
      .stripMargin.parse[Stat].asInstanceOf[scala.meta.internal.ast.Defn.Class]

    val treeCp = tree.copy()
    println(tree.tokens)
    println(treeCp.tokens)
    println
    println(tree.tokens.map(_.show[Code]).mkString)
    println(treeCp.tokens.map(_.show[Code]).mkString)

    compareTokensCode(tree, treeCp)
  }
}