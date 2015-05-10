import scala.meta._
import org.scalatest._
import scala.meta.dialects.Scala211

import scala.meta.internal.ui.inferTokens

class InferSuite extends ParseSuite { // TODO

  test("inferSource") {
    val tree = "class Test { }".parse[Source].asInstanceOf[scala.meta.internal.ast.Source]
    val treeCp = tree.copy()
    println(treeCp.tokens)
  }

}