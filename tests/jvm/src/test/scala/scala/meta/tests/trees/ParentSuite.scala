package scala.meta
package tests
package trees

import org.scalatest._

import scala.meta.contrib._

class ParentSuite extends FunSuite {

  test("Tree.transform does not preserve parent chain origins") {
    val original = 
      """|object a {
         |  def bar = 4
         |  // comment
         |  def foo = 2
         |}""".stripMargin.parse[Source].get

    val originalPositions = original.collect { case t => t.pos }
    assert(originalPositions.forall(_ != Position.None))

    val refactored = original.transform { case q"2" => q"3" }

    val originalNodes = original.collect{ case t => t }
    val refactoredNode = refactored.collect{ case t => t }

    assert(originalNodes.size == refactoredNode.size)
    
    originalNodes.zip(refactoredNode).foreach{
      case (o @ q"2", r @ q"3") => 
        assert(o.pos != Position.None)
        assert(r.pos == Position.None)
      case (o, r) => 
        assert(o.pos == r.pos)
    }
  }
}
