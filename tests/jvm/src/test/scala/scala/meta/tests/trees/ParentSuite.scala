package scala.meta
package tests
package trees

import org.scalatest._

class ParentSuite extends FunSuite {

  test("Tree.transform does not preserve parent chain origins") {
    val code = """
      object a {
        def bar = 4
        // comment
        def foo = 2
      }
    """.parse[Source].get

    val originalPositions = code.collect { case t => t.pos }
    assert(originalPositions.forall(_ != Position.None))

    val obtained =
      code.transform { case q"2" => q"3" }.collect { case t => t.pos }

    assert(obtained.count(_ == Position.None) == 1)
  }
}