package scaladays

import org.scalatest._
import scaladays.adt._
import scaladays.serialization._

sealed trait List
final case class Cons(head: Int, tail: List) extends List
final case object Nil extends List

class ScalaDays extends FunSuite {
  // NOTE: In memory of: https://github.com/scalareflect/core/commit/7f3058d1293b46fc255f525a92deaeec4d64026d
  // Initial commit
  // master
  //  scalareflect  authored on Mar 8, 2014
  test("San Francisco 2015") {
    val list: List = Cons(1, Cons(2, Nil))
    assert(serialize(list) === """{"head": 1, "tail": {"head": 2, "tail": {$tag: 1}, $tag: 0}, $tag: 0}""")
  }
}
