package selects

import scala.language.reflectiveCalls

class C[T] {
  def t: T = ???
}

class Structural {
  def s1: { val x: Int } = ???
  def s2 = new { val x: Int = ??? }
}

class Existential {
  def e1: List[_] = ???
  def e2: C[List[T] forSome { type T }] = ???
}

object Test {
  val s = new Structural
  val s1 = s.s1
  val s1x = s.s1.x
  val s2 = s.s2
  val s2x = s.s2.x

  val e = new Existential
  val e1 = e.e1
  val e1x = e.e1.head
  val e2 = e.e2
  val e2x = e.e2.t
  val e2xx = e.e2.t.head
  locally {
    (??? : Any) match {
      case e3: List[_] =>
        val e3x = e3.head
        ()
    }
  }
}
