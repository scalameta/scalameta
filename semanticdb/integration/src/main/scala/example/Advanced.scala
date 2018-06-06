package advanced

import scala.language.existentials
import scala.language.higherKinds
import scala.language.reflectiveCalls

class C[T] {
  def t: T = ???
}

class Structural {
  def s1: { val x: Int } = ???
  def s2 = new { val x: Int = ??? }
  def s3 = new { def m(x: Int): Int = ??? }
}

class Existential {
  def e1: List[_] = ???
  def e2: C[List[T] forSome { type T }] = ???
  def e4: U[Int] forSome { type U[T <: Int] } = ???
}

class D[CC[_]] extends C[CC[_]]

object Test {
  val s = new Structural
  val s1 = s.s1
  val s1x = s.s1.x
  val s2 = s.s2
  val s2x = s.s2.x
  val s3 = s.s3
  val s3x = s.s3.m(???)

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
