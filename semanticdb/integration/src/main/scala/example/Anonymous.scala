package example
import scala.language.higherKinds

class Anonymous {
  this: Anonymous =>

  def m1[T[_], _] = ???
  def m2: Map[_, List[_]] = ???
  locally {
    ??? match { case _: List[_] => }
  }
  locally {
    val x: Int => Int = _ => ???
  }

  trait Foo
  new Foo {}
}
