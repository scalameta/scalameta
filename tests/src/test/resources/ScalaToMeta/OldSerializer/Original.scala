sealed trait List
final case class Cons(head: Int, tail: List) extends List
final case object Nil extends List

object OldSerializer {
  val list: List = Cons(1, Cons(2, Nil))
  println(serializer.reflect.separate.serialization.serialize(list))
}