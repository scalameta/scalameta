// See https://github.com/scalameta/scalameta/issues/1586
package example

class ExistentialsB[T]
class ExistentialsC[CC[_]] extends ExistentialsB[CC[_]]

class ExistentialsE {
  def m: List[List[_]] = ???
  def l: List[_] = ???
  val x: Map[_, _] = ???
  def f: T forSome { type T } = ???
  def h[F[_]]: F[_] = ???
  type A = List[_]
}

object ExistentialsTest {
  val x = new ExistentialsE().m
  val y: Map[_, _] = new ExistentialsE().x
}
