sealed abstract class ByName[A] {
  def foldRight[B](z: => B)(f: (=> A) => (=> B) => B): B = ???
  def cons[A](a: => A, as: => ByName[A]): ByName[A] = ???
  def ++(e: => ByName[A]): ByName[A] = foldRight[ByName[A]](e)(cons[A](_, _).curried)
}