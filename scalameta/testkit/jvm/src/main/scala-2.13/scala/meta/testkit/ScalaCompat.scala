package scala.meta.testkit

private[testkit] object ScalaCompat {
  type GenTraversableOnce[+A] = IterableOnce[A]
  type GenIterable[+A] = Iterable[A]

  def foreach[A](xs: GenTraversableOnce[A])(f: A => Unit): Unit = xs match {
    case xs: Iterable[A] => xs.foreach(f)
    case _ => xs.iterator.foreach(f)
  }
}
