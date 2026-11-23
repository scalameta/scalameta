package scala.meta.testkit

private[testkit] object ScalaCompat {
  type GenTraversableOnce[+A] = scala.collection.GenTraversableOnce[A]
  type GenIterable[+A] = scala.collection.GenIterable[A]

  def foreach[A](xs: GenTraversableOnce[A])(f: A => Unit): Unit = xs.foreach(f)
}
