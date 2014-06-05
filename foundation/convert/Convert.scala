package org.scalareflect

package convert {
  trait Convert[A, B] {
    def apply(a: A): B
  }

  // TODO: add default instances
  object Convert {
    def apply[A, B](f: A => B): Convert[A, B] =
      new Convert[A, B] { def apply(a: A): B = f(a) }

    implicit def implicitToExplicitConversion[A, B](implicit conv: A => B): Convert[A, B] = Convert(conv)
  }
}

package object convert {
  implicit class ConvertA2B[A](val a: A) extends AnyVal {
    def convert[B](implicit convert: Convert[A, B]): B = convert(a)
  }
}
