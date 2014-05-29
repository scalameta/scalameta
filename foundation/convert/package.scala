package org.scalareflect

import scala.annotation._

package convert {
  trait Cvt[T, U] {
    def apply(x: T): U
  }

  object Cvt {
    def apply[T, U](f: T => U): Cvt[T, U] = new Cvt[T, U] { def apply(x: T): U = f(x) }
  }

  trait CvtOps {
    implicit class RichCvt(x: Any) {
      @compileTimeOnly("cvt isn't meant to be used outside @converter methods")
      def cvt : Nothing = ???

      @compileTimeOnly("!!! isn't meant to be used outside @converter methods")
      def !!! : Nothing = ???
    }
  }
}

package object convert extends CvtOps