package org.scalareflect

import scala.annotation._

package convert {
  trait Cvt[T, U] {
    def cvt(x: T): U
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