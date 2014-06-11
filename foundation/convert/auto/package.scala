package org.scalareflect.convert

import scala.annotation._

package object auto {
  implicit class Ops(x: Any) {
    @compileTimeOnly("cvt isn't meant to be used outside @converter methods")
    def cvt : Nothing = ???

    @compileTimeOnly("cvt_! isn't meant to be used outside @converter methods")
    def cvt_! : Nothing = ???
  }

  @compileTimeOnly("derive isn't meant to be used outside @converter methods")
  def derive: Nothing = ???
}