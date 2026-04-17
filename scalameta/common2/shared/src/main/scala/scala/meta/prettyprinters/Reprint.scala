package scala.meta.prettyprinters

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to ${T}.reprint")
trait Reprint[-T] {
  def apply(obj: T, withComments: Boolean, useOriginal: Boolean): String
}
