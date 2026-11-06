package example

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object ConstantExpansion {
  def one[T](t: T): Any = macro oneImpl

  def oneImpl(c: whitebox.Context)(t: c.Tree): c.Tree = {
    import c.universe._
    q"1"
  }
}
