package scala

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object invariants {
  def require[T](x: T): Unit = macro Macros.require
}

package invariants {
  class Macros(val c: Context) {
    import c.universe._
    def require(x: c.Tree): c.Tree = {
      q"scala.Predef.require($x)"
    }
  }
}