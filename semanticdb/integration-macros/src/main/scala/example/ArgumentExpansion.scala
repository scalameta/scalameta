package example

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object ArgumentExpansion {
  def same(t: Any): Any = macro sameImpl

  def sameImpl(c: whitebox.Context)(t: c.Tree): c.Tree = t
}
