package scala.meta
package syntactic.quasiquotes

import scala.reflect.macros.whitebox.Context
import scala.meta.{Tree => PalladiumTree}

class Macros[C <: Context](val c: C) {
  import c.universe._
  def apply(macroApplication: Tree, parse: String => PalladiumTree): Tree = {
    ???
  }
}
