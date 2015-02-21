package scala.meta
package internal
package dialects

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class Macros(val c: Context) {
  import c.universe._
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
  def dialectFromSemanticContext(c: Tree): Tree = q"$c.dialect"
}
