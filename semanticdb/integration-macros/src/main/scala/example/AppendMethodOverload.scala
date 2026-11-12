package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** Keeps the annotated method and adds an overload `(s: Int): Int` after it. */
class AppendMethodOverload extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AppendMethodOverload.impl
}

object AppendMethodOverload {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val Seq(d: DefDef) = annottees
    q"$d; def ${d.name}(s: Int): Int = 2"
  }
}
