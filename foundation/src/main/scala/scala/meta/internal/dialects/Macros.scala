package scala.meta
package internal
package dialects

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import org.scalameta.adt.{Reflection => AdtReflection}
import org.scalameta.internal.MacroHelpers

class Macros(val c: Context) extends MacroHelpers with AdtReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._

  def all: c.Tree = {
    val dialects = mirror.staticClass("scala.meta.Dialect").asRoot.allLeafs
    val relevantDialects = dialects.filter(_.sym.name.toString != "Quasiquote")
    q"${relevantDialects.map(leaf => hygienicRef(leaf.sym)).toList}"
  }

  def current: c.Tree = {
    // TODO: We'll have to expand this in the future,
    // but for now the only other supported dialect is Dotty,
    // and we don't yet have a dottyhost, so yolo.
    q"_root_.scala.meta.dialects.Scala211"
  }
}
