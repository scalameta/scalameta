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

  lazy val DialectClass = mirror.staticClass("scala.meta.Dialect")
  lazy val Scala210 = mirror.staticModule("scala.meta.dialects.Scala210")
  lazy val Scala211 = mirror.staticModule("scala.meta.dialects.Scala211")
  lazy val Dotty = mirror.staticModule("scala.meta.dialects.Dotty")

  def all: c.Tree = {
    val dialects = DialectClass.asRoot.allLeafs
    val relevantDialects = dialects.filter(_.sym.name.toString != "Quasiquote")
    q"${relevantDialects.map(leaf => hygienicRef(leaf.sym)).toList}"
  }

  def current: c.Tree = {
    val version = scala.util.Properties.versionNumberString
    if (version.startsWith("2.10")) hygienicRef(Scala210)
    else if (version.startsWith("2.11")) hygienicRef(Scala211)
    else c.abort(c.enclosingPosition, "unsupported Scala version " + version)
  }
}
