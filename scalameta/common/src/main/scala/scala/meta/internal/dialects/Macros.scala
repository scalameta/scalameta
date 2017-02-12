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

  lazy val Dialect = mirror.staticClass("scala.meta.Dialect")
  lazy val dialects = mirror.staticModule("scala.meta.dialects.package")
  lazy val Scala210 = dialects.info.member(TermName("Scala210"))
  lazy val Scala211 = dialects.info.member(TermName("Scala211"))
  lazy val Scala212 = dialects.info.member(TermName("Scala212"))

  def current: c.Tree = {
    // NOTE: Dotty doesn't support def macros yet, so it's implemented differently.
    val version = scala.util.Properties.versionNumberString
    if (version.startsWith("2.10")) hygienicRef(Scala210)
    else if (version.startsWith("2.11")) hygienicRef(Scala211)
    else if (version.startsWith("2.12")) hygienicRef(Scala212)
    else c.abort(c.enclosingPosition, "unsupported Scala version " + version)
  }
}
