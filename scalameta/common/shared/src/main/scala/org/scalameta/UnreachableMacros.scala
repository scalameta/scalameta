package org.scalameta

import scala.reflect.macros.blackbox.Context
import org.scalameta.internal.MacroHelpers

class UnreachableMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  def unreachable: c.Tree = q"$UnreachableErrorModule.raise(${Map.empty[String, Tree]})"
  def unreachableWithDebug(dsl: c.Tree): c.Tree =
    q"$UnreachableErrorModule.raise(${debuggees(dsl)})"
}
