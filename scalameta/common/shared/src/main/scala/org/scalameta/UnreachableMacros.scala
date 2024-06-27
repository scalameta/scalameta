package org.scalameta

import org.scalameta.internal.MacroHelpers

import scala.reflect.macros.blackbox.Context

class UnreachableMacros(val c: Context) extends MacroHelpers {
  import c.universe._
  def unreachable: c.Tree = q"$UnreachableErrorModule.raise(${Map.empty[String, Tree]})"
  def unreachableWithDebug(dsl: c.Tree): c.Tree =
    q"$UnreachableErrorModule.raise(${debuggees(dsl)})"
  def unreachableWithDebugAndClue(dsl: c.Tree, clue: c.Tree): c.Tree =
    q"$UnreachableErrorModule.raise(${debuggees(dsl)}, $clue)"
}
