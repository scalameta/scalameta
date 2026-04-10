package org.scalameta.data

import org.scalameta.adt.{Reflection => AdtReflection}
import org.scalameta.internal.MacroHelpers

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

// Parts of @data logic that need a typer context and can't be run in a macro annotation.
object DataTyperMacros {
  def nullCheck[T](x: T): Unit = macro DataTyperMacrosBundle.nullCheck
  def emptyCheck[T](x: T): Unit = macro DataTyperMacrosBundle.emptyCheck
}

// NOTE: can't call this `DataTyperMacros`, because then typechecking the macro defs will produce spurious cyclic errors
class DataTyperMacrosBundle(val c: Context) extends AdtReflection with MacroHelpers {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._
  import definitions._

  def nullCheck(x: c.Tree): c.Tree =
    if (!x.tpe.baseClasses.contains(ObjectClass)) q"()"
    else {
      val clue = s"${showCode(x)} should be non-null"
      q"""$InvariantsRequireMethod($x != null, $clue)"""
    }

  def emptyCheck(x: c.Tree): c.Tree = {
    val emptyCheckRequested =
      try x.symbol.asTerm.accessed.nonEmpty
      catch { case _: AssertionError => x.symbol.nonEmpty }
    if (emptyCheckRequested) {
      val clue = s"${showCode(x)} should be non-empty"
      q"""
        $InvariantsRequireMethod(
          $x != null && ($x.isInstanceOf[$QuasiClass] || $x.nonEmpty),
          $clue
        )
      """
    } else q"()"
  }
}
