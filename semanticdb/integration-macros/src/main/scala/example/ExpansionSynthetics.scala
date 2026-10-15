package example

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object ExpansionSynthetics {
  def typeApply(): Any = macro typeApplyImpl
  def implicitArg(): Any = macro implicitArgImpl
  def implicitView(): Any = macro implicitViewImpl
  def untyped(): Any = macro untypedImpl

  def typeApplyImpl(c: blackbox.Context)(): c.Tree = {
    import c.universe._
    c.typecheck(q"_root_.scala.collection.immutable.List(1).map(x => x)")
  }
  def implicitArgImpl(c: blackbox.Context)(): c.Tree = {
    import c.universe._
    c.typecheck(q"_root_.scala.collection.immutable.List(1, 2).sorted")
  }
  def implicitViewImpl(c: blackbox.Context)(): c.Tree = {
    import c.universe._
    c.typecheck(q""""a".reverse""")
  }
  def untypedImpl(c: blackbox.Context)(): c.Tree = {
    import c.universe._
    q"""_root_.scala.collection.immutable.List(1, 2).sorted.map(x => x + "a")"""
  }
}
