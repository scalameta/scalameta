package scala.meta
package internal
package ui

import scala.meta.internal.{ast => impl}
import scala.meta.dialects.`package`.Scala211

private[meta] object toString {
  def apply(tree: Tree) = {
    val prettyprinter = TreeSyntax[Tree](Scala211)
    val code = prettyprinter(tree).toString
    tree match {
      case _: impl.Quasi => code
      case impl.Ctor.Primary(_, name, _) => s"def this$code"
      case _ => code
    }
  }
}
