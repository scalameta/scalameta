package scala.meta
package internal
package prettyprinters

import scala.meta.internal.ast.Quasi

private[meta] object toString {
  def apply(tree: Tree) = {
    val prettyprinter = TreeSyntax[Tree](scala.meta.dialects.`package`.Scala211)
    val code = prettyprinter(tree).toString
    tree match {
      case _: Quasi => code
      case Ctor.Primary(_, name, _) => s"def this$code"
      case _ => code
    }
  }
}
