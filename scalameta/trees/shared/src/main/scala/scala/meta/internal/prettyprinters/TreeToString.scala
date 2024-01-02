package scala.meta
package internal
package prettyprinters

import scala.meta.dialects.{Scala211, QuasiquoteTerm}
import scala.meta.internal.trees.{Quasi, Origin}

object TreeToString {
  def apply(tree: Tree) = {
    val dialect = tree.origin match {
      case x: Origin.Parsed => x.dialect
      case Origin.None if tree.isInstanceOf[Quasi] => QuasiquoteTerm(Scala211, multiline = true)
      case Origin.None => Scala211 // this dialect is as good as any as a default
    }
    val prettyprinter = TreeSyntax[Tree](dialect)
    val code = prettyprinter(tree).toString
    tree match {
      case _: Quasi => code
      case Ctor.Primary(_, name, _) => s"def this$code"
      case _ => code
    }
  }
}
