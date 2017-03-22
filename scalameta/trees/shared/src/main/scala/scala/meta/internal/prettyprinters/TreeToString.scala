package scala.meta
package internal
package prettyprinters

import scala.meta.internal.ast.{Quasi, Origin}
import scala.meta.dialects.{Scala211, QuasiquoteTerm}
import scala.meta.prettyprinters.Options.Lazy

object TreeToString {
  def apply(tree: Tree) = {
    val dialect = tree.origin match {
      case Origin.Parsed(_, dialect, _) => dialect
      case Origin.None if tree.isInstanceOf[Quasi] => QuasiquoteTerm(Scala211, multiline = true)
      case Origin.None => Scala211 // this dialect is as good as any as a default
    }
    val prettyprinter = TreeSyntax[Tree](dialect, Lazy)
    val code = prettyprinter(tree).toString
    tree match {
      case _: Quasi => code
      case Ctor.Primary(_, name, _) => s"def this$code"
      case _ => code
    }
  }
}
