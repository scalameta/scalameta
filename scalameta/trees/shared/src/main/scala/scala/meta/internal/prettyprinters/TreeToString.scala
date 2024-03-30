package scala.meta
package internal
package prettyprinters

import scala.meta.dialects
import scala.meta.internal.trees.Quasi

object TreeToString {
  // this dialect is as good as any as a default
  private val defaultDialect = dialects.Scala213

  def apply(tree: Tree) = {
    val isQuasi = tree.isInstanceOf[Quasi]
    val dialect = tree.origin.dialectOpt
      .getOrElse(if (isQuasi) defaultDialect.unquoteTerm(multiline = true) else defaultDialect)
    val prettyprinter = TreeSyntax[Tree](dialect)
    val code = prettyprinter(tree).toString
    if (!isQuasi && tree.isInstanceOf[Ctor.Primary]) s"def this$code" else code
  }
}
