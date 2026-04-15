package scala.meta
package internal
package prettyprinters

import scala.meta.dialects
import scala.meta.internal.trees.Quasi
import scala.meta.prettyprinters.Show

object TreeToString {
  // this dialect is as good as any as a default
  private val defaultDialect = dialects.Scala213

  def apply(tree: Tree): String = {
    val isQuasi = tree.isInstanceOf[Quasi]
    implicit val dialect: Dialect = tree.origin.dialectOpt
      .getOrElse(if (isQuasi) defaultDialect.unquoteTerm(multiline = true) else defaultDialect)
    val code = TreeSyntax.syntax(tree)
    val isCtor = !isQuasi && tree.isInstanceOf[Ctor.Primary]
    (if (isCtor) Show.sequence("def this", code) else code).toString
  }
}
