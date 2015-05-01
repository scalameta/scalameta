package scala.meta
package internal
package ui

import scala.meta.internal.{ast => impl}
import scala.meta.dialects.`package`.Scala211
import scala.meta.ui.Code.{codeTree, Style}

private[meta] object toString {
  def apply(tree: Tree) = {
    // NOTE: if we leave implicit inference to chance, we're going to get
    // `illegal cyclic reference involving package object meta`
    // probably a scalac bug, but I've no time or desire to troubleshoot it
    // import scala.meta.ui.`package`.ShowOps
    // import scala.meta.ui.Code
    // import scala.meta.dialects.`package`.Scala211
    // tree.show[Code]
    val prettyprinter = codeTree[Tree](Scala211, Style.Lazy)
    val code = prettyprinter(tree).toString
    tree match {
      case _: impl.Quasi => code
      case impl.Ctor.Primary(_, name, _) => s"def this$code"
      case _ => code
    }
  }
}
