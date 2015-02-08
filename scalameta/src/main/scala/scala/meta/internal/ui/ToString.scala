package scala.meta
package internal
package ui

object toString {
  def apply(tree: Tree) = {
    // NOTE: if we leave implicit inference to chance, we're going to get
    // `illegal cyclic reference involving package object meta`
    // probably a scalac bug, but I've no time or desire to troubleshoot it
    // import scala.meta.ui.`package`.ShowOps
    // import scala.meta.ui.Code
    // import scala.meta.dialects.`package`.Scala211
    // tree.show[Code]
    val dialect = scala.meta.dialects.`package`.Scala211
    val prettyprinter = scala.meta.ui.Code.codeTree[Tree](dialect)
    prettyprinter(tree).toString
  }
}
