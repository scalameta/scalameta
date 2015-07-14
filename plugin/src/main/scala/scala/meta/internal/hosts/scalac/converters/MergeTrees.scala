package scala.meta
package internal.hosts.scalac
package converters

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.ast._
import org.scalameta.invariants._
import org.scalameta.unreachable

object mergeTrees {
  def apply(syntacticTree: Tree, semanticTree: Tree): Tree = {
    unreachable(debug(syntacticTree, syntacticTree.show[Structure], semanticTree, semanticTree.show[Structure]))
  }
}