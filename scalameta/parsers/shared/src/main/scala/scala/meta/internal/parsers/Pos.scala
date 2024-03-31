package scala.meta.internal.parsers

import scala.meta.Tree
import scala.meta.prettyprinters._
import scala.meta.trees.Origin

// NOTE: `startTokenPos` and `endTokenPos` are BOTH INCLUSIVE.
// This is at odds with the rest of scala.meta, where ends are non-inclusive.
trait StartPos {
  def startTokenPos: Int
}

trait EndPos {
  def endTokenPos: Int
}

trait Pos extends StartPos with EndPos

class IndexPos(index: => Int) extends Pos {
  def startTokenPos = index
  def endTokenPos = index
}

class TreePos(tree: Tree) extends Pos {
  val (startTokenPos, endTokenPos) = tree.origin match {
    case x: Origin.Parsed => (x.begTokenIdx, x.endTokenIdx - 1)
    case _ => sys.error(s"internal error: unpositioned prototype ${tree.syntax}: ${tree.structure}")
  }
}
