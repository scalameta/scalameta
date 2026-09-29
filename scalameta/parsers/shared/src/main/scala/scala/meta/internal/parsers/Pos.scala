package scala.meta.internal.parsers

import scala.meta.Tree
import scala.meta.prettyprinters._
import scala.meta.trees.Origin

// NOTE: `startTokenPos` and `endTokenPos` are BOTH INCLUSIVE.
// This is at odds with the rest of scala.meta, where ends are non-inclusive.
trait StartPos extends Any {
  def begIndex: Int
}

trait EndPos extends Any {
  def endIndex: Int
}

trait Pos extends Any with StartPos with EndPos

class IndexPos(val index: Int) extends AnyVal with Pos {
  def begIndex = index
  def endIndex = index
}

class TreePos(val tree: Tree) extends AnyVal with Pos {
  def begIndex = tree.begTokenIdx
  def endIndex = tree.endTokenIdx - 1
}
