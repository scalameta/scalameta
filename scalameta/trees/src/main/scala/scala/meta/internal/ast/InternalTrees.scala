package scala.meta
package internal
package ast

import org.scalameta.debug
import org.scalameta.invariants._
import scala.compat.Platform.EOL
import scala.meta.classifiers._
import scala.meta.prettyprinters._
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.tokens.Token._
import scala.meta.tokenizers._
import scala.meta.internal.inputs._
import scala.meta.internal.prettyprinters._
import scala.meta.internal.tokens._

// NOTE: Methods that start with "private" are NOT intended to be called outside scala.meta.
// Calling these methods from hosts will compile (because hosts are in meta), but is strongly discouraged.
trait InternalTree {
  self: Tree =>

  // =============================================================================================
  // Pieces of internal state of scala.meta ASTs.
  // Some nodes define all of them as fields, some nodes define only a subset.
  // However, all nodes have corresponding defs to simplify uniform treatment.
  // Everyone except for scala.meta's core should be using "private"-less versions of these methods,
  // because those are only available on appropriate trees.
  // =============================================================================================

  private[meta] def privatePrototype: Tree
  private[meta] def privateParent: Tree
  private[meta] def privateOrigin: Origin
  private[meta] def privateCopy(
    prototype: Tree = this,
    parent: Tree = privateParent,
    origin: Origin = privateOrigin): Tree

  // =============================================================================================
  // Getters for pieces of internal state defined above.
  // =============================================================================================

  def parent: Option[Tree] = {
    if (privateParent != null) scala.Some(privateParent) else None
  }

  private[meta] def origin: Origin = {
    if (privateOrigin != null) privateOrigin else Origin.None
  }

  def pos: Position = {
    origin match {
      case Origin.Parsed(input, dialect, pos) =>
        val tokens = dialect(input).tokenize.get
        val startToken = tokens(pos.start)
        val endToken = tokens(pos.end - 1)
        Position.Range(input, startToken.start, endToken.end)
      case _ =>
        Position.None
    }
  }

  def tokens(implicit dialect: Dialect): Tokens = {
    origin match {
      case Origin.Parsed(input, dialect, pos) =>
        val tokens = dialect(input).tokenize.get
        tokens.slice(pos.start, pos.end)
      case _ =>
        val virtualInput = VirtualInput({ implicit val eagerPrettyprinting = Options.Eager; this.syntax })
        dialect(virtualInput).tokenize.get
    }
  }

  // =============================================================================================
  // Setters for pieces of internal state defined above.
  // Everyone except for scala.meta's core should be using "private"-less versions of these methods,
  // because those are only available on appropriate trees.
  // =============================================================================================

  private[meta] def privateWithOrigin(origin: Origin): Tree = {
    this.privateCopy(origin = origin)
  }
}

trait InternalTreeXtensions {
  private[meta] implicit class XtensionOriginTree[T <: Tree](tree: T) {
    def origin: Origin = if (tree.privateOrigin != null) tree.privateOrigin else Origin.None
    def withOrigin(origin: Origin): T = tree.privateWithOrigin(origin).asInstanceOf[T]
  }
}