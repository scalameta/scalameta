package scala.meta
package internal
package trees

import scala.meta.prettyprinters._
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.tokens.Token._
import scala.meta.tokenizers._

// NOTE: Methods that start with "private" are NOT intended to be called outside scala.meta.
// Calling these methods from hosts will compile (because hosts are in meta), but is strongly discouraged.
trait InternalTree extends Product {
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
    destination: String = null,
    origin: Origin = privateOrigin): Tree

  // =============================================================================================
  // Getters for pieces of internal state defined above.
  // =============================================================================================

  def parent: Option[Tree] = {
    if (privateParent != null) scala.Some(privateParent) else None
  }

  // NOTE: InternalTree inherits traditional productXXX methods from Product
  // and also adds a new method called productFields.
  // def productPrefix: String
  // def productArity: Int
  // def productElement(n: Int): Any
  // def productIterator: Iterator[Any]
  def productFields: List[String]

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
        dialect(Input.VirtualFile("<InternalTrees.tokens>", this.syntax)).tokenize.get
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

  // =============================================================================================
  // Intellij-friendly stubs.
  // See https://github.com/scalameta/scalameta/pull/907#discussion_r120090447.
  // =============================================================================================

  protected def checkFields(x: Any): Unit = ()
  protected def checkParent(x: Any): Unit = ()
}

trait InternalTreeXtensions {
  private[meta] implicit class XtensionOriginTree[T <: Tree](tree: T) {
    def origin: Origin = if (tree.privateOrigin != null) tree.privateOrigin else Origin.None
    def withOrigin(origin: Origin): T = tree.privateWithOrigin(origin).asInstanceOf[T]
  }
}
