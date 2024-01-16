package scala.meta
package internal
package trees

import scala.collection.mutable
import scala.meta.inputs._
import scala.meta.internal.tokenizers.Compat
import scala.meta.internal.prettyprinters.TreeSyntax
import scala.meta.prettyprinters._
import scala.meta.tokenizers._
import scala.meta.tokens._
import scala.meta.tokens.Token._
import scala.meta.trees.Error
import scala.meta.trees.Origin

// NOTE: Methods that start with "private" are NOT intended to be called outside scala.meta.
// Calling these methods from hosts will compile (because hosts are in meta), but is strongly discouraged.
trait InternalTree extends Product {
  self: Tree =>

  // ==============================================================
  // Pieces of internal state of scala.meta ASTs.
  // Some nodes define all of them as fields, some nodes define only a subset.
  // However, all nodes have corresponding defs to simplify uniform treatment.
  // Everyone except for scala.meta's core should be using "private"-less versions of these methods,
  // because those are only available on appropriate trees.
  // ==============================================================

  private[meta] def privatePrototype: Tree
  private[meta] def privateParent: Tree
  private[meta] def privateCopy(
      prototype: Tree = this,
      parent: Tree = privateParent,
      destination: String = null,
      origin: Origin = origin
  ): Tree

  // ==============================================================
  // Getters for pieces of internal state defined above.
  // ==============================================================

  def parent: Option[Tree] = Option(privateParent)

  // NOTE: InternalTree inherits traditional productXXX methods from Product
  // and also adds a new method called productFields.
  // def productPrefix: String
  // def productArity: Int
  // def productElement(n: Int): Any
  // def productIterator: Iterator[Any]
  def productFields: List[String]

  def origin: Origin

  def pos: Position = origin.position

  // ==============================================================
  // Tokens
  // ==============================================================

  @deprecated("dialect is ignored, use parameterless `tokens` method", "4.9.0")
  final def tokens(dialect: Dialect): Tokens = tokens

  def tokens: Tokens = tokensOpt.getOrElse {
    throw new Error.MissingDialectException(
      "Tree missing a dialect; update root tree `.withDialectIfRootAndNotSet` first, or call `.tokenizeFor`."
    )
  }

  def tokenizeFor(dialect: Dialect): Tokens =
    if (origin.dialectOpt.contains(dialect)) tokensOpt.get else lookupOrTokenizeFor(dialect)

  private val tokenCache: mutable.Map[Dialect, Tokens] =
    Compat.newMutableMap[Dialect, Tokens]

  private def lookupOrTokenizeFor(dialect: Dialect): Tokens =
    tokenCache.getOrElseUpdate(dialect, tokenizeForDialect(dialect))

  private lazy val tokensOpt: Option[Tokens] =
    origin match {
      case x: Origin.Parsed => Some(x.tokens)
      case _ => origin.dialectOpt.map(tokenizeForDialect)
    }

  private def tokenizeForDialect(dialect: Dialect): Tokens =
    this match {
      case Lit.String(value) =>
        val input = Input.VirtualFile("<InternalTrees.tokens>", value)
        Tokens(Array(Constant.String(input, dialect, 0, value.length, value)))
      case _ => dialect(textAsInput(dialect)).tokenize.get
    }

  private[meta] def textAsInput(implicit dialect: Dialect): Input =
    Input.VirtualFile("<InternalTrees.text>", printSyntaxFor(dialect))

  // ==============================================================
  // Text or syntax
  // ==============================================================

  def text: String = textOpt.getOrElse {
    throw new Error.MissingDialectException(
      "Tree missing a dialect; update root tree `.withDialectIfRootAndNotSet` first, or call `.printSyntaxFor`."
    )
  }

  def printSyntaxFor(dialect: Dialect): String =
    if (origin.dialectOpt.contains(dialect)) textOpt.get else reprintSyntax(dialect)

  private def reprintSyntax(dialect: Dialect): String =
    TreeSyntax.reprint(this)(dialect).toString

  private lazy val textOpt: Option[String] =
    origin match {
      case x: Origin.Parsed => Some(x.position.text)
      case _ => origin.dialectOpt.map(reprintSyntax)
    }

  // ==============================================================
  // Intellij-friendly stubs.
  // See https://github.com/scalameta/scalameta/pull/907#discussion_r120090447.
  // ==============================================================

  protected def checkFields(x: Any): Unit = ()
  protected def checkParent(x: Any): Unit = ()
}

trait InternalTreeXtensions {
  private[meta] implicit class XtensionOriginTree[T <: Tree](tree: T) {
    def withOrigin(origin: Origin): T = tree.privateCopy(origin = origin).asInstanceOf[T]

    def withDialectNonRecursiveIfNotSet(implicit dialect: Dialect): T =
      if (tree.origin ne Origin.None) tree else withOrigin(Origin.DialectOnly(dialect))
  }
}
