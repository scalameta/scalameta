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
import scala.meta.internal.equality._
import scala.meta.internal.flags._
import scala.meta.internal.inputs._
import scala.meta.internal.prettyprinters._
import scala.meta.internal.semantic._
import scala.meta.internal.tokens._

// NOTE: Methods that start with "private" are NOT intended to be called outside scala.meta.
// Calling these methods from hosts will compile (because hosts are in meta), but is strongly discouraged.
trait InternalTree {
  self: Tree =>

  // =============================================================================================
  // Pieces of internal state of scala.meta ASTs.
  // Some nodes define all of them as fields, some nodes define only a subset
  // (e.g. Term.Param has everything but privateDenot).
  // However, all nodes have corresponding defs to simplify uniform treatment.
  // Everyone except for scala.meta's core should be using "private"-less versions of these methods,
  // because those are only available on appropriate trees (e.g. there's no Term.Param.denot).
  // =============================================================================================

  private[meta] def privateFlags: Flags
  private[meta] def privatePrototype: Tree
  private[meta] def privateParent: Tree
  private[meta] def privateOrigin: Origin
  private[meta] def privateEnv: Environment = null
  private[meta] def privateDenot: Denotation = null
  private[meta] def privateTyping: Typing = null
  private[meta] def privateCopy(
    flags: Flags = ZERO,
    prototype: Tree = this,
    parent: Tree = privateParent,
    origin: Origin = privateOrigin,
    env: Environment = privateEnv,
    denot: Denotation = privateDenot,
    typing: Typing = privateTyping): Tree

  private[meta] def privateHasEnv: Boolean = this.is[Term] || this.is[Name]
  private[meta] def privateHasDenot: Boolean = this.is[Name]
  private[meta] def privateHasTyping: Boolean = this.is[Term] || this.is[Term.Param]

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
  // because those are only available on appropriate trees (e.g. there's no Term.Param.withDenot).
  // =============================================================================================

  private[meta] def privateWithFlags(flags: Flags): Tree = {
    if ((flags & TYPECHECKED) == TYPECHECKED) {
      val violatesDenot = privateHasDenot && (this.privateDenot == null || this.privateDenot == Denotation.None)
      val violatesTyping = privateHasTyping && (this.privateTyping == null || this.privateTyping == Typing.None)
      if (violatesDenot || violatesTyping) {
        implicit val recursion = Attributes.Recursion.Deep
        implicit val force = Attributes.Force.Never
        val message = "failed to enable TYPECHECKED for " + this.show[Attributes]
        throw new UnsupportedOperationException(message)
      }
    }
    this.privateCopy(flags = flags)
  }

  private[meta] def privateWithOrigin(origin: Origin): Tree = {
    this.privateCopy(origin = origin)
  }

  // NOTE: No state validation in withEnv, because withEnv can be called on U, PA and A.
  // Hygiene (embodied by envs in scala.meta) applies to unattributed and attributed trees alike.
  // NOTE: We shouldn't clean up denots in withEnv,
  // because that would destroy links between defs and refs,
  // which may irreversibly hamper subsequent retypechecks.

  private[meta] def privateWithEnv(env: Environment): Tree = {
    this.privateCopy(
      flags = this.privateFlags & ~TYPECHECKED,
      env = env,
      denot = this.privateDenot,
      typing = Typing.None
    )
  }

  private def stateCheckWithAttrs(): Unit = {
    if (isAttributed) {
      implicit val recursion = Attributes.Recursion.Deep
      implicit val force = Attributes.Force.Never
      val message = "can only call withAttrs on unattributed or partially attributed trees"
      val diagnostic = "if necessary, call .copy() to unattribute and then do .withAttrs(...)"
      val details = this.show[Attributes]
      throw new UnsupportedOperationException(message + ";" + diagnostic + EOL + details)
    }
  }

  private def partialCheckWithAttrs(): Unit = {
    if (this.is[Term.Name] || this.is[Ctor.Name]) {
      throw new UnsupportedOperationException("need to simultaneously set both denotation and typing for " + this.productPrefix)
    }
  }

  // TODO: Not sure whether we should clean up envs or not.
  // Let's be on the safe side, I guess.

  private[meta] def privateWithAttrs(denot: Denotation): Tree = {
    stateCheckWithAttrs()
    partialCheckWithAttrs()
    this.privateCopy(
      flags = privateFlags & ~TYPECHECKED,
      env = Environment.None,
      denot = denot,
      typing = privateTyping
    )
  }

  private[meta] def privateWithAttrs(typing: Typing): Tree = {
    stateCheckWithAttrs()
    partialCheckWithAttrs()
    this.privateCopy(
      flags = privateFlags & ~TYPECHECKED,
      env = Environment.None,
      denot = privateDenot,
      typing = typing
    )
  }

  private[meta] def privateWithAttrs(denot: Denotation, typing: Typing): Tree = {
    stateCheckWithAttrs()
    this.privateCopy(
      flags = privateFlags & ~TYPECHECKED,
      env = Environment.None,
      denot = denot,
      typing = typing
    )
  }

  private[meta] def privateInheritAttrs(that: Tree): Tree = {
    def areCompatible = (
      !(this.privateHasDenot ^ that.privateHasDenot) &&
      !(this.privateHasTyping ^ that.privateHasTyping)
    )
    if (!areCompatible) sys.error(s"${this.productPrefix} can't inherit attrs from ${that.productPrefix}")
    this match {
      case tree: Term.Name => tree.privateWithAttrs(that.privateDenot, that.privateTyping)
      case tree: Ctor.Name => tree.privateWithAttrs(that.privateDenot, that.privateTyping)
      case tree: Name => tree.privateWithAttrs(that.privateDenot)
      case tree: Term => tree.privateWithAttrs(that.privateTyping)
      case tree: Term.Param => tree.privateWithAttrs(that.privateTyping)
      case _ => this // do nothing
    }
  }

  // =============================================================================================
  // Internal statuses of scala.meta ASTs with respect to attribution.
  // We start from a tree completely devoid of semantics (U)
  // then do withAttrs to fill in denots/typings (PA)
  // and finally call setTypechecked to finalize the tree (A).
  // =============================================================================================

  private[meta] def isUnattributed: _root_.scala.Boolean = {
    val isEnvEmpty = privateEnv == null || privateEnv == Environment.None
    val isDenotEmpty = privateDenot == null || privateDenot == Denotation.None
    val isTypingEmpty = privateTyping == null || privateTyping == Denotation.None
    this match {
      case tree: Term.Name => isEnvEmpty && isDenotEmpty && isTypingEmpty
      case tree: Ctor.Name => isEnvEmpty && isDenotEmpty && isTypingEmpty
      case tree: Term.Param => isTypingEmpty
      case tree: Term => isEnvEmpty && isTypingEmpty
      case tree: Name => isEnvEmpty && isDenotEmpty
      case _ => true
    }
  }
  private[meta] def isPartiallyAttributed: _root_.scala.Boolean = this match {
    case tree: Term.Name => !isUnattributed && !isAttributed
    case tree: Ctor.Name => !isUnattributed && !isAttributed
    case tree: Term.Param => !isUnattributed && !isAttributed
    case tree: Term => !isUnattributed && !isAttributed
    case tree: Name => !isUnattributed && !isAttributed
    case _ => false
  }
  private[meta] def isAttributed: _root_.scala.Boolean = this.isTypechecked
}

trait InternalTreeXtensions {
  private[meta] implicit class XtensionOriginTree[T <: Tree](tree: T) {
    def origin: Origin = if (tree.privateOrigin != null) tree.privateOrigin else Origin.None
    def withOrigin(origin: Origin): T = tree.privateWithOrigin(origin).asInstanceOf[T]
  }

  private[meta] implicit class XtensionAttributedName[T <: Name](tree: T) {
    def env: Environment = if (tree.privateEnv != null) tree.privateEnv else Environment.None
    def denot: Denotation = if (tree.privateDenot != null) tree.privateDenot else Denotation.None
    def withEnv(env: Environment): T = tree.privateWithEnv(env).asInstanceOf[T]
    def withAttrs(denot: Denotation): T = tree.privateWithAttrs(denot).asInstanceOf[T]
  }

  private[meta] implicit class XtensionAttributedTerm[T <: Term](tree: T) {
    def env: Environment = if (tree.privateEnv != null) tree.privateEnv else Environment.None
    def typing: Typing = if (tree.privateTyping != null) tree.privateTyping else Typing.None
    def withEnv(env: Environment): T = tree.privateWithEnv(env).asInstanceOf[T]
    def withAttrs(typingLike: TypingLike): T = tree.privateWithAttrs(typingLike.typing).asInstanceOf[T]
  }

  private[meta] implicit class XtensionAttributedTermParam(tree: Term.Param) {
    def typing: Typing = if (tree.privateTyping != null) tree.privateTyping else Typing.None
    def withAttrs(typingLike: TypingLike): Term.Param = tree.privateWithAttrs(typingLike.typing).asInstanceOf[Term.Param]
  }

  private[meta] implicit class XtensionAttributedTermName(tree: Term.Name) {
    def withAttrs(denot: Denotation, typingLike: TypingLike): Term.Name = tree.privateWithAttrs(denot, typingLike.typing).asInstanceOf[Term.Name]
  }

  private[meta] implicit class XtensionAttributedCtorName(tree: Ctor.Name) {
    def withAttrs(denot: Denotation, typingLike: TypingLike): Ctor.Name = tree.privateWithAttrs(denot, typingLike.typing).asInstanceOf[Ctor.Name]
  }

  private[meta] implicit class XtensionInheritedTree[T <: Tree](tree: T) {
    def inheritAttrs(other: Tree): T = {
      tree.privateInheritAttrs(other).asInstanceOf[T]
    }
  }

  private[meta] implicit class XtensionTypecheckableTree[T <: Tree](tree: T) {
    def isTypechecked: Boolean = (tree.privateFlags & TYPECHECKED) == TYPECHECKED
    def setTypechecked: T = tree.privateWithFlags(tree.privateFlags | TYPECHECKED).asInstanceOf[T]
    def resetTypechecked: T = tree.privateWithFlags(tree.privateFlags & ~TYPECHECKED).asInstanceOf[T]
    def withTypechecked(value: Boolean): T = if (value) tree.setTypechecked else tree.resetTypechecked
  }

  private[meta] implicit class XtensionSemanticEquality[T1 <: Tree](tree1: T1) {
    def ===[T2 <: Tree](tree2: T2)(implicit ev: AllowEquality[T1, T2]): Boolean = Semantic.equals(tree1, tree2)
    def =/=[T2 <: Tree](tree2: T2)(implicit ev: AllowEquality[T1, T2]): Boolean = !Semantic.equals(tree1, tree2)
  }
}