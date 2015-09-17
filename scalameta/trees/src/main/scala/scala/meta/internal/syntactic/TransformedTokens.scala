package scala.meta
package internal
package syntactic

// NOTE: Actually doesn't contain any tokens and simply signalizes the internal infrastructure
// that it's necessary to call inferTokens on the current tree + the prototype (the one that's stored in this object).
// Once I get to refactoring Tokens to not inherit from Seq and friends, it'll become possible
// to merge this class into Tokens.Synthetic.
private[meta] case class TransformedTokens(prototype: Tree) extends Tokens(Nil: _*) {
  override def input = this
  override def dialect = scala.meta.dialects.Scala211
  override def isAuthentic = false
  override def toString = s"Transformed($prototype)"
}
