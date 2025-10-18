package scala.meta.internal.parsers

/** Utility for tracking a context, like whether we are inside a pattern or a quote. */
private[parsers] trait NestedContext {
  private var nested = 0
  def within[T](body: => T): T = {
    nested += 1
    try body
    finally nested -= 1
  }
  @inline
  def isInside() = isDeeper(0)
  @inline
  def isDeeper(level: Int) = nested > level
}

private[parsers] abstract class NestedContextWithOwner[A <: AnyRef](private var _owner: A)
    extends NestedContext {
  def within[T](newOwner: A)(body: => T): T = {
    val oldOwner = _owner
    _owner = newOwner
    try within(body)
    finally _owner = oldOwner
  }
  @inline
  def owner: A = _owner
}
