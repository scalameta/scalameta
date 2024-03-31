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
