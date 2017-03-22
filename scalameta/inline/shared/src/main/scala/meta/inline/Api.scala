package scala.meta
package inline

import scala.meta.inputs.Position
import scala.meta.internal.inline.AbortException

private[meta] trait Api {
  // Enables `meta` blocks, the foundation for new-style ("inline") macros.
  // This is a magic method whose argument is typechecked using special rules.
  // See https://github.com/scalameta/paradise for more information.
  def apply(body: Any): Nothing = ???

  // NOTE: Maybe in the future we will add support for error/warning,
  // but for now let's limit ourselves to what's absolutely necessary.
  def abort(msg: String): Nothing = throw new AbortException(msg)
  def abort(pos: Position, msg: String): Nothing = throw new AbortException(pos, msg)
}

private[meta] trait Aliases {
}
