package scala.meta
package semantic

import org.scalameta.data._
import org.scalameta.unreachable
import scala.meta.inputs._

@data class SemanticException(pos: Option[Position], message: String, cause: Option[Throwable])
extends Exception(message, cause.orNull) with ScalametaException {
  def this(message: String) = this(None, message, None)
  def this(message: String, cause: Throwable) = this(None, message, Some(cause))
  def this(pos: Position, message: String) = this(Some(pos), message, None)
  def this(pos: Position, message: String, cause: Throwable) = this(Some(pos), message, Some(cause))
  override def toString = super.toString
}
