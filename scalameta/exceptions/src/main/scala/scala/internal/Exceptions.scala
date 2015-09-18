package scala.meta
package internal

import org.scalameta.data._
import org.scalameta.unreachable

// TODO: Any -> Position
@data class AbortException(pos: Option[Any], message: String, cause: Option[Throwable])
extends Exception(message, cause.orNull) with ScalametaError {
  def this(message: String) = this(None, message, None)
  def this(message: String, cause: Throwable) = this(None, message, Some(cause))
  def this(pos: Any, message: String) = this(Some(pos), message, None)
  def this(pos: Any, message: String, cause: Throwable) = this(Some(pos), message, Some(cause))
  override def toString = super.toString
}
