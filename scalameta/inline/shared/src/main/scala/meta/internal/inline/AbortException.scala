package scala.meta
package internal
package inline

import org.scalameta.data._
import scala.meta.inputs.Position

@data class AbortException(pos: Option[Position], message: String, cause: Option[Throwable])
extends Exception(message, cause.orNull) {
  def this(message: String) = this(None, message, None)
  def this(message: String, cause: Throwable) = this(None, message, Some(cause))
  def this(pos: Position, message: String) = this(Some(pos), message, None)
  def this(pos: Position, message: String, cause: Throwable) = this(Some(pos), message, Some(cause))
  override def toString = super.toString
}