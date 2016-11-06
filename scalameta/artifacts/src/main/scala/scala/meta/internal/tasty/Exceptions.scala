package scala.meta
package internal
package tasty

import org.scalameta.data._
import org.scalameta.unreachable

@data class TastyException(message: String, cause: Option[Throwable])
extends Exception(s"failed to serialize TASTY because $message", cause.orNull) {
  def this(message: String) = this(message, None)
  def this(message: String, cause: Throwable) = this(message, Some(cause))
  override def toString = super.toString
}

@data class UntastyException(message: String, cause: Option[Throwable])
extends Exception(s"failed to deserialize TASTY because $message", cause.orNull) {
  def this(message: String) = this(message, None)
  def this(message: String, cause: Throwable) = this(message, Some(cause))
  override def toString = super.toString
}
