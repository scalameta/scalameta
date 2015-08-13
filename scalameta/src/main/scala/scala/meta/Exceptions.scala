package scala.meta

import org.scalameta.data._
import org.scalameta.unreachable

// NOTE: An exception should only be put here if it's supposed to be caught by the user.
// Otherwise, consider putting it into scala.meta.internal.something and extending ScalametaError,
// e.g. like I did with TastyException and UntastyException.

@data class ParseException(pos: Position, message: String)
extends Exception(s"$message at ${pos.start.offset}..${pos.end.offset}") with ScalametaException {
  override def toString = super.toString
}

@data class ArtifactException(artifact: Artifact, message: String, cause: Option[Throwable])
extends Exception(s"failed to resolve $artifact because $message", cause.orNull) with ScalametaException {
  def this(artifact: Artifact, message: String) = this(artifact, message, None)
  def this(artifact: Artifact, message: String, cause: Throwable) = this(artifact, message, Some(cause))
  override def toString = super.toString
}

@data class SemanticException(pos: Option[Position], message: String, cause: Option[Throwable])
extends Exception(message, cause.orNull) with ScalametaException {
  def this(message: String) = this(None, message, None)
  def this(message: String, cause: Throwable) = this(None, message, Some(cause))
  def this(pos: Position, message: String) = this(Some(pos), message, None)
  def this(pos: Position, message: String, cause: Throwable) = this(Some(pos), message, Some(cause))
  override def toString = super.toString
}
