package scala.meta
package taxonomic

import org.scalameta.data._
import org.scalameta.unreachable

@data class ArtifactException(artifact: Artifact, message: String, cause: Option[Throwable])
extends Exception(s"failed to resolve $artifact because $message", cause.orNull) with ScalametaException {
  def this(artifact: Artifact, message: String) = this(artifact, message, None)
  def this(artifact: Artifact, message: String, cause: Throwable) = this(artifact, message, Some(cause))
  override def toString = super.toString
}
