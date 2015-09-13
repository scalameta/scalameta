package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.data._
import org.scalameta.unreachable

@data class ConvertException(culprit: Any, message: String, cause: Option[Throwable] = None)
extends Exception(message, cause.orNull) with ScalahostError {
  override def toString = super.toString
}
