package scala.meta
package internal.hosts.scalac

import org.scalameta.adt._
import org.scalameta.unreachable

@root trait ScalahostException extends Exception
@leaf class ConvertException(culprit: Any, message: String, cause: Option[Throwable] = None) extends Exception(message, cause.orNull) with ScalahostException
@leaf class StandaloneException(message: String) extends Exception(message) with ScalahostException
