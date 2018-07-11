package scala.meta.internal.classpath

import scala.util.control.NoStackTrace

final case class MissingSymbolException(symbol: String) extends Exception with NoStackTrace {
  override def getMessage: String = s"missing symbol: $symbol"
}
