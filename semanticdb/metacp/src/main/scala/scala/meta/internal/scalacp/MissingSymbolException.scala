package scala.meta.internal.scalacp

import scala.util.control.NoStackTrace

case class MissingSymbolException(symbol: String) extends Exception(symbol) with NoStackTrace {
  override def getMessage: String = s"missing symbol: $symbol"
}
