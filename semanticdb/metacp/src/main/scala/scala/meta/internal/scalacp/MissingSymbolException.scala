package scala.meta.internal.scalacp

import scala.util.control.NoStackTrace
import scala.tools.scalap.scalax.rules.scalasig._

case class MissingSymbolException(symbol: Symbol) extends Exception with NoStackTrace {
  override def getMessage: String = s"missing symbol: ${symbol.path}"
}
