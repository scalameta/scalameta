package scala.meta.internal.scalacp

import scala.util.control.NoStackTrace

case class MissingSymbolException(symbol: String) extends Exception(symbol) with NoStackTrace {
  override def getMessage: String =
    s"$symbol (to fix this problem, either update --dependency-classpath or pass " +
      s"one of the options '--assume-scala $symbol' '--assume-java $symbol')"
}
