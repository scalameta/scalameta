package scala.meta.internal.classpath

final case class MissingSymbolException(symbol: String) extends Exception {
  override def getMessage: String = s"missing symbol: $symbol"
}
