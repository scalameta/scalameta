package scala.meta
package internal
package tokenizers

// TODO: when I grow up I want to become a monad, just like my daddy
private[meta] trait Reporter {
  def deprecationWarning(msg: String, at: Offset): Unit      = ()
  def readerError(msg: String, at: Offset): Nothing          = throw Exception(s"$msg at $at")
  def syntaxError(msg: String, at: Offset): Nothing          = throw Exception(s"syntax error at $at: $msg")
  def incompleteInputError(msg: String, at: Offset): Nothing = throw Exception(s"incomplete input at $at: $msg")
}

private[meta] object Reporter {
  def apply() = new Reporter {}
}
