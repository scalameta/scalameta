package scala.meta
package internal
package tokenizers

// NOTE: moved to the package object
// type Offset = Int

// TODO: when I grow up I want to become a monad, just like my daddy
private[meta] trait Reporter {
  def currentOffset: Offset
  def deprecationWarning(msg: String, at: Offset = currentOffset): Unit      = ()
  def readerError(msg: String, at: Offset = currentOffset): Nothing          = throw Exception(s"$msg at $at")
  def syntaxError(msg: String, at: Offset = currentOffset): Nothing          = throw Exception(s"syntax error at $at: $msg")
  def incompleteInputError(msg: String, at: Offset = currentOffset): Nothing = throw Exception(s"incomplete input at $at: $msg")
}

private[meta] object Reporter {
  def apply(current: () => Offset) = new Reporter { def currentOffset = current() }
}
