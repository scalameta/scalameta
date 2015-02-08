package scala.meta
package syntactic
package tokenizers

// NOTE: moved to the package object
// type Offset = Int

// TODO: when I grow up I want to become a monad, just like my daddy
trait Reporter {
  def currentOffset: Offset
  def deprecationWarning(msg: String, at: Offset = currentOffset): Unit      = ()
  def readerError(msg: String, at: Offset = currentOffset): Nothing          = throw Exception(s"$msg at $at")
  def syntaxError(msg: String, at: Offset = currentOffset): Nothing          = throw Exception(s"syntax error at $at: $msg")
  def incompleteInputError(msg: String, at: Offset = currentOffset): Nothing = throw Exception(s"incomplete input at $at: $msg")
}

object Reporter {
  def apply(current: () => Offset) = new Reporter { def currentOffset = current() }
}
