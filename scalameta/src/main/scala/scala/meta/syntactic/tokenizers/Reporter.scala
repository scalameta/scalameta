package scala.meta
package syntactic
package tokenizers

// NOTE: moved to the package object
// type Offset = Int

// TODO: when I grow up I want to become a monad, just like my daddy
trait Reporter {
  def currentOffset: Offset
  def deprecationWarning(msg: String, at: Offset = currentOffset): Unit      = ()
  def readerError(msg: String, at: Offset = currentOffset): Nothing          = throw Reporter.ReaderError(msg, at)
  def syntaxError(msg: String, at: Offset = currentOffset): Nothing          = throw Reporter.SyntaxError(msg, at)
  def incompleteInputError(msg: String, at: Offset = currentOffset): Nothing = throw Reporter.IncompleteInputError(msg, at)
}

object Reporter {
  def apply(current: () => Offset) = new Reporter { def currentOffset = current() }
  sealed abstract class Exception(msg: String) extends scala.Exception(msg)
  final case class ReaderError(msg: String, at: Offset) extends Exception(s"$msg at $at")
  final case class SyntaxError(msg: String, at: Offset) extends Exception(s"syntax error at $at: $msg")
  final case class IncompleteInputError(msg: String, at: Offset) extends Exception(s"incomplete input at $at: $msg")
}
