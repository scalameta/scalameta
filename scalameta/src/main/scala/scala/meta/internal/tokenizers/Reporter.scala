package scala.meta
package internal
package tokenizers

// TODO: when I grow up I want to become a monad, just like my daddy
// TODO: distinguish flavors of errors with exception types
private[meta] trait Reporter {
  def input: Input.Real
  def deprecationWarning(msg: String, at: Offset): Unit      = ()
  def readerError(msg: String, at: Offset): Nothing          = throw new TokenizeException(input, at, msg)
  def syntaxError(msg: String, at: Offset): Nothing          = throw new TokenizeException(input, at, msg)
  def incompleteInputError(msg: String, at: Offset): Nothing = throw new TokenizeException(input, at, msg)
}

private[meta] object Reporter {
  def apply(input0: Input.Real) = new Reporter { def input = input0 }
}
