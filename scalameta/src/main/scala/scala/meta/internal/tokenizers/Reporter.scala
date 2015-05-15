package scala.meta
package internal
package tokenizers

// TODO: when I grow up I want to become a monad, just like my daddy
// TODO: distinguish flavors of errors with exception types
private[meta] trait Reporter {
  def content: Content
  def deprecationWarning(msg: String, at: Offset): Unit      = ()
  def readerError(msg: String, at: Offset): Nothing          = throw new TokenizeException(content, at, msg)
  def syntaxError(msg: String, at: Offset): Nothing          = throw new TokenizeException(content, at, msg)
  def incompleteInputError(msg: String, at: Offset): Nothing = throw new TokenizeException(content, at, msg)
}

private[meta] object Reporter {
  def apply(content0: Content) = new Reporter { def content = content0 }
}
