package scala.meta
package internal
package tokenizers

// TODO: when I grow up I want to become a monad, just like my daddy
// TODO: distinguish flavors of errors with exception types
private[meta] trait Reporter {
  // NOTE: not making this public, e.g. by exposing Position.Offset
  // because I don't want to advertise this style of positioning
  private implicit class XtensionOffsetPosition(offset: Offset) {
    private val point = Point.Offset(content, offset)
    def position = Position.Range(content, point, point, point)
  }

  def content: Content
  def deprecationWarning(msg: String, at: Position): Unit      = ()
  def deprecationWarning(msg: String, at: Offset): Unit        = deprecationWarning(msg, at.position)
  def readerError(msg: String, at: Position): Nothing          = throw new TokenizeException(at, msg)
  def readerError(msg: String, at: Offset): Nothing            = readerError(msg, at.position)
  def syntaxError(msg: String, at: Position): Nothing          = throw new TokenizeException(at, msg)
  def syntaxError(msg: String, at: Offset): Nothing            = syntaxError(msg, at.position)
  def incompleteInputError(msg: String, at: Position): Nothing = throw new TokenizeException(at, msg)
  def incompleteInputError(msg: String, at: Offset): Nothing   = incompleteInputError(msg, at.position)
}

private[meta] object Reporter {
  def apply(content0: Content) = new Reporter { def content = content0 }
}
