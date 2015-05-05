package scala.meta
package internal
package parsers

// TODO: when I grow up I want to become a monad, just like my daddy
// TODO: when we have Positions, attach them to the exception being thrown
private[meta] trait Reporter {
  def deprecationWarning(msg: String, at: Token): Unit = ()
  def deprecationWarning(msg: String, at: Tree): Unit = {
    def fallback = ()
    at.origin.tokens.headOption.map(at => deprecationWarning(msg, at)).getOrElse(())
  }
  def syntaxWarning(msg: String, at: Token): Unit = ()
  def syntaxWarning(msg: String, at: Tree): Unit = {
    def fallback = ()
    at.origin.tokens.headOption.map(at => syntaxWarning(msg, at)).getOrElse(())
  }
  def syntaxError(msg: String, at: Token): Nothing = throw new ParseException(at.input, at, msg)
  def syntaxError(msg: String, at: Tree): Nothing = syntaxError(msg, at = at.origin.tokens.head)
}

private[meta] object Reporter {
  def apply() = new Reporter {}
}
