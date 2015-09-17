package scala.meta
package syntactic

private[meta] trait TokenApi {
  type Input = scala.meta.syntactic.Input
  val Input = scala.meta.syntactic.Input

  type Content = scala.meta.syntactic.Content
  // val Content = scala.meta.syntactic.Content

  type Position = scala.meta.syntactic.Position
  val Position = scala.meta.syntactic.Position

  type Point = scala.meta.syntactic.Point
  val Point = scala.meta.syntactic.Point

  type Token = scala.meta.syntactic.Token
  val Token = scala.meta.syntactic.Token

  type Tokens = scala.meta.syntactic.Tokens
  val Tokens = scala.meta.syntactic.Tokens
  implicit class XtensionTokens(tokens: Seq[Token]) {
    def toTokens: Tokens = Tokens(tokens: _*)
  }
}

private[meta] trait GenericTokenizeApi {
  type TokenizeException = scala.meta.syntactic.TokenizeException
  val TokenizeException = scala.meta.syntactic.TokenizeException

  implicit class XtensionTokenizeInputLike[T](inputLike: T) {
    def tokens(implicit convert: Convert[T, Input], tokenize: Tokenize): Tokens = {
      tokenize(convert(inputLike))
    }
  }
}