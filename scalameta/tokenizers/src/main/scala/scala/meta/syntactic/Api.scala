package scala.meta
package syntactic

private[meta] trait ScalametaTokenizeApi {
  implicit def tokenize(implicit dialect: Dialect): Tokenize = {
    Tokenize(content => new ScalametaTokenizer(content).tokenize)
  }
}