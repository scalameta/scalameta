package scala.meta
package tokens

private[meta] trait Api {
}

private[meta] trait Aliases {
  type Token = scala.meta.tokens.Token
  lazy val Token = scala.meta.tokens.Token

  type Tokens = scala.meta.tokens.Tokens
  lazy val Tokens = scala.meta.tokens.Tokens
}