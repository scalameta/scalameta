package scala.meta
package tokenquasiquotes

import org.scalameta.tokens._

private[meta] trait TokenQuasiquoteApi {
  @quasiquote('toks) implicit class XtensionQuasiquoteTokens(ctx: StringContext)
}