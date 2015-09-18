package scala.meta
package tokenquasiquotes

import org.scalameta.tokens._

private[meta] trait Api {
  @quasiquote('toks) implicit class XtensionQuasiquoteTokens(ctx: StringContext)
}

private[meta] trait Aliases {
}
