package scala.meta
package tokenquasiquotes

import scala.meta.internal.tokens.quasiquote

private[meta] trait Api {
  @quasiquote('toks) implicit class XtensionQuasiquoteTokens(ctx: StringContext)
}

private[meta] trait Aliases {
}
