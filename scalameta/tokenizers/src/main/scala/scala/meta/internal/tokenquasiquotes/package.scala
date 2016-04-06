package scala.meta
package internal

import scala.meta.internal.tokens.quasiquote

package object tokenquasiquotes {
  @quasiquote('toks) implicit class XtensionQuasiquoteTokens(ctx: StringContext)
}