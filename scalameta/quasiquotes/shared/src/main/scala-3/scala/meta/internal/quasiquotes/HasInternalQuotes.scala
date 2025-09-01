package scala.meta
package internal
package quasiquotes

import scala.quoted._

trait HasInternalQuotes:
  val internalQuotes: Quotes
  given Quotes = internalQuotes
