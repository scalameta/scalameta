package scala.meta
package internal
package inputs

import org.scalameta.data._
import scala.meta.inputs._
import scala.meta.tokens._

// TODO: Our quasiquoting macros rely on the Parse[T] infrastructure
// to invoke the correct parsing subroutine depending on the flavor of the quasiquote being expanded.
//
// I could've implemented this differently, e.g. by maintaining a map of names
// that looks like Map("q" -> "parseQuasiquoteStat", ...). However, then I would
// have to repeat myself and wouldn't get such natural error messages.
//
// Unfortunately, this scheme has become a weakness now when Tokens no longer subclass Input.
// Previously, we could assemble to stream of tokens to parse and push it through the Parse[T] pipeline
// that works with Input. Now we can't.
//
// Since I don't like the idea of abandoning DRY, and no obvious alternative comes to mind,
// I'm creating a hacky Input whose sole purpose is to internally smuggle tokens to the tokenizers.
// It's not that I like this idea, but I think it's the lesser evil in this situation.

@data class InputTokens(tokens: Tokens) extends Input {
  def chars: Array[Char] = throw new UnsupportedOperationException()
}