package scala.meta
package syntactic

import scala.meta.internal.tokenizers._

trait Content extends Input {
  def chars: Array[Char]

  private val tokenCache = scala.collection.mutable.Map[Dialect, Tokens]()
  final override def tokens(implicit dialect: Dialect, tokenizer: Tokenizer): Tokens = {
    tokenCache.getOrElseUpdate(dialect, tokenizer(this))
  }
}
