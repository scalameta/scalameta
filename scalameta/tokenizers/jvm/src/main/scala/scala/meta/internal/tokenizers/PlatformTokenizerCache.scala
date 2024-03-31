package scala.meta
package internal
package tokenizers

import scala.meta.inputs._
import scala.meta.tokens._

import java.util.{concurrent => juc}
import java.{util => ju}

import scala.collection.mutable

@deprecated("No longer used", "4.3.0")
object PlatformTokenizerCache {
  // NOTE: Manipulated by tokenization code in the ScalametaTokenizer class.
  // Caching just in toTokenize wouldn't be enough, because someone could call the tokenizer directly.
  // still used in scalafmt-dynamic-3.7.10
  val megaCache: ju.Map[Dialect, mutable.Map[Input, Tokens]] =
    new juc.ConcurrentHashMap[Dialect, mutable.Map[Input, Tokens]]()
}
