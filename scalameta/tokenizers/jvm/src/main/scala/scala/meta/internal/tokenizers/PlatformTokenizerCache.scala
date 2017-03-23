package scala.meta
package internal
package tokenizers

import scala.collection.mutable
import scala.meta.inputs._
import scala.meta.tokens._

import java.{util => ju}
import java.util.{concurrent => juc}

object PlatformTokenizerCache {
  // NOTE: Manipulated by tokenization code in the ScalametaTokenizer class.
  // Caching just in toTokenize wouldn't be enough, because someone could call the tokenizer directly.
  val megaCache: ju.Map[Dialect, mutable.Map[Input, Tokens]] =
    new juc.ConcurrentHashMap[Dialect, mutable.Map[Input, Tokens]]()
  val miniCacheSyncRoot = new Object
  def newUnsyncResult: mutable.Map[Input, Tokens] = mutable.WeakHashMap.empty[Input, Tokens]
  def putIfAbsent(dialect: Dialect,
                  cache: mutable.Map[Input, Tokens]): mutable.Map[Input, Tokens] =
    megaCache.putIfAbsent(dialect, cache)
}
