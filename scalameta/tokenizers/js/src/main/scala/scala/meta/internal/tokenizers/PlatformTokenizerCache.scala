package scala.meta
package internal
package tokenizers

import scala.collection.mutable
import scala.meta.inputs._
import scala.meta.tokens._

import java.{util => ju}

object PlatformTokenizerCache {
  // On the JVM, this is a weak hashmap.
  val megaCache = new ju.HashMap[Dialect, mutable.Map[Input, Tokens]]()
  val miniCacheSyncRoot = new Object
  def newUnsyncResult: mutable.Map[Input, Tokens] = mutable.HashMap.empty[Input, Tokens]
  def putIfAbsent(dialect: Dialect,
                  cache: mutable.Map[Input, Tokens]): mutable.Map[Input, Tokens] =
    if (!megaCache.containsKey(dialect)) megaCache.put(dialect, cache)
    else megaCache.get(dialect)
}
