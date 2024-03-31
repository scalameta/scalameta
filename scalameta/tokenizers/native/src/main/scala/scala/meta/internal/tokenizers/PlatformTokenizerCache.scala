package scala.meta
package internal
package tokenizers

import scala.meta.inputs._
import scala.meta.tokens._

import java.{util => ju}

import scala.collection.mutable

@deprecated("No longer used", "4.3.0")
object PlatformTokenizerCache {
  // On the JVM, this is a weak hashmap.
  // still used in scalafmt-dynamic-3.7.10
  val megaCache = new ju.HashMap[Dialect, mutable.Map[Input, Tokens]]()
}
