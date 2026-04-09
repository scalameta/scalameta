package scala.meta
package tokenizers

import scala.meta.inputs._
import scala.meta.internal.SingletonReference

trait Tokenize {
  def apply(input: Input, dialect: Dialect): Tokenized
}

object Tokenize {

  val global = new SingletonReference[Tokenize](UndefinedTokenize)
  implicit def scalametaTokenize: Tokenize = {
    val obj = global.value
    if (obj ne UndefinedTokenize) obj else PlatformCompat.loadTokenize.getOrElse(obj)
  }

  def getOpt: Option[Tokenize] = {
    val obj = scalametaTokenize
    if (obj eq UndefinedTokenize) None else Some(obj)
  }

  private object UndefinedTokenize extends Tokenize {
    def apply(input: Input, dialect: Dialect): Tokenized =
      throw new UnsupportedOperationException("No implementation of Tokenize service is provided")
  }

}
