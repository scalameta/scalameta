package scala.meta
package syntactic

import scala.reflect.macros.blackbox.Context

import scala.meta.internal.tokenizers._

trait Content extends Input {
  def chars: Array[Char]
  private val tokenCache = scala.collection.mutable.Map[Dialect, Tokens]()
  final override def tokens(implicit dialect: Dialect): Tokens = tokenCache.getOrElseUpdate(dialect, tokenize(this))
}

trait ContentLiftables {
  val c: Context

  private val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  import c.universe._

  implicit lazy val liftContent: Liftable[Content] = Liftable[Content] { content =>
    q"_root_.scala.meta.Input.String(${new String(content.chars)})"
  }
}
