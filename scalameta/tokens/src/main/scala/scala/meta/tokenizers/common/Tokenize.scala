package scala.meta
package tokenizers
package common

import org.scalameta.convert._
import scala.annotation.implicitNotFound
import scala.meta.inputs._
import scala.meta.tokens._

trait Tokenize {
  def apply(content: Content)(implicit dialect: Dialect): Tokens
}
