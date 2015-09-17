package scala.meta
package syntactic

import org.scalameta.convert._
import scala.annotation.implicitNotFound

trait Tokenize {
  def apply(content: Content)(implicit dialect: Dialect): Tokens
}
