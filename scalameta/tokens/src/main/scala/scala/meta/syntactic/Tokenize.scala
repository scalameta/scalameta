package scala.meta
package syntactic

import org.scalameta.convert._
import scala.annotation.implicitNotFound

trait Tokenize extends Convert[Content, Tokens]
object Tokenize {
  def apply[T](f: Input => T): Tokenize[T] = new Tokenize[T] {
    def apply(content: Content): Tokens = f(input)
  }
}
