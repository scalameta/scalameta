package scala.meta
package syntactic

import org.scalameta.convert._
import scala.annotation.implicitNotFound

trait Tokenize extends Convert[Content, Tokens]
object Tokenize {
  def apply(f: Content => Tokens): Tokenize = new Tokenize {
    def apply(content: Content): Tokens = f(content)
  }
}
