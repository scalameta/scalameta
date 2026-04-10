package scala.meta
package tokenizers

import scala.meta.inputs._

case class TokenizeException(pos: Position, shortMessage: String)
    extends InputException(pos, shortMessage) {
  override def toString = getMessage
}
