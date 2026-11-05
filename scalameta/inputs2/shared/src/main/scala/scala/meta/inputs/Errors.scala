package scala.meta.inputs

import scala.meta.internal.inputs._

class InputException(pos: Position, shortMessage: String)
    extends Exception(pos.formatMessage("error", shortMessage)) {
  def fullMessage: String = getMessage
  def getPos: Position = pos
  def getShortMessage: String = shortMessage
  override def toString = getMessage
}
