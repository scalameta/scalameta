package scala.meta
package parsers

import scala.meta.inputs._

case class ParseException(pos: Position, shortMessage: String)
    extends InputException(pos, shortMessage)
