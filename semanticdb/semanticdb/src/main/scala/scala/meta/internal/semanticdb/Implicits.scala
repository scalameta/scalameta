package scala.meta.internal.semanticdb

import scala.meta.inputs.Input
import scala.meta.inputs.Position

object Implicits {

  implicit class XtensionPositionToRange(private val pos: Position) extends AnyVal {
    def toRange: Range = Range(
      startLine = pos.startLine,
      startCharacter = pos.startColumn,
      endLine = pos.endLine,
      endCharacter = pos.endColumn
    )
  }

  implicit class XtensionRangeToPosition(private val range: Range) extends AnyVal {
    def toPosition(input: Input): Position = Position.Range(
      input = input,
      startLine = range.startLine,
      startColumn = range.startCharacter,
      endLine = range.endLine,
      endColumn = range.endCharacter
    )
  }

}
