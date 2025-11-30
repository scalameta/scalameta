package scala.meta.internal.semanticdb

import scala.meta.inputs.{Input, Position}

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
    def toSemanticOriginal: OriginalTree = OriginalTree(range = Some(range))
  }

  implicit val rangeOrdering: Ordering[Range] = new Ordering[Range] {
    override def compare(a: Range, b: Range): Int = {
      val byLine = Integer.compare(a.startLine, b.startLine)
      if (byLine != 0) byLine
      else {
        val byCharacter = Integer.compare(a.startCharacter, b.startCharacter)
        byCharacter
      }
    }
  }

  implicit val rangeOptionOrdering: Ordering[Option[Range]] = Ordering.Option[Range]
}
