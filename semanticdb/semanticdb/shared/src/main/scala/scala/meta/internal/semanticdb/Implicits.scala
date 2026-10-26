package scala.meta.internal.semanticdb

import scala.meta.inputs.{Input, Position}
import scala.meta.internal.metap.IdentifierOrdering

object Implicits {

  implicit class XtensionPositionToRange(private val pos: Position) extends AnyVal {
    def toRange: Range = Range(
      startLine = pos.startLine,
      startCharacter = pos.startColumn,
      endLine = pos.endLine,
      endCharacter = pos.endColumn,
    )
  }

  implicit class XtensionRangeToPosition(private val range: Range) extends AnyVal {
    def toPosition(input: Input): Position = Position.Range(
      input = input,
      startLine = range.startLine,
      startColumn = range.startCharacter,
      endLine = range.endLine,
      endColumn = range.endCharacter,
    )
    def toSemanticOriginal: OriginalTree = OriginalTree(range = Some(range))
  }

  implicit val rangeOrdering: Ordering[Range] = new Ordering[Range] {
    override def compare(a: Range, b: Range): Int = {
      var cmp = Integer.compare(a.startLine, b.startLine)
      if (cmp != 0) return cmp

      cmp = Integer.compare(a.startCharacter, b.startCharacter)
      if (cmp != 0) return cmp

      cmp = Integer.compare(a.endLine, b.endLine)
      if (cmp != 0) return cmp

      Integer.compare(a.endCharacter, b.endCharacter)
    }
  }

  implicit val rangeOptionOrdering: Ordering[Option[Range]] = Ordering.Option[Range]

  private val identifierOrdering = new IdentifierOrdering[String]

  implicit val symbolOccurrenceOrdering: Ordering[SymbolOccurrence] =
    (a: SymbolOccurrence, b: SymbolOccurrence) => {
      val cmp = rangeOptionOrdering.compare(a.range, b.range)
      if (cmp != 0) cmp
      else {
        val bySymbol = identifierOrdering.compare(a.symbol, b.symbol)
        if (bySymbol != 0) bySymbol else Integer.compare(a.role.value, b.role.value)
      }
    }

  implicit val symbolInformationOrdering: Ordering[SymbolInformation] =
    (a: SymbolInformation, b: SymbolInformation) => identifierOrdering.compare(a.symbol, b.symbol)

  implicit val diagnosticOrdering: Ordering[Diagnostic] = (a: Diagnostic, b: Diagnostic) => {
    val cmp = rangeOptionOrdering.compare(a.range, b.range)
    if (cmp != 0) cmp
    else {
      val bySeverity = Integer.compare(a.severity.value, b.severity.value)
      if (bySeverity != 0) bySeverity else a.message.compareTo(b.message)
    }
  }

  implicit val syntheticOrdering: Ordering[Synthetic] =
    (a: Synthetic, b: Synthetic) => rangeOptionOrdering.compare(a.range, b.range)

}
