package scala.meta
package internal.semantic

case class SugarRange(start: Int, end: Int, symbol: Symbol) {
  def addOffset(offset: Int) = SugarRange(start + offset, end + offset, symbol)
  def toMeta(input: Input): ResolvedName =
    ResolvedName(Position.Range(input, start, end), symbol)
}
case class AttributedSugar(text: String, names: List[SugarRange]) {
  def +(other: String) = AttributedSugar(text + other, names)
  def +(other: AttributedSugar) =
    AttributedSugar(text + other.text, names ++ other.names.map(_.addOffset(text.length)))
}

object AttributedSugar {
  val empty = AttributedSugar("", Nil)
  val star = AttributedSugar("*", List(SugarRange(0, 1, Symbol("_star_."))))
  def apply(text: String): AttributedSugar = AttributedSugar(text, Nil)
  def mkString(sugars: List[AttributedSugar], sep: String): AttributedSugar = sugars match {
    case Nil => empty
    case head :: Nil => head
    case head :: lst =>
      lst.foldLeft(head) {
        case (accum, sugar) =>
          accum + sep + sugar
      }
  }
}

// data structure to manage multiple inferred sugars at the same position.
case class Inferred(
    select: Option[AttributedSugar] = None,
    targs: Option[AttributedSugar] = None,
    conversion: Option[AttributedSugar] = None,
    args: Option[AttributedSugar] = None
) {
  assert(
    args.isEmpty || conversion.isEmpty,
    s"Not possible to define conversion + args! $args $conversion"
  )

  private def all: List[AttributedSugar] = (select :: targs :: conversion :: args :: Nil).flatten

  def toSugar(input: Input, pos: Position): Sugar = {
    def onlyConversionIsDefined =
      conversion.isDefined &&
        select.isEmpty &&
        targs.isEmpty &&
        args.isEmpty

    def needsPrefix: Boolean =
      !onlyConversionIsDefined

    val sugar: AttributedSugar = {
      val start =
        if (needsPrefix) AttributedSugar.star
        else AttributedSugar.empty
      all.foldLeft(start)(_ + _)
    }
    val sugarInput = Input.Sugar(sugar.text, input, pos.start, pos.end)
    val names = sugar.names.map(_.toMeta(sugarInput))
    new Sugar(pos, sugar.text, names)
  }
}
