package scala.meta
package internal.semanticdb.scalac

case class SyntheticRange(start: Int, end: Int, symbol: Symbol) {
  def addOffset(offset: Int) = SyntheticRange(start + offset, end + offset, symbol)
  def toMeta(input: Input): ResolvedName =
    ResolvedName(Position.Range(input, start, end), symbol, isDefinition = false)
}
case class AttributedSynthetic(text: String, names: List[SyntheticRange]) {
  def +(other: String) = AttributedSynthetic(text + other, names)
  def +(other: AttributedSynthetic) =
    AttributedSynthetic(text + other.text, names ++ other.names.map(_.addOffset(text.length)))
}

object AttributedSynthetic {
  val empty = AttributedSynthetic("", Nil)
  val star = AttributedSynthetic("*", List(SyntheticRange(0, 1, Symbol("_star_."))))
  def apply(text: String): AttributedSynthetic = AttributedSynthetic(text, Nil)
  def mkString(synthetics: List[AttributedSynthetic], sep: String): AttributedSynthetic =
    synthetics match {
      case Nil => empty
      case head :: Nil => head
      case head :: lst =>
        lst.foldLeft(head) {
          case (accum, synthetic) =>
            accum + sep + synthetic
        }
    }
}

// data structure to manage multiple inferred synthetics at the same position.
case class Inferred(
    select: Option[AttributedSynthetic] = None,
    targs: Option[AttributedSynthetic] = None,
    conversion: Option[AttributedSynthetic] = None,
    args: Option[AttributedSynthetic] = None
) {
  assert(
    args.isEmpty || conversion.isEmpty,
    s"Not possible to define conversion + args! $args $conversion"
  )

  private def all: List[AttributedSynthetic] =
    (select :: targs :: conversion :: args :: Nil).flatten

  def toSynthetic(input: Input, pos: Position): Synthetic = {
    def onlyConversionIsDefined =
      conversion.isDefined &&
        select.isEmpty &&
        targs.isEmpty &&
        args.isEmpty

    def needsPrefix: Boolean =
      !onlyConversionIsDefined

    val synthetic: AttributedSynthetic = {
      val start =
        if (needsPrefix) AttributedSynthetic.star
        else AttributedSynthetic.empty
      all.foldLeft(start)(_ + _)
    }
    val syntheticInput = Input.Synthetic(synthetic.text, input, pos.start, pos.end)
    val names = synthetic.names.map(_.toMeta(syntheticInput))
    new Synthetic(pos, synthetic.text, names)
  }
}
