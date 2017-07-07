package scala.meta
package internal.semantic

import scala.meta.inputs.Position.Range

case class SugarRange(start: Int, end: Int, symbol: Symbol) {
  def withOffset(offset: Int) = SugarRange(start + offset, end + offset, symbol)
  def toMeta(input: Input): (Range, Symbol) =
    (Position.Range(input, start, end), symbol)
}
case class PlainSugar(syntax: String, names: List[SugarRange]) {
  def +(other: String) = PlainSugar(syntax + other, names)
  def +(other: PlainSugar) =
    PlainSugar(syntax + other.syntax, names ++ other.names.map(_.withOffset(syntax.length)))
}

object PlainSugar {
  val empty = PlainSugar("", Nil)
  def apply(syntax: String): PlainSugar = PlainSugar(syntax, Nil)
  def mkString(sugars: List[PlainSugar], sep: String): PlainSugar = sugars match {
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
    select: Option[PlainSugar] = None,
    types: Option[PlainSugar] = None,
    conversion: Option[PlainSugar] = None,
    args: Option[PlainSugar] = None
) {
  assert(
    args.isEmpty || conversion.isEmpty,
    s"Not possible to define conversion + args! $args $conversion"
  )

  private def all: List[PlainSugar] = (select :: types :: conversion :: args :: Nil).flatten

  private def onlyConversionIsDefined =
    conversion.isDefined &&
      select.isEmpty &&
      types.isEmpty &&
      args.isEmpty

  private def needsPrefix: Boolean =
    !onlyConversionIsDefined

  def toSugar(input: Input, pos: Position): Sugar = {
    val sugar: PlainSugar = {
      val start =
        if (needsPrefix) PlainSugar("*")
        else PlainSugar.empty
      all.foldLeft(start)(_ + _)
    }
    new Sugar(
      Input.Sugar(sugar.syntax, input, pos.start, pos.end),
      sugar.names.map(_.toMeta(input))
    )
  }

  def withConversion(syntax: PlainSugar): Inferred =
    if (conversion.isDefined) this
    else copy(conversion = Some(syntax))
}
