package scala.meta.internal.semantic

// data structure to manage multiple inferred sugars at the same position.
case class Inferred(
    select: Option[String] = None,
    types: Option[String] = None,
    conversion: Option[String] = None,
    args: Option[String] = None
) {
  assert(
    args.isEmpty || conversion.isEmpty,
    s"Not possible to define conversion + args! $args $conversion"
  )

  private def onlyConversionIsDefined =
    conversion.isDefined &&
      select.isEmpty &&
      types.isEmpty &&
      args.isEmpty

  private def needsPrefix: Boolean =
    !onlyConversionIsDefined

  def syntax: String = {
    val result = new StringBuilder
    if (needsPrefix) result.append("(*)")
    select.foreach(result.append)
    types.foreach(result.append)
    conversion.foreach(result.append)
    args.foreach(result.append)
    result.toString()
  }
  def withConversion(syntax: String): Inferred =
    if (conversion.isDefined) this
    else copy(conversion = Some(syntax))
}
