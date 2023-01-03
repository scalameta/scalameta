package scala.meta.internal.parsers

sealed trait SepRegion {
  def indent = -1
  def closeOnNonCase = false
  def indentOnArrow = true
  def isIndented: Boolean
}

sealed trait SepRegionIndented extends SepRegion {
  override final def isIndented = true
}
sealed trait SepRegionNonIndented extends SepRegion {
  override final def isIndented = false
}

case class RegionIndent(override val indent: Int, override val closeOnNonCase: Boolean)
    extends SepRegionIndented

case class RegionParen(canProduceLF: Boolean) extends SepRegionNonIndented

case object RegionBracket extends SepRegionNonIndented

case class RegionBrace(override val indent: Int, override val indentOnArrow: Boolean)
    extends SepRegionNonIndented

case class RegionCase(override val indent: Int) extends SepRegionNonIndented

case class RegionEnum(override val indent: Int) extends SepRegionNonIndented

case class RegionIndentEnum(override val indent: Int) extends SepRegionIndented

case object RegionArrow extends SepRegionNonIndented

// NOTE: Special case for Enum region is needed because parsing of 'case' statement is done differently
case object RegionEnumArtificialMark extends SepRegionNonIndented
