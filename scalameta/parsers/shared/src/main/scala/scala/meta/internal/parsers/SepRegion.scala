package scala.meta.internal.parsers

sealed trait SepRegion {
  def indent = -1
  def closeOnNonCase = false
  def indentOnArrow = true
  def isIndented = false
}

case class RegionIndent(override val indent: Int, override val closeOnNonCase: Boolean)
    extends SepRegion {
  override def isIndented: Boolean = true
}

case class RegionParen(canProduceLF: Boolean) extends SepRegion

case object RegionBracket extends SepRegion

case class RegionBrace(override val indent: Int, override val indentOnArrow: Boolean)
    extends SepRegion

case class RegionCase(override val indent: Int) extends SepRegion

case class RegionEnum(override val indent: Int) extends SepRegion

case class RegionIndentEnum(override val indent: Int) extends SepRegion {
  override def isIndented: Boolean = true
}

case object RegionArrow extends SepRegion

// NOTE: Special case for Enum region is needed because parsing of 'case' statement is done differently
case object RegionEnumArtificialMark extends SepRegion
