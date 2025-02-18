package scala.meta.metap

sealed trait Format {
  def isCompact: Boolean = this == Format.Compact
  def isDetailed: Boolean = this == Format.Detailed
  def isProto: Boolean = this == Format.Proto
}

object Format {
  case object Compact extends Format
  case object Detailed extends Format
  case object Proto extends Format
}
