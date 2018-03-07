package scala.meta.metap

sealed trait Format {
  def isPretty: Boolean = this == Format.Pretty
  def isProto: Boolean = this == Format.Proto
}

object Format {
  case object Pretty extends Format
  case object Proto extends Format
}
