package star.meta
package semanticdb

sealed trait Severity extends Product {
  def syntax: String = s"[${productPrefix.toLowerCase}]"
  def structure: String = s"Severity.$productPrefix"
  override def toString: String = syntax
}

object Severity {
  case object Info extends Severity
  case object Warning extends Severity
  case object Error extends Severity
}
