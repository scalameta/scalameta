package org.langmeta
package semanticdb

import org.langmeta.internal.semanticdb.DeprecationMessage

@deprecated(DeprecationMessage, "3.8.0")
sealed trait Severity extends Product {
  def syntax: String = s"[${productPrefix.toLowerCase}]"
  def structure: String = s"Severity.$productPrefix"
  override def toString: String = syntax
}

@deprecated(DeprecationMessage, "3.8.0")
object Severity {
  case object Info extends Severity
  case object Warning extends Severity
  case object Error extends Severity
  case object Hint extends Severity
}
