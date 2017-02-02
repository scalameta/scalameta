package scala.meta.internal
package scalahost
package v1

import scala.util.Properties
import scala.meta.Dialect

trait DialectOps {
  def dialect: Dialect = {
    val version = Properties.versionNumberString
    if (version.startsWith("2.10")) scala.meta.dialects.Scala210
    else if (version.startsWith("2.11")) scala.meta.dialects.Scala211
    else if (version.startsWith("2.12")) scala.meta.dialects.Scala212
    else sys.error(s"Unsupported Scala version $version")
  }
}