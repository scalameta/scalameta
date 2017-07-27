package scala.meta.internal
package semantic

import scala.util.Properties
import scala.{meta => m}

trait LanguageOps { self: DatabaseOps =>

  lazy val language: String = {
    val version = Properties.versionNumberString
    if (version.startsWith("2.10")) "Scala210"
    else if (version.startsWith("2.11")) "Scala211"
    else if (version.startsWith("2.12")) "Scala212"
    else sys.error(s"unsupported Scala version $version")
  }
}
