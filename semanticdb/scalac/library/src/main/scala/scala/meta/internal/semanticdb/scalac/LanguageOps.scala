package scala.meta.internal.semanticdb.scalac

import scala.util.Properties
import scala.{meta => m}

trait LanguageOps { self: SemanticdbOps =>

  lazy val language: String = {
    val version = Properties.versionNumberString
    if (version.startsWith("2.10")) "Scala210"
    else if (version.startsWith("2.11")) {
      if (!version.endsWith("-bin-typelevel-4")) "Scala211"
      else "Typelevel211"
    } else if (version.startsWith("2.12")) {
      if (!version.endsWith("-bin-typelevel-4")) "Scala212"
      else "Typelevel212"
    } else sys.error(s"unsupported Scala version $version")
  }
}
