package scala.meta.internal.semanticdb.scalac

import scala.tools.nsc.settings.ScalaVersion
import scala.util.Properties
import scala.util.control.NonFatal
import scala.{meta => m}

trait LanguageOps {
  self: SemanticdbOps =>

  lazy val language: String = {
    val version = Properties.versionNumberString
    if (version.startsWith("2.10")) "Scala210"
    else if (version.startsWith("2.11")) "Scala211"
    else if (version.startsWith("2.12"))
      if (scala3SyntaxSupported(version)) "Scala212Source3" else "Scala212"
    else if (version.startsWith("2.13"))
      if (scala3SyntaxSupported(version)) "Scala213Source3" else "Scala213"
    else sys.error(s"unsupported Scala version $version")
  }

  private def isScala3CompatSet = {
    val version3 = ScalaVersion("3.0.0")
    global.settings.source.value >= version3
  }

  private def scala3SyntaxSupported(version: String): Boolean =
    try {
      val Array(major, minor, patch) = version.replaceAll("(-|\\+).+$", "").split('.').map(_.toInt)
      val correctVersion = (major, minor) match {
        case (2, 13) => patch >= 6
        case (2, 12) => patch >= 14
        case _ => false
      }
      correctVersion && isScala3CompatSet
    } catch { case NonFatal(_) => false }

}
