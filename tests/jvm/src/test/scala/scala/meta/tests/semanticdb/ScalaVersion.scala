package scala.meta.tests.semanticdb

import scala.meta.tests.BuildInfo
import scala.util.Properties

object ScalaVersion {
  val version = Properties.versionNumberString

  sealed trait Version
  case object Scala211 extends Version
  case object Scala212 extends Version
  case object Scala213 extends Version
  case class Full(ver: String) extends Version

  def isLatest212Or213: Boolean = {
    version == BuildInfo.latestScala212Version ||
    version == BuildInfo.latestScala213Version
  }

  def is212: Boolean = {
    version.startsWith("2.12")
  }

  def doIf212[T](what: String)(thunk: => T): Unit = {
    if (is212) {
      thunk
    } else {
      println(s"Skipping $what because scalaVersion is ${Properties.versionNumberString}")
    }
  }

  def isSupported(minimal212: Int, minimal213: Int): Boolean = {
    val Array(major, minor, patch) =
      version.replaceAll("(-|\\+).+$", "").split('.').map(_.toInt)
    (major, minor) match {
      case (2, 13) => patch >= minimal213
      case (2, 12) => patch >= minimal212
      case _ => false
    }
  }

  def atLeast212_14 = isSupported(minimal212 = 14, minimal213 = 0)

  def getExpected(compat: Seq[(Version, String)], expected: String) = {
    compat
      .collectFirst {
        case (Full(ver), expected) if version == ver => expected
        case (Scala213, expected) if version.startsWith("2.13") => expected
        case (Scala212, expected) if version.startsWith("2.12") => expected
        case (Scala211, expected) if version.startsWith("2.11") => expected
      }
      .getOrElse(expected)
  }

}
