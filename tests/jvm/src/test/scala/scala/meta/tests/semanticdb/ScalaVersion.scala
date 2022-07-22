package scala.meta.tests.semanticdb

import scala.meta.tests.BuildInfo
import scala.util.Properties

object ScalaVersion {
  val version = Properties.versionNumberString

  // both the compiler and stdlib are different between Scala versions.
  // For the sake of simplicity, we only run the expect test against the
  // output of 2.12. It's possible to add another expect file for 2.11
  // later down the road if that turns out to be useful.
  def isLatest212: Boolean = {
    version == BuildInfo.latestScala212Version
  }

  def is212: Boolean = {
    version.startsWith("2.12")
  }

  def doIfLatest212[T](what: String)(thunk: => T): Unit = {
    if (isLatest212) {
      thunk
    } else {
      println(s"Skipping $what because scalaVersion is ${Properties.versionNumberString}, which is not the latest Scala 2.12 version ${BuildInfo.latestScala212Version}")
    }
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

  def getExpected(compat: Seq[(String, String)], expected: String) = {
    compat
      .collectFirst {
        case (ver, expected) if version.startsWith(ver) => expected
      }
      .getOrElse(expected)
  }

}
