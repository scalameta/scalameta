package scala.meta.tests.semanticdb

import scala.meta.internal.trees.{Version => SV}
import scala.meta.tests.BuildInfo

import scala.math.Ordered.orderingToOrdered
import scala.util.Properties

object ScalaVersion {

  val version = Properties.versionNumberString
  val parsedVersion = parse(version)

  private def parse(v: String): SV = SV.parse(v.replaceAll("[-+].*$", "")).getOrElse(SV.zero)

  sealed trait Version
  class Between(val minVer: SV, val maxVer: SV) extends Version
  object Between {
    def apply(min: String, max: String): Between = new Between(parse(min), parse(max))
  }
  case object Scala211 extends Between(SV(2, 11, 0), SV(2, 11, Int.MaxValue))
  case object Scala212 extends Between(SV(2, 12, 0), SV(2, 12, Int.MaxValue))
  case object Scala213 extends Between(SV(2, 13, 0), SV(2, 13, Int.MaxValue))
  case class Full(ver: String) extends Version
  case class AtMost213(patch: Int) extends Between(SV(2, 13, 0), SV(2, 13, patch))

  def atLeast(ver: String): Between =
    new Between(parse(ver), SV(Int.MaxValue, Int.MaxValue, Int.MaxValue))
  def atMost(ver: String): Between = new Between(SV.zero, parse(ver))

  def isLatest212Or213: Boolean = version == BuildInfo.latestScala212Version ||
    version == BuildInfo.latestScala213Version

  def is(major: Int, minor: Int): Boolean = BuildInfo.scalaBinaryVersion == s"$major.$minor"

  def is212: Boolean = is(2, 12)
  def is213: Boolean = is(2, 13)

  def doIf[T](what: String, flag: Boolean)(thunk: => T): Unit =
    if (flag) thunk else println(s"Skipping $what because scalaVersion is $version")

  def isSupported(minimal212: Int, minimal213: Int): Boolean = {
    val Array(major, minor, patch) = version.replaceAll("(-|\\+).+$", "").split('.').map(_.toInt)
    (major, minor) match {
      case (2, 13) => patch >= minimal213
      case (2, 12) => patch >= minimal212
      case _ => false
    }
  }

  def atLeast213(patch: Int) = parsedVersion.major > 2 ||
    parsedVersion.major == 2 &&
    (parsedVersion.minor > 13 || parsedVersion.minor == 13 && parsedVersion.patch >= patch)
  def atLeast213_15 = atLeast213(15)
  def atLeast213_16 = atLeast213(16)
  def atLeast213_17 = atLeast213(17)

  def getExpected(compat: Seq[(Version, String)], expected: String): String =
    if (compat.isEmpty) expected
    else compat.collectFirst {
      case (Full(`version`), expected) => expected
      case (v: Between, expected) if parsedVersion >= v.minVer && parsedVersion <= v.maxVer =>
        expected
    }.getOrElse(expected)

}
