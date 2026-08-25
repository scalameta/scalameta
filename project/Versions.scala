package org.scalameta
package build

import scala.collection.mutable

object Versions {
  val Scala2ReleaseCandidate = "" // type the entire RC version here, e.g. "2.13.19-RC1"
  val Scala212Versions = getVersions2(12, 18 to 21)
  val Scala213Versions = getVersions2(13, 15 to 18)

  val Scala3Rows = Seq(
    // version to build label mapping; if you change a build label, adjust build.sbt and CI
    // the first row is published and checked for mima
    // the first two are tested in pre-merge for JVM and JS, and in post-merge for Native
    // NB: use one of these two lines for RC testing, as long as it's not merged
    "3.3.8" -> "3_lts",
    "3.8.4" -> "3_next",
    // lines below will only be tested for JVM in post-merge CI
  )
  val Scala3PostMerge = Scala3Rows.drop(2)
  val Scala3Published = Scala3Rows.head._1

  // returns the RC when this line lists one, and the newest patch otherwise
  def getLatest(v: Seq[String]) = if (v.head == Scala2ReleaseCandidate) v.head else v.last
  val LatestScala212 = getLatest(Scala212Versions)
  val LatestScala213 = getLatest(Scala213Versions)
  val LatestScala2 = Seq(LatestScala212, LatestScala213)

  val PublishedScala212 = Scala212Versions.head
  val PublishedScala213 = Scala213Versions.head
  val PublishedScala2 = Seq(PublishedScala212, PublishedScala213)
  // a row publishes only if it builds one of these patches, one per binary version
  val PublishedScalaVersions = PublishedScala2 :+ Scala3Published
  val TestedScalaVersions = PublishedScalaVersions ++ Scala3Rows.tail.map(_._1)

  def getForScalaBinaryVersion(v: String, vs: Seq[String]): String = {
    val prefix = s"$v."
    vs.find(_.startsWith(prefix)).getOrElse(
      throw new Exception(vs.mkString(s"No matching version for Scala binary version $v: ", ", ", "")),
    )
  }

  def getPublishedForScalaVersion(v: String): String =
    getForScalaBinaryVersion(sbt.CrossVersion.binaryScalaVersion(v), PublishedScalaVersions)

  /**
   * Scala.js and Native republish the standard library per full version, and for releases only. A
   * JS row takes the newest release, because scalajs-library depends on that scala-library.
   */
  private def releases(versions: Seq[String]) = versions.filterNot(_ == Scala2ReleaseCandidate)
  val PublishedScala212ForJS = releases(Scala212Versions).last
  val PublishedScala213ForJS = releases(Scala213Versions).last
  val PublishedScala212ForNative = releases(Scala212Versions).head
  val PublishedScala213ForNative = releases(Scala213Versions).head

  val AllScala2Versions = Scala213Versions ++ Scala212Versions
  require(
    Scala2ReleaseCandidate.isEmpty || AllScala2Versions.contains(Scala2ReleaseCandidate),
    s"$Scala2ReleaseCandidate is not used, perhaps its minor is not yet listed",
  )

  // returns versions from oldest to newest
  // put RC first, as published, so it's fully tested, not just with semanticdb
  private def getVersions2(minor: Int, range: Range) = {
    val prefix = s"2.$minor."
    if (range.length > 4)
      throw new Exception(s"Too many versions for scala-${prefix}x: ${range.length} > 4")
    val ordered = if (range.step > 0) range else range.reverse
    val prod = ordered.map(x => s"$prefix$x")
    if (Scala2ReleaseCandidate.startsWith(prefix)) Scala2ReleaseCandidate +: prod else prod
  }

  /**
   * Every Scala 3 version has binary version 3, so a build keyed by the binary version alone holds
   * one version and drops the rest. Each version carries a label instead, and CI steps name those
   * labels: an RC put in the list above keeps the label its place in the list gives it.
   */
  val Scala3RowIds: Map[String, String] = {
    val versions = new mutable.HashSet[String]()
    val labels = new mutable.HashSet[String]()
    val builder = Map.newBuilder[String, String]
    Scala3Rows.foreach { case (ver, label) =>
      if (!versions.add(ver)) throw new Exception(s"Scala 3 version $ver is listed twice")
      if (!labels.add(label)) throw new Exception(s"Scala 3 label $label is listed twice")
      builder += ver -> label
    }
    builder.result()
  }

}
