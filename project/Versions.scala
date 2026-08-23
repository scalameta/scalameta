package org.scalameta
package build

import scala.collection.mutable

object Versions {
  val Scala2ReleaseCandidate = "" // type the entire RC version here, e.g. "2.13.19-RC1"
  val Scala212Versions = getVersions2(12, 18 to 21)
  val Scala213Versions = getVersions2(13, 15 to 18)

  val Scala3Rows = Seq(
    // version to build label; only the first one is published; others are tested
    "3.3.8" -> "3_lts",
    "3.8.4" -> "3_next",
    // you can add an RC here, e.g. "3.9.0-RC1" -> "3_rc"
  )
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

  /**
   * Scala.js publishes a compiler plugin for each full Scala version. Scala Native publishes a
   * compiler plugin and a standard library. Both publish them for releases only, so a JS build and
   * a Native build keep the release an RC replaces.
   *
   * A JVM build takes the oldest release of a Scala 2 line. A Native build takes the same release,
   * the one it publishes from. A JS build takes the newest release, because Scala.js publishes for
   * recent patches only. Name the previous release here when either project has not published for a
   * new patch yet. Scala.js did that after 2.13.16 came out.
   *
   * Scala 3 needs no plugin for JS, because the Scala 3 compiler writes JS itself. A Scala 3 build
   * keeps its own version.
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
  val AllScalaVersions = AllScala2Versions ++ Scala3Rows.map(_._1)

  // returns versions from oldest to newest
  private def getVersions2(minor: Int, range: Range) = {
    val prefix = s"2.$minor."
    if (range.length > 4)
      throw new Exception(s"Too many versions for scala-${prefix}x: ${range.length} > 4")
    val ordered = if (range.step > 0) range else range.reverse
    val prod = ordered.map(x => s"$prefix$x")
    // the RC comes first, in place of the patch its line publishes from. A JVM build then compiles
    // and tests every module with it. A plain switch reaches the test sources only.
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
