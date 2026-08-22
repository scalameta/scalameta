package org.scalameta
package build

object Versions {
  val Scala2ReleaseCandidate = "" // type the entire RC version here, e.g. "2.13.19-RC1"
  val Scala212Versions = getVersions2(12, 18 to 21)
  val Scala213Versions = getVersions2(13, 15 to 18)

  // put any Scala 3 RC under one of these two
  val Scala3LTS = "3.3.8"
  val Scala3Next = "3.8.4"

  // returns the RC when this line lists one, and the newest patch otherwise
  def getLatest(v: Seq[String]) = if (v.head == Scala2ReleaseCandidate) v.head else v.last
  val LatestScala212 = getLatest(Scala212Versions)
  val LatestScala213 = getLatest(Scala213Versions)
  val LatestScala2 = Seq(LatestScala212, LatestScala213)

  val PublishedScala212 = Scala212Versions.head
  val PublishedScala213 = Scala213Versions.head
  val PublishedScala2 = Seq(PublishedScala212, PublishedScala213)
  // a module publishes only if it builds one of these patches, one per binary version
  val PublishedScalaVersions = PublishedScala2 :+ Scala3LTS

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
    AllScala2Versions.contains(Scala2ReleaseCandidate) || Scala2ReleaseCandidate.isEmpty,
    s"$Scala2ReleaseCandidate is not used, perhaps its minor is not yet listed",
  )
  val AllScalaVersions = AllScala2Versions :+ Scala3LTS :+ Scala3Next

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

}
