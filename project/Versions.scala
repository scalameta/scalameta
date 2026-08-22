package org.scalameta
package build

object Versions {
  val Scala212Versions = getVersions2(12, 18 to 21)
  val Scala213Versions = getVersions2(13, 15 to 18)

  val Scala3LTS = "3.3.8"
  val Scala3Next = "3.8.4"

  val LatestScala212 = Scala212Versions.last
  val LatestScala213 = Scala213Versions.last
  val LatestScala2 = Seq(LatestScala212, LatestScala213)

  val PublishedScala212 = Scala212Versions.head
  val PublishedScala213 = Scala213Versions.head
  val PublishedScala2 = Seq(PublishedScala212, PublishedScala213)
  // a module publishes only if it builds one of these patches, one per binary version
  val PublishedScalaVersions = PublishedScala2 :+ Scala3LTS

  /**
   * Scala.js publishes a compiler plugin for each full Scala version. Scala Native publishes a
   * compiler plugin and a standard library. A JS build and a Native build can therefore use only a
   * patch those projects published for.
   *
   * A JVM build takes the oldest patch of a Scala 2 line. A Native build takes the same patch, the
   * one it publishes from. A JS build takes the newest patch, because Scala.js publishes for recent
   * patches only. Name the previous patch here when either project has not published for a new one
   * yet. Scala.js did that after 2.13.16 came out.
   *
   * Scala 3 needs no plugin for JS, because the Scala 3 compiler writes JS itself. A Scala 3 build
   * keeps its own version.
   */
  val PublishedScala212ForJS = LatestScala212
  val PublishedScala213ForJS = LatestScala213
  val PublishedScala212ForNative = PublishedScala212
  val PublishedScala213ForNative = PublishedScala213

  val AllScala2Versions = Scala213Versions ++ Scala212Versions
  val AllScalaVersions = AllScala2Versions :+ Scala3LTS :+ Scala3Next

  // returns versions from oldest to newest
  private def getVersions2(minor: Int, range: Range) = {
    val prefix = s"2.$minor."
    if (range.length > 4)
      throw new Exception(s"Too many versions for scala-${prefix}x: ${range.length} > 4")
    val ordered = if (range.step > 0) range else range.reverse
    ordered.map(x => s"$prefix$x")
  }

}
