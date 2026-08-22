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
   * A JVM build takes the oldest patch of a Scala 2 line. A JS build takes the newest one, because
   * Scala.js publishes its compiler plugin for each full Scala version, and a JS build can use only
   * a patch Scala.js published for. Name the previous patch here when Scala.js has not published
   * for a new one yet. Scala.js did that after 2.13.16 came out.
   *
   * Scala 3 needs no plugin, because the Scala 3 compiler writes JS itself.
   */
  val PublishedScala212ForJS = LatestScala212
  val PublishedScala213ForJS = LatestScala213

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
