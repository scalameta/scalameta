package org.scalameta
package build

object Versions {
  val Scala211Versions = getVersions(2, 11, 12 to 12)
  val Scala212Versions = getVersions(2, 12, 15 to 18)
  val Scala213Versions = getVersions(2, 13, 9 to 12)
  val LatestScala211 = Scala211Versions.head
  val LatestScala212 = Scala212Versions.head
  val LatestScala213 = Scala213Versions.head
  val AllScalaVersions =
    Scala213Versions ++ Scala212Versions ++ Scala211Versions
  val LatestScalaVersions =
    Seq(LatestScala213, LatestScala212, LatestScala211)

  // returns versions from newest to oldest
  private def getVersions(major: Int, minor: Int, range: Range) = {
    if (range.length > 4)
      throw new Exception(s"Too many versions for scala-$major.$minor: ${range.length} > 4")
    val desc = if (range.step > 0) range.reverse else range
    desc.map { x => s"$major.$minor.$x" }
  }

}
