package org.scalameta
package build

object Versions {
  val Scala211Versions = getVersions2(11, 12 to 12)
  val Scala212Versions = getVersions2(12, 17 to 20)
  val Scala213Versions = getVersions2(13, 13 to 16)
  val Scala3Versions = getVersions3(3 -> 6, 7 -> 2)
  val Scala2ReleaseCandidates = getReleaseCandidates(2)
  val Scala3ReleaseCandidates = getReleaseCandidates(3, (7, 3, "RC2"))
  val LatestScala211 = Scala211Versions.head
  val LatestScala212 = Scala212Versions.head
  val LatestScala213 = Scala213Versions.head
  val LatestScala213ForJS = "2.13.16"
  val EarliestScala211 = Scala211Versions.last
  val EarliestScala212 = Scala212Versions.last
  val EarliestScala213 = Scala213Versions.last
  val EarliestScala3 = Scala3Versions.last
  val AllScala2Versions = Scala213Versions ++ Scala212Versions ++ Scala211Versions ++
    Scala2ReleaseCandidates
  val AllScalaVersions = AllScala2Versions ++ Scala3Versions ++ Scala3ReleaseCandidates
  val EarliestScala2Versions = Seq(EarliestScala213, EarliestScala212, EarliestScala211) ++
    Scala2ReleaseCandidates
  val EarliestScalaVersions = (EarliestScala2Versions :+ EarliestScala3) ++ Scala3ReleaseCandidates
  val LatestScala2Versions = Seq(LatestScala213, LatestScala212, LatestScala211)

  private def getVersions[A](prefix: String, suffixes: Seq[A]) = {
    if (suffixes.length > 4)
      throw new Exception(s"Too many versions for scala-$prefix: ${suffixes.length} > 4")
    suffixes.map(x => s"$prefix.$x")
  }

  // returns versions from newest to oldest
  private def getVersions2(minor: Int, range: Range) =
    getVersions(s"2.$minor", if (range.step > 0) range.reverse else range)

  // returns versions from newest to oldest
  private def getVersions3(versions: (Int, Int)*) = {
    val ordering = implicitly[Ordering[(Int, Int)]].reverse
    getVersions("3", versions.sorted(ordering).map { case (minor, patch) => s"$minor.$patch" })
  }

  private def getReleaseCandidates(major: Int, versions: (Int, Int, String)*) =
    getVersions(major.toString, versions.map { case (minor, patch, rc) => s"$minor.$patch-$rc" })

}
