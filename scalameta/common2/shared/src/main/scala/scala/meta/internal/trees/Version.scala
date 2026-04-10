package scala.meta.internal.trees

import scala.util.Try

case class Version(major: Int, minor: Int, patch: Int) {
  def asString(delim: Char): String = s"$major$delim$minor$delim$patch"

  override def toString: String = asString('.')
}

object Version {

  final val zero = Version(0, 0, 0)

  implicit val ordering: Ordering[Version] = new Ordering[Version] {
    override def compare(x: Version, y: Version): Int = {
      val cmpMajor = x.major.compare(y.major)
      if (cmpMajor != 0) return cmpMajor
      val cmpMinor = x.minor.compare(y.minor)
      if (cmpMinor != 0) return cmpMinor
      x.patch.compare(y.patch)
    }
  }

  def parse(v: String, delim: Char = '.'): Try[Version] = Try {
    val versions = v.split(delim).map(_.toInt)
    Version(versions(0), versions(1), versions(2))
  }

}
