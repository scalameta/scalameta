package scala.meta.io

import PlatformIO._

// sealed abstract to ensure that Absolute.apply constructor is used.
sealed abstract class AbsolutePath(val absolute: String) extends Serializable {
  def relative: String = absolute.stripPrefix(workingDirectory.absolute + fileSeparator)
  override def hashCode(): Int = absolute.hashCode
  override def equals(obj: scala.Any): Boolean = obj match {
    case p: AbsolutePath => absolute == p.absolute
    case _ => false
  }
  override def toString: String = s"AbsolutePath($absolute)"
  @deprecated("Use .absolute instead", "1.8.0") // kept for source compabilitity with old Input.File/Address.File
  def getAbsolutePath: String = absolute
  def /(other: String): AbsolutePath =
    new AbsolutePath(absolute + fileSeparator + other) {}
}

object AbsolutePath {
  def unapply(arg: AbsolutePath): Option[String] = Some(arg.absolute)
  def apply(file: java.io.File): AbsolutePath =
    new AbsolutePath(file.getAbsolutePath) {}
  def apply(path: String): AbsolutePath =
    if (isAbsolutePath(path)) {
      new AbsolutePath(path) {}
    } else {
      workingDirectory / path
    }
}
