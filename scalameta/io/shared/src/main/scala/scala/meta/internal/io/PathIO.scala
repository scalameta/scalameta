package scala.meta.internal.io

import java.nio.file._
import java.io.File
import scala.meta.io._

object PathIO {

  def workingDirectory: AbsolutePath =
    AbsolutePath(PlatformPathIO.workingDirectoryString)

  // These two methods work on strings instead of AbsolutePath because AbsolutePath
  // with unix / slashes is non-sensical on Windows.
  def toUnix(path: String): String =
    if (File.separatorChar != '/') path.replace(File.separatorChar, '/')
    else path

  def fromUnix(path: String): String =
    if (File.separatorChar != '/') path.replace('/', File.separatorChar)
    else path

  /** Returns file extension of this path, returns empty string if path has no extension */
  def extension(path: Path): String = {
    val filename = path.getFileName.toString
    val idx = filename.lastIndexOf('.')
    if (idx == -1) ""
    else filename.substring(idx + 1)
  }
}
