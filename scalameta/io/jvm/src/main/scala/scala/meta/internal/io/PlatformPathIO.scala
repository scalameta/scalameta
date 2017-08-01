package scala.meta.internal.io

import java.io.File
import java.nio.file.Paths
import scala.meta.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    sys.props("user.dir")

  def workingDirectory: AbsolutePath =
    AbsolutePath(workingDirectoryString)

  def isAbsolutePath(path: String): Boolean =
    Paths.get(path).isAbsolute

  def normalizePath(path: String): String =
    Paths.get(path).normalize().toString
}
