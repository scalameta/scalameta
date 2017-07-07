package scala.meta.internal.io

import java.io.File
import java.nio.file.Paths
import scala.meta.io._

object PlatformPathIO {

  def fileSeparatorChar: Char =
    File.separatorChar

  def fileSeparator: String =
    File.separator

  def pathSeparator: String =
    File.pathSeparator

  def workingDirectoryString: String =
    sys.props("user.dir")

  def workingDirectory: AbsolutePath =
    AbsolutePath(workingDirectoryString)

  def rootDirectory: AbsolutePath =
    AbsolutePath(Paths.get("").toAbsolutePath.getRoot)

  def homeDirectory: AbsolutePath =
    AbsolutePath(sys.props("user.home"))

  def isAbsolutePath(path: String): Boolean =
    Paths.get(path).isAbsolute

  def normalizePath(path: String): String =
    Paths.get(path).normalize().toString
}
