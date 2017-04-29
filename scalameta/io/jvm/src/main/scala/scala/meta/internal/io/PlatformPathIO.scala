package scala.meta.internal.io

import java.io.File
import scala.meta.io._

object PlatformPathIO {
  def fileSeparator: String =
    File.separator

  def pathSeparator: String =
    File.pathSeparator

  def workingDirectory: AbsolutePath =
    AbsolutePath(sys.props("user.dir"))

  def isAbsolutePath(path: String): Boolean =
    new File(path).isAbsolute

  def normalizePath(path: String): String =
    new File(path).toString

  def resolve(path1: AbsolutePath, path2: RelativePath): AbsolutePath =
    AbsolutePath(new File(path1.toFile, path2.toString))

  def resolve(path1: RelativePath, path2: RelativePath): RelativePath =
    RelativePath(new File(path1.toFile, path2.toString))
}
