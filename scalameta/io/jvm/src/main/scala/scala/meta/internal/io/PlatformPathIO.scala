package scala.meta.internal.io

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import scala.meta.io._

object PlatformPathIO {

  def fileSeparatorChar: Char =
    File.separatorChar

  def fileSeparator: String =
    File.separator

  def pathSeparator: String =
    File.pathSeparator

  def workingDirectory: AbsolutePath =
    AbsolutePath(sys.props("user.dir"))(AbsolutePath.root)

  def rootDirectory: AbsolutePath =
    AbsolutePath(Paths.get("").toAbsolutePath.getRoot)(AbsolutePath.root)

  def homeDirectory: AbsolutePath =
    AbsolutePath(sys.props("user.home"))(AbsolutePath.root)

  def isAbsolutePath(path: String): Boolean =
    Paths.get(path).isAbsolute

  def normalizePath(path: String): String =
    Paths.get(path).normalize().toString

  def pathGet(first: String, more: String*): Path =
    Paths.get(first, more: _*)
}
