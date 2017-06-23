package scala.meta.internal.io

import java.nio.file.Path
import scala.meta.io._

object PlatformPathIO {
  def workingDirectoryString: String = JSShell.pwd().toString

  def workingDirectory: AbsolutePath =
    if (JSIO.isNode) AbsolutePath(workingDirectoryString)
    else AbsolutePath(fileSeparator)

  def fileSeparatorChar: Char =
    JSPath.sep.toCharArray.head

  def fileSeparator: String =
    JSPath.sep

  def pathSeparator: String =
    JSPath.delimiter

  def isAbsolutePath(path: String): Boolean =
    JSPath.isAbsolute(path)

  def normalizePath(path: String): String =
    JSPath.normalize(path)

  def resolve(path1: AbsolutePath, path2: RelativePath): AbsolutePath =
    AbsolutePath(JSPath.resolve(path1.toString, path2.toString))

  def resolve(path1: RelativePath, path2: RelativePath): RelativePath =
    RelativePath(JSPath.resolve(path1.toString, path2.toString))

  def pathGet(first: String, more: String*): Path =
    NodeNIOPath(JSPath.join(first, more: _*))
}
