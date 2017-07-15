package scala.meta.internal.io

import scala.meta.io._

object PlatformPathIO {
  def workingDirectoryString: String = JSShell.pwd().toString

  def workingDirectory: AbsolutePath =
    if (JSIO.isNode) AbsolutePath(workingDirectoryString)
    else AbsolutePath(fileSeparator)

  def fileSeparatorChar: Char =
    JSIO.sep.toCharArray.head

  def fileSeparator: String =
    JSIO.sep

  def pathSeparator: String =
    JSIO.delimiter

  def isAbsolutePath(path: String): Boolean =
    JSIO.isAbsolute(path)

  def normalizePath(path: String): String =
    JSIO.normalize(path)

  def resolve(path1: AbsolutePath, path2: RelativePath): AbsolutePath =
    AbsolutePath(JSIO.resolve(path1.toString, path2.toString))

  def resolve(path1: RelativePath, path2: RelativePath): RelativePath =
    RelativePath(JSIO.resolve(path1.toString, path2.toString))
}
