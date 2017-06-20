package scala.meta.internal.io

import scala.meta.io._

object PlatformPathIO {
  private implicit val cwd = PathIO.workingDirectory
  def workingDirectory: AbsolutePath =
    if (JSIO.isNode) AbsolutePath(JSShell.pwd().toString)
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
}
