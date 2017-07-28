package scala.meta.internal.io

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
}
