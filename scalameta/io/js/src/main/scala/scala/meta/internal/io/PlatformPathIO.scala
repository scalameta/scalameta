package scala.meta.internal.io

import scala.meta.io._

object PlatformPathIO {
  def workingDirectoryString: String =
    if (JSIO.isNode) JSShell.pwd().toString
    else fileSeparator

  def workingDirectory: AbsolutePath =
    AbsolutePath(workingDirectoryString)

  def fileSeparatorChar: Char =
    fileSeparator.charAt(0)

  def fileSeparator: String =
    if (JSIO.isNode) JSPath.sep
    else "/"

  def pathSeparator: String =
    if (JSIO.isNode) JSPath.delimiter
    else ":"

  def isAbsolutePath(path: String): Boolean =
    if (JSIO.isNode) JSPath.isAbsolute(path)
    else path.startsWith(fileSeparator)

  def normalizePath(path: String): String =
    if (JSIO.isNode) JSPath.normalize(path)
    else path
}
