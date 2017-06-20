package scala.meta.internal.io

import scala.meta.io._

object PathIO {

  def fileSeparatorChar: Char =
    PlatformPathIO.fileSeparatorChar

  def fileSeparator: String =
    PlatformPathIO.fileSeparator

  def pathSeparator: String =
    PlatformPathIO.pathSeparator

  def workingDirectory: AbsolutePath =
    PlatformPathIO.workingDirectory

  def isAbsolutePath(path: String): Boolean =
    PlatformPathIO.isAbsolutePath(path)

  def isRelativePath(path: String): Boolean =
    !isAbsolutePath(path)

  def normalizePath(path: String): String =
    PlatformPathIO.fileSeparator
}
