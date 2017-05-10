package scala.meta.internal.io

import scala.meta.io._

import java.nio.charset.Charset

object PathIO {
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

  def resolve(path1: AbsolutePath, path2: RelativePath): AbsolutePath =
    PlatformPathIO.resolve(path1, path2)

  def resolve(path1: RelativePath, path2: RelativePath): RelativePath =
    PlatformPathIO.resolve(path1, path2)

  def unresolve(path1: AbsolutePath, path2: AbsolutePath): RelativePath = {
    if (path1 == path2) return RelativePath(".")
    var s1 = path1.toString
    if (!s1.endsWith(fileSeparator)) s1 += fileSeparator
    val s2 = path2.toString
    if (s2.startsWith(s1)) RelativePath(s2.substring(s1.length))
    else throw new IllegalArgumentException(s"$path2 is not relative to $path1")
  }
}
