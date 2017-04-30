package scala.meta.internal.io

import scala.meta.io._

trait CommonPathIO {
  def fileSeparator: String

  def pathSeparator: String

  def workingDirectory: AbsolutePath

  def isAbsolutePath(path: String): Boolean

  def isRelativePath(path: String): Boolean =
    !isAbsolutePath(path)

  def normalizePath(path: String): String

  def resolve(path1: AbsolutePath, path2: RelativePath): AbsolutePath

  def resolve(path1: RelativePath, path2: RelativePath): RelativePath

  def unresolve(path1: AbsolutePath, path2: AbsolutePath): RelativePath = {
    if (path1 == path2) return RelativePath(".")
    var s1 = path1.toString
    if (!s1.endsWith(fileSeparator)) s1 += fileSeparator
    val s2 = path2.toString
    if (s2.startsWith(s1)) RelativePath(s2.substring(s1.length))
    else throw new IllegalArgumentException(s"$path2 is not relative to $path1")
  }
}