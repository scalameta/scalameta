package scala.meta.internal.io

import scala.meta.io._

object PathIO extends CommonPathIO {
  def fileSeparator: String =
    "/"

  def pathSeparator: String =
    ":"

  def workingDirectory: AbsolutePath =
    AbsolutePath("/")

  def isAbsolutePath(path: String): Boolean =
    path.startsWith("/")

  def normalizePath(path: String): String =
    path.stripSuffix("/")

  private def resolve(s1: String, s2: String): String =
    s1.stripSuffix(fileSeparator) + fileSeparator + s2

  def resolve(path1: AbsolutePath, path2: RelativePath): AbsolutePath =
    AbsolutePath(resolve(path1.toString, path2.toString))

  def resolve(path1: RelativePath, path2: RelativePath): RelativePath =
    RelativePath(resolve(path1.toString, path2.toString))
}
