package scala.meta.internal.io

import java.nio.charset.Charset
import scala.meta.io._

object PlatformIO {
  def fileSeparator: String =
    "/"

  def pathSeparator: String =
    ":"

  def workingDirectory: AbsolutePath =
    AbsolutePath("/")

  def isAbsolutePath(path: String): Boolean =
    path.startsWith("/")

  def isRelativePath(path: String): Boolean =
    !path.startsWith("/")

  def absolutize(parent: AbsolutePath, path: RelativePath): AbsolutePath = {
    AbsolutePath(parent.toString + fileSeparator + path.toString)
  }

  def relativize(parent: AbsolutePath, path: AbsolutePath): RelativePath = {
    if (parent == path) return RelativePath(".")
    var s_parent = parent.toString
    if (!s_parent.endsWith(fileSeparator)) s_parent += fileSeparator
    val s_path = path.toString
    if (s_path.startsWith(s_parent)) RelativePath(s_path.substring(s_parent.length))
    else throw new IllegalArgumentException(s"$path is not relative to $parent")
  }

  def slurp(path: AbsolutePath, charset: Charset): String =
    throw new UnsupportedOperationException("AbsolutePath.slurp is not supported in JavaScript.")

  def slurp(path: AbsolutePath): String =
    throw new UnsupportedOperationException("AbsolutePath.slurp is not supported in JavaScript.")
}
