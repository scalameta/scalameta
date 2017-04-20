package scala.meta.internal.io

import java.io.File
import java.nio.charset.Charset
import scala.meta.io._

object PlatformIO {
  def fileSeparator: String =
    File.separator

  def pathSeparator: String =
    File.pathSeparator

  def workingDirectory: AbsolutePath =
    AbsolutePath(sys.props("user.dir"))

  def isAbsolutePath(path: String): Boolean =
    new File(path).isAbsolute

  def isRelativePath(path: String): Boolean =
    !isAbsolutePath(path)

  def absolutize(parent: AbsolutePath, path: RelativePath): AbsolutePath = {
    AbsolutePath(new File(parent.toFile, path.toString))
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
    scala.io.Source.fromFile(path.toFile)(scala.io.Codec(charset)).mkString

  def slurp(path: AbsolutePath): String =
    slurp(path, Charset.forName("UTF-8"))
}
