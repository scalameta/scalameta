package star.meta
package io

import java.io._
import java.nio.{file => nio}
import java.net._
import java.nio.file.Path
import java.nio.file.Paths
import star.meta.internal.io.PlatformPathIO
import star.meta.internal.io.FileIO
import star.meta.internal.io.PathIO

/** Wrapper around an absolute nio.Path. */
sealed abstract case class AbsolutePath(toNIO: nio.Path) {
  require(toNIO.isAbsolute, s"$toNIO is not absolute!")
  def toFile: File = toNIO.toFile
  def toURI: URI = toNIO.toUri

  def syntax: String = toString
  def structure: String = s"""AbsolutePath("$syntax")"""
  override def toString: String = toNIO.toString

  def toRelative: RelativePath = toRelative(PathIO.workingDirectory)
  def toRelative(prefix: AbsolutePath): RelativePath = RelativePath(prefix.toNIO.relativize(toNIO))

  def resolve(other: RelativePath): AbsolutePath = AbsolutePath(toNIO.resolve(other.toNIO))(this)
  def resolve(other: String): AbsolutePath = AbsolutePath(toNIO.resolve(other))(this)

  def isFile: Boolean = FileIO.isFile(this)
  def isDirectory: Boolean = FileIO.isDirectory(this)
  def readAllBytes: Array[Byte] = FileIO.readAllBytes(this)
}

object AbsolutePath {
  lazy val root = new AbsolutePath(Paths.get("").toAbsolutePath.getRoot) {}
  // java.{io,nio} implicitly assume sys.props("user.dir") as the working directory.
  // This assumption does not hold for JS runtimes.
  implicit def workingDirectory: AbsolutePath =
    new AbsolutePath(Paths.get(PlatformPathIO.workingDirectoryString)) {}
  // Use working directory as cwd, that's the default behavior of java.io.File.
  def apply(file: File)(implicit cwd: AbsolutePath): AbsolutePath = apply(file.toPath)(cwd)
  def apply(path: String)(implicit cwd: AbsolutePath): AbsolutePath = apply(Paths.get(path))(cwd)
  def apply(path: Path)(implicit cwd: AbsolutePath): AbsolutePath =
    if (path.isAbsolute) {
      new AbsolutePath(path) {}
    } else {
      cwd.resolve(path.toString)
    }
}
