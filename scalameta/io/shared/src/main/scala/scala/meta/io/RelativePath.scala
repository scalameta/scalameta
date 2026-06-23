package scala.meta.io

import scala.meta.internal.io.PathIO

import java.io._
import java.net._
import java.nio.file.{Path, Paths}
import java.nio.{file => nio}

/** Wrapper around a relative nio.Path. */
sealed abstract case class RelativePath(toNIO: Path) {
  require(!toNIO.isAbsolute, s"$toNIO is not relative!")
  def toFile: File = toNIO.toFile
  def toURI(isDirectory: Boolean): URI = RelativePath.toURI(toNIO, isDirectory)

  def syntax: String = toString
  def structure: String = s"""RelativePath("$syntax")"""
  override def toString: String = toNIO.toString

  def toAbsolute: AbsolutePath = PathIO.workingDirectory.resolve(this)
  def toAbsolute(root: AbsolutePath): AbsolutePath = root.resolve(this)

  def relativize(other: RelativePath): RelativePath = RelativePath(toNIO.relativize(other.toNIO))

  def resolve(other: nio.Path): RelativePath = RelativePath(toNIO.resolve(other))
  def resolve(other: RelativePath): RelativePath = resolve(other.toNIO)
  def resolve(path: String): RelativePath = resolve(Paths.get(path))
  def resolveSibling(f: String => String): RelativePath =
    RelativePath(toNIO.resolveSibling(f(toNIO.getFileName.toString)))

  /** The parent directory, or None if there is no parent segment. */
  def parentOpt: Option[RelativePath] = Option(toNIO.getParent).map(RelativePath.apply)

  /** The parent directory, or the empty relative path if there is no parent segment. */
  def parent: RelativePath = parentOpt.getOrElse(RelativePath(""))

  /** The last path segment, or "" if the path is empty. */
  def fileName: String = Option(toNIO.getFileName).fold("")(_.toString)
}

object RelativePath {
  def apply(file: File): RelativePath = apply(file.getPath)
  def apply(path: String): RelativePath = apply(Paths.get(path))

  // throws Illegal argument exception if path is not relative.
  def apply(path: nio.Path): RelativePath = new RelativePath(path) {}

  private[meta] def toURI(path: Path, isDirectory: Boolean): URI = {
    // Can't use toNIO.toUri because it produces an absolute URI.
    val sb = new StringBuilder
    val pathiter = path.iterator()
    while (pathiter.hasNext) {
      if (sb.nonEmpty) sb.append('/')
      // URI encode each part of the path individually.
      sb.append(new URI(null, null, pathiter.next().toString, null))
    }
    if (isDirectory) sb.append('/')
    URI.create(sb.toString())
  }

}
