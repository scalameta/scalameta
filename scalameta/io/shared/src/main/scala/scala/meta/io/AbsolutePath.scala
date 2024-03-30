package scala.meta.io

import scala.meta.internal.io.FileIO
import scala.meta.internal.io.PathIO
import scala.meta.internal.io.PlatformPathIO

import java.io._
import java.net._
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.{file => nio}

/** Wrapper around an absolute nio.Path. */
sealed abstract case class AbsolutePath(toNIO: nio.Path) {
  require(toNIO.isAbsolute, s"$toNIO is not absolute!")
  def toFile: File = toNIO.toFile
  def toURI: URI = toURI(Files.isDirectory(toNIO))
  def toURI(isDirectory: Boolean): URI = {
    val uri = toNIO.toUri
    if (isDirectory && uri.getPath != null && !uri.getPath.endsWith("/"))
      // If toNIO exists, toUri will return a trailing slash, otherwise it won't (at least on JDK 8).
      // This is important because URI.resolve(String) will drop the last segment of the URI's path if
      // there is not a trailing slash:
      //    scala> Paths.get("/tmp/test/doesNotExist").toUri.resolve("bar")
      //    res1: java.net.URI = file:/tmp/test/bar
      //    scala> Paths.get("/tmp/test/doesExist").toUri.resolve("bar")
      //    res2: java.net.URI = file:/tmp/test/doesExist/bar
      URI.create(uri.toString + "/")
    else uri
  }

  def syntax: String = toString
  def structure: String = s"""AbsolutePath("$syntax")"""
  override def toString: String = toNIO.toString

  def toRelative: RelativePath = toRelative(PathIO.workingDirectory)
  def toRelative(prefix: AbsolutePath): RelativePath = RelativePath(prefix.toNIO.relativize(toNIO))

  def resolve(other: RelativePath): AbsolutePath = AbsolutePath(toNIO.resolve(other.toNIO))(this)
  def resolve(other: String): AbsolutePath = AbsolutePath(toNIO.resolve(other))(this)
  def resolveSibling(f: String => String): AbsolutePath =
    AbsolutePath(toNIO.resolveSibling(f(toNIO.getFileName.toString)))

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
    if (path.isAbsolute) new AbsolutePath(path) {}
    else cwd.resolve(path.toString)
  def fromAbsoluteUri(uri: URI)(implicit cwd: AbsolutePath): AbsolutePath = {
    require(uri.isAbsolute, "This method only works on absolute URIs at present.") // Limitation of Paths.get(URI)
    apply(Paths.get(uri))(cwd)
  }
}
