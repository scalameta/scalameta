package scala.meta.internal.io

import java.io.File
import java.net.URI
import java.nio.file.Path
import java.util
import scala.collection.JavaConverters._

// Rough implementation of java.nio.Path, should work similarly for the happy
// path but has undefined behavior for error handling.
case class NodeNIOPath(filename: String) extends Path {
  private[this] val escapedSeparator = java.util.regex.Pattern.quote(File.separator)

  private def adjustIndex(idx: Int): Int =
    if (isAbsolute) idx + 1 else idx
  override def subpath(beginIndex: Int, endIndex: Int): Path =
    NodeNIOPath(
      filename
        .split(escapedSeparator)
        .slice(adjustIndex(beginIndex), adjustIndex(endIndex))
        .mkString)
  override def toFile: File =
    new File(filename)
  override def isAbsolute: Boolean =
    if (JSIO.isNode) JSIO.path.isAbsolute(filename)
    else filename.startsWith(File.separator)
  override def getName(index: Int): Path =
    NodeNIOPath(
      filename
        .split(escapedSeparator)
        .lift(adjustIndex(index))
        .getOrElse(throw new IllegalArgumentException))
  override def getParent: Path =
    NodeNIOPath(JSIO.path.dirname(filename))
  override def toAbsolutePath: Path =
    if (isAbsolute) this
    else NodeNIOPath.workingDirectory.resolve(this)
  override def relativize(other: Path): Path =
    NodeNIOPath(JSIO.path.relative(filename, other.toString))
  override def getNameCount: Int = {
    val strippeddrive =
      if ((filename.length > 1) && (filename(1) == ':')) filename.substring(2) else filename
    val (first, remaining) = strippeddrive.split(escapedSeparator + "+").span(_.isEmpty)
    if (remaining.isEmpty) first.length
    else remaining.length
  }
  override def toUri: URI = toFile.toURI
  override def getFileName: Path =
    NodeNIOPath(JSIO.path.basename(filename))
  override def getRoot: Path =
    if (!isAbsolute) null
    else NodeNIOPath(File.separator)
  override def normalize(): Path =
    if (JSIO.isNode) NodeNIOPath(JSIO.path.normalize(filename))
    else this
  override def endsWith(other: Path): Boolean =
    endsWith(other.toString)
  override def endsWith(other: String): Boolean =
    paths(filename).endsWith(paths(other))
  // JSIO.path.resolve(relpath, relpath) produces an absolute path from cwd.
  // This method turns the generated absolute path back into a relative path.
  private def adjustResolvedPath(resolved: Path): Path =
    if (isAbsolute) resolved
    else NodeNIOPath.workingDirectory.relativize(resolved)
  override def resolveSibling(other: Path): Path =
    resolveSibling(other.toString)
  override def resolveSibling(other: String): Path =
    adjustResolvedPath(NodeNIOPath(JSIO.path.resolve(JSIO.path.dirname(filename), other)))
  override def resolve(other: Path): Path =
    resolve(other.toString)
  override def resolve(other: String): Path =
    adjustResolvedPath(NodeNIOPath(JSIO.path.resolve(filename, other)))
  override def startsWith(other: Path): Boolean =
    startsWith(other.toString)
  override def startsWith(other: String): Boolean =
    paths(filename).startsWith(paths(other))
  private def paths(name: String) =
    name.split(escapedSeparator)
  override def toString: String =
    filename
  override def iterator(): util.Iterator[Path] =
    filename.split(File.separator).iterator.map(name => NodeNIOPath(name): Path).asJava
}

object NodeNIOPath {
  def workingDirectory = NodeNIOPath(PlatformPathIO.workingDirectoryString)
}
