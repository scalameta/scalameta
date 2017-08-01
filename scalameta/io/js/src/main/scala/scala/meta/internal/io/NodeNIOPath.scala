package scala.meta.internal.io

import java.io.File
import java.net.URI
import java.nio.file.Path

// Rough implementation of java.nio.Path, should work similarly for the happy
// path but has undefined behavior for error handling.
case class NodeNIOPath(filename: String) extends Path {
  private def adjustIndex(idx: Int): Int =
    if (isAbsolute) idx + 1 else idx
  override def subpath(beginIndex: Int, endIndex: Int): Path =
    NodeNIOPath(
      filename
        .split(File.separator)
        .slice(adjustIndex(beginIndex), adjustIndex(endIndex))
        .mkString)
  override def toFile: File =
    new File(filename)
  override def isAbsolute: Boolean =
    if (JSIO.isNode) JSPath.isAbsolute(filename)
    else filename.startsWith(File.separator)
  override def getName(index: Int): Path =
    NodeNIOPath(
      filename
        .split(File.separator)
        .lift(adjustIndex(index))
        .getOrElse(throw new IllegalArgumentException))
  override def getParent: Path =
    NodeNIOPath(JSPath.dirname(filename))
  override def toAbsolutePath: Path =
    if (isAbsolute) this
    else NodeNIOPath.workingDirectory.resolve(this)
  override def relativize(other: Path): Path =
    NodeNIOPath(JSPath.relative(filename, other.toString))
  override def getNameCount: Int =
    filename.count(_ == File.separatorChar)
  override def toUri: URI = toFile.toURI
  override def getFileName: Path =
    NodeNIOPath(JSPath.basename(filename))
  override def getRoot: Path =
    if (!isAbsolute) null
    else NodeNIOPath(File.separator)
  override def normalize(): Path =
    if (JSIO.isNode) NodeNIOPath(JSPath.normalize(filename))
    else this
  override def endsWith(other: Path): Boolean =
    endsWith(other.toString)
  override def endsWith(other: String): Boolean =
    paths(filename).endsWith(paths(other))
  // JSPath.resolve(relpath, relpath) produces an absolute path from cwd.
  // This method turns the generated absolute path back into a relative path.
  private def adjustResolvedPath(resolved: Path): Path =
    if (isAbsolute) resolved
    else NodeNIOPath.workingDirectory.relativize(resolved)
  override def resolveSibling(other: Path): Path =
    resolveSibling(other.toString)
  override def resolveSibling(other: String): Path =
    adjustResolvedPath(NodeNIOPath(JSPath.resolve(JSPath.dirname(filename), other)))
  override def resolve(other: Path): Path =
    resolve(other.toString)
  override def resolve(other: String): Path =
    adjustResolvedPath(NodeNIOPath(JSPath.resolve(filename, other)))
  override def startsWith(other: Path): Boolean =
    startsWith(other.toString)
  override def startsWith(other: String): Boolean =
    paths(filename).startsWith(paths(other))
  private def paths(name: String) =
    name.split(File.separator)
  override def toString: String =
    filename
}

object NodeNIOPath {
  def workingDirectory = NodeNIOPath(PlatformPathIO.workingDirectoryString)
}
