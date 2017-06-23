package scala.meta.internal.io

import java.io.File
import java.net.URI
import java.nio.file.Path

case class NodeJSPath(filename: String) extends Path {
  override def subpath(beginIndex: Int, endIndex: Int): Path =
    NodeJSPath(filename.split(PathIO.fileSeparator).slice(beginIndex, endIndex).mkString)
  override def toFile: File =
    new File(filename)
  override def resolveSibling(other: Path): Path =
    resolveSibling(other.toString)
  override def resolveSibling(other: String): Path =
    NodeJSPath(JSPath.resolve(JSPath.dirname(filename), other))
  override def isAbsolute: Boolean =
    JSPath.isAbsolute(filename)
  override def getName(index: Int): Path =
    NodeJSPath(filename.split(PathIO.fileSeparator)(index))
  override def getParent: Path =
    NodeJSPath(JSPath.dirname(filename))
  override def toAbsolutePath: Path =
    if (JSPath.isAbsolute(filename)) this
    else PathIO.workingDirectory.path.resolve(this)
  override def relativize(other: Path): Path =
    NodeJSPath(JSPath.relative(filename, other.toString))
  override def getNameCount: Int =
    filename.count(_ == PathIO.fileSeparatorChar)
  override def toUri: URI =toFile.toURI
  override def getFileName: Path =
    NodeJSPath(JSPath.basename(filename))
  override def getRoot: Path =
    NodeJSPath(PathIO.fileSeparator)
  override def normalize(): Path =
    NodeJSPath(JSPath.normalize(filename))
  override def endsWith(other: Path): Boolean =
    endsWith(other.toString)
  override def endsWith(other: String): Boolean =
    filename.endsWith(other)
  override def resolve(other: Path): Path =
    resolve(other.toString)
  override def resolve(other: String): Path =
    NodeJSPath(JSPath.resolve(filename, other))
  override def startsWith(other: Path): Boolean =
    startsWith(other.toString)
  override def startsWith(other: String): Boolean =
    filename.startsWith(other)
  override def toString: String =
    filename
}
