package java.nio.file

import java.io.File
import java.io.NodeJSFile
import java.net.URI
import scala.meta.internal.io.JSPath
import scala.meta.internal.io.PathIO

trait Path {
  def isAbsolute: Boolean
  def getRoot: Path
  def getFileName: Path
  def getParent: Path
  def getNameCount: Int
  def getName(index: Int): Path
  def subpath(beginIndex: Int, endIndex: Int): Path
  def startsWith(other: Path): Boolean
  def startsWith(other: String): Boolean
  def endsWith(other: Path): Boolean
  def endsWith(other: String): Boolean
  def normalize: Path
  def resolve(other: Path): Path
  def resolve(other: String): Path
  def resolveSibling(other: Path): Path
  def resolveSibling(other: String): Path
  def relativize(other: Path): Path
  def toUri: URI
  def toAbsolutePath: Path
  def toFile: java.io.File
}

case class NodeJSPath(filename: String) extends Path {
  override def subpath(beginIndex: Int, endIndex: Int): Path =
    NodeJSPath(filename.split(PathIO.fileSeparator).slice(beginIndex, endIndex).mkString)
  override def toFile: File =
    NodeJSFile(filename)
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
  override def toUri: URI =
    new URI("file", null, filename, null)
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
