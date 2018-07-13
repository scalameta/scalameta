package java.nio.file

import java.net.URI
import java.lang.Iterable

// obtained interface by experimentation on the JDK.
trait Path extends Iterable[Path] {
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
  def normalize(): Path
  def resolve(other: Path): Path
  def resolve(other: String): Path
  def resolveSibling(other: Path): Path
  def resolveSibling(other: String): Path
  def relativize(other: Path): Path
  def toUri: URI
  def toAbsolutePath: Path
  def toFile: java.io.File
}
