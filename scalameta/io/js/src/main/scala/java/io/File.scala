package java.io

import java.net.URI
import java.nio.file.Path
import scala.meta.internal.io._

// obtained implementation by experimentation on the JDK.
class File(path: String) {
  private val filename = JSPath.normalize(path)
  def this(parent: String, child: String) =
    this(JSPath.resolve(child, parent))
  def this(parent: File, child: String) =
    this(parent.getPath, child)
  def this(uri: URI) =
    this(
      if (uri.getScheme != "file") {
        throw new IllegalArgumentException("URI scheme is not \"file\"")
      } else {
        uri.getPath
      }
    )
  def toPath: Path =
    NodeNIOPath(filename)
  def toURI: URI = {
    val file = getAbsoluteFile.toString
    val path =
      if (isDirectory && !file.endsWith("/")) file + "/"
      else file
    new URI("file", null, path, null)
  }
  def getAbsoluteFile: File =
    if (PathIO.isAbsolutePath(filename)) this
    else new File(PathIO.workingDirectory.resolve(filename).toString)
  def getAbsolutePath: String =
    getAbsoluteFile.toString
  def getPath: String =
    filename
  def exists(): Boolean =
    JSFs.existsSync(filename)
  private def lstat: JSStats = JSFs.lstatSync(filename)
  def isFile: Boolean =
    lstat.isFile()
  def isDirectory: Boolean =
    lstat.isDirectory()
  override def toString: String =
    filename
}
