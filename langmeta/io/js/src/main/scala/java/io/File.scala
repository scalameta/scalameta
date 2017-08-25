package java.io

import java.net.URI
import java.nio.file.Path
import org.langmeta.internal.io._

// obtained implementation by experimentation on the JDK.
class File(path: String) {
  def this(parent: String, child: String) =
    this(parent + File.separator + child)
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
    NodeNIOPath(path)
  def toURI: URI = {
    val file = getAbsoluteFile.toString
    val path =
      if (isDirectory && !file.endsWith("/")) file + "/"
      else file
    new URI("file", null, path, null)
  }
  def getAbsoluteFile: File =
    toPath.toAbsolutePath.toFile
  def getAbsolutePath: String =
    getAbsoluteFile.toString
  def getPath: String =
    path
  def exists(): Boolean =
    JSIO.exists(path)
  def isFile: Boolean =
    JSIO.isFile(path)
  def isDirectory: Boolean =
    JSIO.isDirectory(path)
  override def toString: String =
    path
}

object File {
  def listRoots(): Array[File] = Array(
    new File(
      if (JSIO.isNode) JSIO.path.parse(JSIO.path.resolve()).root
      else "/"
    )
  )

  def separatorChar: Char =
    separator.charAt(0)

  def separator: String =
    if (JSIO.isNode) JSIO.path.sep
    else "/"

  def pathSeparator: String =
    if (JSIO.isNode) JSIO.path.delimiter
    else ":"
}
