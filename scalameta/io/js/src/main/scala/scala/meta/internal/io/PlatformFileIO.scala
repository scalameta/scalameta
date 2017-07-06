package scala.meta.internal.io

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.net.URI
import java.nio.charset.Charset
import scala.meta.io._

object PlatformFileIO {
  def newInputStream(uri: URI): InputStream =
    if (uri.getScheme == "file") new ByteArrayInputStream(readAllBytes(AbsolutePath(uri.getPath)))
    else throw new UnsupportedOperationException(s"Can't read $uri as InputStream")

  def readAllBytes(path: AbsolutePath): Array[Byte] = JSIO.inNode {
    val jsArray = JSFs.readFileSync(path.toString)
    val len = jsArray.length
    val result = new Array[Byte](len)
    var curr = 0
    while (curr < len) {
      result(curr) = jsArray(curr).toByte
      curr += 1
    }
    result
  }

  def slurp(path: AbsolutePath, charset: Charset): String =
    JSIO.inNode(JSFs.readFileSync(path.toString, charset.toString))

  def listFiles(path: AbsolutePath): ListFiles = JSIO.inNode {
    if (path.isFile) new ListFiles(path, Nil)
    else {
      val jsArray = JSFs.readdirSync(path.toString)
      val builder = List.newBuilder[RelativePath]
      builder.sizeHint(jsArray.length)
      var curr = 0
      while (curr < jsArray.length) {
        builder += RelativePath(jsArray(curr))
        curr += 1
      }
      new ListFiles(path, builder.result())
    }
  }

  def isFile(path: AbsolutePath): Boolean = JSIO.inNode {
    JSFs.lstatSync(path.toString).isFile()
  }

  def isDirectory(path: AbsolutePath): Boolean =
    JSFs.lstatSync(path.toString).isDirectory()

  def listAllFilesRecursively(root: AbsolutePath): ListFiles = {
    val builder = List.newBuilder[RelativePath]
    def loop(path: AbsolutePath): Unit = {
      if (path.isDirectory) listFiles(path).foreach(loop)
      else builder += path.toRelative(root)
    }
    loop(root)
    new ListFiles(root, builder.result())
  }
}
