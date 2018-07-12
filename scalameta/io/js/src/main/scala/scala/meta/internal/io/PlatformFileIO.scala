package scala.meta.internal.io

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.net.URI
import java.nio.charset.Charset
import java.nio.file.Paths
import scala.scalajs.js.JSConverters._
import scalapb.GeneratedMessage
import scala.meta.internal.semanticdb._
import scala.meta.internal.io._
import scala.meta.io._

object PlatformFileIO {
  def newInputStream(uri: URI): InputStream =
    new ByteArrayInputStream(readAllBytes(uri))

  def readAllBytes(uri: URI): Array[Byte] =
    if (uri.getScheme == "file") {
      val filepath = Paths.get(uri)
      readAllBytes(AbsolutePath(filepath.toString))
    } else {
      throw new UnsupportedOperationException(s"Can't read $uri as InputStream")
    }

  def readAllBytes(path: AbsolutePath): Array[Byte] = JSIO.inNode {
    val jsArray = JSIO.fs.readFileSync(path.toString)
    val len = jsArray.length
    val result = new Array[Byte](len)
    var curr = 0
    while (curr < len) {
      result(curr) = jsArray(curr).toByte
      curr += 1
    }
    result
  }

  def readAllDocuments(path: AbsolutePath): Seq[TextDocument] = JSIO.inNode {
    val bytes = readAllBytes(path)
    TextDocuments.parseFrom(bytes).documents
  }

  def write(path: AbsolutePath, proto: GeneratedMessage): Unit = JSIO.inNode {
    JSIO.fs.mkdirSync(path.toNIO.getParent.toString)
    val os = new ByteArrayOutputStream
    proto.writeTo(os)
    val buffer = os.toByteArray.map(_.toInt).toJSArray
    JSIO.fs.writeFileSync(path.toString, buffer)
  }

  def slurp(path: AbsolutePath, charset: Charset): String =
    JSIO.inNode(JSIO.fs.readFileSync(path.toString, charset.toString))

  def listFiles(path: AbsolutePath): ListFiles = JSIO.inNode {
    if (path.isFile) new ListFiles(path, Nil)
    else {
      val jsArray = JSIO.fs.readdirSync(path.toString)
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

  def isFile(path: AbsolutePath): Boolean =
    JSIO.isFile(path.toString)

  def isDirectory(path: AbsolutePath): Boolean =
    JSIO.isDirectory(path.toString)

  def listAllFilesRecursively(root: AbsolutePath): ListFiles = {
    val builder = List.newBuilder[RelativePath]
    def loop(path: AbsolutePath): Unit = {
      if (path.isDirectory) listFiles(path).foreach(loop)
      else builder += path.toRelative(root)
    }
    loop(root)
    new ListFiles(root, builder.result())
  }

  def jarRootPath(jarFile: AbsolutePath): AbsolutePath =
    throw new UnsupportedOperationException("Can't expand jar file in Scala.js")

  def withJarFileSystem[T](path: AbsolutePath, create: Boolean, close: Boolean = false)(
      f: AbsolutePath => T): T =
    throw new UnsupportedOperationException("Can't expand jar file in Scala.js")
}
