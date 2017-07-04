package scala.meta.internal.io

import java.io.InputStream
import java.net.URI
import scala.meta.io._
import java.nio.charset.Charset

object FileIO {

  def newInputStream(uri: URI): InputStream =
    PlatformFileIO.newInputStream(uri)

  def readAllBytes(path: AbsolutePath): Array[Byte] =
    PlatformFileIO.readAllBytes(path)

  def readAllBytes(uri: URI): Array[Byte] =
    InputStreamIO.readBytes(newInputStream(uri))

  def slurp(path: AbsolutePath, charset: Charset): String =
    PlatformFileIO.slurp(path, charset)

  def slurp(path: AbsolutePath): String =
    slurp(path, Charset.forName("UTF-8"))

  def listFiles(path: AbsolutePath): ListFiles =
    PlatformFileIO.listFiles(path)

  def isFile(path: AbsolutePath): Boolean =
    PlatformFileIO.isFile(path)

  def isDirectory(path: AbsolutePath): Boolean =
    PlatformFileIO.isDirectory(path)

  def listAllFilesRecursively(path: AbsolutePath): ListFiles =
    PlatformFileIO.listAllFilesRecursively(path)

}

