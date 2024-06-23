package scala.meta.internal.io

import scala.meta.io._

import java.net.URI
import java.nio.charset.Charset

object FileIO {

  def readAllBytes(path: AbsolutePath): Array[Byte] = PlatformFileIO.readAllBytes(path)

  def readAllBytes(uri: URI): Array[Byte] = PlatformFileIO.readAllBytes(uri)

  def read[A: InputStreamIO](path: AbsolutePath): A = PlatformFileIO.read[A](path)

  def write[A: OutputStreamIO](path: AbsolutePath, msg: A): Unit = PlatformFileIO.write(path, msg)

  def slurp(path: AbsolutePath, charset: Charset): String = PlatformFileIO.slurp(path, charset)

  def listFiles(path: AbsolutePath): ListFiles = PlatformFileIO.listFiles(path)

  def isFile(path: AbsolutePath): Boolean = PlatformFileIO.isFile(path)

  def isDirectory(path: AbsolutePath): Boolean = PlatformFileIO.isDirectory(path)

  def listAllFilesRecursively(path: AbsolutePath): ListFiles = PlatformFileIO
    .listAllFilesRecursively(path)

  def jarRootPath(jarFile: AbsolutePath): AbsolutePath = PlatformFileIO.jarRootPath(jarFile)

  def withJarFileSystem[T](path: AbsolutePath, create: Boolean, close: Boolean = false)(
      f: AbsolutePath => T
  ): T = PlatformFileIO.withJarFileSystem[T](path, create, close)(f)
}
