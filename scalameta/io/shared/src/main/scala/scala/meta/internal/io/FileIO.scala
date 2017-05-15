package scala.meta.internal.io

import scala.meta.io._

import java.nio.charset.Charset

object FileIO {
  def readAllBytes(path: AbsolutePath): Array[Byte] =
    PlatformFileIO.readAllBytes(path)

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

  def walk(path: AbsolutePath, walker: FileWalker): ListFiles =
    PlatformFileIO.walk(path, walker)
}
