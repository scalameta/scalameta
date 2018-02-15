package org.langmeta.internal.io

import java.net.URI
import java.nio.charset.Charset
import scalapb.GeneratedMessage
import org.langmeta.io._

object FileIO {

  def readAllBytes(path: AbsolutePath): Array[Byte] =
    PlatformFileIO.readAllBytes(path)

  def readAllBytes(uri: URI): Array[Byte] =
    PlatformFileIO.readAllBytes(uri)

  def write(path: AbsolutePath, proto: GeneratedMessage): Unit =
    PlatformFileIO.write(path, proto)

  def slurp(path: AbsolutePath, charset: Charset): String =
    PlatformFileIO.slurp(path, charset)

  def listFiles(path: AbsolutePath): ListFiles =
    PlatformFileIO.listFiles(path)

  def isFile(path: AbsolutePath): Boolean =
    PlatformFileIO.isFile(path)

  def isDirectory(path: AbsolutePath): Boolean =
    PlatformFileIO.isDirectory(path)

  def listAllFilesRecursively(path: AbsolutePath): ListFiles =
    PlatformFileIO.listAllFilesRecursively(path)

  def jarRootPath(jarFile: AbsolutePath): AbsolutePath =
    PlatformFileIO.jarRootPath(jarFile)
}

