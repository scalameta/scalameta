package org.langmeta.internal.io

import java.net.URI
import java.nio.charset.Charset
import org.langmeta.highlevel.io._

object FileIO {

  def readAllBytes(path: AbsolutePath): Array[Byte] =
    PlatformFileIO.readAllBytes(path)

  def readAllBytes(uri: URI): Array[Byte] =
    PlatformFileIO.readAllBytes(uri)

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

}

