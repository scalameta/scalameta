package scala.meta.internal.io

import java.net.URI
import java.nio.charset.Charset
import java.nio.file.FileVisitOption
import scala.meta.io._
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.SimpleFileVisitor

object PlatformFileIO {
  def readAllBytes(uri: URI): Array[Byte] =
    InputStreamIO.readBytes(uri.toURL.openStream())

  def readAllBytes(path: AbsolutePath): Array[Byte] =
    Files.readAllBytes(path.toNIO)

  def slurp(path: AbsolutePath, charset: Charset): String =
    scala.io.Source.fromFile(path.toFile)(scala.io.Codec(charset)).mkString

  def listFiles(path: AbsolutePath): ListFiles =
    new ListFiles(path, Option(path.toFile.list()).toList.flatten.map(RelativePath.apply))

  def isFile(path: AbsolutePath): Boolean =
    Files.isRegularFile(path.path)

  def isDirectory(path: AbsolutePath): Boolean =
    Files.isDirectory(path.path)

  def listAllFilesRecursively(root: AbsolutePath): ListFiles = {
    import scala.collection.JavaConverters._
    val relativeFiles = Files
      .walk(root.toNIO)
      .iterator()
      .asScala
      .collect {
        case path if Files.isRegularFile(path) =>
          RelativePath(root.path.relativize(path))
      }
      .toSeq
    new ListFiles(root, relativeFiles)
  }
}
