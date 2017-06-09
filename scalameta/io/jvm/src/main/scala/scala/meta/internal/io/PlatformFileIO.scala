package scala.meta.internal.io

import java.io.InputStream
import java.net.URI
import java.nio.charset.Charset
import scala.meta.io._
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes

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
    path.toFile.isFile

  def isDirectory(path: AbsolutePath): Boolean =
    path.toFile.isDirectory

  def listAllFilesRecursively(root: AbsolutePath): ListFiles = {
    val builder = List.newBuilder[RelativePath]
    Files.walkFileTree(root.toNIO, new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        val abspath = AbsolutePath(file.toAbsolutePath)
        builder += abspath.toRelative(root)
        FileVisitResult.CONTINUE
      }
    })
    new ListFiles(root, builder.result())
  }
}
