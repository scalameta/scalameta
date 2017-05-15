package scala.meta.internal.io

import java.nio.charset.Charset
import scala.meta.io._

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes

object PlatformFileIO {
  def slurp(path: AbsolutePath, charset: Charset): String =
    scala.io.Source.fromFile(path.toFile)(scala.io.Codec(charset)).mkString

  def listFiles(path: AbsolutePath): ListFiles =
    new ListFiles(path, path.toFile.list().map(RelativePath.apply))

  def isFile(path: AbsolutePath): Boolean =
    path.toFile.isFile

  def isDirectory(path: AbsolutePath): Boolean =
    path.toFile.isDirectory

  def walk(root: AbsolutePath, walker: FileWalker): ListFiles = {
    val builder = List.newBuilder[RelativePath]
    Files.walkFileTree(root.toNIO, new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        val abspath = AbsolutePath(file.toAbsolutePath)
        builder += abspath.toRelative(root)
        if (walker.skip(abspath)) FileVisitResult.SKIP_SUBTREE
        else FileVisitResult.CONTINUE
      }
    })
    new ListFiles(root, builder.result())
  }
}
