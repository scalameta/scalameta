package org.langmeta.internal.io

import java.net.URI
import java.nio.charset.Charset
import java.nio.file.FileSystemAlreadyExistsException
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.OpenOption
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import java.util.stream.Collectors
import scalapb.GeneratedMessage
import scala.meta.internal.semanticdb3._
import org.langmeta.io._

object PlatformFileIO {

  def readAllBytes(uri: URI): Array[Byte] = {
    val is = uri.toURL.openStream()
    try {
      InputStreamIO.readBytes(is)
    } finally {
      is.close()
    }
  }

  def readAllBytes(path: AbsolutePath): Array[Byte] =
    Files.readAllBytes(path.toNIO)

  def readAllDocuments(path: AbsolutePath): Seq[TextDocument] = {
    val stream = Files.newInputStream(path.toNIO)
    try TextDocuments.parseFrom(stream).documents
    finally stream.close()
  }

  def write(path: AbsolutePath, proto: GeneratedMessage): Unit = {
    Files.createDirectories(path.toNIO.getParent)
    val os = Files.newOutputStream(path.toNIO, StandardOpenOption.APPEND, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.CREATE)
    try proto.writeTo(os)
    finally os.close()
  }

  def slurp(path: AbsolutePath, charset: Charset): String =
    scala.io.Source.fromFile(path.toFile)(scala.io.Codec(charset)).mkString

  def listFiles(path: AbsolutePath): ListFiles =
    new ListFiles(path, Option(path.toFile.list()).toList.flatten.map(RelativePath.apply))

  def isFile(path: AbsolutePath): Boolean =
    Files.isRegularFile(path.toNIO)

  def isDirectory(path: AbsolutePath): Boolean =
    Files.isDirectory(path.toNIO)

  def listAllFilesRecursively(root: AbsolutePath): ListFiles = {
    import scala.collection.JavaConverters._
    val relativeFiles = Files
      .walk(root.toNIO)
      .collect(Collectors.toList[Path])
      .asScala
      .collect {
        case path if Files.isRegularFile(path) =>
          RelativePath(root.toNIO.relativize(path))
      }
    new ListFiles(root, relativeFiles.toList)
  }

  def jarRootPath(jarFile: AbsolutePath): AbsolutePath = {
    val uri = URI.create("jar:file:" + jarFile.toNIO.toUri.getPath)
    val roo = newFileSystem(uri, new java.util.HashMap[String, Any]()).getPath("/")
    AbsolutePath(roo)
  }

  private def newFileSystem(uri: URI, map: java.util.Map[String, Any]) =
    try FileSystems.newFileSystem(uri, map)
    catch { case _: FileSystemAlreadyExistsException => FileSystems.getFileSystem(uri) }
}
