package org.langmeta.internal.io

import java.net.URI
import java.nio.charset.Charset
import java.nio.file.FileSystem
import java.nio.file.FileSystemAlreadyExistsException
import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.util
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

  def readIndex(path: AbsolutePath): Index = {
    val stream = Files.newInputStream(path.toNIO)
    try Index.parseFrom(stream)
    finally stream.close()
  }

  def write(path: AbsolutePath, proto: GeneratedMessage): Unit = {
    Files.createDirectories(path.toNIO.getParent)
    val os = Files.newOutputStream(path.toNIO)
    try proto.writeTo(os)
    finally os.close()
  }

  def slurp(path: AbsolutePath, charset: Charset): String =
    new String(Files.readAllBytes(path.toNIO), charset)

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
    val fs = newJarFileSystem(jarFile, create = false)
    AbsolutePath(fs.getPath("/"))
  }

  def withJarFileSystem[T](path: AbsolutePath, create: Boolean = true)(f: AbsolutePath => T): T = {
    val fs = newJarFileSystem(path, create)
    try f(AbsolutePath(fs.getPath("/")))
    finally fs.close()
  }

  def newJarFileSystem(path: AbsolutePath, create: Boolean): FileSystem = {
    Files.createDirectories(path.toNIO.getParent)
    val map = new util.HashMap[String, String]()
    if (create) {
      map.put("create", "true")
    }
    val uri = URI.create("jar:file:" + path.toNIO.toUri.getPath)
    newFileSystem(uri, map)
  }

  def newFileSystem(uri: URI, map: java.util.Map[String, _] = new util.HashMap()): FileSystem =
    try FileSystems.newFileSystem(uri, map)
    catch { case _: FileSystemAlreadyExistsException => FileSystems.getFileSystem(uri) }
}
