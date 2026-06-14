package scala.meta.internal.io

import scala.meta.io._

import java.io.IOException
import java.net.URI
import java.nio.charset.Charset
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util

object PlatformFileIO {

  def readAllBytes(uri: URI): Array[Byte] = {
    val is = uri.toURL.openStream()
    try InputStreamIO.readBytes(is)
    finally is.close()
  }

  def readAllBytes(path: AbsolutePath): Array[Byte] = Files.readAllBytes(path.toNIO)

  def read[A](path: AbsolutePath)(implicit isio: InputStreamIO[A]): A = {
    val stream = Files.newInputStream(path.toNIO)
    try isio.read(stream)
    finally stream.close()
  }

  def write[A](path: AbsolutePath, msg: A, openOptions: OpenOption*)(implicit
      osio: OutputStreamIO[A],
  ): Unit = {
    Files.createDirectories(path.toNIO.getParent)
    val os = Files.newOutputStream(path.toNIO, openOptions: _*)
    try osio.write(msg, os)
    finally os.close()
  }

  def slurp(path: AbsolutePath, charset: Charset): String =
    new String(Files.readAllBytes(path.toNIO), charset)

  def listFiles(path: AbsolutePath): ListFiles =
    new ListFiles(path, Option(path.toFile.list()).toList.flatten.map(RelativePath.apply))

  def isFile(path: AbsolutePath): Boolean = Files.isRegularFile(path.toNIO)

  def isDirectory(path: AbsolutePath): Boolean = Files.isDirectory(path.toNIO)

  def listAllFilesRecursively(root: AbsolutePath): ListFiles = {
    val relativeFiles = List.newBuilder[RelativePath]
    // FOLLOW_LINKS so a symlink's target attributes are used (symlinked files are listed); note this
    // also traverses symlinked directories.
    Files.walkFileTree(
      root.toNIO,
      util.EnumSet.of(FileVisitOption.FOLLOW_LINKS),
      Integer.MAX_VALUE,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (attrs.isRegularFile) relativeFiles += RelativePath(root.toNIO.relativize(file))
          FileVisitResult.CONTINUE
        }
        // #1168: skip unreadable paths (AccessDeniedException); FOLLOW_LINKS can also raise
        // FileSystemLoopException on a symlink cycle, so skip that too. Any other I/O error
        // propagates, so a partial scan is never reported as complete. postVisitDirectory is left
        // to SimpleFileVisitor, which likewise rethrows a non-null exception.
        override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = exc match {
          case _: AccessDeniedException | _: FileSystemLoopException => FileVisitResult.CONTINUE
          case _ => throw exc
        }
      },
    )
    ListFiles(root, relativeFiles.result())
  }

  def jarRootPath(jarFile: AbsolutePath, create: Boolean = false): AbsolutePath = {
    val fs = newJarFileSystem(jarFile, create = create)
    AbsolutePath(fs.getPath("/"))
  }

  def withJarFileSystem[T](path: AbsolutePath, create: Boolean, close: Boolean = false)(
      f: AbsolutePath => T,
  ): T = {
    val fs = newJarFileSystem(path, create)
    val root = AbsolutePath(fs.getPath("/"))
    if (create || close)
      try f(root)
      finally fs.close()
    else
      // NOTE(olafur): We don't fs.close() because that can affect another place where `FileSystems.getFileSystems`
      // was used due to a `FileSystemAlreadyExistsException`. This leaks resources, but I see no alternative that does
      // not involve refactoring everything to java.io or global mutable state for reference counting open file systems
      // per zip file.
      f(root)
  }

  def newJarFileSystem(path: AbsolutePath, create: Boolean): FileSystem = {
    if (create && !Files.exists(path.toNIO.getParent)) Files.createDirectories(path.toNIO.getParent)
    val map = new util.HashMap[String, String]()
    if (create) map.put("create", "true")
    val uri = URI.create("jar:" + path.toNIO.toUri.toString)
    newFileSystem(uri, map)
  }

  def newFileSystem(uri: URI, map: java.util.Map[String, _] = new util.HashMap()): FileSystem =
    try FileSystems.newFileSystem(uri, map)
    catch { case _: FileSystemAlreadyExistsException => FileSystems.getFileSystem(uri) }
}
