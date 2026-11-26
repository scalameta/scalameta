package scala.meta.internal.io

import scala.meta.io._

import java.io.{ByteArrayOutputStream, IOException}
import java.net.URI
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.charset.Charset
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util

import scala.concurrent.{ExecutionContextExecutorService, Future, Promise}
import scala.io.Codec

object PlatformFileIO extends NioFileIO {

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

  def slurp(path: AbsolutePath, charset: Charset): String = readFile(path.toNIO, charset)

  def listFiles(path: AbsolutePath): ListFiles =
    new ListFiles(path, Option(path.toFile.list()).toList.flatten.map(RelativePath.apply))

  def isFile(path: AbsolutePath): Boolean = isRegularFile(path.toNIO)

  def isDirectory(path: AbsolutePath): Boolean = isDirectory(path.toNIO)

  def exists(path: AbsolutePath): Boolean = exists(path.toNIO)

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

  def newFileSystem(uri: URI, map: java.util.Map[String, String] = new util.HashMap()): FileSystem =
    try FileSystems.newFileSystem(uri, map)
    catch { case _: FileSystemAlreadyExistsException => FileSystems.getFileSystem(uri) }

  def readFileAsync(path: Path, ec: ExecutionContextExecutorService)(implicit
      codec: Codec,
  ): Future[String] = Future {
    val opts = new util.HashSet(util.Arrays.asList(StandardOpenOption.READ))
    AsynchronousFileChannel.open(path, opts, ec)
  }(ec).flatMap { channel =>
    val promise = Promise[String]()
    new ReadContext(promise, channel).read(0)
    promise.future
  }(ExecutionContexts.parasitic)

  def writeFileAsync(path: Path, content: String, ec: ExecutionContextExecutorService)(implicit
      codec: Codec,
  ): Future[Unit] = Future {
    val opts = new util.HashSet(util.Arrays.asList(
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE,
      StandardOpenOption.TRUNCATE_EXISTING,
    ))
    AsynchronousFileChannel.open(path, opts, ec)
  }(ec).flatMap { channel =>
    val promise = Promise[Unit]()
    val buf = ByteBuffer.wrap(content.getBytes(codec.charSet))
    new WriteContext(promise, channel, buf).write()
    promise.future
  }(ExecutionContexts.parasitic)

  private abstract class Handler[A <: Context[_]] extends CompletionHandler[Integer, A] {
    override def failed(exc: Throwable, obj: A): Unit = obj.failed(exc)
  }

  private object ReadHandler extends Handler[ReadContext] {
    override def completed(res: Integer, obj: ReadContext): Unit = obj.read(res.intValue())
  }

  private object WriteHandler extends Handler[WriteContext] {
    override def completed(res: Integer, obj: WriteContext): Unit = obj.write()
  }

  private abstract class Context[A] {
    val promise: Promise[A]
    val channel: AsynchronousFileChannel
    def failed(exc: Throwable): Unit = {
      promise.tryFailure(exc)
      channel.close()
    }
  }

  private class ReadContext(val promise: Promise[String], val channel: AsynchronousFileChannel)(
      implicit codec: Codec,
  ) extends Context[String] {
    private val arr = new Array[Byte](1024)
    private val buf = ByteBuffer.wrap(arr)
    private val os = new ByteArrayOutputStream()

    def read(count: Int): Unit =
      if (count < 0) {
        promise.trySuccess(os.toString(codec.charSet.name()))
        channel.close()
      } else {
        if (count > 0) {
          os.write(arr, 0, count)
          buf.clear()
        }
        try channel.read(buf, os.size(), this, ReadHandler)
        catch { case exc: Throwable => failed(exc) }
      }
  }

  private class WriteContext(
      val promise: Promise[Unit],
      val channel: AsynchronousFileChannel,
      buf: ByteBuffer,
  ) extends Context[Unit] {
    def write(): Unit =
      if (buf.hasRemaining)
        try channel.write(buf, buf.position(), this, WriteHandler)
        catch { case exc: Throwable => failed(exc) }
      else {
        promise.trySuccess(())
        channel.close()
      }
  }

}
