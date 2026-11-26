package scala.meta.internal.io

import scala.meta.io._

import java.net.URI
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.io.Codec

object PlatformFileIO extends NioFileIO {

  def readAllBytes(uri: URI): Array[Byte] =
    // NOTE: URI.toURL isn't yet available in Scala Native,
    // so I had to steal the Scala.js implementation from js/.
    // Please find below the reference implementation from jvm/.
    //
    //   val is = uri.toURL.openStream()
    //   try {
    //     InputStreamIO.readBytes(is)
    //   } finally {
    //     is.close()
    //   }
    if (uri.getScheme == "file") {
      val filepath = Paths.get(uri)
      readAllBytes(AbsolutePath(filepath.toString))
    } else throw new UnsupportedOperationException(s"Can't read $uri as InputStream")

  // copy-pasted from JVM
  def write[A](path: AbsolutePath, msg: A)(implicit osio: OutputStreamIO[A]): Unit = {
    Files.createDirectories(path.toNIO.getParent)
    val os = Files.newOutputStream(path.toNIO)
    try osio.write(msg, os)
    finally os.close()
  }

  def readAllBytes(path: AbsolutePath): Array[Byte] = Files.readAllBytes(path.toNIO)

  def read[A](path: AbsolutePath)(implicit isio: InputStreamIO[A]): A = {
    val stream = Files.newInputStream(path.toNIO)
    try isio.read(stream)
    finally stream.close()
  }

  def slurp(path: AbsolutePath, charset: Charset): String = scala.io.Source
    .fromFile(path.toFile)(scala.io.Codec(charset)).mkString

  def listFiles(path: AbsolutePath): ListFiles =
    new ListFiles(path, Option(path.toFile.list()).toList.flatten.map(RelativePath.apply))

  def isFile(path: AbsolutePath): Boolean = isRegularFile(path.toNIO)

  def isDirectory(path: AbsolutePath): Boolean = isDirectory(path.toNIO)

  def exists(path: AbsolutePath): Boolean = exists(path.toNIO)

  def listAllFilesRecursively(root: AbsolutePath): ListFiles = {
    // NOTE: Some Java stream APIs aren't yet available in Scala Native,
    // so I had to steal the Scala.js implementation from js/.
    // Please find below the reference implementation from jvm/.
    //
    //   import scala.collection.JavaConverters._
    //   val relativeFiles = Files
    //     .walk(root.toNIO)
    //     .collect(Collectors.toList[Path])
    //     .asScala
    //     .collect {
    //       case path if Files.isRegularFile(path) =>
    //         RelativePath(root.toNIO.relativize(path))
    //     }
    //   new ListFiles(root, relativeFiles.toList)
    val builder = List.newBuilder[RelativePath]
    def loop(path: AbsolutePath): Unit =
      if (path.isDirectory) listFiles(path).foreach(loop) else builder += path.toRelative(root)
    loop(root)
    new ListFiles(root, builder.result())
  }
  def jarRootPath(jarFile: AbsolutePath): AbsolutePath = throw new UnsupportedOperationException()
  def withJarFileSystem[T](path: AbsolutePath, create: Boolean, close: Boolean = false)(
      f: AbsolutePath => T,
  ): T = throw new UnsupportedOperationException()

  def readFileAsync(path: Path, ec: ExecutionContextExecutorService)(implicit
      codec: Codec,
  ): Future[String] = Future(readFile(path, codec.charSet))(ec)

  def writeFileAsync(path: Path, content: String, ec: ExecutionContextExecutorService)(implicit
      codec: Codec,
  ): Future[Unit] = Future(writeFile(path, content, codec.charSet))(ec)

}
