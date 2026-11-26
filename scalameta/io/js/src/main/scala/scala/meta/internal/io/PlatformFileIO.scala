package scala.meta.internal.io

import scala.meta.io._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.net.URI
import java.nio.charset.Charset
import java.nio.file.{Path, Paths}

import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Codec
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.util.Try
import scala.util.control.NonFatal

object PlatformFileIO {

  import FileIO.{WalkVisit, WalkVisitor}

  private class JSFileStat(obj: JSStats) extends FileStat {
    def isDirectory: Boolean = obj.isDirectory()
    def isRegularFile: Boolean = obj.isFile()
    def isSymlink: Boolean = obj.isSymbolicLink()
  }

  def newInputStream(uri: URI): InputStream = new ByteArrayInputStream(readAllBytes(uri))

  def readAllBytes(uri: URI): Array[Byte] =
    if (uri.getScheme == "file") {
      val filepath = Paths.get(uri)
      readAllBytes(AbsolutePath(filepath.toString))
    } else throw new UnsupportedOperationException(s"Can't read $uri as InputStream")

  def readAllBytes(path: AbsolutePath): Array[Byte] = JSIO.inNode {
    val jsArray = JSFs.readFileSync(path.toString)
    val len = jsArray.length
    val result = new Array[Byte](len)
    var curr = 0
    while (curr < len) {
      result(curr) = jsArray(curr).toByte
      curr += 1
    }
    result
  }

  def read[A](path: AbsolutePath)(implicit isio: InputStreamIO[A]): A = JSIO.inNode {
    val bytes = readAllBytes(path)
    isio.read(bytes)
  }

  def write[A](path: AbsolutePath, msg: A)(implicit osio: OutputStreamIO[A]): Unit = JSIO.inNode {
    JSFs.mkdirSync(path.toNIO.getParent.toString)
    val os = new ByteArrayOutputStream
    osio.write(msg, os)
    val buffer = os.toByteArray.map(_.toInt).toJSArray
    JSFs.writeFileSync(path.toString, buffer)
  }

  def slurp(path: AbsolutePath, charset: Charset): String = JSIO.inNode(readFile(path.toNIO, charset))

  def readFile(path: Path, charset: Charset): String = JSFs.readFileSync(path.toString, charset.name)

  def writeFile(path: Path, content: String, charset: Charset): Unit = JSFs
    .writeFileSync(path.toString, content, charset.name)

  def readStdinAsync: Future[String] = JSIO.readStdinAsync

  def readFileAsync(path: Path)(implicit codec: Codec): Future[String] = JSIO
    .readFileAsync(path.toString, codec.name)

  def writeFileAsync(path: Path, content: String)(implicit codec: Codec): Future[Unit] = JSIO
    .writeFileAsync(path.toString, content, codec.name)

  def listFiles(path: AbsolutePath): ListFiles = JSIO.inNode(
    if (path.isFile) ListFiles(path, Nil)
    else {
      val jsArray = JSFs.readdirSync(path.toString)
      val builder = List.newBuilder[RelativePath]
      builder.sizeHint(jsArray.length)
      var curr = 0
      while (curr < jsArray.length) {
        builder += RelativePath(jsArray(curr))
        curr += 1
      }
      ListFiles(path, builder.result())
    },
  )

  def isFile(path: AbsolutePath): Boolean = JSIO.isFile(path.toString)

  def isDirectory(path: AbsolutePath): Boolean = JSIO.isDirectory(path.toString)
  def isDirectory(path: Path): Boolean = FileIO.getFileStat(path, followLinks = false)
    .exists(_.isDirectory)

  def exists(path: AbsolutePath): Boolean = JSIO.exists(path.toString)
  def exists(path: String): Boolean = JSFs.existsSync(path)
  def exists(path: Path): Boolean = exists(path.toString)

  def isRegularFile(path: Path): Boolean = FileIO.getFileStat(path, followLinks = true)
    .exists(_.isRegularFile)
  def isRegularFileNoLinks(path: Path): Boolean = FileIO.getFileStat(path, followLinks = false)
    .exists(_.isRegularFile)

  def tryGetFileStat(path: Path, followLinks: Boolean): Try[FileStat] =
    Try(new JSFileStat(JSIO.stat(path.toString, followLinks)))

  def symlink(link: Path, target: Path): Unit = JSFs.symlinkSync(target.toString, link.toString)

  def mkdir(dir: String, recursive: Boolean): Unit = JSFs
    .mkdirSync(dir, js.Dynamic.literal(recursive = recursive))
  def mkdir(path: Path): Unit = mkdir(path.toString, recursive = false)
  def mkdirs(path: Path): Unit = mkdir(path.toString, recursive = true)
  def mkdtemp(prefix: String): Path = Paths
    .get(JSFs.mkdtempSync(Paths.get(JSOS.tmpdir()).resolve(prefix).toString))

  def move(src: Path, dst: Path): Unit = JSFs.renameSync(src.toString, dst.toString)

  def delete(path: String): Unit = JSFs.unlinkSync(path)
  def delete(path: Path): Unit = delete(path.toString)
  def rmdir(path: String, recursive: Boolean, force: Boolean): Unit = JSFs
    .rmSync(path, js.Dynamic.literal(recursive = recursive, force = force))

  def listAllFilesRecursively(root: AbsolutePath): ListFiles = {
    val builder = List.newBuilder[RelativePath]
    val rootPath = root.toNIO
    walkFiles(new WalkVisitor {
      override def onFile(file: Path, fileStat: FileStat): WalkVisit = {
        builder += RelativePath(rootPath.relativize(file))
        WalkVisit.Skip
      }
      override def onFailStop(file: Path, exc: Throwable): Boolean =
        if (JSIO.isDirectory(file.toString)) throw exc
        else {
          builder += RelativePath(rootPath.relativize(file))
          false
        }
    })(rootPath)
    ListFiles(root, builder.result())
  }

  def findFiles(path: Path, matches: (Path, FileStat) => Boolean): Seq[Path] = walkFiles(
    new WalkVisitor {
      override def onFile(file: Path, fileStat: FileStat): WalkVisit =
        if (matches(file, fileStat)) WalkVisit.Good else WalkVisit.Skip
    },
  )(path)

  def walkFiles(visitor: WalkVisitor)(basePath: Path): Seq[Path] = {
    val res = Seq.newBuilder[Path]
    val paths = mutable.ArrayBuffer[Path](basePath)
    while (paths.nonEmpty) {
      val path = paths.remove(paths.length - 1)
      val filename = path.toString
      val stop =
        try {
          val fileStat = new JSFileStat(JSIO.stat(filename, followLinks = false))
          if (fileStat.isDirectory) visitor.onTree(path, fileStat) match {
            case WalkVisit.Stop => true
            case WalkVisit.Skip => false
            case WalkVisit.Good =>
              // push in reverse, to visit the entries in the order of `readdir`
              val entries = JSFs.readdirSync(filename)
              var idx = entries.length
              while (idx > 0) {
                idx -= 1
                paths += path.resolve(entries(idx))
              }
              false
          }
          else visitor.onFile(path, fileStat) match {
            case WalkVisit.Stop => true
            case WalkVisit.Skip => false
            case WalkVisit.Good =>
              res += path
              false
          }
        } catch { case NonFatal(exc) => visitor.onFailStop(path, exc) }
      if (stop) paths.clear()
    }
    res.result()
  }

  def jarRootPath(jarFile: AbsolutePath): AbsolutePath =
    throw new UnsupportedOperationException("Can't expand jar file in Scala.js")

  def withJarFileSystem[T](path: AbsolutePath, create: Boolean, close: Boolean = false)(
      f: AbsolutePath => T,
  ): T = throw new UnsupportedOperationException("Can't expand jar file in Scala.js")

}
