package scala.meta.internal.io

import scala.meta.io._

import java.net.URI
import java.nio.charset.Charset
import java.nio.file.Path

import scala.concurrent.Future
import scala.io.Codec
import scala.util.Try

object FileIO {

  def readAllBytes(path: AbsolutePath): Array[Byte] = PlatformFileIO.readAllBytes(path)

  def readAllBytes(uri: URI): Array[Byte] = PlatformFileIO.readAllBytes(uri)

  def read[A: InputStreamIO](path: AbsolutePath): A = PlatformFileIO.read[A](path)

  def write[A: OutputStreamIO](path: AbsolutePath, msg: A): Unit = PlatformFileIO.write(path, msg)

  def slurp(path: AbsolutePath, charset: Charset): String = PlatformFileIO.slurp(path, charset)

  def readFile(path: Path)(implicit codec: Codec): String = PlatformFileIO
    .readFile(path, codec.charSet)

  def writeFile(path: Path, content: String)(implicit codec: Codec): Unit = PlatformFileIO
    .writeFile(path, content, codec.charSet)

  /** Returns standard input, read to its end and decoded as UTF-8. */
  def readStdinAsync: Future[String] = PlatformFileIO.readStdinAsync

  def listFiles(path: AbsolutePath): ListFiles = PlatformFileIO.listFiles(path)

  def isFile(path: AbsolutePath): Boolean = PlatformFileIO.isFile(path)

  def isDirectory(path: AbsolutePath): Boolean = PlatformFileIO.isDirectory(path)
  def isDirectory(path: Path): Boolean = PlatformFileIO.isDirectory(path)

  def exists(path: AbsolutePath): Boolean = PlatformFileIO.exists(path)
  def exists(path: Path): Boolean = PlatformFileIO.exists(path)

  /** Returns true for a regular file, or a link to one. */
  def isRegularFile(path: Path): Boolean = PlatformFileIO.isRegularFile(path)
  def isRegularFileNoLinks(path: Path): Boolean = PlatformFileIO.isRegularFileNoLinks(path)

  def tryGetFileStat(path: Path, followLinks: Boolean): Try[FileStat] = PlatformFileIO
    .tryGetFileStat(path, followLinks)
  def getFileStat(path: Path, followLinks: Boolean): Option[FileStat] =
    tryGetFileStat(path, followLinks).toOption

  def symlink(link: Path, target: Path): Unit = PlatformFileIO.symlink(link, target)

  def mkdir(path: Path): Unit = PlatformFileIO.mkdir(path)
  def mkdirs(path: Path): Unit = PlatformFileIO.mkdirs(path)
  def mkdtemp(prefix: String): Path = PlatformFileIO.mkdtemp(prefix)

  /** Moves a file or a directory, and replaces the target if it exists. */
  def move(src: Path, dst: Path): Unit = PlatformFileIO.move(src, dst)

  def delete(path: Path): Unit = PlatformFileIO.delete(path)

  def listAllFilesRecursively(path: AbsolutePath): ListFiles = PlatformFileIO
    .listAllFilesRecursively(path)

  /**
   * Visits the tree at the path in pre-order, without following links, and returns the files for
   * which the visitor returns `Good`.
   */
  def walkFiles(visitor: WalkVisitor)(path: Path): Seq[Path] = PlatformFileIO
    .walkFiles(visitor)(path)

  /**
   * Returns the entries that `matches` accepts, in the tree at the path, without following links.
   * On JS, `findFiles` does not pass a directory to `matches`.
   */
  def findFiles(matches: (Path, FileStat) => Boolean)(path: Path): Seq[Path] = PlatformFileIO
    .findFiles(path, matches)

  /** Returns the regular files in the tree at the path, without following links. */
  def findFiles(path: Path): Seq[Path] = findFiles((_, stat) => stat.isRegularFile)(path)

  def jarRootPath(jarFile: AbsolutePath): AbsolutePath = PlatformFileIO.jarRootPath(jarFile)

  def withJarFileSystem[T](path: AbsolutePath, create: Boolean, close: Boolean = false)(
      f: AbsolutePath => T,
  ): T = PlatformFileIO.withJarFileSystem[T](path, create, close)(f)

  sealed trait WalkVisit
  object WalkVisit {
    case object Good extends WalkVisit
    case object Stop extends WalkVisit
    case object Skip extends WalkVisit
  }

  abstract class WalkVisitor {
    def onTree(dir: Path, fileStat: FileStat): WalkVisit = WalkVisit.Good
    def onFile(file: Path, fileStat: FileStat): WalkVisit = WalkVisit.Good
    def onFailStop(file: Path, exc: Throwable): Boolean = throw exc
  }
  object WalkVisitor {
    val empty: WalkVisitor = new WalkVisitor {}
  }

}
