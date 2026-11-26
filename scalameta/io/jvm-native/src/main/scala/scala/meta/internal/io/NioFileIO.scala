package scala.meta.internal.io

import java.io.IOException
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import scala.concurrent.Future
import scala.util.Try

/** The members of `PlatformFileIO` that the JVM and Native share. */
private[io] trait NioFileIO {

  import FileIO.{WalkVisit, WalkVisitor}
  import NioFileIO._

  def exists(path: Path): Boolean = Files.exists(path)

  def isDirectory(path: Path): Boolean = Files.isDirectory(path)
  def isRegularFile(path: Path): Boolean = Files.isRegularFile(path)
  def isRegularFileNoLinks(path: Path): Boolean = Files
    .isRegularFile(path, LinkOption.NOFOLLOW_LINKS)

  def tryGetFileStat(path: Path, followLinks: Boolean): Try[FileStat] = {
    val linkOpts = if (followLinks) Nil else Seq(LinkOption.NOFOLLOW_LINKS)
    Try(Files.readAttributes(path, classOf[BasicFileAttributes], linkOpts: _*))
      .map(new NioFileStat(_))
  }

  def symlink(link: Path, target: Path): Unit = Files.createSymbolicLink(link, target)

  def mkdir(path: Path): Unit = Files.createDirectory(path)
  def mkdirs(path: Path): Unit = Files.createDirectories(path)
  def mkdtemp(prefix: String): Path = Files.createTempDirectory(prefix)

  def move(src: Path, dst: Path): Unit = Files.move(src, dst, StandardCopyOption.REPLACE_EXISTING)

  def delete(path: Path): Unit = Files.delete(path)

  def readFile(path: Path, charset: Charset): String = new String(Files.readAllBytes(path), charset)

  def writeFile(path: Path, content: String, charset: Charset): Unit = Files
    .write(path, content.getBytes(charset))

  def readStdinAsync: Future[String] = Future
    .successful(InputStreamIO.readString(System.in, StandardCharsets.UTF_8))

  def walkFiles(visitor: WalkVisitor)(path: Path): Seq[Path] = {
    val res = Seq.newBuilder[Path]
    val simpleFileVisitor = new SimpleFileVisitor[Path] {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
        visitor.onTree(dir, new NioFileStat(attrs)) match {
          case WalkVisit.Stop => FileVisitResult.TERMINATE
          case WalkVisit.Skip => FileVisitResult.SKIP_SUBTREE
          case WalkVisit.Good => FileVisitResult.CONTINUE
        }
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult =
        visitor.onFile(file, new NioFileStat(attrs)) match {
          case WalkVisit.Stop => FileVisitResult.TERMINATE
          case WalkVisit.Skip => FileVisitResult.CONTINUE
          case WalkVisit.Good =>
            res += file
            FileVisitResult.CONTINUE
        }
      override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
        if (visitor.onFailStop(file, exc)) FileVisitResult.TERMINATE else FileVisitResult.CONTINUE
    }
    Files.walkFileTree(path, simpleFileVisitor)
    res.result()
  }

  def findFiles(path: Path, matches: (Path, FileStat) => Boolean): Seq[Path] = {
    val res = Seq.newBuilder[Path]
    val stream = Files.find(path, Integer.MAX_VALUE, (p, attrs) => matches(p, new NioFileStat(attrs)))
    try stream.forEach(p => res += p)
    finally stream.close()
    res.result()
  }

}

private[io] object NioFileIO {

  private class NioFileStat(obj: BasicFileAttributes) extends FileStat {
    def isDirectory: Boolean = obj.isDirectory
    def isRegularFile: Boolean = obj.isRegularFile
    def isSymlink: Boolean = obj.isSymbolicLink
  }

}
