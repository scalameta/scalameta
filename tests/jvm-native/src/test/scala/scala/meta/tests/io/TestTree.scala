package scala.meta.tests.io

import scala.meta.io.AbsolutePath

import java.io.IOException
import java.nio.file._
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermissions}

/** Builds and removes a tree of files in a temporary directory. */
object TestTree {

  /** Whether the owner can still list a directory with mode `000`, as root can. */
  lazy val ignoresPermissions: Boolean = {
    val dir = mkdtemp()
    chmod(dir, "---------")
    try Files.isReadable(dir.toNIO)
    finally {
      chmod(dir, "rwx------")
      delete(dir)
    }
  }

  def mkdtemp(): AbsolutePath = AbsolutePath(Files.createTempDirectory("scalameta-"))

  def mkdir(path: AbsolutePath): Unit = Files.createDirectory(path.toNIO)

  def writeBytes(path: AbsolutePath, bytes: Array[Byte]): Unit = Files.write(path.toNIO, bytes)

  def symlink(link: AbsolutePath, target: AbsolutePath): Unit = Files
    .createSymbolicLink(link.toNIO, target.toNIO)

  /** @param permissions such as `rwxr-x---` */
  def chmod(path: AbsolutePath, permissions: String): Unit = Files
    .setPosixFilePermissions(path.toNIO, PosixFilePermissions.fromString(permissions))

  /**
   * Returns the names of the entries of a directory, in the order that the file system lists them.
   */
  def readdir(path: AbsolutePath): List[String] = path.toFile.list().toList

  def delete(root: AbsolutePath): Unit = Files.walkFileTree(
    root.toNIO,
    new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    },
  )

}
