package scala.meta.internal.io

import java.nio.file._
import java.io.File
import scala.meta.io._

object PathIO {

  def workingDirectory: AbsolutePath =
    AbsolutePath(PlatformPathIO.workingDirectoryString)

  // These two methods work on strings instead of AbsolutePath because AbsolutePath
  // with unix / slashes is non-sensical on Windows.
  def toUnix(path: String): String =
    if (File.separatorChar != '/') path.replace(File.separatorChar, '/')
    else path

  def fromUnix(path: String): String =
    if (File.separatorChar != '/') path.replace('/', File.separatorChar)
    else path

  /** Returns file extension of this path, returns empty string if path has no extension */
  def extension(path: Path): String = {
    val filename = path.getFileName.toString
    val idx = filename.lastIndexOf('.')
    if (idx == -1) ""
    else filename.substring(idx + 1)
  }

  /** Returns the parent directory of the absolute string path
    *
    * Examples:
    *
    * {{{
    *   dirname("/a/b/") == "/a/"
    *   dirname("/a/b") == "/a/"
    *   dirname("/") == "/"
    * }}}
    *
    * @param abspath a string path that matches the syntax of ZipFile entries.
    */
  def dirname(abspath: String): String = {
    val isDir = abspath.endsWith("/")
    val end =
      if (isDir) abspath.lastIndexOf('/', abspath.length - 2)
      else abspath.lastIndexOf('/')
    if (end < 0) "/"
    else abspath.substring(0, end + 1)
  }

  /** Returns the name of top-level file or directory of the absolute string path
    *
    * Examples:
    *
    * {{{
    *   basename("/a/b/") == "b"
    *   basename("/a/b") == "b"
    *   basename("/") == ""
    * }}}
    *
    * @param abspath a string path that matches the syntax of ZipFile entries.
    */
  def basename(abspath: String): String = {
    val end = abspath.lastIndexOf('/')
    val isDir = end == abspath.length - 1
    if (end < 0) abspath
    else if (!isDir) abspath.substring(end + 1)
    else {
      val start = abspath.lastIndexOf('/', end - 1)
      if (start < 0) abspath.substring(0, end)
      else abspath.substring(start + 1, end)
    }
  }

}
