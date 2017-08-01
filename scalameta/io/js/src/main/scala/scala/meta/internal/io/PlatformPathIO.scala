package scala.meta.internal.io

import scala.meta.io._
import java.io.File

object PlatformPathIO {
  def workingDirectoryString: String =
    JSIO.cwd()

  def workingDirectory: AbsolutePath =
    AbsolutePath(workingDirectoryString)

  def isAbsolutePath(path: String): Boolean =
    if (JSIO.isNode) JSPath.isAbsolute(path)
    else path.startsWith(File.separator)

  def normalizePath(path: String): String =
    if (JSIO.isNode) JSPath.normalize(path)
    else path
}
