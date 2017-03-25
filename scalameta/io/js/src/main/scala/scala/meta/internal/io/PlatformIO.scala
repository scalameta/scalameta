package scala.meta.internal.io

import java.nio.charset.Charset
import scala.meta.io._

object PlatformIO {
  def workingDirectory: AbsolutePath = AbsolutePath(fileSeparator).get
  def slurp(path: AbsolutePath, charset: Charset): String = slurp(path)
  def slurp(path: AbsolutePath): String =
    throw new IllegalStateException("Slurping from an AbsolutePath is not supported in JavaScript.")
  def fileSeparator: String = "/"
  def isAbsolutePath(path: String): Boolean = path.startsWith(fileSeparator)
}
