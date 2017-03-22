package scala.meta.io

import java.io.InputStream
import java.nio.charset.Charset

object PlatformIO {
  def workingDirectory: AbsolutePath = AbsolutePath(fileSeparator)
  def read(stream: InputStream, charset: Charset): String =
    throw new IllegalStateException("Reading from java.io.InputStream is not supported in JavaScript.")
  def read(path: AbsolutePath, charset: Charset): String = read(path)
  def read(path: AbsolutePath): String =
    throw new IllegalStateException("Reading from an AbsoluteFile is not supported in JavaScript.")
  def fileSeparator: String = "/"
  def isAbsolutePath(path: String): Boolean = path.startsWith(fileSeparator)
}
