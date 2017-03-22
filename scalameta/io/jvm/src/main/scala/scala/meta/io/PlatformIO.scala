package scala.meta.io

import java.io.File
import java.io.InputStream
import java.nio.charset.Charset

object PlatformIO {
  def workingDirectory: AbsolutePath = AbsolutePath(sys.props("user.dir"))
  def read(stream: InputStream, charset: Charset): String =
    scala.io.Source.fromInputStream(stream)(scala.io.Codec(charset)).mkString
  def read(path: AbsolutePath, charset: Charset): String =
    scala.io.Source.fromFile(new File(path.absolute))(scala.io.Codec(charset)).mkString
  def read(path: AbsolutePath): String =
    read(path, Charset.forName("UTF-8"))
  def fileSeparator: String = File.separator
  def isAbsolutePath(path: String): Boolean = new File(path).isAbsolute
}
