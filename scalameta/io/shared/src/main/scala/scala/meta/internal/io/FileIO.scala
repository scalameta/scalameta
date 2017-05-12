package scala.meta.internal.io

import scala.meta.io._

import java.nio.charset.Charset

object FileIO {
  def slurp(path: AbsolutePath, charset: Charset): String =
    PlatformFileIO.slurp(path, charset)

  def slurp(path: AbsolutePath): String =
    slurp(path, Charset.forName("UTF-8"))
}
