package scala.meta.internal.io

import java.nio.charset.Charset
import scala.meta.io._

object FileIO {
  def slurp(path: AbsolutePath, charset: Charset): String =
    scala.io.Source.fromFile(path.toFile)(scala.io.Codec(charset)).mkString

  def slurp(path: AbsolutePath): String =
    slurp(path, Charset.forName("UTF-8"))
}
