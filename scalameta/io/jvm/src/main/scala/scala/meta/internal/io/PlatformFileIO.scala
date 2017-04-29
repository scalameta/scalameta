package scala.meta.internal.io

import java.nio.charset.Charset
import scala.meta.io._

object PlatformFileIO {
  def slurp(path: AbsolutePath, charset: Charset): String =
    scala.io.Source.fromFile(path.toFile)(scala.io.Codec(charset)).mkString
}
