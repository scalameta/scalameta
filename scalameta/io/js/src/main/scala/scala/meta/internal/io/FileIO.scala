package scala.meta.internal.io

import java.nio.charset.Charset
import scala.meta.io._

object FileIO {
  def slurp(path: AbsolutePath, charset: Charset): String =
    throw new UnsupportedOperationException("AbsolutePath.slurp is not supported in JavaScript.")

  def slurp(path: AbsolutePath): String =
    throw new UnsupportedOperationException("AbsolutePath.slurp is not supported in JavaScript.")
}
