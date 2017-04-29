package scala.meta.internal.io

import java.nio.charset.Charset
import scala.meta.io._

object PlatformFileIO {
  def slurp(path: AbsolutePath, charset: Charset): String =
    if (PlatformPathIO.isNode) JSFs.readFileSync(path.toString, charset.toString).toString
    else {
      throw new IllegalStateException(
        "Slurping from an AbsolutePath is not supported in this environment.")
    }
}
