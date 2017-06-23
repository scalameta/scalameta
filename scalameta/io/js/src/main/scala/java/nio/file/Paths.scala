package java.nio.file

import scala.meta.internal.io.JSPath
import scala.meta.internal.io.NodeJSPath

object Paths {
  def get(first: String, more: String*): Path =
    NodeJSPath(JSPath.join(first, more: _*))
}
