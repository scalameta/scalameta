package java.nio.file

import scala.meta.internal.io.JSPath

object Paths {
  def get(first: String, more: String*): Path =
    NodeJSPath(JSPath.join(first, more: _*))
}
