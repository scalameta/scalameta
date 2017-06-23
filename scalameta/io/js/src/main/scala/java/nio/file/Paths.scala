package java.nio.file

import scala.meta.internal.io.JSPath
import scala.meta.internal.io.NodeNIOPath

object Paths {
  def get(first: String, more: String*): Path =
    NodeNIOPath(JSPath.join(first, more: _*))
}
