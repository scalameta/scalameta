package java.nio.file

import scala.meta.internal.io.JSPath
import scala.meta.internal.io.NodeNIOPath

object Paths {
  // NOTE: We can't use Scala-style varargs since those have a different jvm
  // signature than Java-style varargs. The boot classpath contains nio.file.Path
  // so call-sites to `get` will resolve to the original java.nio.file.Paths.get,
  // which results in a Scala.js linking error when using Scala varargs.
  def get(first: String, more: Array[String] = Array.empty): Path =
    NodeNIOPath(JSPath.join(first, more: _*))
}
