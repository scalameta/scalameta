package java.nio.file

import scala.meta.internal.io.NodeNIOPath

import java.io.File
import java.net.URI

object Paths {
  // NOTE: We can't use Scala-style varargs since those have a different jvm
  // signature than Java-style varargs. The boot classpath contains nio.file.Path
  // so call-sites to `get` will resolve to the original java.nio.file.Paths.get,
  // which results in a Scala.js linking error when using Scala varargs.
  def get(first: String, more: Array[String] = Array.empty): Path = {
    val path = if (more.isEmpty) first else first + File.separator + more.mkString(File.separator)
    NodeNIOPath(path)
  }

  def get(uri: URI): Path = {
    if (uri.getScheme != "file") throw new IllegalArgumentException("only file: URIs are supported")
    val uripath = uri.getPath
    val trailing = uripath.split('/').toList.dropWhile(_ == "")
    NodeNIOPath(trailing match {
      case drive :: _ if drive.length == 2 && drive(1) == ':' => trailing.mkString("\\")
      case _ => uripath
    })
  }
}
