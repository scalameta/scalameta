package scala.meta.io

import java.io._
import org.scalameta.data._
import scala.meta.internal.io.PlatformIO

@data class RelativePath(underlying: String) {
  override def toString: String = underlying

  def toFile: File = new File(underlying)

  def absolutize(parent: AbsolutePath): AbsolutePath = PlatformIO.absolutize(parent, this)
}

object RelativePath {
  def apply(file: File): RelativePath = {
    RelativePath(file.getPath)
  }

  def apply(path: String): RelativePath = {
    if (PlatformIO.isRelativePath(path)) new RelativePath(path)
    else sys.error(s"not a relative path: $path")
  }
}
