package scala.meta.io

import java.io._
import org.scalameta.data._
import scala.meta.internal.io.PlatformIO

@data class AbsolutePath(underlying: String) {
  override def toString: String = underlying

  def toFile: File = new File(underlying)

  @deprecated("Use .toString instead", "1.7.0") // kept for source compabilitity with old Input.File/Address.File
  def getAbsolutePath: String = underlying

  def relativize(path: AbsolutePath): RelativePath = PlatformIO.relativize(path, this)
}

object AbsolutePath {
  def apply(file: File): AbsolutePath = {
    AbsolutePath(file.getAbsolutePath)
  }

  def apply(path: String): AbsolutePath = {
    if (PlatformIO.isAbsolutePath(path)) new AbsolutePath(path)
    else sys.error(s"not an absolute path: $path")
  }
}
