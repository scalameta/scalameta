package scala.meta.io

import java.io._
import org.scalameta.data._
import scala.meta.internal.io.{FileIO, PathIO}

@data class AbsolutePath private (value: String) {
  override def toString: String = value
  def toFile: File = new File(value)

  def toRelative: RelativePath = toRelative(PathIO.workingDirectory)
  def toRelative(path: AbsolutePath): RelativePath = PathIO.unresolve(path, this)
  def toRelative(file: File): RelativePath = toRelative(AbsolutePath(file))
  def toRelative(path: String): RelativePath = toRelative(AbsolutePath(path))

  def resolve(path: RelativePath): AbsolutePath = PathIO.resolve(this, path)
  def resolve(file: File): AbsolutePath = resolve(RelativePath(file))
  def resolve(path: String): AbsolutePath = resolve(RelativePath(path))
}

object AbsolutePath {
  def apply(file: File): AbsolutePath = {
    AbsolutePath(file.getAbsolutePath)
  }

  def apply(path: String): AbsolutePath = {
    if (PathIO.isAbsolutePath(path)) new AbsolutePath(path)
    else sys.error(s"not an absolute path: $path")
  }
}
