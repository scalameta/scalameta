package scala.meta
package io

import java.io._
import java.net._
import org.scalameta.data._
import scala.meta.internal.io.PathIO

@data class RelativePath private (value: String) {
  def syntax: String = value
  def structure: String = s"""RelativePath("$value")"""
  override def toString: String = syntax

  def toFile: File = new File(value)
  def toURI: URI = toFile.toURI

  def toAbsolute: AbsolutePath = toAbsolute(PathIO.workingDirectory)
  def toAbsolute(root: AbsolutePath): AbsolutePath = root.resolve(this)
  def toAbsolute(file: File): AbsolutePath = toAbsolute(AbsolutePath(file))
  def toAbsolute(path: String): AbsolutePath = toAbsolute(AbsolutePath(path))

  def resolve(path: RelativePath): RelativePath = PathIO.resolve(this, path)
  def resolve(file: File): RelativePath = resolve(RelativePath(file))
  def resolve(path: String): RelativePath = resolve(RelativePath(path))
}

object RelativePath {
  def apply(file: File): RelativePath = {
    RelativePath(file.getPath)
  }

  def apply(path: String): RelativePath = {
    if (!PathIO.isAbsolutePath(path)) new RelativePath(path)
    else sys.error(s"not a relative path: $path")
  }
}
