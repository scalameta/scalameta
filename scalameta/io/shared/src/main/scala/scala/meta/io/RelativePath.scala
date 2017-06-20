package scala.meta
package io

import java.io._
import java.net._
import java.nio.file.Path
import java.nio.{file => nio}
import java.nio.file.Paths
import scala.meta.internal.io.PathIO

sealed abstract case class RelativePath(path: Path) {
  require(!path.isAbsolute, s"$path is not relative!")
  def syntax: String = toString
  def structure: String = s"""RelativePath("$syntax")"""
  override def toString: String = path.toString

  def toFile: File = path.toFile
  def toURI: URI = toFile.toURI

  def toAbsolute: AbsolutePath = PathIO.workingDirectory.resolve(this)
  def toAbsolute(root: AbsolutePath): AbsolutePath = root.resolve(this)

  def resolve(other: nio.Path): RelativePath = RelativePath(path.resolve(other))
  def resolve(other: RelativePath): RelativePath = resolve(other.path)
  def resolve(path: String): RelativePath = resolve(Paths.get(path))
}

object RelativePath {
  def apply(file: File): RelativePath = apply(file.getPath)
  def apply(path: String): RelativePath = apply(Paths.get(path))

  // throws Illegal argument exception if path is not relative.
  def apply(path: nio.Path): RelativePath =
    new RelativePath(path) {}
}
