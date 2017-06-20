package scala.meta
package io

import java.io._
import java.nio.{file => nio}
import java.net._
import java.nio.file.Path
import java.nio.file.Paths
import scala.meta.internal.io.{FileIO, PathIO}

sealed abstract case class AbsolutePath(path: nio.Path) {
  require(path.isAbsolute, s"$path is not absolute!")
  def toFile: File = path.toFile
  def toURI: URI = path.toUri
  def toNIO: nio.Path = path

  def syntax: String = toString
  def structure: String = s"""AbsolutePath("$syntax")"""
  override def toString: String = path.toString

  def toRelative: RelativePath = toRelative(PathIO.workingDirectory)
  def toRelative(prefix: AbsolutePath): RelativePath = RelativePath(prefix.path.relativize(path))

  def resolve(other: RelativePath): AbsolutePath = AbsolutePath(path.resolve(other.path))(this)
  def resolve(other: String): AbsolutePath = AbsolutePath(path.resolve(other))(this)

  def isFile: Boolean = FileIO.isFile(this)
  def isDirectory: Boolean = FileIO.isDirectory(this)
  def readAllBytes: Array[Byte] = FileIO.readAllBytes(this)
}

object AbsolutePath {
  private[meta] val root = new AbsolutePath(Paths.get("").toAbsolutePath.getRoot) {}
  def apply(file: File): AbsolutePath = apply(file.toPath)(PathIO.workingDirectory)
  def apply(path: String)(implicit cwd: AbsolutePath): AbsolutePath = apply(Paths.get(path))
  def apply(path: Path)(implicit cwd: AbsolutePath): AbsolutePath =
    if (path.isAbsolute) {
      new AbsolutePath(path) {}
    } else {
      cwd.resolve(path.toString)
    }
}
