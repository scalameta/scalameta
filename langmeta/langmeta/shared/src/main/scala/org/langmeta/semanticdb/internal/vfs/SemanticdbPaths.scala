package org.langmeta.internal
package semanticdb
package vfs

import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath
import org.langmeta.io.RelativePath
import scala.meta.internal.{semanticdb3 => s}

object SemanticdbPaths {
  val semanticdbPrefix: RelativePath = RelativePath("META-INF").resolve("semanticdb")
  val semanticdbExtension = "semanticdb"
  private val scalaExtension = "scala"
  private val scalaScriptExtension = "sc"

  def isSemanticdb(path: RelativePath): Boolean = {
    path.toNIO.startsWith(semanticdbPrefix.toNIO) &&
    PathIO.extension(path.toNIO) == semanticdbExtension
  }

  def toScala(
      semanticdb: AbsolutePath,
      sourceroot: AbsolutePath,
      targetroot: AbsolutePath): AbsolutePath = {
    sourceroot.resolve(toScala(semanticdb.toRelative(targetroot)))
  }

  def toScala(path: RelativePath): RelativePath = {
    require(isSemanticdb(path))
    val scalaSibling = path.resolveSibling(_.stripSuffix("." + semanticdbExtension))
    semanticdbPrefix.relativize(scalaSibling)
  }

  def isScala(path: RelativePath): Boolean = {
    val extension = PathIO.extension(path.toNIO)
    extension == scalaExtension || extension == scalaScriptExtension
  }

  def fromScala(path: RelativePath): RelativePath = {
    require(isScala(path))
    val semanticdbSibling = path.resolveSibling(_ + "." + semanticdbExtension)
    semanticdbPrefix.resolve(semanticdbSibling)
  }

  def toSemanticdb(path: RelativePath, targetroot: AbsolutePath): AbsolutePath = {
    require(isScala(path))
    val semanticdbPath = path.resolveSibling(_ + "." + semanticdbExtension)
    targetroot.resolve(semanticdbPrefix).resolve(semanticdbPath)
  }

  def toSemanticdb(doc: s.TextDocument, targetroot: AbsolutePath): AbsolutePath = {
    targetroot.resolve(semanticdbPrefix).resolve(doc.uri + "." + semanticdbExtension)
  }
}
