package star.meta.internal
package semanticdb
package vfs

import star.meta.internal.io.PathIO
import star.meta.io.RelativePath

object SemanticdbPaths {
  private val semanticdbPrefix = RelativePath("META-INF").resolve("semanticdb")
  private val semanticdbExtension = "semanticdb"
  private val scalaExtension = "scala"

  def isSemanticdb(path: RelativePath): Boolean = {
    path.toNIO.startsWith(semanticdbPrefix.toNIO) &&
    PathIO.extension(path.toNIO) == semanticdbExtension
  }

  def toScala(path: RelativePath): RelativePath = {
    require(isSemanticdb(path))
    val scalaSibling =
      path.resolveSibling(_.stripSuffix(semanticdbExtension) + scalaExtension)
    semanticdbPrefix.relativize(scalaSibling)
  }

  def isScala(path: RelativePath): Boolean = {
    PathIO.extension(path.toNIO) == scalaExtension
  }

  def fromScala(path: RelativePath): RelativePath = {
    require(isScala(path))
    val semanticdbSibling =
      path.resolveSibling(_.stripSuffix(scalaExtension) + semanticdbExtension)
    semanticdbPrefix.resolve(semanticdbSibling)
  }
}
