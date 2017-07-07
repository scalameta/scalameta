package scala.meta.internal.semantic.vfs

import scala.meta.internal.io.PathIO
import scala.meta.io.RelativePath
import org.scalameta.invariants.require

object Paths {
  private val semanticdbPrefix: RelativePath = RelativePath("META-INF").resolve("semanticdb")
  private val semanticdbExtension = "semanticdb"
  private val scalaExtension = "scala"

  def isSemanticdb(path: RelativePath): Boolean = {
    path.toNIO.startsWith(semanticdbPrefix.toNIO) &&
    PathIO.extension(path.toNIO).contains(semanticdbExtension)
  }

  def semanticdbToScala(path: RelativePath): RelativePath = {
    require(isSemanticdb(path))
    val scalaSibling =
      path.resolveSibling(_.stripSuffix(semanticdbExtension) + scalaExtension)
    semanticdbPrefix.relativize(scalaSibling)
  }

  def isScala(path: RelativePath): Boolean = {
    PathIO.extension(path.toNIO).contains(scalaExtension)
  }

  def scalaToSemanticdb(path: RelativePath): RelativePath = {
    require(isScala(path))
    val semanticdbSibling =
      path.resolveSibling(_.stripSuffix(scalaExtension) + semanticdbExtension)
    semanticdbPrefix.resolve(semanticdbSibling)
  }
}
