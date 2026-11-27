package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.io.PathIO
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.{AbsolutePath, RelativePath}

object SemanticdbPaths {

  val semanticdbPrefix: RelativePath = RelativePath("META-INF").resolve("semanticdb")
  private[meta] val DbExtension = ".semanticdb"
  private val scalaExtension = ".scala"
  private val scalaScriptExtension = ".sc"

  def isSemanticdb(path: RelativePath): Boolean = path.toNIO.startsWith(semanticdbPrefix.toNIO) &&
    PathIO.fileExtension(path.toNIO) == DbExtension

  def toScala(
      semanticdb: AbsolutePath,
      sourceroot: AbsolutePath,
      targetroot: AbsolutePath,
  ): AbsolutePath = sourceroot.resolve(toScala(semanticdb.toRelative(targetroot)))

  def toScala(path: RelativePath): RelativePath = {
    require(isSemanticdb(path))
    val scalaSibling = path.resolveSibling(_.stripSuffix(DbExtension))
    semanticdbPrefix.relativize(scalaSibling)
  }

  def isScala(path: RelativePath): Boolean = {
    val extension = PathIO.fileExtension(path.toNIO)
    extension == scalaExtension || extension == scalaScriptExtension
  }

  def fromScala(path: RelativePath): RelativePath = {
    require(isScala(path))
    val semanticdbSibling = path.resolveSibling(_ + DbExtension)
    semanticdbPrefix.resolve(semanticdbSibling)
  }

  def toSemanticdb(path: RelativePath, targetroot: AbsolutePath): AbsolutePath = {
    require(isScala(path))
    val semanticdbPath = path.resolveSibling(_ + DbExtension)
    targetroot.resolve(semanticdbPrefix).resolve(semanticdbPath)
  }

  def toSemanticdb(doc: s.TextDocument, targetroot: AbsolutePath): AbsolutePath = {
    val targetrootWithSemanticdbPrefix = targetroot.resolve(semanticdbPrefix)
    // Doing this operation in URI space is important if doc.uri contains encoded chars:
    val uri = targetrootWithSemanticdbPrefix.toURI(isDirectory = true).resolve(doc.uri + DbExtension)
    AbsolutePath.fromAbsoluteUri(uri)
  }

}
