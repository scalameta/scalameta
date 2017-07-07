package scala.meta.internal.semantic.vfs

import scala.meta.io.RelativePath
import org.scalameta.invariants.require

object Paths {
  private val semanticDbPrefix: RelativePath = RelativePath("META-INF").resolve("semanticdb")
  private val semanticDbSuffix = ".semanticdb"
  private val scalaPrefix = ""
  private val scalaSuffix = ".scala"

  def isSemanticdb(path: RelativePath): Boolean = {
    path.toNIO.startsWith(semanticDbPrefix.toNIO) &&
    path.path.getFileName.toString.endsWith(semanticDbSuffix)
  }

  def semanticdbToScala(path: RelativePath): RelativePath = {
    require(isSemanticdb(path))
    RelativePath(path.toNIO
      .relativize(path.toNIO)
      .resolveSibling(path.path.getFileName.toString.stripSuffix(semanticDbSuffix) + scalaSuffix))
  }

  def isScala(path: RelativePath): Boolean = {
    path.toString.startsWith(scalaPrefix) &&
    path.toString.endsWith(scalaSuffix)
  }

  def scalaToSemanticdb(path: RelativePath): RelativePath = {
    require(isScala(path))
    semanticDbPrefix.resolve(path.resolveSibling(_.stripSuffix(scalaSuffix) + semanticDbSuffix))
  }
}
