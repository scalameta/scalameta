package scala.meta.internal.semantic.vfs

import scala.meta.internal.io.PathIO.fileSeparator
import scala.meta.io.RelativePath
import org.scalameta.invariants.require

object Paths {
  private val semanticDbPrefix = "META-INF" + fileSeparator + "semanticdb" + fileSeparator
  private val semanticDbSuffix = ".semanticdb"
  private val scalaPrefix = ""
  private val scalaSuffix = ".scala"

  def isSemanticdb(path: RelativePath): Boolean = {
    path.toString.contains(semanticDbPrefix) &&
    path.toString.endsWith(semanticDbSuffix)
  }

  def semanticdbToScala(path: RelativePath): RelativePath = {
    require(isSemanticdb(path))
    RelativePath(
      scalaPrefix +
        path.toString
          .stripPrefix(semanticDbPrefix)
          .stripSuffix(semanticDbSuffix) +
        scalaSuffix)
  }

  def isScala(path: RelativePath): Boolean = {
    path.toString.startsWith(scalaPrefix) &&
    path.toString.endsWith(scalaSuffix)
  }

  def scalaToSemanticdb(path: RelativePath): RelativePath = {
    require(isScala(path))
    RelativePath(
      semanticDbPrefix +
        path.toString
          .stripPrefix(scalaPrefix)
          .stripSuffix(scalaSuffix) +
        semanticDbSuffix)
  }
}
