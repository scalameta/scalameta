package scala.meta.internal.semantic.vfs

import java.io._
import java.net._
import org.scalameta.adt._
import org.scalameta.data._
import org.scalameta.invariants._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.io._
import scala.meta.internal.io.InputStreamIO
import scala.meta.internal.io.PathIO.fileSeparator
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.internal.semantic.{schema => s}

@root trait Entry {
  def fragment: Fragment
  def base: AbsolutePath = fragment.base
  def name: RelativePath = fragment.name
  def uri: URI = fragment.uri
  def bytes: Array[Byte]
}

object Entry {
  private def readBytes(fragment: Fragment) = InputStreamIO.readBytes(fragment.uri.toURL.openStream)
  @leaf class OnDisk(fragment: Fragment) extends Entry { lazy val bytes = readBytes(fragment) }
  @leaf class InMemory(fragment: Fragment, bytes: Array[Byte]) extends Entry
}

object Paths {
  private val semanticDbPrefix = "META-INF" + fileSeparator + "semanticdb" + fileSeparator
  private val semanticDbSuffix = ".semanticdb"
  private val scalaPrefix = ""
  private val scalaSuffix = ".scala"

  def isSemanticdb(path: RelativePath): Boolean = {
    path.toString.startsWith(semanticDbPrefix) && path.toString.endsWith(semanticDbSuffix)
  }

  def semanticdbToScala(path: RelativePath): RelativePath = {
    require(isSemanticdb(path))
    RelativePath(scalaPrefix + path.toString.stripPrefix(semanticDbPrefix).stripSuffix(semanticDbSuffix) + scalaSuffix)
  }

  def isScala(path: RelativePath): Boolean = {
    path.toString.startsWith(scalaPrefix) && path.toString.endsWith(scalaSuffix)
  }

  def scalaToSemanticdb(path: RelativePath): RelativePath = {
    require(isScala(path))
    RelativePath(semanticDbPrefix + path.toString.stripPrefix(scalaPrefix).stripSuffix(scalaSuffix) + semanticDbSuffix)
  }
}
