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

