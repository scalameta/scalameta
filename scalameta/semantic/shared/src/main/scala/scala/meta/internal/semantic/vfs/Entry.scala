package scala.meta.internal.semantic.vfs

import java.net.URI
import org.scalameta.adt._
import scala.collection.immutable.Seq
import scala.meta.io._
import scala.meta.internal.io.InputStreamIO

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

