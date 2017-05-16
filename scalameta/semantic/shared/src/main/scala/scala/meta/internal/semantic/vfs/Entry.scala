package scala.meta.internal.semantic.vfs

import java.net.URI
import org.scalameta.adt._
import scala.collection.immutable.Seq
import scala.meta.internal.io.FileIO
import scala.meta.io._
import scala.meta.internal.io.InputStreamIO
import scala.meta.internal.platform

@root
trait Entry {
  def fragment: Fragment
  def base: AbsolutePath = fragment.base
  def name: RelativePath = fragment.name
  def uri: URI = fragment.uri
  def bytes: Array[Byte]
}

object Entry {
  private def readBytes(fragment: Fragment): Array[Byte] =
    FileIO.readAllBytes(fragment.uri)
  @leaf class OnDisk(fragment: Fragment) extends Entry { lazy val bytes = readBytes(fragment) }
  @leaf class InMemory(fragment: Fragment, bytes: Array[Byte]) extends Entry
}
