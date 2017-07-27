package star.meta.internal
package semanticdb
package vfs

import java.net.URI
import star.meta.internal.io.FileIO
import star.meta.io._

sealed trait Entry {
  def fragment: Fragment
  def base: AbsolutePath = fragment.base
  def name: RelativePath = fragment.name
  def uri: URI = fragment.uri
  def bytes: Array[Byte]
}

object Entry {
  final case class OnDisk(fragment: Fragment) extends Entry {
    lazy val bytes = FileIO.readAllBytes(fragment.uri)
  }
  final case class InMemory(fragment: Fragment, bytes: Array[Byte]) extends Entry
}
