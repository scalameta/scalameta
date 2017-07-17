package scala.meta.internal.semantic.vfs

import java.net.URI
import org.scalameta.adt._
import scala.meta.internal.io.FileIO
import scala.meta.io._

@root
trait Entry {
  def fragment: Fragment
  def base: AbsolutePath = fragment.base
  def name: RelativePath = fragment.name
  def uri: URI = fragment.uri
  def bytes: Array[Byte]
}

object Entry {
  @leaf class OnDisk(fragment: Fragment) extends Entry {
    lazy val bytes = FileIO.readAllBytes(fragment.uri)
  }
  @leaf class InMemory(fragment: Fragment, bytes: Array[Byte]) extends Entry
}
