package scala.meta.internal.semantic.vfs

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.net.URI
import org.scalameta.adt._
import scala.meta.internal.io.FileIO
import scala.meta.internal.io.InputStreamIO
import scala.meta.io._

@root
trait Entry {
  def fragment: Fragment
  def base: AbsolutePath = fragment.base
  def name: RelativePath = fragment.name
  def uri: URI = fragment.uri
  def bytes: Array[Byte]
  def inputStream: InputStream = new ByteArrayInputStream(bytes)
}

object Entry {
  @leaf class OnDisk(fragment: Fragment) extends Entry {
    lazy val bytes = InputStreamIO.readBytes(inputStream)
    override def inputStream: InputStream = FileIO.newInputStream(fragment.uri)
  }
  @leaf class InMemory(fragment: Fragment, bytes: Array[Byte]) extends Entry
}
