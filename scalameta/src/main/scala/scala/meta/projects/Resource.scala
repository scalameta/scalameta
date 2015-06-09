package scala.meta
package projects

import java.io._
import java.net._
import org.scalameta.adt._
import org.scalameta.invariants._

trait Resource {
  def uri: URI
  def stream: InputStream
}

object Resource {
  final case class File(file: java.io.File) extends Resource {
    def uri = file.toURI
    def stream = new FileInputStream(file)
  }
}