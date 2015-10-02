package scala.meta
package artifacts

import java.io._
import java.net.URI
import org.scalameta.data._
import org.scalameta.invariants._

@data class Resource(name: String, uri: URI) {
  def open(): InputStream = uri.toURL.openStream()
  override def toString = "Resource(\"" + name + "\", new URI(\"" + uri + "\"))"
}
