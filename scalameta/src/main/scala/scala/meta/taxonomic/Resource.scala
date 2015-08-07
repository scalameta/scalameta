package scala.meta
package taxonomic

import java.net.URI
import java.io.InputStream
import org.scalameta.adt._
import org.scalameta.invariants._

trait Resource {
  def uri: URI
  def stream: InputStream
}
