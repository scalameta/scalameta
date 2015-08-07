package scala.meta
package taxonomic

import org.scalameta.annotations._
import scala.annotation._

@opaque
@implicitNotFound("this method requires an implicit scala.meta.taxonomic.Context")
trait Context {
  def domain: Domain
}
