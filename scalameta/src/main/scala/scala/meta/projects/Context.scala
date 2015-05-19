package scala.meta
package projects

import org.scalameta.annotations._
import scala.annotation._

@opaque
@implicitNotFound("this method requires an implicit scala.meta.projects.Context")
trait Context {
  def project: Project
}
