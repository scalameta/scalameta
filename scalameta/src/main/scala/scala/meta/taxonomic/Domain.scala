package scala.meta
package taxonomic

import org.scalameta.data._
import scala.{Seq => _}
import scala.collection.immutable.Seq

@data class Domain(modules: Module*) {
  override def toString = "Domain(" + modules.mkString(", ") + ")"
}
