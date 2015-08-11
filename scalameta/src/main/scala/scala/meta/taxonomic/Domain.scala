package scala.meta
package taxonomic

import scala.{Seq => _}
import scala.collection.immutable.Seq

final case class Domain(modules: Module*) {
  override def toString = "Domain(" + modules.mkString(", ") + ")"
}
