package scala.meta
package artifacts

import org.scalameta.data._
import scala.{Seq => _}
import scala.collection.immutable.Seq

// TODO: there's some bug in @data that prevents this from compiling
// @data class Domain(artifacts: Artifact*)(implicit resolver: Resolver) {
case class Domain(artifacts: Artifact*)(implicit resolver: Resolver) {
  def sources: Seq[Source] = artifacts.flatMap(_.sources).toList
  def resources: Seq[Resource] = artifacts.flatMap(_.resources).toList
  override def toString = "Domain(" + artifacts.mkString(", ") + ")(" + resolver + ")"
}
