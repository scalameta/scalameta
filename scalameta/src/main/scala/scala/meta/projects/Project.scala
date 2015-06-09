package scala.meta
package projects

import java.net._
import java.io._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._
import org.scalameta.invariants._

@root trait Project {
  def uri: URI
  def sources: Seq[Source]
  def resources: Seq[Resource]
  def dependencies: Seq[Project]
}

object Project {
  @leaf class Local(uri: URI, sources: Seq[Source], resources: Seq[Resource], dependencies: Seq[Project]) extends Project
}
