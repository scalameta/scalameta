package scala.meta
package semantic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.annotations._
import scala.annotation._
import scala.meta.artifacts.Domain

trait Mirror {
  def domain: Domain
}