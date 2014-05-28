package scala.reflect
package semantic

import org.scalareflect.annotations._
import org.scalareflect.errors._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._
import scala.reflect.semantic.errors.wrapHosted

trait TermOps {
  implicit class SemanticTermOps(tree: Term) {
    @hosted def tpe: Type = tree.internalTpe
  }
}
