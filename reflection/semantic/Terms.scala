package scala.reflect
package semantic

import org.scalareflect.annotations._
import org.scalareflect.errors._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._
import scala.reflect.semantic.errors.wrapHosted

trait TermOps {
  implicit class RichTerm(tree: Term) {
    @hosted def tpe: Type = tree.internalTpe
  }

  implicit class RichTermRef(tree: Term.Ref) {
    @hosted def defn: Overload[Member.Term] = wrapHosted(_.defn(tree)).map(Overload.apply)
  }
}
object TermOps extends TermOps