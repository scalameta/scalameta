package scala.reflect
package semantic

import org.scalareflect.annotations._
import org.scalareflect.errors._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._
import scala.reflect.semantic.errors.wrapHosted

trait RefOps {
  implicit class SemanticRefOps(tree: Ref) {
    private[semantic] def toTypeRef: Type.Ref = ??? // TODO: t"$tree"
    @hosted def defns: Seq[Member] = delegate
  }

  implicit class SemanticTypeRefOps(tree: Type.Ref) {
    @hosted def defns: Seq[Member.Type] = ???
    @hosted def defn: Member.Type = ???
  }

  implicit class SemanticTermRefOps(tree: Term.Ref) {
    @hosted def defns: Seq[Member.Type] = ???
    @hosted def defn: Overload[Member.Term] = ???
  }
}
