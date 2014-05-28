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
    @hosted def defns: Seq[Member] = wrapHosted(_.defns(tree))
  }

  implicit class SemanticTypeRefOps(tree: Type.Ref) {
    @hosted def defns: Seq[Member.Type] = ???
    // TODO: I guarantee that we'll need Type.Ref.defn here
  }

  implicit class SemanticTermRefOps(tree: Term.Ref) {
    @hosted def defns: Seq[Member.Type] = ???
    // TODO: I guarantee that we'll need Type.Ref.defn here
  }

  implicit class SemanticMembers[A <: Member.Term](tree: Seq[A]) {
    def resolve(tpes: Seq[core.Type]): A = ??? // TODO: implement this in terms of Tree.attrs and Attribute.Ref
  }
}
