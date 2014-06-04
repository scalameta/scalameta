package scala.reflect
package semantic

import org.scalareflect.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._

trait RefOps {
  implicit class SemanticRefOps(tree: Ref) {
    private[semantic] def toTypeRef: Type.Ref = ??? // TODO: t"$tree"
    @hosted def defns: Seq[Member] = wrapHosted(_.defns(tree))
  }

  implicit class SemanticTypeRefOps(tree: Type.Ref) {
    @hosted def defns: Seq[Member.Type] = (tree: Ref).defns.flatMap(defns => {
      if (defns.exists(!_.isInstanceOf[Member.Type])) fail(s"unexpected $defns for ref $tree")
      else succeed(defns.asInstanceOf[Seq[Member.Type]])
    })
    @hosted def defn: Member.Type = defns.flatMap(_.findUnique)
  }

  implicit class SemanticTermRefOps(tree: Term.Ref) {
    @hosted def defns: Seq[Member.Term] = (tree: Ref).defns.flatMap(defns => {
      if (defns.exists(!_.isInstanceOf[Member.Term])) fail(s"unexpected $defns for ref $tree")
      else succeed(defns.asInstanceOf[Seq[Member.Term]])
    })
    @hosted def defn: Member.Term = defns.flatMap(_.findUnique)
  }

  implicit class SemanticMembers[A <: Member.Term](tree: Seq[A]) {
    def resolve(tpes: Seq[core.Type]): A = ??? // TODO: implement this in terms of Tree.attrs and Attribute.Ref
  }
}
