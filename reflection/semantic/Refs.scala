package scala.reflect
package semantic

import org.scalareflect.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._

trait RefOps {
  implicit class SemanticRefOps(tree: Ref) {
    private[semantic] def toTypeRef: Type.Ref = ??? // TODO: t"$tree"
    @hosted def alts: Seq[Member] = wrapHosted(_.defns(tree))
  }

  implicit class SemanticTypeRefOps(tree: Type.Ref) {
    @hosted def alts: Seq[Member.Type] = alts.flatMap(alts => {
      if (alts.exists(!_.isInstanceOf[Member.Type])) fail(s"unexpected $alts for ref $tree")
      else succeed(alts.asInstanceOf[Seq[Member.Type]])
    })
    @hosted def defn: Member.Type = alts.flatMap(_.findUnique)
  }

  implicit class SemanticTermRefOps(tree: Term.Ref) {
    @hosted def alts: Seq[Member.Term] = alts.flatMap(alts => {
      if (alts.exists(!_.isInstanceOf[Member.Term])) fail(s"unexpected $alts for ref $tree")
      else succeed(alts.asInstanceOf[Seq[Member.Term]])
    })
    @hosted def defn: Member.Term = alts.flatMap(_.findUnique)
  }

  implicit class SemanticMembers[A <: Member.Term](tree: Seq[A]) {
    def resolve(tpes: Seq[core.Type]): A = ??? // TODO: implement this in terms of Tree.attrs and Attribute.Ref
  }
}
