package scala.reflect
package semantic

import org.scalareflect.annotations._
import org.scalareflect.errors._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._
import scala.reflect.semantic.errors.wrapHosted

trait TypeOps {
  implicit class SemanticTypeOps(tree: Type) {
    @hosted def <:<(other: Type): Boolean = delegate
    @hosted def weak_<:<(other: Type): Boolean = delegate
    @hosted def widen: Type = delegate
    @hosted def dealias: Type = delegate
    @hosted def erasure: Type = delegate
    @hosted def companion: Type.Ref = tree match {
      case ref: Type.Ref => ref.defn.flatMap {
        case t: Member.Template => t.companion
        case _ => fail(ReflectionException(s"companion not found"))
      }.map(_.ref.toTypeRef)
      case _ => fail(ReflectionException(s"companion not found"))
    }
  }

  implicit class SemanticRefOps(tree: Ref) {
    private[semantic] def toTypeRef: Type.Ref = ??? // TODO: t"$tree"
  }

  implicit class SemanticTypeRefOps(tree: Type.Ref) {
    @hosted def defn: Member = delegate
  }

  @hosted private[semantic] def supertypesToMembers(tpes: Seq[Type]): Seq[Member.Template] = {
    def extractTemplate(ref: Type.Ref) = {
      for {
        defn <- ref.defn
        result <- defn match {
          case t: Member.Template => succeed(t)
          case d => fail(ReflectionException(s"unexpected ref $ref to $d returned from supertypes"))
        }
      } yield result
    }
    succeed(tpes) mmap {
      case ref: Type.Ref => extractTemplate(ref)
      case Type.Apply(ref: Type.Ref, _) => extractTemplate(ref)
      case tpe => fail(ReflectionException(s"unexpected type $tpe returned from supertypes"))
    }
  }
  implicit class SemanticTemplatesOps(val parents: Seq[Member.Template]) {
    @hosted def linearization: Seq[Member.Template] = {
      val linearization = parents.map(_.ref.toTypeRef).linearization
      linearization.flatMap(tpes => supertypesToMembers(tpes))
    }
  }

  implicit class SemanticTypesOps(val parents: Seq[Type]) {
    @hosted def linearization: Seq[Type] = wrapHosted(_.linearization(parents))
  }

  @hosted def lub(tpes: Seq[Type]): Type = delegate
  @hosted def glb(tpes: Seq[Type]): Type = delegate
}
