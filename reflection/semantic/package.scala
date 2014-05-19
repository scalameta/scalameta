package scala.reflect

import org.scalareflect.adt._
import org.scalareflect.annotations._
import org.scalareflect.errors._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._

package object semantic extends MemberOps with ScopeOps with TermOps with TypeOps {
  def root = Pkg.Template(Term.Name("_root_")(Origin.None), Nil)(Origin.None)

  @hosted def semanticProfile: SemanticProfile = delegate
  final case class SemanticProfile(dynamics: Boolean,
                                   postfixOps: Boolean,
                                   reflectiveCalls: Boolean,
                                   implicitConversions: Boolean,
                                   higherKinds: Boolean,
                                   existentials: Boolean,
                                   macros: Boolean)

  implicit class RichTree(tree: Tree) {
    @hosted def attrs: Seq[Attribute] = delegate
    @hosted private[semantic] def internalTpe: Type = attrs.flatMap(_.collect{ case tpe: Attribute.Type => tpe } match {
      case Attribute.Type(tpe: Type) :: Nil => succeed(tpe)
      case _ => fail(ReflectionException("typecheck has failed"))
    })
    @hosted private[semantic] def internalParamTpe: Aux.ParamType = attrs.flatMap(_.collect{ case tpe: Attribute.Type => tpe } match {
      case Attribute.Type(paramTpe) :: Nil => succeed(paramTpe)
      case _ => fail(ReflectionException("typecheck has failed"))
    })
  }

  @root trait Attribute
  object Attribute {
    @leaf class Ref(ref: core.Tree) extends Attribute
    @leaf class Type(tpe: core.Aux.ParamType) extends Attribute
    @leaf class InferredTargs(targs: Seq[core.Type]) extends Attribute
    @leaf class InferredVargs(vargs: Seq[core.Term]) extends Attribute
    @leaf class MacroExpansion(tree: core.Tree) extends Attribute
  }
}
