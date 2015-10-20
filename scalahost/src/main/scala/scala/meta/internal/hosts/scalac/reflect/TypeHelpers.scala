package scala.meta.internal.hosts.scalac
package reflect

import scala.tools.nsc.Global
import scala.reflect.internal.Flags
import scala.collection.mutable
import org.scalameta.invariants._

trait TypeHelpers {
  self: ReflectToolkit =>

  import global.{require => _, _}
  import definitions._

  implicit class RichHelperType(tpe: Type) {
    def etaReduce: Type = EtaReduce.unapply(tpe).getOrElse(tpe)
    def normalizeVararg: Type = tpe match {
      case TypeRef(_, sym, List(arg)) if sym == RepeatedParamClass => appliedType(SeqClass, arg)
      case _ => tpe
    }
    def directBaseTypes: List[Type] = ???
  }

  implicit class RichHelperClassInfoType(tpe: ClassInfoType) {
    def realParents: List[Type] = {
      tpe.parents match {
        case classTpe :: traitTpe :: rest if traitTpe <:< classTpe =>
          // NOTE: this is obviously imprecise, but at least it captures the common case
          traitTpe :: rest
        case other =>
          other
      }
    }
  }

  object EtaReduce {
    def unapply(tpe: Type): Option[Type] = tpe match {
      case PolyType(tparams, TypeRef(pre, sym, targs)) =>
        val canReduce = tparams.zip(targs).forall({
          case (tparam, TypeRef(_, targsym, Nil)) => tparam == targsym
          case _ => false
        })
        if (canReduce) Some(TypeRef(pre, sym, Nil))
        else None
      case _ =>
        None
    }
  }
}
