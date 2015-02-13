package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.reflect.internal.Flags
import scala.collection.mutable
import org.scalameta.invariants._

trait TypeHelpers {
  self: GlobalToolkit =>

  import global.{require => _, _}
  import definitions._

  implicit class RichHelperType(tpe: Type) {
    def etaReduce: Type = EtaReduce.unapply(tpe).getOrElse(tpe)
    def directBaseTypes: List[Type] = ???
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
