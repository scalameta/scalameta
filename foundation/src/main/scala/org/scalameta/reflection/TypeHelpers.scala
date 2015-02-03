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
    def depoly: Type = tpe match {
      case PolyType(_, tpe) => tpe.depoly
      case _ => tpe
    }
  }
}
