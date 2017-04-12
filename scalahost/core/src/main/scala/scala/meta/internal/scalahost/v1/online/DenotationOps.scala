package scala.meta.internal
package scalahost
package v1
package online

import scala.{meta => m}
import scala.meta.semantic.v1._

trait DenotationOps { self: Mirror =>

  implicit class XtensionGSymbolMDenotation(sym: g.Symbol) {
    def toDenotation: m.Denotation = {
      var flags: Long = 0
      // TODO: support other flags
      if (sym.isClass && !sym.isTrait) flags |= CLASS
      if (sym.isTrait) flags |= TRAIT
      m.Denotation(flags)
    }
  }
}
