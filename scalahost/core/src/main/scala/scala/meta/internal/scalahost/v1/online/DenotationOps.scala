package scala.meta.internal
package scalahost
package v1
package online

import scala.{meta => m}
import scala.meta.semantic.v1._

trait DenotationOps { self: Mirror =>
  import g._

  implicit class XtensionGSymbolMDenotation(sym: g.Symbol) {
    def toDenotation: m.Denotation = {
      if (sym.isModuleClass) return sym.asClass.module.toDenotation

      var flags: Long = 0

      if (sym.isModule && !sym.hasPackageFlag && sym.name != nme.PACKAGE) flags |= OBJECT
      if (sym.hasPackageFlag) flags |= PACKAGE
      if (sym.isModule && sym.name == nme.PACKAGE) flags |= PACKAGEOBJECT
      if (sym.isClass && !sym.isTrait) flags |= CLASS
      if (sym.isTrait) flags |= TRAIT

      m.Denotation(flags)
    }
  }
}
