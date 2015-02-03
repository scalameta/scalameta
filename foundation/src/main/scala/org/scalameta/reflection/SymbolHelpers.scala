package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.reflect.internal.Flags
import scala.collection.mutable
import org.scalameta.invariants._

trait SymbolHelpers {
  self: GlobalToolkit =>

  import global.{require => _, _}
  import definitions._

  object PkgSymbol {
    def unapply(sym: Symbol): Option[ModuleSymbol] = {
      if (sym.hasPackageFlag) Some(sym.asModule)
      else None
    }
  }

  object ClassSymbol {
    def unapply(sym: Symbol): Option[ClassSymbol] = {
      if (sym.isClass && !sym.isTrait) Some(sym.asClass)
      else None
    }
  }

  object TraitSymbol {
    def unapply(sym: Symbol): Option[ClassSymbol] = {
      if (sym.isTrait) Some(sym.asClass)
      else None
    }
  }

  object PkgObjectSymbol {
    def unapply(sym: Symbol): Option[ModuleSymbol] = {
      if (sym.isModule && sym.name == nme.PACKAGE) Some(sym.asModule)
      else None
    }
  }

  object ObjectSymbol {
    def unapply(sym: Symbol): Option[ModuleSymbol] = {
      if (sym.isModule && sym.name != nme.PACKAGE) Some(sym.asModule)
      else None
    }
  }

  object AbstractValSymbol {
    def unapply(sym: Symbol): Option[TermSymbol] = {
      if (sym.isTerm && !sym.isMethod && !sym.isModule && !sym.isMutable && sym.isDeferred) Some(sym.asTerm)
      else None
    }
  }

  object ValSymbol {
    def unapply(sym: Symbol): Option[TermSymbol] = {
      if (sym.isTerm && !sym.isMethod && !sym.isModule && !sym.isMutable && !sym.isDeferred) Some(sym.asTerm)
      else None
    }
  }

  object AbstractVarSymbol {
    def unapply(sym: Symbol): Option[TermSymbol] = {
      if (sym.isTerm && !sym.isMethod && !sym.isModule && sym.isMutable && sym.isDeferred) Some(sym.asTerm)
      else None
    }
  }

  object VarSymbol {
    def unapply(sym: Symbol): Option[TermSymbol] = {
      if (sym.isTerm && !sym.isMethod && !sym.isModule && sym.isMutable && !sym.isDeferred) Some(sym.asTerm)
      else None
    }
  }

  object AbstractDefSymbol {
    def unapply(sym: Symbol): Option[MethodSymbol] = {
      if (sym.isMethod && !sym.isMacro && sym.isDeferred) Some(sym.asMethod)
      else None
    }
  }

  object DefSymbol {
    def unapply(sym: Symbol): Option[MethodSymbol] = {
      if (sym.isMethod && !sym.isMacro && !sym.isDeferred) Some(sym.asMethod)
      else None
    }
  }

  object MacroSymbol {
    def unapply(sym: Symbol): Option[MethodSymbol] = {
      if (sym.isMethod && sym.isMacro) Some(sym.asMethod)
      else None
    }
  }

  object AbstractTypeSymbol {
    def unapply(sym: Symbol): Option[TypeSymbol] = {
      if (sym.isType && sym.isAbstractType) Some(sym.asType)
      else None
    }
  }

  object AliasTypeSymbol {
    def unapply(sym: Symbol): Option[TypeSymbol] = {
      if (sym.isType && sym.isAliasType) Some(sym.asType)
      else None
    }
  }
}