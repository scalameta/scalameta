package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.reflect.internal.Flags
import scala.collection.mutable
import org.scalameta.invariants._

trait SymbolHelpers {
  self: GlobalToolkit =>

  import global.{require => _, _}
  import definitions._

  implicit class RichHelperSymbol(sym: Symbol) {
    def isAnonymous: Boolean = {
      // NOTE: not all symbols whose names start with x$ are placeholders
      // there are also at least artifact vals created for named arguments
      val isTermPlaceholder = sym.isTerm && sym.isParameter && sym.name.startsWith(nme.FRESH_TERM_NAME_PREFIX)
      val isTypePlaceholder = sym.isType && sym.isAbstract && sym.name.startsWith("_$")
      val isAnonymousSelfName = sym.name.startsWith(nme.FRESH_TERM_NAME_PREFIX) || sym.name == nme.this_
      val isAnonymousSelf = sym.isTerm && isAnonymousSelfName && sym.owner.isClass // TODO: more precise detection, probably via attachments
      val isAnonymousTypeParameter = sym.name == tpnme.WILDCARD
      isTermPlaceholder || isTypePlaceholder || isAnonymousSelf || isAnonymousTypeParameter
    }
  }
}
