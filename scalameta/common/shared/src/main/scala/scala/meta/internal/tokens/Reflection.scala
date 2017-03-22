package scala.meta
package internal
package tokens

import org.scalameta.invariants._
import org.scalameta.adt.{Reflection => AdtReflection}
import scala.meta.internal.tokens.{Metadata => TokenMetadata}

trait Reflection extends AdtReflection {
  import u._

  implicit class XtensionTokenMetadataSymbol(sym: Symbol) {
    def isToken: Boolean = {
      sym.hasAnnotation[TokenMetadata.tokenClass]
    }
    def isFreeform: Boolean = {
      Predef.require(sym.isToken)
      val Some(q"new $_($_, ${freeform: Boolean})") = sym.getAnnotation[TokenMetadata.tokenClass]
      freeform
    }
    def isFixed: Boolean = {
      !sym.isFreeform
    }
    def tokenName: String = {
      Predef.require(sym.isToken)
      val Some(q"new $_(${tokenName: String}, $_)") = sym.getAnnotation[TokenMetadata.tokenClass]
      tokenName
    }
  }

  implicit class XtensionTokenMetadataLeaf(leaf: Leaf) {
    def isToken: Boolean = leaf.sym.isToken
    def isFreeform: Boolean = leaf.sym.isFreeform
    def isFixed: Boolean = leaf.sym.isFixed
    def tokenName: String = leaf.sym.tokenName
  }
}