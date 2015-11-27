package scala.meta.internal.hosts.scalac
package reflect

import scala.tools.nsc.Global
import scala.reflect.macros.Attachments
import scala.reflect.internal.Flags
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable

trait TreeHelpers {
  self: ReflectToolkit =>

  import global.{require => _, _}
  import definitions._
  import treeInfo._
  import build._
  import analyzer._

  implicit class RichFoundationSymbol(sym: Symbol) {
    def displayName: String = sym.name.displayName
  }

  implicit class RichFoundationHelperName(name: Name) {
    def isAnonymous = {
      val isTermPlaceholder = name.isTermName && name.startsWith(nme.FRESH_TERM_NAME_PREFIX)
      val isTypePlaceholder = name.isTypeName && name.startsWith("_$")
      val isAnonymousSelf = name.isTermName && (name.startsWith(nme.FRESH_TERM_NAME_PREFIX) || name == nme.this_)
      val isAnonymousTypeParameter = name == tpnme.WILDCARD
      isTermPlaceholder || isTypePlaceholder || isAnonymousSelf || isAnonymousTypeParameter
    }
    def looksLikeInfix = {
      val hasSymbolicName = !name.decoded.forall(c => Character.isLetter(c) || Character.isDigit(c) || c == '_')
      val idiomaticallyUsedAsInfix = name == nme.eq || name == nme.ne
      hasSymbolicName || idiomaticallyUsedAsInfix
    }
    def displayName = {
      // NOTE: "<empty>", the internal name for empty package, isn't a valid Scala identifier, so we hack around
      if (name == rootMirror.EmptyPackage.name) "_empty_"
      else if (name == rootMirror.EmptyPackageClass.name) "_empty_"
      else if (name.isAnonymous) "_"
      else name.decodedName.toString
    }
  }
}
