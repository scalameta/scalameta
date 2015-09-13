package scala.meta.internal.hosts.scalac
package reflect

import scala.tools.nsc.Global
import scala.reflect.internal.Flags
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable

trait TreeHelpers {
  self: GlobalToolkit =>

  import global.{require => _, _}
  import definitions._
  import treeInfo._
  import build._

  implicit class RichFoundationSymbol(sym: Symbol) {
    def displayName: String = Ident(sym).displayName
  }

  implicit class RichFoundationNameTree(tree: Tree) {
    // NOTE: scala.reflect's tree don't have parent links, so we have to approximate if we encounter an unattributed package object
    def displayName: String = {
      def packageName(tree: ModuleDef): Name = {
        if (tree.symbol != NoSymbol) tree.symbol.owner.name
        else tree.parent match {
          case _: PackageDef => TermName(tree.parent.displayName)
          case _ => TermName("package")
        }
      }
      tree match {
        case tree: ModuleDef if tree.name == nme.PACKAGE => packageName(tree).displayName
        case tree: NameTree => tree.name.displayName
        case This(name) => name.displayName
        case Super(_, name) => name.displayName
        case _ => unreachable(debug(tree, showRaw(tree)))
      }
    }
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
