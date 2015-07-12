package scala
package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.reflect.internal.Flags
import scala.collection.mutable
import _root_.org.scalameta.invariants._
import _root_.org.scalameta.unreachable

trait TreeHelpers {
  self: _root_.org.scalameta.reflection.GlobalToolkit =>

  import global.{require => _, _}
  import definitions._
  import treeInfo._
  import build._

  implicit class RichFoundationHelperTree(tree: Tree) {
    // NOTE: scala.reflect's tree don't have parent links, so we have to approximate if we encounter an unattributed package object
    // NOTE: "<empty>", the internal name for empty package, isn't a valid Scala identifier, so we hack around
    def displayName: String = displayName(EmptyTree)
    def displayName(parent: Tree): String = {
      def packageName(tree: ModuleDef): Name = {
        if (tree.symbol != NoSymbol) tree.symbol.owner.name
        else parent match {
          case _: PackageDef => TermName(parent.displayName)
          case _ => TermName("package")
        }
      }
      val name = tree match {
        case tree: ModuleDef if tree.name == nme.PACKAGE => packageName(tree)
        case tree: NameTree if tree.name == rootMirror.EmptyPackage.name => TermName("_empty_")
        case tree: NameTree if tree.name == rootMirror.EmptyPackageClass.name => TypeName("_empty_")
        case tree: NameTree => tree.name
        case This(name) => name
        case Super(_, name) => name
        case _ => unreachable(debug(tree, showRaw(tree)))
      }
      if (name.isAnonymous) "_" else name.decodedName.toString
    }
  }

  protected implicit class RichFoundationHelperName(name: Name) {
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
  }
}
