package scala
package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.reflect.internal.Flags
import scala.collection.mutable
import _root_.org.scalameta.invariants._

trait TreeHelpers {
  self: _root_.org.scalameta.reflection.GlobalToolkit =>

  import global.{require => _, _}
  import definitions._
  import treeInfo._
  import build._

  implicit class RichAliasHelperTree(tree: Tree) {
    def alias: String = tree match {
      case _ if tree.symbol == rootMirror.EmptyPackage || tree.symbol == rootMirror.EmptyPackageClass => "_empty_"
      case tree: ModuleDef if tree.symbol.isPackageObject => tree.symbol.owner.name.decodedName.toString
      case tree: NameTree => tree.name.decodedName.toString
      case This(name) => name.decodedName.toString
      case Super(_, name) => name.decodedName.toString
    }
  }

  protected implicit class RichFoundationHelperName(name: Name) {
    def looksLikeInfix = {
      val hasSymbolicName = !name.decoded.forall(c => Character.isLetter(c) || Character.isDigit(c) || c == '_')
      val idiomaticallyUsedAsInfix = name == nme.eq || name == nme.ne
      hasSymbolicName || idiomaticallyUsedAsInfix
    }
  }
}
