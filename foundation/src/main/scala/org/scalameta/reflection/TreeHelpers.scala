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
    def displayName: String = displayName(EmptyTree)
    def displayName(parent: Tree): String = {
      def packageName(tree: ModuleDef): Name = {
        if (tree.symbol != NoSymbol) tree.symbol.owner.name
        else parent match {
          case _: PackageDef => TermName(parent.displayName)
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
    def displayName = {
      // NOTE: "<empty>", the internal name for empty package, isn't a valid Scala identifier, so we hack around
      if (name == rootMirror.EmptyPackage.name) "_empty_"
      else if (name == rootMirror.EmptyPackageClass.name) "_empty_"
      else if (name.isAnonymous) "_"
      else name.decodedName.toString
    }
  }

  class ParameterOps(tparams: List[TypeDef], vparamss: List[List[ValDef]]) {
    private def referencedTparam(targ: Tree): Option[TypeDef] = tparams.filter(tparam => {
      if (tparam.symbol != NoSymbol) tparam.symbol == targ.symbol
      else targ match { case Ident(name) => name == tparam.name; case _ => false }
    }).headOption

    object ViewBound {
      def unapply(tree: ValDef): Option[(TypeDef, Tree)] = tree match {
        case ValDef(_, _, tpt @ TypeTree(), _) =>
          val tycon = if (tpt.tpe != null) tpt.tpe.typeSymbolDirect else NoSymbol
          val targs = if (tpt.tpe != null) tpt.tpe.typeArgs else Nil
          val (fromTpe, toTpe) = targs match { case List(from, to) => (from, to); case _ => (NoType, NoType) }
          val fromSym = fromTpe.typeSymbol
          val tyconMatches = tycon == definitions.FunctionClass(1)
          if (tyconMatches) referencedTparam(Ident(fromSym)).map(tparam => (tparam, TypeTree(toTpe)))
          else None
        case ValDef(_, _, AppliedTypeTree(tycon, from :: to :: Nil), _) =>
          val tyconMatches = tycon match {
            // NOTE: this doesn't handle every possible case (e.g. an Ident binding to renamed import),
            // but it should be ood enough for 95% of the situations
            case Select(pre, TermName("Function1")) => pre.symbol == definitions.ScalaPackage
            case tycon: RefTree => tycon.symbol == definitions.FunctionClass(1)
            case _ => false
          }
          if (tyconMatches) referencedTparam(from).map(tparam => (tparam, to))
          else None
        case _ =>
          None
      }
    }

    object ContextBound {
      def unapply(tree: ValDef): Option[(TypeDef, Tree)] = tree match {
        case ValDef(_, _, tpt @ TypeTree(), _) =>
          val tycon = if (tpt.tpe != null) tpt.tpe.typeSymbolDirect else NoSymbol
          val targs = if (tpt.tpe != null) tpt.tpe.typeArgs else Nil
          val targ = targs.map(_.typeSymbol) match { case List(sym) => sym; case _ => NoSymbol }
          referencedTparam(Ident(targ)).map(tparam => (tparam, Ident(tycon)))
        case ValDef(_, _, AppliedTypeTree(tycon, targ :: Nil), _) =>
          referencedTparam(targ).map(tparam => (tparam, tycon))
        case _ =>
          None
      }
    }
  }

  class ClassParameterOps(tree: Tree, parents: List[Tree]) {
    def isClassParameter = ???
  }
}
