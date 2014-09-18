package scala
package org.scalameta.reflection

import scala.tools.nsc.Global
import scala.reflect.internal.Flags

trait Helpers {
  self: _root_.org.scalameta.reflection.GlobalToolkit =>

  import global._
  import definitions._
  import treeInfo._

  implicit class RichHelperTree[T <: Tree](tree: T) {
    def copyAttrs(other: Tree): T = tree.copyAttrs(other)
  }

  // NOTE: partially copy/pasted from TreeInfo.scala and ReificationSupport.scala in scalac
  object SyntacticTemplate {
    def unapply(in: Template): Some[(List[Tree], ValDef, List[Tree], List[Tree])] = {
      def filterBody(body: List[Tree]) = body filter {
        case _: ValDef | _: TypeDef => true
        // keep valdef or getter for val/var
        case dd: DefDef if dd.mods.hasAccessorFlag => !nme.isSetterName(dd.name) && !in.body.exists {
          case vd: ValDef => dd.name == vd.name.dropLocal
          case _ => false
        }
        case md: MemberDef => !md.mods.isSynthetic
        case tree => true
      }
      def lazyValDefRhs(body: Tree) = {
        body match {
          case Block(List(Assign(_, rhs)), _) => rhs
          case _ => body
        }
      }
      def recoverBody(body: List[Tree]) = body map {
        case vd @ ValDef(vmods, vname, _, vrhs) if nme.isLocalName(vname) =>
          in.body find {
            case dd: DefDef => dd.name == vname.dropLocal
            case _ => false
          } map { dd =>
            val DefDef(dmods, dname, _, _, _, drhs) = dd
            // get access flags from DefDef
            val vdMods = (vmods &~ Flags.AccessFlags) | (dmods & Flags.AccessFlags).flags
            // for most cases lazy body should be taken from accessor DefDef
            val vdRhs = if (vmods.isLazy) lazyValDefRhs(drhs) else vrhs
            copyValDef(vd)(mods = vdMods, name = dname, rhs = vdRhs)
          } getOrElse (vd)
        // for abstract and some lazy val/vars
        case dd @ DefDef(mods, name, _, _, tpt, rhs) if mods.hasAccessorFlag =>
          // transform getter mods to field
          val vdMods = (if (!mods.hasStableFlag) mods | Flags.MUTABLE else mods &~ Flags.STABLE) &~ Flags.ACCESSOR
          ValDef(vdMods, name, tpt, rhs)
        case tr => tr
      }
      object UnCtor {
        def unapply(tree: Tree): Option[(Modifiers, List[List[ValDef]], List[Tree], Symbol, List[List[Tree]])] = tree match {
          case DefDef(mods, nme.MIXIN_CONSTRUCTOR, _, _, _, build.SyntacticBlock(lvdefs :+ _)) =>
            Some((mods | Flag.TRAIT, Nil, lvdefs, NoSymbol, Nil))
          case DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, _, build.SyntacticBlock(lvdefs :+ Applied(core, _, argss) :+ _)) =>
            Some((mods, vparamss, lvdefs, core.symbol, argss))
          case _ => None
        }
      }
      def indexOfCtor(trees: List[Tree]) = {
        trees.indexWhere { case UnCtor(_, _, _, _, _) => true ; case _ => false }
      }
      val body1 = recoverBody(filterBody(in.body))
      val (rawEdefs, rest) = body1.span(treeInfo.isEarlyDef)
      val (gvdefs, etdefs) = rawEdefs.partition(treeInfo.isEarlyValDef)
      var (fieldDefs, lvdefs, superSym, superArgss, body2) = rest.splitAt(indexOfCtor(rest)) match {
        case (fieldDefs, UnCtor(_, _, lvdefs, superSym, superArgss) :: body2) => (fieldDefs, lvdefs, superSym, superArgss, body2)
        case (Nil, body2) => (Nil, Nil, NoSymbol, Nil, body2)
      }
      // TODO: really discern `... extends C()` and `... extends C`
      if (superArgss == List(Nil) && superSym.info.paramss.flatten.isEmpty) superArgss = Nil
      val evdefs = gvdefs.zip(lvdefs).map {
        case (gvdef @ ValDef(_, _, tpt, _), ValDef(_, _, _, rhs)) =>
          copyValDef(gvdef)(tpt = tpt, rhs = rhs)
      }
      val edefs = evdefs ::: etdefs
      val parents = in.parents match {
        case firstParent +: otherParents =>
          val firstParent1 = superArgss.foldLeft(firstParent)((curr, args) => Apply(curr, args).setType(firstParent.tpe).appendScratchpad(superSym))
          firstParent1 +: otherParents
        case Nil =>
          Nil
      }
      Some(parents, in.self, edefs, body2)
    }
  }

  object collapseEmptyTrees extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case tree @ PackageDef(pid, stats) if stats.exists(_.isEmpty) => transform(treeCopy.PackageDef(tree, pid, stats.filter(!_.isEmpty)))
      case tree @ Block(stats, expr) if stats.exists(_.isEmpty) => transform(treeCopy.Block(tree, stats.filter(!_.isEmpty), expr))
      case tree @ Template(parents, self, stats) if stats.exists(_.isEmpty) => transform(treeCopy.Template(tree, parents, self, stats.filter(!_.isEmpty)))
      case _ => super.transform(tree)
    }
  }

  object ApplyImplicitView {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case tree: ApplyImplicitView => Some((tree.fun, tree.args.head))
      case _ => None
    }
  }

  object ApplyToImplicitArgs {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case tree: ApplyToImplicitArgs => Some((tree.fun, tree.args))
      case _ => None
    }
  }
}