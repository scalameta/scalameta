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

  implicit class RichFoundationHelperTree[T <: Tree](tree: T) {
    def copyAttrs(other: Tree): T = tree.copyAttrs(other)
  }

  // NOTE: partially copy/pasted from TreeInfo.scala and ReificationSupport.scala in scalac
  object SyntacticTemplate {
    def unapply(in: Template): Some[(Symbol, List[Tree], ValDef, List[Tree], List[Tree])] = {
      object UnCtor {
        def unapply(tree: Tree): Option[(Modifiers, List[List[ValDef]], List[Tree], Symbol, List[List[Tree]])] = tree match {
          case DefDef(mods, nme.MIXIN_CONSTRUCTOR, _, _, _, build.SyntacticBlock(lvdefs :+ _)) =>
            Some((mods | Flag.TRAIT, Nil, lvdefs, NoSymbol, Nil))
          case DefDef(mods, nme.CONSTRUCTOR, Nil, vparamss, _, build.SyntacticBlock(lvdefs :+ Applied(core, _, argss) :+ _)) =>
            Some((mods, vparamss, lvdefs, core.symbol, argss))
          case DefDef(mods, nme.CONSTRUCTOR, Nil, List(Nil), _, Block(Nil, Literal(Constant(()))))
          if in.symbol.owner.isPrimitiveValueClass || in.symbol.owner == AnyValClass =>
            Some((mods, Nil, Nil, NoSymbol, Nil))
          case _ =>
            None
        }
      }
      def indexOfCtor(trees: List[Tree]) = {
        trees.indexWhere { case UnCtor(_, _, _, _, _) => true ; case _ => false }
      }
      val body1 = in.body // NOTE: filterBody and recoverBody moved to Ensugar
      val (rawEdefs, rest) = body1.span(treeInfo.isEarlyDef)
      val (gvdefs, etdefs) = rawEdefs.partition(treeInfo.isEarlyValDef)
      var (fieldDefs, lvdefs, superSym, superArgss, body2) = rest.splitAt(indexOfCtor(rest)) match {
        case (fieldDefs, UnCtor(_, _, lvdefs, superSym, superArgss) :: body2) => (fieldDefs, lvdefs, superSym, superArgss, body2)
        case (Nil, body2) => (Nil, Nil, NoSymbol, Nil, body2)
      }
      // TODO: really discern `... extends C()` and `... extends C`
      if (superArgss == List(Nil) && superSym.info.paramss.flatten.isEmpty) superArgss = Nil
      val evdefs = gvdefs.zip(lvdefs).map {
        case (gvdef @ ValDef(_, _, _, _), ValDef(_, _, tpt, rhs)) =>
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
      Some(superSym, parents, in.self, edefs, body2)
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

  object DesugaredSetter {
    def unapply(tree: Tree): Option[(Tree, Tree)] = tree.metadata.get("originalAssign").map(_.asInstanceOf[(Tree, Name, List[Tree])]) match {
      case Some((Desugared(lhs), name: TermName, Desugared(List(rhs)))) if nme.isSetterName(tree.symbol.name) && name == nme.EQL =>
        require(lhs.symbol.name.setterName == tree.symbol.name)
        Some((lhs, rhs))
      case _ =>
        None
    }
  }

  object DesugaredUpdate {
    def unapply(tree: Tree): Option[(Tree, List[List[Tree]], Tree)] = tree match {
      case Apply(core @ Select(lhs, _), args :+ rhs)
      if core.symbol.name == nme.update && !tree.hasMetadata("originalAssign") && !lhs.isInstanceOf[Super] && (args :+ rhs).forall(!_.isInstanceOf[AssignOrNamedArg]) =>
        Some((lhs, List(args), rhs))
      case _ =>
        None
    }
  }

  object DesugaredOpAssign {
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree.metadata.get("originalAssign").map(_.asInstanceOf[(Tree, Name, List[Tree])]) match {
      // TODO: can't use `args` here, because they might be untyped, so we have to workaround
      // case Some((Desugared(qual), name: TermName, Desugared(args))) if name != nme.EQL =>
      case Some((Desugared(qual), name: TermName, _)) if name != nme.EQL =>
        def extractArgs(tree: Tree): List[Tree] = tree match {
          case Assign(_, Apply(_, args)) => args // case #1 of convertToAssignment
          case Block(_, expr) => extractArgs(expr) // case #2 of convertToAssignment
          case Apply(_, _ :+ Apply(_, args)) => args // case #3 of convertToAssignment
        }
        val args = extractArgs(tree)
        Some((Select(qual, name).setType(NoType), args))
      case _ => None
    }
  }

  object DesugaredApply {
    private object Target {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case Select(target, _) if !target.isInstanceOf[Super] => Some(target)
        case tree @ TypeApply(Select(target, _), targs) if !target.isInstanceOf[Super] => Some(treeCopy.TypeApply(tree, target, targs))
        case _ => None
      }
    }
    // TODO: make `metadata.get("originalApplee")` on `fn` work
    // until this is implemented, I'm putting a workaround in place
    def unapply(tree: Tree): Option[(Tree, List[Tree])] = tree match {
      case Apply(Target(target), args) if tree.symbol.name == nme.apply && tree.symbol.paramss.nonEmpty => Some((target, args))
      case _ => None
    }
  }

  object DesugaredSymbolLiteral {
    def unapply(tree: Tree): Option[scala.Symbol] = tree match {
      case DesugaredApply(module, List(Literal(Constant(s_value: String)))) if module.symbol == SymbolModule => Some(scala.Symbol(s_value))
      case _ => None
    }
  }

  object DesugaredTuple {
    def unapply(tree: Tree): Option[List[Tree]] = tree match {
      case DesugaredApply(module, args) if module.symbol != null && TupleClass.seq.contains(module.symbol.companion) && args.length > 1 => Some(args)
      case _ => None
    }
  }

  object DesugaredInterpolation {
    def unapply(tree: Tree): Option[(Select, List[Tree], List[Tree])] = tree match {
      case Apply(fn @ q"$stringContext.apply(..$parts).$prefix", args) if stringContext.symbol == StringContextClass.companion => Some((fn.asInstanceOf[Select], parts, args))
      case DesugaredApply(fn @ q"$stringContext.apply(..$parts).$prefix", args) if stringContext.symbol == StringContextClass.companion => Some((fn.asInstanceOf[Select], parts, args))
      case _ => None
    }
  }

  sealed trait Enum
  case class Generator(pat: Tree, rhs: Tree) extends Enum
  case class Val(pat: Tree, rhs: Tree) extends Enum
  case class Guard(cond: Tree) extends Enum

  object DesugaredFor {
    def unapply(tree: Tree): Option[(List[Enum], Tree, Boolean)] = {
      // TODO: looks like a bug / tree shape incompatibility in UnFor
      // seemingly innocent `(l1, l2) = i` produces four SyntacticValEq's
      // (x$2 @ (_: <empty>)) = i: @scala.unchecked match {
      //   case (x$1 @ scala.Tuple2((l1 @ _), (l2 @ _))) => scala.Tuple3(x$1, l1, l2)
      // }
      // (x$1 @ (_: <empty>)) = x$2._1
      // (l1 @ (_: <empty>)) = x$2._2
      // (l2 @ (_: <empty>)) = x$2._3
      def postprocessSyntacticEnums(enums: List[Tree]): List[Tree] = {
        val buf = mutable.ListBuffer[Tree]()
        var i = 0
        while (i < enums.length) {
          val enum = enums(i)
          i += 1
          enum match {
            case SyntacticValEq(
                Bind(name, Typed(Ident(nme.WILDCARD), EmptyTree)),
                Match(Annotated(_, rhs), List(CaseDef(pat, _, marker)))) =>
              val skip = definitions.TupleClass.seq.indexOf(marker.tpe.typeSymbolDirect) + 1
              require(skip > 0)
              i += skip
              buf += SyntacticValEq(pat, rhs)
            case _ =>
              buf += enum
          }
        }
        buf.toList
      }
      // TODO: looks like there's another problem in UnFor that sometimes appends (or fails to remove) extraneous refutability checks
      // I don't have time to catch it right now, so I'm just getting rid of those checks as a workaround
      object DropRefutabilityChecks {
        def unapply(tree: Tree): Some[Tree] = tree match {
          case Apply(
            Select(tree, nme.withFilter),
            List(Function(List(ValDef(_, name, _, _)), _)))
          if name.startsWith(nme.CHECK_IF_REFUTABLE_STRING) =>
            unapply(tree)
          case _ =>
            Some(tree)
        }
      }
      // TODO: not sure whether it's a bug or a feature, but SyntacticValFrom and SyntacticValEq sometimes produce weird trees, e.g.:
      // (x$1 @ scala.Tuple2((i1 @ _), (i2 @ _))) <- List(scala.Tuple2(1, 2), scala.Tuple2(2, 3))
      // (j @ (_: <empty>)) = i1
      // Again, there's too much to do and too little time, so I'm going to work around
      // upd. Actually, the EmptyTree thingie is the result of Ensugar, which translates all originless TypeTrees to EmptyTree
      // It looks like we'll have to copy/paste the entire UnFor here and adapt it accordingly
      object DropUselessPatterns {
        def unapply(tree: Tree): Some[Tree] = tree match {
          case tree @ Bind(name, pat) if name.startsWith("x$") => unapply(pat)
          case tree @ Bind(name, Typed(wild @ Ident(nme.WILDCARD), EmptyTree)) => unapply(treeCopy.Bind(tree, name, wild))
          case tree @ Bind(nme.WILDCARD, pat @ Ident(nme.WILDCARD)) => Some(pat)
          case tree => Some(tree)
        }
      }
      // NOTE: UnClosure creates binds that are totally unattributed
      // here we somewhat approximate the attributes to at least give the host an idea of what's going on
      def approximateAttributes(pat: Tree, rhs: Tree): Tree = pat match {
        case pat @ Ident(nme.WILDCARD) if pat.tpe == null && pat.symbol == NoSymbol =>
          Ident(nme.WILDCARD).setType(rhs.tpe)
        case pat @ Bind(name: TermName, Ident(nme.WILDCARD)) if pat.tpe == null && pat.symbol == NoSymbol =>
          Bind(NoSymbol.newTermSymbol(name).setInfo(rhs.tpe), Ident(nme.WILDCARD).setType(rhs.tpe)).setType(rhs.tpe)
        case pat =>
          pat
      }
      def extractOurEnums(enums: List[Tree]): List[Enum] = enums map {
        case SyntacticValFrom(DropUselessPatterns(pat), DropRefutabilityChecks(rhs)) => Generator(approximateAttributes(pat, rhs), rhs)
        case SyntacticValEq(DropUselessPatterns(pat), DropRefutabilityChecks(rhs)) => Val(approximateAttributes(pat, rhs), rhs)
        case SyntacticFilter(tree) => Guard(tree)
      }
      tree match {
        case SyntacticFor(enums, body) => Some((extractOurEnums(postprocessSyntacticEnums(enums)), body, false))
        case SyntacticForYield(enums, body) => Some((extractOurEnums(postprocessSyntacticEnums(enums)), body, true))
        case _ => None
      }
    }
  }
}