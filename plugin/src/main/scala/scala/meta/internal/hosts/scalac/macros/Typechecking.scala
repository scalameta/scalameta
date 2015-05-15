package scala.meta
package internal.hosts.scalac
package macros

import scala.reflect.internal.Flags._
import scala.meta.internal.hosts.scalac.{PluginBase => ScalahostPlugin}

trait Typechecking {
  self: ScalahostPlugin =>

  import global._
  import definitions._
  import treeInfo._
  import analyzer.{MacroPlugin => NscMacroPlugin, _}
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  object ScalametaMacro {
    // TODO: would be nice if we had a way to know whether `tree` is a collapsed single-element block
    // then we could reliably discern vanilla macro syntax `def foo = macro bar` and scala.meta macro syntax `def foo = macro { ... }`
    // for now we have to approximate, and maybe later we could make this approximation more precise
    object ScalametaMacroBody {
      def unapply(tree: Tree): Option[Tree] = {
        def isVanillaBody(tree: Tree): Boolean = tree match {
          case Ident(_) => true
          case Select(_, _) => true
          case TypeApply(fn, _) => isVanillaBody(fn)
          case _ => false
        }
        if (isVanillaBody(tree)) None else Some(tree)
      }
    }
    def unapply(tree: Tree): Option[(Modifiers, TermName, List[TypeDef], List[List[ValDef]], Tree, Tree)] = {
      tree match {
        case DefDef(mods, name, tparams, vparamss, tpt, ScalametaMacroBody(body)) if mods.hasFlag(MACRO) =>
          Some((mods, name, tparams, vparamss, tpt, body))
        case _ =>
          None
      }
    }
  }

  // TODO: this is temporary until we implement an interpreter for scala.meta trees
  private def scalahostMacroImpl(ddef: DefDef): DefDef = {
    val ScalametaMacro(_, name, tparams, vparamss, _, body) = duplicateAndKeepPositions(ddef)
    def cleanupMods(mods: Modifiers) = mods &~ IMPLICIT
    val vtparams = tparams match {
      case Nil => Nil
      case tparams => List(tparams.map(tparam => q"val ${tparam.name.toTermName}: _root_.scala.meta.Type"))
    }
    val vparamss1 = vtparams ++ mmap(vparamss) {
      case p @ q"$mods val $pname: $_ = $_" =>
        val p1 = atPos(p.pos)(q"${cleanupMods(mods)} val $pname: _root_.scala.meta.Term")
        if (isRepeated(p.symbol)) copyValDef(p1)(tpt = tq"_root_.scala.<repeated>[${p1.tpt}]") else p1
    }
    val c = q"implicit val ${TermName("c$" + globalFreshNameCreator.newName(""))}: _root_.scala.meta.semantic.Context"
    atPos(ddef.pos)(q"def $name(...$vparamss1)(implicit $c): _root_.scala.meta.Term = $body")
  }

  def scalahostEnterStats(typer: Typer, stats: List[Tree]): List[Tree] = stats.flatMap {
    case ddef @ ScalametaMacro(_, _, _, _, _, _) if !ddef.symbol.owner.isStaticOwner =>
      typer.context.error(ddef.pos, "implementation restriction: until the TASTY interpreter is implemented, scala.meta macros can only be defined in static objects")
      List(ddef)
    case ddef @ ScalametaMacro(_, name, _, _, _, _) =>
      val implDdef = copyDefDef(scalahostMacroImpl(ddef.asInstanceOf[DefDef]))(mods = Modifiers(PRIVATE), name = TermName(name + "$impl"))
      newNamer(typer.context).enterSym(implDdef)
      List(implDdef, ddef)
    case other =>
      List(other)
  }

  def scalahostTypedMacroBody(typer: Typer, ddef: DefDef): Option[Tree] = {
    ddef match {
      case ScalametaMacro(_, _, _, _, TypeTree(), _) =>
        typer.context.error(ddef.pos, "scala.meta macros must have explicitly specified return types")
        Some(EmptyTree)
      case ScalametaMacro(_, _, _, _, _, _) =>
        val q"{ ${typedImplDdef: DefDef}; () }" = typer.typed(q"{ ${scalahostMacroImpl(ddef)}; () }")
        if (typedImplDdef.exists(_.isErroneous)) {
          if (ddef.symbol != null) ddef.symbol setFlag IS_ERROR
          ddef setType ErrorType
        } else {
          // NOTE: order is actually very important here, because at the end of the day
          // we need the legacy annotation to come first so that it can be picked up by the 2.11.x macro engine
          // (otherwise half of the standard macro infrastructure will cease to function)
          ddef.symbol.addAnnotation(MacroImplAnnotation, ScalahostSignature(typedImplDdef))
          ddef.symbol.addAnnotation(MacroImplAnnotation, LegacySignature())
        }
        Some(EmptyTree)
      case _ =>
        None
    }
  }

  object LegacySignature extends FixupSignature {
    def apply(): Tree = fixup(Apply(Ident(TermName("macro")), List(Assign(Literal(Constant("macroEngine")), Literal(Constant("Scalahost experimental macro engine compatible with scala.meta APIs"))))))
  }

  object ScalahostSignature extends FixupSignature {
    def apply(implDdef: DefDef): Tree = {
      fixup(Apply(Ident(TermName("ScalametaMacro")), List(
        Assign(Literal(Constant("implDdef")), implDdef))))
    }
    def unapply(tree: Tree): Option[DefDef] = {
      tree match {
        case Apply(Ident(TermName("ScalametaMacro")), List(
          Assign(Literal(Constant("implDdef")), (implDdef: DefDef)))) => Some(implDdef)
        case _ => None
      }
    }
  }

  // NOTE: this fixup is necessary for signatures to be accepted as annotations
  // if we don't set types in annotations, then pickler is going to crash
  // apart from constants, it doesn't really matter what types we assign, so we just go for NoType
  trait FixupSignature {
    protected def fixup(tree: Tree): Tree = {
      new Transformer {
        override def transform(tree: Tree) = {
          tree match {
            case Literal(const @ Constant(x)) if tree.tpe == null => tree setType ConstantType(const)
            case _ if tree.tpe == null => tree setType NoType
            case _ => ;
          }
          super.transform(tree)
        }
      }.transform(tree)
    }
  }

  def scalahostIsBlackbox(macroDef: Symbol): Option[Boolean] = {
    val macroSignatures = macroDef.annotations.filter(_.atp.typeSymbol == MacroImplAnnotation)
    macroSignatures match {
      case _ :: AnnotationInfo(_, List(ScalahostSignature(_)), _) :: Nil => Some(true)
      case _ => None
    }
  }
}
