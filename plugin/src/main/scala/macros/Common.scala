package scala.reflect.internal.hosts
package scalacompiler
package macros

import scala.tools.nsc.Global

trait Common extends Printers {
  val global: Global
  import global._

  // TODO: think what to do with importers
  // ImportableAttachment is kinda screwed up, so I didn't implement it right off the bat
  private case class PalladiumMacroAttachment(isBlackbox: Boolean, rhs: Tree)

  object PalladiumMacro {
    def apply(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], isBlackbox: Boolean, tpt: Tree, rhs: Tree): DefDef = {
      val ddef = DefDef(mods, name.toTermName, tparams, vparamss, tpt, EmptyTree)
      ddef.updateAttachment(PalladiumMacroAttachment(isBlackbox, rhs))
    }
    def unapply(ddef: DefDef): Option[(Modifiers, TermName, List[TypeDef], List[List[ValDef]], Boolean, Tree, Tree)] = {
      ddef.attachments.get[PalladiumMacroAttachment].map(att => (ddef.mods, ddef.name, ddef.tparams, ddef.vparamss, att.isBlackbox, ddef.tpt, att.rhs))
    }
  }

  private def palladiumMacroEngine = "Palladium experimental macro engine"
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
  object LegacySignature extends FixupSignature {
    def apply(): Tree = fixup(Apply(Ident(TermName("macro")), List(Assign(Literal(Constant("macroEngine")), Literal(Constant(palladiumMacroEngine))))))
  }
  object PalladiumSignature extends FixupSignature {
    def apply(isBlackbox: Boolean, implDdef: DefDef): Tree = {
      fixup(Apply(Ident(TermName("palladiumMacro")), List(
        Assign(Literal(Constant("isBlackbox")), Literal(Constant(isBlackbox))),
        Assign(Literal(Constant("implDdef")), implDdef))))
    }
    def unapply(tree: Tree): Option[(Boolean, DefDef)] = {
      tree match {
        case Apply(Ident(TermName("palladiumMacro")), List(
          Assign(Literal(Constant("isBlackbox")), Literal(Constant(isBlackbox: Boolean))),
          Assign(Literal(Constant("implDdef")), (implDdef: DefDef)))) => Some((isBlackbox, implDdef))
        case _ => None
      }
    }
  }
}