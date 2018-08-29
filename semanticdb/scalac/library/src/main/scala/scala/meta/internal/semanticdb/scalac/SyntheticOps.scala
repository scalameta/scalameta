package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.inputs._

trait SyntheticOps { self: SemanticdbOps =>
  import g._

  implicit class XtensionGTreeSTree(gTree: g.Tree) {

    def toSemanticTree: s.Tree = gTree match {
      case gTree: g.Apply =>
        s.ApplyTree(
          function = gTree.fun.toSemanticId,
          arguments = gTree.args.map(_.toSemanticTree)
        )
      case gTree: g.TypeApply =>
        s.TypeApplyTree(
          function = gTree.fun.toSemanticTree,
          typeArguments = gTree.args.map(_.tpe.toSemanticTpe)
        )
      case gTree: g.Select => gTree.toSemanticId
      case gTree: g.Ident => gTree.toSemanticId
      case gTree: g.This => gTree.toSemanticId
      case gTree: g.Typed if gTree.hasAttachment[g.analyzer.MacroExpansionAttachment] =>
        val expandeeTree = gTree.attachments.get[g.analyzer.MacroExpansionAttachment].get.expandee
        val beforeExpansion =
          if (expandeeTree.pos.isRange) expandeeTree.toSemanticOriginal
          else expandeeTree.toSemanticTree
        s.MacroExpansionTree(
          beforeExpansion = beforeExpansion,
          tpe = gTree.tpt.tpe.toSemanticTpe
        )
      case _ =>
        s.NoTree
    }

    def toSemanticId: s.IdTree = s.IdTree(symbol = gTree.symbol.toSemantic)

    def toSemanticOriginal: s.Tree = s.OriginalTree(
      range = Some(gTree.pos.toMeta.toRange)
    )

  }

}
